use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

use codegen::{CodeGen, CodeGenMode};
use lang::SourceCodeMetadata;
use serde_json::json;

const FIXTURE_MAIN: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/benches/fixtures/perf/perf_main.glue");

fn extract_import_path(line: &str) -> Option<String> {
    let trimmed = line.trim_start();
    if !trimmed.starts_with("import ") || !trimmed.contains(" from ") {
        return None;
    }
    let from_idx = trimmed.find(" from ")?;
    let after_from = &trimmed[from_idx + 6..];
    let start_quote = after_from.find('"')?;
    let rest = &after_from[start_quote + 1..];
    let end_quote = rest.find('"')?;
    Some(rest[..end_quote].to_string())
}

fn extract_wildcard_alias(line: &str) -> Option<String> {
    let trimmed = line.trim_start();
    if !trimmed.starts_with("import * as ") {
        return None;
    }
    let rest = trimmed.strip_prefix("import * as ")?;
    let alias = rest.split_whitespace().next()?;
    if alias.is_empty() {
        return None;
    }
    Some(alias.to_string())
}

fn normalize_source_after_flatten(source: &str, aliases: &[String]) -> String {
    let mut transformed = source.to_string();
    for alias in aliases {
        transformed = transformed.replace(&format!("{alias}."), "");
    }

    let mut out = String::new();
    for line in transformed.lines() {
        if line.trim_start().starts_with("import ") {
            continue;
        }
        out.push_str(line);
        out.push('\n');
    }
    out
}

fn flatten_imports(path: &Path, visited: &mut HashSet<PathBuf>) -> String {
    let canonical = fs::canonicalize(path).expect("failed to canonicalize fixture path");
    if !visited.insert(canonical.clone()) {
        return String::new();
    }

    let source = fs::read_to_string(&canonical).expect("failed to read fixture file");
    let base_dir = canonical.parent().unwrap_or_else(|| Path::new("."));

    let mut combined = String::new();
    let mut aliases = Vec::new();
    for line in source.lines() {
        if let Some(import_path) = extract_import_path(line) {
            if let Some(alias) = extract_wildcard_alias(line) {
                aliases.push(alias);
            }
            let dep_path = base_dir.join(import_path);
            combined.push_str(&flatten_imports(&dep_path, visited));
            if !combined.ends_with('\n') {
                combined.push('\n');
            }
        }
    }

    combined.push_str(&normalize_source_after_flatten(&source, &aliases));
    combined
}

fn read_env_usize(key: &str, default: usize) -> usize {
    env::var(key).ok().and_then(|v| v.parse::<usize>().ok()).unwrap_or(default)
}

fn percentile_95(sorted_samples: &[f64]) -> f64 {
    if sorted_samples.is_empty() {
        return 0.0;
    }
    let idx = ((sorted_samples.len() as f64) * 0.95).ceil() as usize;
    sorted_samples[idx.saturating_sub(1)]
}

fn median(sorted_samples: &[f64]) -> f64 {
    let n = sorted_samples.len();
    if n == 0 {
        return 0.0;
    }
    if n % 2 == 1 {
        sorted_samples[n / 2]
    } else {
        (sorted_samples[n / 2 - 1] + sorted_samples[n / 2]) / 2.0
    }
}

fn count_models_fields_enums_endpoints(source: &str) -> (usize, usize, usize, usize) {
    let mut model_count = 0usize;
    let mut field_count = 0usize;
    let mut enum_count = 0usize;
    let mut endpoint_count = 0usize;
    let mut in_model = false;
    let mut brace_depth = 0usize;

    for line in source.lines() {
        let trimmed = line.trim();

        if trimmed.starts_with("enum ") {
            enum_count += 1;
        }
        if trimmed.starts_with("endpoint ") {
            endpoint_count += 1;
        }

        if trimmed.starts_with("model ") && trimmed.contains('{') {
            model_count += 1;
            in_model = true;
            brace_depth = 1;
            continue;
        }

        if in_model {
            brace_depth += trimmed.matches('{').count();
            brace_depth = brace_depth.saturating_sub(trimmed.matches('}').count());

            if brace_depth == 0 {
                in_model = false;
                continue;
            }

            if trimmed.is_empty() || trimmed.starts_with("//") || trimmed.starts_with("model ") {
                continue;
            }
            if trimmed.contains(':') {
                field_count += 1;
            }
        }
    }

    (model_count, field_count, enum_count, endpoint_count)
}

fn main() {
    let iterations = read_env_usize("GLUE_BENCH_ITERATIONS", 10);
    let warmups = read_env_usize("GLUE_BENCH_WARMUPS", 3);
    let main_path = PathBuf::from(FIXTURE_MAIN);
    if !main_path.exists() {
        panic!("missing benchmark fixture file: {}", main_path.display());
    }

    let mut stats_visited = HashSet::new();
    let flattened_for_stats = flatten_imports(&main_path, &mut stats_visited);
    let flattened_lines = flattened_for_stats.lines().count();
    let flattened_bytes = flattened_for_stats.len();
    let (model_count, field_count, enum_count, endpoint_count) = count_models_fields_enums_endpoints(&flattened_for_stats);
    let avg_fields_per_model = if model_count == 0 { 0.0 } else { field_count as f64 / model_count as f64 };

    let run = || {
        let mut visited = HashSet::new();
        let flattened = flatten_imports(&main_path, &mut visited);
        let source = SourceCodeMetadata {
            file_name: "perf_main.glue",
            file_contents: &flattened,
        };
        let generated = CodeGen::generate(CodeGenMode::Python, &source, None).expect("codegen failed in benchmark");
        std::hint::black_box(generated);
    };

    for _ in 0..warmups {
        run();
    }

    let mut samples_ms = Vec::with_capacity(iterations);
    for _ in 0..iterations {
        let started = Instant::now();
        run();
        samples_ms.push(started.elapsed().as_secs_f64() * 1000.0);
    }

    samples_ms.sort_by(|a, b| a.partial_cmp(b).expect("NaN in benchmark samples"));
    let med = median(&samples_ms);
    let p95 = percentile_95(&samples_ms);
    let min = *samples_ms.first().unwrap_or(&0.0);
    let max = *samples_ms.last().unwrap_or(&0.0);
    let mean = if samples_ms.is_empty() { 0.0 } else { samples_ms.iter().sum::<f64>() / samples_ms.len() as f64 };

    let payload = json!({
        "iterations": iterations,
        "warmups": warmups,
        "fixture": {
            "main": main_path,
            "source_files_count": stats_visited.len(),
            "flattened_model_count": model_count,
            "flattened_field_count": field_count,
            "flattened_enum_count": enum_count,
            "flattened_endpoint_count": endpoint_count,
            "flattened_avg_fields_per_model": avg_fields_per_model,
            "flattened_line_count": flattened_lines,
            "flattened_size_bytes": flattened_bytes,
        },
        "median_ms": med,
        "p95_ms": p95,
        "mean_ms": mean,
        "min_ms": min,
        "max_ms": max,
    });
    let json = serde_json::to_string_pretty(&payload).expect("failed to serialize benchmark output") + "\n";

    println!("{json}");

    if let Ok(out_path) = env::var("GLUE_BENCH_JSON_OUT") {
        let out = PathBuf::from(out_path);
        if let Some(parent) = out.parent() {
            fs::create_dir_all(parent).expect("failed to create benchmark output dir");
        }
        fs::write(out, json).expect("failed to write benchmark JSON output");
    }
}
