set shell := ["bash", "-lc"]

default:
	@just --list

assets_dir := "assets"
config_schema := "assets/config_schema.glue"

# Build the Rust workspace with all features.
build:
	cd glue && cargo build --workspace --all-features

# Build the wasm package and sync generated schema bindings.
build-wasm:
	cd glue && cargo run --bin glue -- gen typescript -i {{config_schema}} -o {{assets_dir}}/config_schema.ts
	cd glue/wasm && wasm-pack build --release --target web
	cp glue/assets/config_schema.ts glue/wasm/pkg/config_schema.ts
	cp glue/assets/config_schema.json glue/wasm/pkg/config_schema.json
	# Patch wasm.d.ts to use typed config from the Glue-generated config_schema.ts
	sed -i '' '1s/^/import type { GlueConfigSchema, GlueConfigSchemaGeneration } from ".\/config_schema";\n/' glue/wasm/pkg/wasm.d.ts
	sed -i '' 's/config\?: any | null/config?: GlueConfigSchemaGeneration | GlueConfigSchema | null/' glue/wasm/pkg/wasm.d.ts

# Generate schema artifacts from the config schema.
generate: build
	#!/usr/bin/env bash

	set -e

	cd glue
	cargo run --bin glue -- gen jsonschema -i {{config_schema}} -o {{assets_dir}}/config_schema.json

	config_rust_file="config/src/schema.rs"
	cargo run --bin glue -- gen rust -i {{config_schema}} -o $config_rust_file
	cargo fmt -- $config_rust_file

# Build and install the CLI locally.
install-cli:
	cd glue && cargo build --release --bin glue
	cd glue && cargo install --path cli --bin glue

# Run Rust workspace unit tests.
test-unit-cli:
	cd glue && cargo test --workspace --all-features

# Run CLI E2E tests.
test-e2e-cli:
	#!/usr/bin/env bash
	set -euo pipefail
	cd glue
	targets=$(find cli/tests -maxdepth 1 -type f -name 'e2e_tests*.rs' -exec basename {} .rs \; | sort)
	if [ -z "$targets" ]; then
	  echo "No E2E test targets found matching cli/tests/e2e_tests*.rs"
	  exit 1
	fi

	for target in $targets; do
	  echo "Running E2E target: $target"
	  cargo test --test "$target" -- --test-threads=1
	done

# Run codecoverage pipeline and generate lcov.info.
codecov:
	#!/usr/bin/env bash
	set -euo pipefail
	cd glue
	cargo llvm-cov --workspace --all-features --lcov --output-path lcov.info --quiet >/dev/null
	cargo llvm-cov report --summary-only --fail-under-lines "${COVERAGE_THRESHOLD:-60}"

# Run lint and formatting checks for Rust.
lint-cli:
	cd glue && cargo clippy --workspace --all
	cd glue && cargo fmt --all -- --check

# Auto-fix Rust formatting and clippy warnings.
fix-cli:
	cd glue && cargo fmt --all
	cd glue && cargo clippy --workspace --all --fix --allow-dirty --allow-staged

# Run VS Code extension tests.
test-extension:
	cd extension && npm install && npm test

# Package and install the extension locally for development.
extension-dev profile="":
	cd extension && rm -rf out && find . -maxdepth 1 -type f -name '*.vsix' -delete && npm install && npm run package && code --install-extension glue-*.vsix --force {{ if profile != "" { "--profile " + profile } else { "" } }}

# Publish the extension to the marketplace.
extension-publish:
	cd extension && npm install && npm test && npm run publish

# Run all precommit hooks.
precommit:
	prek run --all-files
