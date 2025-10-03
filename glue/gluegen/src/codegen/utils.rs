use crate::codegen::Watermark;

/// Generate watermark comments based on the specified mode.
/// The lines themselves are generated, they should be commented appropriately by the caller.
pub fn generate_watermark(mode: Watermark) -> Vec<String> {
    match mode {
        Watermark::None => vec![],
        Watermark::Short => vec!["This file is auto-generated. Do not edit manually.".to_string()],
        Watermark::Full => {
            let today = chrono::Utc::now().format("%Y-%m-%d").to_string();
            vec![
                "--------------------------------------------------".to_string(),
                "This file is auto-generated. Do not edit manually.".to_string(),
                format!("Generated on {today}"),
                "--------------------------------------------------".to_string(),
            ]
        }
    }
}
