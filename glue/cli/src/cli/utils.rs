use anyhow::{Context, Result};
use std::path::PathBuf;

use crate::codegen::{GlueConfigSchema, GlueConfigSchemaGeneration, GlueConfigSchemaGenerationPythonPydantic, GlueConfigSchemaGenerationWatermark};

pub fn read_config(path: Option<&PathBuf>) -> Result<GlueConfigSchema> {
    let Some(path) = path else {
        return Ok(GlueConfigSchema {
            generation: GlueConfigSchemaGeneration {
                watermark: GlueConfigSchemaGenerationWatermark::None,
                python_pydantic: GlueConfigSchemaGenerationPythonPydantic {
                    base_model: "pydantic.BaseModel".to_string(),
                },
            },
        });
    };
    let config_contents = std::fs::read_to_string(path).with_context(|| format!("failed to read config file '{}'", path.display()))?;
    let config = serde_yaml::from_str(&config_contents).with_context(|| format!("failed to parse config file '{}'", path.display()))?;
    Ok(config)
}
