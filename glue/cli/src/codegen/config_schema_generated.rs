#![allow(unused_imports)]
#![allow(dead_code)]

fn glue_config_schema_generation_watermark_default() -> GlueConfigSchemaGenerationWatermark {
    GlueConfigSchemaGenerationWatermark::Short
}

fn default_false() -> bool {
    false
}

fn glue_config_schema_generation_python_pydantic_base_model_default() -> String {
    "pydantic.BaseModel".to_string()
}

use serde::{Deserialize, Serialize};
use serde_json::ser;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GlueConfigSchema {
    /// Configuration for code generation (`glue gen [...]`)
    #[serde(default)]
    pub generation: GlueConfigSchemaGeneration,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GlueConfigSchemaGeneration {
    /// Configurations for Python code generation using Pydantic (`glue gen py-pydantic [...]`)
    #[serde(default)]
    pub python_pydantic: GlueConfigSchemaGenerationPythonPydantic,
    /// Configurations for Rust code generation using Serde (`glue gen rust-serde [...]`)
    #[serde(default)]
    pub rust_serde: GlueConfigSchemaGenerationRustSerde,
    /// Mode for the watermark at the top of the generated files
    #[serde(default = "glue_config_schema_generation_watermark_default")]
    pub watermark: GlueConfigSchemaGenerationWatermark,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GlueConfigSchemaGenerationWatermark {
    #[serde(rename = "full")]
    Full,
    #[serde(rename = "short")]
    Short,
    #[serde(rename = "none")]
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GlueConfigSchemaGenerationRustSerde {
    #[serde(default = "default_false")]
    pub include_yaml: bool,
}
impl Default for GlueConfigSchemaGenerationRustSerde {
    fn default() -> Self {
        Self { include_yaml: default_false() }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GlueConfigSchemaGenerationPythonPydantic {
    /// The full import path for the base model class to inherit from (e.g., `pydantic.BaseModel` or `my.module.CustomBaseModel`)
    #[serde(default = "glue_config_schema_generation_python_pydantic_base_model_default")]
    pub base_model: String,
}
impl Default for GlueConfigSchemaGenerationPythonPydantic {
    fn default() -> Self {
        Self {
            base_model: glue_config_schema_generation_python_pydantic_base_model_default(),
        }
    }
}

impl Default for GlueConfigSchemaGeneration {
    fn default() -> Self {
        Self {
            python_pydantic: Default::default(),
            rust_serde: Default::default(),
            watermark: glue_config_schema_generation_watermark_default(),
        }
    }
}

impl Default for GlueConfigSchema {
    fn default() -> Self {
        Self { generation: Default::default() }
    }
}
