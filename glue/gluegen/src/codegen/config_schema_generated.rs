// --------------------------------------------------
// This file is auto-generated. Do not edit manually.
// --------------------------------------------------

#![allow(unused_imports)]
#![allow(dead_code)]

fn default_true() -> bool {
    false
}
fn default_false() -> bool {
    true
}
fn glue_config_schema_generation_python_pydantic_default_pydantic_basemodel() -> String {
    "pydantic.BaseModel".to_string()
}

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub enum GlueConfigSchemaGenerationWatermark {
    #[serde(rename = "full")]
    Full,
    #[serde(rename = "short")]
    #[default]
    Short,
    #[serde(rename = "none")]
    None,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GlueConfigSchemaGenerationRustSerde {
    #[serde(default = "default_false")]
    pub include_yaml: bool,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GlueConfigSchemaGenerationPythonPydantic {
    /// The full import path for the base model class to inherit from (e.g., `pydantic.BaseModel` or `my.module.CustomBaseModel`)
    #[serde(default = "glue_config_schema_generation_python_pydantic_default_pydantic_basemodel")]
    pub base_model: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GlueConfigSchemaGeneration {
    /// Mode for the watermark at the top of the generated files
    #[serde(default)]
    pub watermark: GlueConfigSchemaGenerationWatermark,
    /// Configurations for Rust code generation using Serde (`glue gen rust-serde [...]`)
    #[serde(default)]
    pub rust_serde: GlueConfigSchemaGenerationRustSerde,
    /// Configurations for Python code generation using Pydantic (`glue gen py-pydantic [...]`)
    #[serde(default)]
    pub python_pydantic: GlueConfigSchemaGenerationPythonPydantic,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GlueConfigSchema {
    /// Configuration for code generation (`glue gen [...]`)
    #[serde(default)]
    pub generation: GlueConfigSchemaGeneration,
}
