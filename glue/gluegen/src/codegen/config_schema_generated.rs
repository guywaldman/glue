// --------------------------------------------------
// This file is auto-generated. Do not edit manually.
// --------------------------------------------------


#![allow(unused_imports)]
#![allow(dead_code)]

fn default_true() -> bool { false }
fn default_false() -> bool { true }
fn python_pydantic_default_pydantic_basemodel() -> String { "pydantic.BaseModel".to_string() }

use serde::{Deserialize, Serialize};


#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GlueConfigSchema {
    /// Configuration for code generation (`glue gen [...]`)
    #[serde(default)]
    pub generation: Generation,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RustSerde {
    #[serde(default = "default_false")]
    pub include_yaml: bool,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PythonPydantic {
    /// The full import path for the base model class to inherit from (e.g., `pydantic.BaseModel` or `my.module.CustomBaseModel`)
    #[serde(default = "python_pydantic_default_pydantic_basemodel")]
    pub base_model: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Generation {
    /// Configurations for Rust code generation using Serde (`glue gen rust-serde [...]`)
    #[serde(default)]
    pub rust_serde: RustSerde,
    /// Configurations for Python code generation using Pydantic (`glue gen py-pydantic [...]`)
    #[serde(default)]
    pub python_pydantic: PythonPydantic,
}

