// TODO: Remove warning suppressions.
#![allow(dead_code, unused_imports, clippy::new_without_default)]

mod config;
mod schema;

pub use config::GlueConfig;
pub use schema::{
    GlueConfigSchema, GlueConfigSchemaGenConfig, GlueConfigSchemaGeneration, GlueConfigSchemaGenerationGo, GlueConfigSchemaGenerationProtobuf, GlueConfigSchemaGenerationPython,
    GlueConfigSchemaGenerationPythonDataModelLibrary, GlueConfigSchemaGenerationRust, GlueConfigSchemaGenerationTypeScript, GlueConfigSchemaGenerationWatermark, GlueConfigSchemaGlobal,
};
