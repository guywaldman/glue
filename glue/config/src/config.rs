use crate::schema::GlueConfigSchema;

pub type GlueConfig = GlueConfigSchema;

impl GlueConfig {
    pub fn from_json(json_str: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str::<GlueConfigSchema>(json_str)
    }

    pub fn from_yaml(yaml_str: &str) -> Result<Self, serde_yaml::Error> {
        serde_yaml::from_str::<GlueConfigSchema>(yaml_str)
    }
}
