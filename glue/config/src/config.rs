use crate::schema::{GlueConfigSchema, GlueConfigSchemaGenConfigFiles};

pub type GlueConfig = GlueConfigSchema;

impl GlueConfig {
    pub fn from_json(json_str: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json_str)
    }

    pub fn from_yaml(yaml_str: &str) -> Result<Self, serde_yaml::Error> {
        serde_yaml::from_str(yaml_str)
    }
}

impl GlueConfigSchemaGenConfigFiles {
    pub fn as_globs(&self) -> Vec<String> {
        match self {
            Self::String(value) => vec![value.clone()],
            Self::StringArray(values) => values.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::GlueConfig;

    #[test]
    fn from_yaml_accepts_string_and_list_files() {
        let config = GlueConfig::from_yaml(
            r#"
gen:
  - mode: typescript
    files: "models/*.glue"
  - mode: python
    files:
      - "schemas/*.glue"
      - "shared/*.glue"
"#,
        )
        .expect("config should parse");

        let entries = config.r#gen.expect("gen entries should exist");
        assert_eq!(entries[0].files.as_globs(), vec!["models/*.glue"]);
        assert_eq!(entries[1].files.as_globs(), vec!["schemas/*.glue", "shared/*.glue"]);
    }

    #[test]
    fn from_json_accepts_string_files() {
        let config = GlueConfig::from_json(
            r#"
{
  "gen": [
    {
      "mode": "typescript",
      "files": "models/*.glue"
    }
  ]
}
"#,
        )
        .expect("config should parse");

        let entries = config.r#gen.expect("gen entries should exist");
        assert_eq!(entries[0].files.as_globs(), vec!["models/*.glue"]);
    }
}
