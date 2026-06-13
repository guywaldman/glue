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

    #[test]
    fn from_yaml_accepts_rust_extra_derives() {
        let config = GlueConfig::from_yaml(
            r#"
global:
  config:
    rust:
      extra_derives:
        structs:
          - PartialEq
          - Eq
          - Hash
        enums:
          - Ord
          - PartialOrd
        unions:
          - PartialEq
"#,
        )
        .expect("config should parse");

        let extra_derives = config
            .global
            .and_then(|global| global.config)
            .and_then(|generation| generation.rust)
            .and_then(|rust| rust.extra_derives)
            .expect("rust extra derives should parse");
        assert_eq!(extra_derives.structs, Some(vec!["PartialEq".to_string(), "Eq".to_string(), "Hash".to_string()]));
        assert_eq!(extra_derives.enums, Some(vec!["Ord".to_string(), "PartialOrd".to_string()]));
        assert_eq!(extra_derives.unions, Some(vec!["PartialEq".to_string()]));
    }
}
