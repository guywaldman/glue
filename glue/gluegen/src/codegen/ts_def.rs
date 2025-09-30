use gluelang::Program;

use crate::codegen::{CodeGen, CodeGenError};

pub struct TypeScriptDefCodeGen;

impl TypeScriptDefCodeGen {
    pub fn new() -> Self {
        TypeScriptDefCodeGen
    }
}

impl CodeGen for TypeScriptDefCodeGen {
    fn generate(&self, program: &Program) -> Result<String, CodeGenError> {
        let mut output = String::new();
        let models = program.models();
        for model in &models {
            if let Some(doc) = &model.doc {
                output.push_str("  ");
                output.push_str(self.generate_doc(doc)?.as_str());
            }
            output.push_str(&format!("interface {} {{\n", model.name));
            for field in &model.fields {
                let mut ty_strings = Vec::new();
                for atom in &field.ty.atoms {
                    let ty_str = match atom.name.as_str() {
                        "string" => "string".to_string(),
                        "int" => "number".to_string(),
                        "bool" => "boolean".to_string(),
                        other => {
                            if atom.is_ref {
                                if models.iter().any(|m| m.name == other) {
                                    // Check refs
                                    other.to_string()
                                } else {
                                    return Err(CodeGenError::UnresolvedReference(other.to_string()));
                                }
                            } else {
                                return Err(CodeGenError::UnsupportedError(format!(
                                    "TypeScript codegen does not support type: {other}"
                                )));
                            }
                        }
                    };
                    ty_strings.push(ty_str);
                }

                if let Some(doc) = &field.doc {
                    output.push_str("  ");
                    output.push_str(self.generate_doc(doc)?.as_str());
                }

                output.push_str(&format!("  {}: {};\n", field.name, ty_strings.join(" | ")));
            }
            output.push_str("}\n\n");
        }
        Ok(output)
    }
}

// Helper method
impl TypeScriptDefCodeGen {
    fn generate_doc(&self, doc: &str) -> Result<String, CodeGenError> {
        let lines_count = doc.lines().count();
        let mut output = String::with_capacity(doc.len() + lines_count * 5);
        if lines_count == 1 {
            output.push_str(&format!("/** {doc} */\n"));
        } else {
            output.push_str("/**\n");
            for line in doc.lines() {
                output.push_str(&format!(" * {line}\n"));
            }
            output.push_str(" */\n");
        }
        Ok(output)
    }
}
