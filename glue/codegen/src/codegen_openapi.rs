use config::{GlueConfig, GlueConfigSchemaGenerationPythonPydantic};
use convert_case::Casing;
use lang::{AnalyzedProgram, AstNode, DiagnosticContext, Enum, EnumVariant, Field, LNode, LSyntaxKind, Model, PrimitiveType, SourceCodeMetadata, SymId, SymTable, TypeAtom};

use crate::{
    CodeGenError, CodeGenerator,
    codegen_utils::{indent_lines, qualified_symbol_name_to_case},
    types::CodeGenResult,
};

pub struct CodeGenOpenAPI;

// TODO: Refactor such that visitors also emit contributions, and similar refs are shared and not inlined
impl CodeGenerator for CodeGenOpenAPI {
    fn generate(&self, program: AnalyzedProgram, source: &SourceCodeMetadata, _config: Option<GlueConfig>) -> Result<String, crate::CodeGenError> {
        let ast = program.ast_root.clone();

        let mut codegen = CodeGeneratorImpl::new(ast, program.symbols, source);
        let output = codegen.generate()?;
        Ok(output)
    }
}

impl Default for CodeGenOpenAPI {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenOpenAPI {
    pub fn new() -> Self {
        Self
    }
}

struct CodeGeneratorImpl {
    #[allow(dead_code)]
    diag: DiagnosticContext,
    ast: LNode,
    syms: SymTable<LNode>,
    preludes: Vec<String>,
}

impl CodeGeneratorImpl {
    pub fn new(ast: LNode, syms: SymTable<LNode>, source: &SourceCodeMetadata) -> Self {
        let diag = DiagnosticContext::new(source.file_name, source.file_contents);
        Self {
            diag,
            ast,
            syms,
            preludes: Default::default(),
        }
    }

    pub fn generate(&mut self) -> CodeGenResult<String> {
        let mut output = String::new();

        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use insta::assert_snapshot;
    use lang::print_report;

    use crate::{CodeGenError, CodeGenerator, test_utils::analyze_glue_file};

    #[test]
    fn test() {}
}
