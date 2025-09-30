use crate::lexer::Span;
use crate::parser::{CompoundDataType, Field, Model, ParserError, Program};
use crate::utils;
use miette::{Diagnostic, LabeledSpan, NamedSource, SourceCode};
use std::collections::HashSet;
use std::{error::Error, fmt};

#[derive(Debug, Clone)]
pub enum AnalyzerError {
    SemanticErrors(Vec<SemanticError>),
    ParserErrors(Vec<ParserError>),
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub span: Span,
    pub message: String,
    pub note: Option<String>,
    pub code: Option<String>,
    pub source: NamedSource<String>,
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for SemanticError {}
impl Diagnostic for SemanticError {
    fn source_code(&self) -> Option<&dyn SourceCode> {
        Some(&self.source)
    }

    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.code.as_deref().map(|c| Box::new(c) as Box<dyn fmt::Display>)
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.note.as_deref().map(|n| Box::new(n) as Box<dyn fmt::Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(std::iter::once(LabeledSpan::at(
            self.span.start..self.span.end.max(self.span.start + 1),
            self.message.clone(),
        ))))
    }
}

pub struct Analyzer<'a> {
    pub file_name: &'a str,
    pub src: &'a str,
    primitive_types: Vec<&'a str>,
}

impl<'a> Analyzer<'a> {
    pub fn new(file_name: &'a str, src: &'a str) -> Self {
        Self {
            file_name,
            src,
            primitive_types: vec!["string", "int", "bool", "float", "?"],
        }
    }

    pub fn analyze(&self) -> Result<Program, AnalyzerError> {
        let program = crate::parser::Parser::new(self.file_name, self.src, crate::lexer::Lexer::new(self.src).lex())
            .parse_program()
            .map_err(|err| AnalyzerError::ParserErrors(vec![err]))?;

        let models = program.models();

        let errs = models.iter().flat_map(|m| self.check_model(&program, m)).collect::<Vec<_>>();

        if !errs.is_empty() {
            return Err(AnalyzerError::SemanticErrors(errs));
        }
        Ok(program)
    }

    fn check_model(&self, program: &Program, model: &Model) -> Vec<SemanticError> {
        let mut errs = Vec::new();

        let all_models = program.models();
        let models_in_same_level = all_models
            .iter()
            .filter(|m| match &m.parent {
                Some(p) => model.parent == Some(p.clone()),
                None => model.parent.is_none(),
            })
            .collect::<Vec<_>>();

        if models_in_same_level.iter().filter(|m| m.name == model.name).count() > 1
            && models_in_same_level.iter().position(|m| m.name == model.name)
                != Some(models_in_same_level.iter().position(|m| m.span == model.span).unwrap())
        {
            errs.push(self.err(
                model.span,
                format!("duplicate model definition '{}'", model.name),
                Some("remove or rename the duplicate"),
                Some("EDupModel"),
            ));
        }

        for field in &model.fields {
            errs.extend(self.check_field_type(program, field, model));
        }

        errs
    }

    fn check_field_type(&self, program: &Program, field: &Field, associated_model: &Model) -> Vec<SemanticError> {
        let mut errs = Vec::new();
        for atom in &field.ty.atoms {
            if atom.is_ref {
                // Collect all data type names that are either root models or descendants of the associated model
                let all_cdt_names = program
                    .cdts
                    .iter()
                    .filter_map(|cdt| match cdt {
                        CompoundDataType::Enum(e) => Some(e.name.clone()),
                        CompoundDataType::Model(m) => {
                            if m.parent.is_none() || m.parent == Some(associated_model.name.clone()) || m.name == associated_model.name {
                                Some(m.name.clone())
                            } else {
                                None
                            }
                        }
                    })
                    .collect::<HashSet<_>>();
                if !all_cdt_names.contains(atom.name.as_str()) {
                    let model_names_vec: Vec<_> = all_cdt_names.iter().cloned().collect();
                    let similar_models = utils::fuzzy::fuzzy_match(atom.name.as_str(), &model_names_vec, 1);
                    let note = similar_models
                        .first()
                        .filter(|(_, score)| *score > 50)
                        .map(|(candidate, _)| format!("did you mean '{candidate}'?"))
                        .or_else(|| Some("define the referenced model or fix the name".to_string()));
                    errs.push(self.err(atom.span, format!("unknown referenced '{}'", atom.name), note, Some("ERef")));
                }
            } else if !self.primitive_types.contains(&atom.name.as_str()) {
                let valid_types = self
                    .primitive_types
                    .iter()
                    .filter(|t| *t != &"?")
                    .copied()
                    .collect::<Vec<_>>()
                    .join(", ");
                let closest_types = utils::fuzzy::fuzzy_match(atom.name.as_str(), &self.primitive_types, 1);
                let note = closest_types
                    .first()
                    .filter(|(_, score)| *score > 50)
                    .map(|(candidate, _)| format!("did you mean '{candidate}'?"))
                    .or_else(|| Some(format!("valid primitives: {valid_types}")));
                errs.push(self.err(atom.span, format!("unknown primitive type '{}'", atom.name), note, Some("EType")));
            }
        }
        errs
    }

    fn err<T: Into<String>>(&self, span: Span, msg: impl Into<String>, note: Option<T>, code: Option<&str>) -> SemanticError {
        SemanticError {
            span,
            message: msg.into(),
            note: note.map(Into::into),
            code: code.map(Into::into),
            source: NamedSource::new(self.file_name, self.src.to_string()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MultiError {
    pub errors: Vec<SemanticError>,
    pub source: NamedSource<String>,
}

impl fmt::Display for MultiError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} semantic error(s)", self.errors.len())
    }
}

impl Error for MultiError {}
impl Diagnostic for MultiError {
    fn source_code(&self) -> Option<&dyn SourceCode> {
        Some(&self.source)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(self.errors.iter().map(|e| {
            LabeledSpan::at(e.span.start..e.span.end.max(e.span.start + 1), e.message.clone())
        })))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    fn analyze(src: &str) -> Result<Program, AnalyzerError> {
        Analyzer::new("test.glue", src).analyze()
    }

    #[test]
    fn test_valid_basic_model() {
        let src = indoc! {r#"
        model User {
            id: int
            name: string
            age: int?
            friend: User?
        }"#};
        let res = analyze(src);
        assert!(res.is_ok(), "expected valid analysis, got {res:?}");
    }

    #[test]
    fn test_invalid_unknown_ref() {
        let src = indoc! {r#"
        model User {
            id: int
            name: string
            age: int?
            friend: NonExistentModel?
        }"#};
        let res = analyze(src);
        assert!(res.is_err(), "expected invalid analysis, got {res:?}");
        if let Err(AnalyzerError::SemanticErrors(errs)) = res {
            assert_eq!(errs.len(), 1, "expected 1 semantic error, got {}", errs.len());
            let err = &errs[0];
            assert_eq!(err.message, "unknown model 'NonExistentModel'");
            assert_eq!(err.code.as_deref(), Some("ERef"));
        }
    }

    #[test]
    fn test_valid_nested_ref() {
        let src = indoc! {r#"
        model Foo {
            bar: Bar

            model Bar {
                id: int
            }
        }
        "#};

        let res = analyze(src);
        assert!(res.is_ok(), "expected valid analysis, got {res:?}");
    }

    #[test]
    fn test_invalid_nested_ref() {
        let src = indoc! {r#"
        model Foo {
            model Bar {
                id: int
            }
        }

        model Baz {
            bar: #Bar
        }
        "#};

        let res = analyze(src);
        assert!(res.is_err(), "expected invalid analysis, got {res:?}");
        if let Err(AnalyzerError::SemanticErrors(errs)) = res {
            assert_eq!(errs.len(), 1, "expected 1 semantic error, got {}", errs.len());
            let err = &errs[0];
            assert_eq!(err.message, "unknown model 'Bar'");
            assert_eq!(err.code.as_deref(), Some("ERef"));
        }
    }
}
