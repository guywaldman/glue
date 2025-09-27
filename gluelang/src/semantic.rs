use crate::lexer::Span;
use crate::parser::{AnnotationArgument, Endpoint, Field, FieldDecorator, Program};
use miette::{Diagnostic, LabeledSpan, NamedSource, SourceCode};
use regex;
use std::{error::Error, fmt};

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
        self.code
            .as_ref()
            .map(|c| Box::new(c.clone()) as Box<dyn fmt::Display>)
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.note
            .as_ref()
            .map(|n| Box::new(n.clone()) as Box<dyn fmt::Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let s = self.span.start;
        let e = if self.span.end > self.span.start {
            self.span.end
        } else {
            s + 1
        };
        Some(Box::new(std::iter::once(LabeledSpan::at(
            s..e,
            self.message.clone(),
        ))))
    }
}

pub struct Analyzer<'a> {
    pub file_name: &'a str,
    pub src: &'a str,
    primitive_types: &'a [&'a str],
}

impl<'a> Analyzer<'a> {
    pub fn new(file_name: &'a str, src: &'a str) -> Self {
        Self {
            file_name,
            src,
            primitive_types: &["string", "int", "bool", "float"],
        }
    }

    pub fn analyze(&self, program: &Program<'a>) -> Vec<SemanticError> {
        let mut errs = Vec::new();
        // Collect model names for reference resolution
        let model_names: std::collections::HashSet<&str> =
            program.models.iter().map(|m| m.name).collect();

        for m in &program.models {
            for f in &m.fields {
                errs.extend(self.check_field_type(f, &model_names));
            }
        }
        for e in &program.endpoints {
            errs.extend(self.check_endpoint(e));
        }
        errs
    }

    fn check_field_type(
        &self,
        field: &Field,
        model_names: &std::collections::HashSet<&'a str>,
    ) -> Vec<SemanticError> {
        let mut errs = Vec::new();
        for atom in &field.ty.atoms {
            if atom.is_ref {
                if !model_names.contains(atom.name.as_str()) {
                    errs.push(self.err(
                        atom.span,
                        format!("unknown model '{}'", atom.name),
                        Some("define the referenced model or fix the name"),
                        Some("ERef"),
                    ));
                }
            } else if !self.primitive_types.contains(&atom.name.as_str()) {
                errs.push(self.err(
                    atom.span,
                    format!("unknown primitive type '{}'", atom.name),
                    Some("valid primitives: string, int, bool, float"),
                    Some("EType"),
                ));
            }
        }
        errs
    }

    fn check_endpoint(&self, ep: &Endpoint) -> Vec<SemanticError> {
        let mut errs = Vec::new();
        if let Some(ann) = &ep.annotation {
            if ann.name != "endpoint" {
                errs.push(self.err(
                    ann.span,
                    format!("unknown annotation '{}'", ann.name),
                    Some("did you mean #[endpoint(...)]?"),
                    Some("EAnnot"),
                ));
            }
            if ann.args.is_empty() {
                errs.push(self.err(
                    ann.span,
                    "endpoint annotation missing path",
                    Some("supply a path e.g. #[endpoint(\"/users/{id}\")]"),
                    Some("EEndpointPath"),
                ));
            }
        } else {
            errs.push(self.err(
                ep.span,
                "endpoint missing #[endpoint(...)] annotation",
                Some("add #[endpoint(\"/path\")] before 'endpoint'"),
                Some("EEndpointAnnot"),
            ));
        }

        // If the path contains a dynamic parameter, ensure it's defined as a @path field
        if let Some(ann) = &ep.annotation {
            let path_arg = ann
                .args
                .iter()
                .filter_map(|arg| match arg {
                    AnnotationArgument::NamedArg { name, value } if *name == "path" => Some(value),
                    AnnotationArgument::PositionalArg { index: 1, value } => Some(value),
                    _ => None,
                })
                .next();
            if let Some(path_arg) = path_arg {
                // Parse the variables within the path
                let path_vars: Vec<&str> = regex::Regex::new(r"\{(\w+)\}")
                    .unwrap()
                    .captures_iter(path_arg)
                    .filter_map(|cap| cap.get(1).map(|m| m.as_str()))
                    .collect();
                for var in path_vars {
                    if !ep
                        .fields
                        .iter()
                        .any(|f| f.decorator == FieldDecorator::Path && f.field.name == var)
                    {
                        // TODO: Make span specific to the variable in the path.
                        errs.push(
                            self.err(
                                ann.span,
                                format!(
                                    "path parameter '{{{var}}}' is not defined as a @path field"
                                ),
                                Some(
                                    format!("define a field with @path decorator: `@path {var}`")
                                        .as_str(),
                                ),
                                Some("EPathParam"),
                            ),
                        );
                    }
                }
            } else {
                errs.push(self.err(
                    ann.span,
                    "endpoint annotation missing path argument",
                    Some("supply a path e.g. #[endpoint(\"/users/{id}\")]"),
                    Some("EEndpointPath"),
                ));
            }
        }
        errs
    }

    fn err(
        &self,
        span: Span,
        msg: impl Into<String>,
        note: Option<&str>,
        code: Option<&str>,
    ) -> SemanticError {
        SemanticError {
            span,
            message: msg.into(),
            note: note.map(|n| n.into()),
            code: code.map(|c| c.into()),
            source: NamedSource::new(self.file_name, self.src.to_string()),
        }
    }
}

// Aggregate multiple semantic errors into a single Diagnostic so miette prints them in one report.
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
        if self.errors.is_empty() {
            return None;
        }
        let it = self.errors.iter().map(|e| {
            let s = e.span.start;
            let e_end = if e.span.end > e.span.start {
                e.span.end
            } else {
                s + 1
            };
            LabeledSpan::at(s..e_end, e.message.clone())
        });
        Some(Box::new(it))
    }
    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        None
    }
}
