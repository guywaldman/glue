use std::error::Error;

use miette::{Diagnostic, LabeledSpan, NamedSource, SourceCode};

use crate::Span;

#[derive(Debug, Clone)]
pub struct LangError {
    pub span: Span,
    pub message: String,
    pub note: Option<String>,
    pub code: Option<String>,
    pub source: NamedSource<String>,
    pub severity: miette::Severity,
}

/// A specialized `Result` type for language processing operations.
/// We box the `LangError` to reduce its size.
pub type LangResult<T = ()> = Result<T, Box<LangError>>;

impl LangError {
    pub fn error<T: Into<String>>(file_name: &str, src: &str, span: Span, message: impl Into<String>, note: Option<T>, code: Option<&str>) -> Self {
        Self {
            span,
            message: message.into(),
            note: note.map(Into::into),
            code: code.map(Into::into),
            source: NamedSource::new(file_name, src.to_string()),
            severity: miette::Severity::Error,
        }
    }

    pub fn warning<T: Into<String>>(file_name: &str, src: &str, span: Span, message: impl Into<String>, note: Option<T>, code: Option<&str>) -> Self {
        Self {
            span,
            message: message.into(),
            note: note.map(Into::into),
            code: code.map(Into::into),
            source: NamedSource::new(file_name, src.to_string()),
            severity: miette::Severity::Warning,
        }
    }
}

impl std::fmt::Display for LangError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for LangError {}
impl Diagnostic for LangError {
    fn source_code(&self) -> Option<&dyn SourceCode> {
        Some(&self.source)
    }

    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.code.as_deref().map(|c| Box::new(c) as Box<dyn std::fmt::Display>)
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.note.as_deref().map(|n| Box::new(n) as Box<dyn std::fmt::Display>)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(std::iter::once(LabeledSpan::at(
            self.span.start..self.span.end.max(self.span.start + 1),
            self.message.clone(),
        ))))
    }

    fn severity(&self) -> Option<miette::Severity> {
        None
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        None
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        None
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        None
    }
}
