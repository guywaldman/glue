use std::{fmt::Arguments, sync::Arc};

use miette::{Diagnostic, GraphicalReportHandler, LabeledSpan, NamedSource, Report, Severity, diagnostic};
use rowan::TextRange;

#[derive(Clone)]
pub struct DiagnosticContext {
    source: Arc<NamedSource<String>>,
}

unsafe impl Send for DiagnosticContext {}
unsafe impl Sync for DiagnosticContext {}

impl DiagnosticContext {
    pub fn new(file_name: impl Into<String>, contents: impl Into<String>) -> Self {
        Self {
            source: Arc::new(NamedSource::new(file_name.into(), contents.into())),
        }
    }

    pub fn error(&self, span: TextRange, message: Arguments<'_>, help: Option<String>) -> Report {
        self.build(span, message, help, Severity::Error, None, Vec::new())
    }

    pub fn error_with_labels(&self, span: TextRange, message: Arguments<'_>, help: Option<String>, primary_label: Option<String>, extra_labels: Vec<LabeledSpan>) -> Report {
        self.build(span, message, help, Severity::Error, primary_label, extra_labels)
    }

    pub fn labeled_span(&self, span: TextRange, label: String) -> LabeledSpan {
        LabeledSpan::at(span.start().into()..span.end().into(), label)
    }

    fn build(&self, span: TextRange, message: Arguments<'_>, help: Option<String>, severity: Severity, primary_label: Option<String>, mut labels: Vec<LabeledSpan>) -> Report {
        let message = message.to_string();
        let primary_label = primary_label.unwrap_or_else(|| message.clone());
        labels.insert(0, LabeledSpan::at(span.start().into()..span.end().into(), primary_label));
        let diagnostic = diagnostic! {
            severity = severity,
            labels = labels,
            "{}",
            message
        };
        let diagnostic = if let Some(help) = help { diagnostic.with_help(help) } else { diagnostic };
        Report::new(diagnostic).with_source_code(self.source.clone())
    }
}

pub fn print_report(report: &Report) -> Result<String, std::fmt::Error> {
    let mut output = String::new();
    let graphic_report_handler = GraphicalReportHandler::new().with_context_lines(3);
    graphic_report_handler.render_report(&mut output, report.as_ref())?;
    eprintln!("{output}");
    Ok(output)
}

#[allow(dead_code)]
pub fn print_diagnostic(diag: &(dyn Diagnostic + Send + Sync)) -> Result<String, std::fmt::Error> {
    let mut output = String::new();
    GraphicalReportHandler::new().render_report(&mut output, diag)?;
    Ok(output)
}
