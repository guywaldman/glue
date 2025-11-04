use std::sync::Arc;

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

    pub fn error(&self, span: TextRange, message: &str) -> Report {
        self.build(span, message, None, Severity::Error, None, Vec::new())
    }

    pub fn error_with_help(&self, span: TextRange, message: &str, help: &str) -> Report {
        self.build(span, message, Some(help), Severity::Error, None, Vec::new())
    }

    pub fn error_with_labels(&self, span: TextRange, message: &str, help: Option<&str>, primary_label: Option<&str>, extra_labels: Vec<LabeledSpan>) -> Report {
        self.build(span, message, help, Severity::Error, primary_label, extra_labels)
    }

    pub fn labeled_span(&self, span: TextRange, label: &str) -> LabeledSpan {
        LabeledSpan::at(span.start().into()..span.end().into(), label)
    }

    fn build(&self, span: TextRange, message: &str, help: Option<&str>, severity: Severity, primary_label: Option<&str>, mut labels: Vec<LabeledSpan>) -> Report {
        let primary_label = primary_label.unwrap_or(message);
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
    let output = generate_report(report)?;
    eprintln!("{}", output);
    Ok(output)
}

pub fn generate_report(report: &Report) -> Result<String, std::fmt::Error> {
    let mut output = String::new();
    let graphic_report_handler = GraphicalReportHandler::new().with_context_lines(3);
    graphic_report_handler.render_report(&mut output, report.as_ref())?;
    Ok(output)
}

pub fn generate_reports(reports: &[&Report]) -> Result<String, std::fmt::Error> {
    let mut output = String::new();
    let graphic_report_handler = GraphicalReportHandler::new().with_context_lines(3);
    for report in reports {
        graphic_report_handler.render_report(&mut output, report.as_ref())?;
        output.push_str("\n\n");
    }
    Ok(output)
}

#[allow(dead_code)]
pub fn print_diagnostic(diag: &(dyn Diagnostic + Send + Sync)) -> Result<String, std::fmt::Error> {
    let mut output = String::new();
    GraphicalReportHandler::new().render_report(&mut output, diag)?;
    Ok(output)
}
