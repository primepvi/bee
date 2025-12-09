use std::iter::repeat;

use crate::ast::Span;

pub struct ErrorFormatter {
    pub local: &'static str,
    pub source: &'static str,
    pub filename: &'static str,
}

impl ErrorFormatter {
    pub fn new(local: &'static str, source: &'static str, filename: &'static str) -> Self {
        Self { local, source, filename }
    }

    pub fn format(&self, span: Span, msg: String) -> String {
        let line = span.line;
        let col = span.col;

        let src_line = self
            .source
            .lines()
            .nth(line - 1)
            .unwrap_or("<line not found>");

        let mut caret = String::new();

        for _ in 1..col {
            caret.push(' ');
        }

        caret.push_str("^".repeat(span.len).as_str());

        format!(
            "error[{}]: {}\n  --> {}:{}:{}\n   |\n{:>2} | {}\n   | {}",
            self.local,
            msg,
            self.filename,
            line,
            col,
            line,
            src_line,
            caret
        )
    }
}
