use crate::common::ast::Span;

const RESET: &str = "\x1b[0m";
const BOLD: &str = "\x1b[1m";

const RED: &str = "\x1b[31m";
const BLUE: &str = "\x1b[34m";
const GRAY: &str = "\x1b[90m";

#[derive(Clone)]
pub struct ErrorFormatter {
    pub local: &'static str,
    pub source: String,
    pub filename: String,
}

impl ErrorFormatter {
    fn colorize_quoted(&self, text: &str) -> String {
        let mut result = String::new();
        let mut in_quotes = false;

        for c in text.chars() {
            if c == '\'' {
                if in_quotes {
                    result.push(c);
                    result.push_str(RESET);
                    result.push_str(BOLD);
                } else {
                    result.push_str(RED);
                    result.push(c);
                }
                in_quotes = !in_quotes;
            } else {
                result.push(c);
            }
        }

        if in_quotes {
            result.push_str(RESET);
        }

        result
    }

    pub fn new(local: &'static str, source: String, filename: String) -> Self {
        Self {
            local,
            source,
            filename,
        }
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
            "{BOLD}{RED}(err){RESET} {BLUE}@{}{RESET}: {BOLD}{}{RESET}\n{GRAY}----> {}:{}:{}\n   |\n{BLUE}{:>2}{GRAY} |{RESET}{BOLD} {}\n{GRAY}   | {RED}{}{RESET}",
            self.local, self.colorize_quoted(&msg), self.filename, line, col, line, src_line, caret
        )
    }
}
