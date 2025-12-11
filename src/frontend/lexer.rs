use crate::common::{ast::*, error_formatter::ErrorFormatter};
use regex::Regex;

pub struct Lexer {
    line: usize,
    col: usize,
    err_fmt: ErrorFormatter,
    source: String,
}

impl Lexer {
    pub fn new(source: String, filepath: String) -> Self {
        Self {
            line: 1,
            col: 1,
            source: source.clone(),
            err_fmt: ErrorFormatter::new("lexer", source, filepath),
        }
    }

    fn make_span(&self, lexeme: &str) -> Span {
        Span {
            len: lexeme.len(),
            line: self.line,
            col: self.col,
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, String> {
        let regexes: Vec<(TokenKind, Regex)> = vec![
            (TokenKind::Const, Regex::new(r"^const\b").unwrap()),
            (TokenKind::Var, Regex::new(r"^var\b").unwrap()),
            (TokenKind::Begin, Regex::new(r"^begin\b").unwrap()),
            (TokenKind::End, Regex::new(r"^end\b").unwrap()),
            (
                TokenKind::Float,
                Regex::new(r"^[+-]?(?:\d+\.\d*|\d*\.\d+)(?:[eE][+-]?\d+)?").unwrap(),
            ),
            (TokenKind::Integer, Regex::new(r"^[+-]?\d+").unwrap()),
            (
                TokenKind::String,
                Regex::new(r#"^"(?:\\.|[^"\\\r\n])*""#).unwrap(),
            ),
            (
                TokenKind::Identifier,
                Regex::new(r"^[A-Za-z_][A-Za-z0-9_]*").unwrap(),
            ),
            (TokenKind::Equal, Regex::new(r"^=").unwrap()),
            (TokenKind::Colon, Regex::new(r"^:").unwrap()),
            (TokenKind::SemiColon, Regex::new(r"^;").unwrap()),
            (TokenKind::At, Regex::new(r"^@").unwrap())
        ];

        let mut tokens: Vec<Token> = Vec::with_capacity(128);
        let mut source = self.source.chars().peekable();

        while source.peek().is_some() {
            while source.next_if_eq(&'\n').is_some() {
                self.line += 1;
                self.col = 1;
            }

            while source.next_if(|c| c.is_whitespace()).is_some() {
                self.col += 1;
            }
            
            let mut matched = false;
            
            for (kind, regex) in regexes.iter() {
                let current: String = source.clone().collect();

                if let Some(m) = regex.find(&current) {
                    let lexeme = m.as_str().to_string();
                    let token = Token {
                        kind: *kind,
                        span: self.make_span(&lexeme),
                        lexeme,
                    };

                    tokens.push(token.clone());

                    let consumed = token.lexeme.chars().count();
                    self.col += consumed;

                    source.nth(consumed - 1);
                    
                    matched = true;
                    
                    break;
                }
            }

            if !matched {
                let invalid = source.next().unwrap_or('\0').to_string();
                let message = self.err_fmt.format(
                    self.make_span(&invalid),
                    format!("unexpected character has found: {}", invalid),
                );

                return Err(message);
            }
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            lexeme: String::new(),
            span: self.make_span(""),
        });

        Ok(tokens)
    }
}
