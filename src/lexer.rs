use regex::Regex;
use crate::ast::*;

pub struct Lexer {
    cursor: usize,
    source: &'static str,
}

impl Lexer {
    pub fn new(source: &'static str) -> Self {
        Self {
            cursor: 0,
            source: source.trim(),
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
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
        ];

        let mut tokens: Vec<Token> = Vec::with_capacity(128);

        while self.cursor < self.source.len() {
            let slice = &self.source[self.cursor..];

            let trimmed = slice.trim_start();
            let skipped = slice.len() - trimmed.len();
            self.cursor += skipped;

            let slice = trimmed;

            let mut matched = false;

            for (kind, regex) in regexes.iter() {
                if self.cursor >= self.source.len() {
                    break;
                }
                if let Some(m) = regex.find(slice) {
                    let lexeme = m.as_str().to_string();
                    let token = Token {
                        kind: *kind,
                        lexeme,
                    };

                    tokens.push(token.clone());
                    self.cursor += token.lexeme.len();

                    matched = true;

                    break;
                }
            }

            if !matched {
                panic!("lexer -> unexpected token has found: {}", slice);
            }
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            lexeme: String::new(),
        });

        tokens
    }
}
