use regex::Regex;
use std::fmt;

#[derive(Copy, Clone)]
pub enum TokenKind {
    Eof,

    Const,
    Var,
    Begin,
    End,

    Integer,
    Float,
    String,
    Identifier,

    Equal,
    Colon,
    SemiColon,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Eof        => write!(f, "Eof"),
            TokenKind::Const      => write!(f, "Const"),
            TokenKind::Var        => write!(f, "Var"),
            TokenKind::Begin      => write!(f, "Begin"),
            TokenKind::End        => write!(f, "End"),
            TokenKind::Integer    => write!(f, "Integer"),
            TokenKind::Float      => write!(f, "Float"),
            TokenKind::String     => write!(f, "String"),
            TokenKind::Identifier => write!(f, "Identifier"),

            TokenKind::Equal      => write!(f, "Equal"),
            TokenKind::Colon      => write!(f, "Colon"),
            TokenKind::SemiColon  => write!(f, "SemiColon"),
        }
    }
}

#[derive(Clone)]
pub struct Token {
    kind: TokenKind,
    lexeme: String,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:<12} '{}'", self.kind, self.lexeme)
    }
}

pub struct Lexer {
    tokens: Vec<Token>,
    cursor: usize,
    source: &'static str,
}

impl Lexer {
    pub fn new(source: &'static str) -> Self {
        Self {
            tokens: vec![],
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

                    self.tokens.push(token.clone());
                    self.cursor += token.lexeme.len();

                    matched = true;

                    break;
                }
            }

            if !matched {
                panic!("lexer -> unexpected token has found: {}", slice);
            }
        }

        self.tokens.push(Token {
            kind: TokenKind::Eof,
            lexeme: String::new(),
        });
        self.tokens.clone()
    }
}
