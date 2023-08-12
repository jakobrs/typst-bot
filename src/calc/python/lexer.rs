use once_cell::sync::Lazy;
use regex::bytes::{Match, Regex};
use thiserror::Error;

use crate::calc::{Value, ArithOp};

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum Token {
    ArithOp(ArithOp),
    Lit(Value),
    LeftParen,
    RightParen,
    Comma,
    Global(String),
}

#[derive(Error, Debug)]
pub enum PythonLexerError {
    #[error("Unexpected byte: {0:x?} (at pos {1})")]
    UnexpectedCharacter(u8, usize),
}

struct Expression<'a> {
    text: &'a [u8],
    pos: usize,
}

impl<'a> Expression<'a> {
    fn new(text: &'a [u8]) -> Self {
        Self { text, pos: 0 }
    }

    fn advance(&mut self, count: usize) {
        self.text = &self.text[count..];
        self.pos += count;
    }

    fn eat(&mut self, pattern: &[u8]) -> bool {
        if self.text.starts_with(pattern) {
            self.advance(pattern.len());
            true
        } else {
            false
        }
    }

    fn eat_word(&mut self, word: &[u8]) -> bool {
        if self.text.starts_with(word)
            && (self.text.len() == word.len() || !self.text[word.len()].is_ascii_alphanumeric())
        {
            self.advance(word.len());
            true
        } else {
            false
        }
    }

    fn eat_regex(&mut self, regex: &Regex) -> Option<Match<'a>> {
        if let Some(m) = regex.find(self.text) {
            self.advance(m.len());
            Some(m)
        } else {
            None
        }
    }

    fn eat_regex_word(&mut self, regex: &Regex) -> Option<Match<'a>> {
        if let Some(m) = regex.find(self.text) {
            if self.text.len() == m.len() || !self.text[m.len()].is_ascii_alphanumeric() {
                self.advance(m.len());
                Some(m)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn next_byte(&self) -> u8 {
        self.text[0]
    }

    fn is_finished(&self) -> bool {
        self.text.is_empty()
    }

    fn pos(&self) -> usize {
        self.pos
    }
}

impl<'a> From<&'a str> for Expression<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            text: value.as_bytes(),
            pos: 0,
        }
    }
}

macro_rules! lazily {
    ($ty:ty, $expr:expr) => {{
        static EXPR: Lazy<$ty> = Lazy::new(|| $expr);

        &*EXPR
    }};
}

macro_rules! regex {
    ($p:literal) => {
        lazily!(Regex, Regex::new($p).unwrap())
    };
}

pub fn lex(expr: &str) -> Result<Vec<Token>, PythonLexerError> {
    let mut tokens = vec![];
    let mut expr: Expression = expr.into();

    loop {
        if expr.is_finished() {
            break;
        }

        if expr.eat(b"+") {
            tokens.push(Token::ArithOp(ArithOp::Plus));
        } else if expr.eat(b"-") {
            tokens.push(Token::ArithOp(ArithOp::Minus));
        } else if expr.eat(b"//") {
            tokens.push(Token::ArithOp(ArithOp::FloorDiv));
        } else if expr.eat(b"**") {
            tokens.push(Token::ArithOp(ArithOp::Pow));
        } else if expr.eat(b"*") {
            tokens.push(Token::ArithOp(ArithOp::Mul));
        } else if expr.eat(b"/") {
            tokens.push(Token::ArithOp(ArithOp::Div));
        } else if expr.eat(b"(") {
            tokens.push(Token::LeftParen);
        } else if expr.eat(b")") {
            tokens.push(Token::RightParen);
        } else if expr.eat(b",") {
            tokens.push(Token::Comma);
        } else if expr.eat(b" ") {
            // do nothing
        } else if expr.eat_word(b"true") {
            tokens.push(Token::Lit(Value::Bool(false)));
        } else if expr.eat_word(b"false") {
            tokens.push(Token::Lit(Value::Bool(false)));
        } else if let Some(m) = expr.eat_regex_word(regex!(
            r"^[[:digit:]]+\.[[:digit:]]*f?|[[:digit:]]*\.[[:digit:]]+f?|[[:digit:]]f"
        )) {
            let mut text = m.as_bytes();
            if text.last() == Some(&b'f') {
                text = &text[..text.len() - 1];
            }
            tokens.push(Token::Lit(Value::Float(
                std::str::from_utf8(text).unwrap().parse().unwrap(),
            )));
        } else if let Some(m) = expr.eat_regex_word(regex!(r"^[[:digit:]]+")) {
            tokens.push(Token::Lit(Value::Int(
                std::str::from_utf8(m.as_bytes()).unwrap().parse().unwrap(),
            )));
        } else if let Some(m) = expr.eat_regex_word(regex!(r"^[[:alpha:]]+")) {
            tokens.push(Token::Global(
                String::from_utf8(m.as_bytes().to_vec()).unwrap(),
            ));
        } else {
            return Err(PythonLexerError::UnexpectedCharacter(
                expr.next_byte(),
                expr.pos(),
            ));
        }
    }

    Ok(tokens)
}
