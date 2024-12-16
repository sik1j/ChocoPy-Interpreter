use crate::tokenizer::TokenType::{Bang, BangEqual, Equal, EqualEqual, Greater, GreaterEqual, LeftParen, Less, LessEqual, Minus, Plus, RightParen, Star};

#[derive(Debug)]
enum TokenType {
    // Single Char
    LeftParen,
    RightParen,
    Plus,
    Minus,
    Star,
    // One or Two Chars
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    // Division Or Comment
    Slash,
}

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    line: u64,
    lexeme: String,
}

impl Token {
    fn new(token_type: TokenType, line: u64, lexeme: String) -> Self {
        Self {
            token_type,
            lexeme,
            line,
        }
    }
}

pub struct Tokenizer {
    line: u64,
}

impl Tokenizer {
    pub fn tokenize(source: String) -> Vec<Token> {
        Tokenizer {
            line: 0,
        }.tokenize_inner(source)
    }

    fn tokenize_inner(&mut self, source: String) -> Vec<Token> {
        let mut chars = source.chars().peekable();

        let mut tokens = vec![];
        while let Some(c) = chars.next() {
            let tok = |token_type: TokenType| { Token { token_type, lexeme: c.to_string(), line: self.line } };
            let mut equal_type_tok = |one, two| {
                if let Some('=') = chars.next_if_eq(&'=') {
                    Token { token_type: two, lexeme: c.to_string() + "=", line: self.line }
                } else {
                    Token { token_type: one, lexeme: c.to_string(), line: self.line }
                }
            };

            tokens.push(match c {
                // one char
                '(' => tok(LeftParen),
                ')' => tok(RightParen),
                '+' => tok(Plus),
                '-' => tok(Minus),
                '*' => tok(Star),
                // one or two char
                '=' => equal_type_tok(Equal, EqualEqual),
                '!' => equal_type_tok(Bang, BangEqual),
                '>' => equal_type_tok(Greater, GreaterEqual),
                '<' => equal_type_tok(Less, LessEqual),
                // whitespace
                ' ' => continue,
                _ => panic!(),
            });
        };

        tokens
    }
}