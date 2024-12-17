use std::iter::Peekable;
use std::str::Chars;
use crate::tokenizer::TokenType::{Bang, BangEqual, Equal, EqualEqual, Greater, GreaterEqual, LeftParen, Less, LessEqual, Minus, Plus, RightParen, Slash, Star};

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

pub struct Tokenizer {}

impl Tokenizer {
    pub fn tokenize(source: String) -> Vec<Token> {
        Tokenizer::tokenize_inner(source)
    }

    fn tokenize_inner(source: String) -> Vec<Token> {
        let mut line = 1;
        let mut chars = source.chars().peekable();

        let mut tokens = vec![];
        while let Some(c) = chars.next() {
            let tok = |token_type: TokenType| {
                Token { token_type, lexeme: c.to_string(), line }
            };

            tokens.push(match c {
                // one char
                '(' => tok(LeftParen),
                ')' => tok(RightParen),
                '+' => tok(Plus),
                '-' => tok(Minus),
                '*' => tok(Star),
                // one or two char
                '=' => equal_type_tok(&mut chars, c, line, Equal, EqualEqual),
                '!' => equal_type_tok(&mut chars, c, line, Bang, BangEqual),
                '>' => equal_type_tok(&mut chars, c, line, Greater, GreaterEqual),
                '<' => equal_type_tok(&mut chars, c, line, Less, LessEqual),
                // comments
                '/' => {
                    if let Some('/') = chars.peek() {
                        while let Some(_) = chars.next_if(|x| *x != '\n'){};
                        continue;
                    } else {
                        tok(Slash)
                    }
                },
                // whitespace
                ' ' | '\t' => continue,
                '\n' => {
                    line += 1;
                    continue;
                }
                _ => panic!(),
            });
        };
        return tokens;

        fn equal_type_tok(
            chars: &mut Peekable<Chars>, c: char, line: u64, one: TokenType, two: TokenType,
        ) -> Token {
            if let Some('=') = chars.next_if_eq(&'=') {
                Token { token_type: two, lexeme: c.to_string() + "=", line }
            } else {
                Token { token_type: one, lexeme: c.to_string(), line }
            }
        }
    }
}