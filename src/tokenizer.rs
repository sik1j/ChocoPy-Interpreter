use crate::tokenizer::TokenType::{LeftParen, Minus, Plus, RightParen, Star};

enum TokenType {
    LeftParen,
    RightParen,
    Plus,
    Minus,
    Slash,
    Star,
}

struct Token {
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

struct Tokenizer {
    line: u64,
}

impl Tokenizer {
    fn tokenize(tokens : String) -> Vec<Token> {
        Tokenizer::inner(tokens, Tokenizer { line: 0})
    }

    fn inner(tokens: String, state: Tokenizer) -> Vec<Token> {
        let mut tokens = vec![];

        match tokens.chars().next() {
            None => { tokens }
            Some(c) =>
                {
                    let tok = |t| {Token::new(t, state.line, c)};
                    match c {
                        '(' => tok(LeftParen),
                        ')' => tok(RightParen),
                        '+' => tok(Plus),
                        '-' => tok(Minus),
                        '*' => tok(Star)
                    }
                }
        }
    }
}