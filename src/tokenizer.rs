#[derive(Debug, PartialEq)]
pub enum TokenType {
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
    // Literals
    Number(f64),
    String(String),
    Bool(bool)
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u64,
}


pub fn tokenize(source: &str) -> Vec<Token> {
    fn single_token(source: &str, line: u64) -> Option<(Token, &str, u64)> {
        fn string(source: &str, line: u64) -> Option<(Token, &str, u64)> {
            let Some(rest) = source.strip_prefix('"') else {
                return None;
            };

            let mut l = line;
            for (i, byte) in rest.as_bytes().iter().enumerate() {
                if byte == &b'\n' {
                    l += 1;
                };
                if byte != &b'"' {
                    continue;
                };

                let tok = Token {
                    token_type: TokenType::String(source[1..=i].to_string()),
                    line: l,
                };
                return Some((tok, &source[i + 2..], l));
            };
            panic!("Unterminated string error at line {line}")
        }

        fn number(source: &str, line: u64) -> Option<(Token, &str, u64)> {
            let make_result = |end| {
                if source[..end].is_empty() {
                    return None;
                };

                Some((
                    Token {token_type: TokenType::Number(source[..end].parse().unwrap()), line},
                    &source[end..], line
                ))
            };

            let mut end = 0;
            let bytes = source.as_bytes();

            while let Some(b'0'..=b'9') = bytes.get(end) { end += 1 };

            if let Some(b'.') = bytes.get(end) { end += 1 }
            else {
                return make_result(end);
            };

            while let Some(b'0'..=b'9') = bytes.get(end) { end += 1 };

            make_result(end)
        }

        fn exact_matches(source: &str, line: u64) -> Option<(Token, &str, u64)> {
            let lex_type_pairs = vec![
                ("(", TokenType::LeftParen),
                (")", TokenType::RightParen),
                ("+", TokenType::Plus),
                ("-", TokenType::Minus),
                ("*", TokenType::Star),
                ("=", TokenType::Equal),
                ("==", TokenType::EqualEqual),
                ("!", TokenType::Bang),
                ("!=", TokenType::BangEqual),
                ("<", TokenType::Less),
                ("<=", TokenType::LessEqual),
                (">", TokenType::Greater),
                (">=", TokenType::GreaterEqual),
                ("/", TokenType::Slash),
                ("true", TokenType::Bool(true)),
                ("false", TokenType::Bool(false)),
            ];

            for (lex, token_type) in lex_type_pairs {
                let Some(rest) = source.strip_prefix(lex) else {
                    continue;
                };

                let token = Token {
                    token_type,
                    line,
                };

                return Some((token, rest, line));
            };
            None
        }

        exact_matches(source, line).or(string(source, line)).or(number(source, line))
    }

    fn tokenize_rec(source: &str, line: u64, mut acc: Vec<Token>) -> Vec<Token> {
        if source.starts_with(&[' ', '\t']) {
            return tokenize_rec(&source[1..], line, acc);
        } else if source.starts_with(&['\n']) {
            return tokenize_rec(&source[1..], line + 1, acc);
        }

        let Some((token, rest, line_new)) = single_token(source, line) else {
            return acc;
        };

        acc.push(token);
        tokenize_rec(rest, line_new, acc)
    }

    tokenize_rec(source, 1, vec![])
}