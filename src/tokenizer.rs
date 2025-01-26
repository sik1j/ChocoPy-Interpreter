use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
pub enum TokenKind {
    Plus,
    Minus,
    Star,
    SlashSlash,
    Percent,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    EqualEqual,
    BangEqual,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Comma,
    Colon,
    Period,
    Arrow,
    // whitespace tokens
    Newline,
    Indent,
    Dedent,
    // Literals
    Identifier(String),
    String(String),
    Integer(u32),
    // Keywords
    False,
    None,
    True,
    And,
    As,
    Assert,
    Async,
    Await,
    Break,
    Class,
    Continue,
    Def,
    Del,
    Elif,
    Else,
    Except,
    Finally,
    For,
    From,
    Global,
    If,
    Import,
    In,
    Is,
    Lambda,
    Nonlocal,
    Not,
    Or,
    Pass,
    Raise,
    Return,
    Try,
    While,
    With,
    Yield,
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    // line: u64
}

pub struct Tokenizer {

}

pub fn tokenize(source: &str) -> Vec<Token> {
    tokenize_inner(source.chars().peekable())
}

fn tokenize_inner(mut source: Peekable<Chars>) -> Vec<Token> {
    let mut tokens = vec![];

    while let Some(ch) = source.peek() {
        let token = match ch {
            // literals
            'A'..='Z' | 'a'..='z' | '_' => identifier(&mut source),
            '"' => string(&mut source),
            '0'..='9' => integer(&mut source),
            // comments
            '#' => {
                while let Some(&ch) = source.peek() {
                    source.next();
                    if ch == '\n' {
                        return tokenize_inner(source);
                    };
                };
                return tokens;
            },
            // whitespace
            ' ' => {
                source.next();
                continue
            }
            _ => tokenize_chars(&mut source)
        };

        tokens.push(token);
    };

    tokens
}

fn tokenize_chars(source: &mut Peekable<Chars>) -> Token {
    fn either(source: &mut Peekable<Chars>, kind1: TokenKind, if_next: char, kind2: TokenKind) -> TokenKind {
        let mut kind = kind1;
        if let Some(_) = source.next_if_eq(&if_next) {
            kind = kind2;
        }
        kind

    }

    let kind = match source.next().unwrap() {
        '+' => TokenKind::Plus,
        '*' => TokenKind::Star,
        '%' => TokenKind::Percent,
        '(' => TokenKind::LeftParen,
        ')' => TokenKind::RightParen,
        '[' => TokenKind::LeftBracket,
        ']' => TokenKind::RightBracket,
        ',' => TokenKind::Comma,
        ':' => TokenKind::Colon,
        '.' => TokenKind::Period,
        '-' => either(source, TokenKind::Minus, '>', TokenKind::Arrow),
        '<' => either(source, TokenKind::Less, '=', TokenKind::LessEqual),
        '>' => either(source, TokenKind::Greater, '=', TokenKind::GreaterEqual),
        '=' => either(source, TokenKind::Equal, '=', TokenKind::EqualEqual),
        '/' => {
            let Some('/') = source.next() else { panic!("Chocopy does not support float division") };
            TokenKind::SlashSlash
        },
        '!' => {
            let Some('=') = source.next() else { panic!("! is not recognized. Try `not` for boolean negation") };
            TokenKind::BangEqual
        }

        ch => panic!("{ch:?}: Unrecognized character")
    };

    Token { kind }
}

fn identifier(chars: &mut Peekable<Chars>) -> Token {
    let mut lex = String::new();
    while let Some('A'..='Z' | 'a'..='z' | '_' | '0'..='9') = chars.peek() {
        lex.push(chars.next().unwrap());
    };

    let token_kind = match lex.as_str() {
        "False" => TokenKind::False,
        "none" => TokenKind::None,
        "True" => TokenKind::True,
        "and" => TokenKind::And,
        "as" => TokenKind::As,
        "assert" => TokenKind::Assert,
        "async" => TokenKind::Async,
        "await" => TokenKind::Await,
        "break" => TokenKind::Break,
        "class" => TokenKind::Class,
        "continue" => TokenKind::Continue,
        "def" => TokenKind::Def,
        "del" => TokenKind::Del,
        "elif" => TokenKind::Elif,
        "else" => TokenKind::Else,
        "except" => TokenKind::Except,
        "finally" => TokenKind::Finally,
        "for" => TokenKind::For,
        "from" => TokenKind::From,
        "global" => TokenKind::Global,
        "if" => TokenKind::If,
        "import" => TokenKind::Import,
        "in" => TokenKind::In,
        "is" => TokenKind::Is,
        "lambda" => TokenKind::Lambda,
        "nonlocal" => TokenKind::Nonlocal,
        "not" => TokenKind::Not,
        "or" => TokenKind::Or,
        "pass" => TokenKind::Pass,
        "raise" => TokenKind::Raise,
        "return" => TokenKind::Return,
        "try" => TokenKind::Try,
        "while" => TokenKind::While,
        "with" => TokenKind::With,
        "yield" => TokenKind::Yield,
        _ => TokenKind::Identifier(lex)
    };

    Token { kind: token_kind }
}

fn string(source: &mut Peekable<Chars>) -> Token {
    source.next();

    let mut str = String::new();
    while let Some(ch) = source.next() {
        if ch == '"' { return Token {kind: TokenKind::String(str)}; }
        if !matches!(ch as u8, 32..=162) { panic!("Only ASCII 32-126 characters allowed") }
        str.push(ch);
    };

    panic!("Unterminated String error")


}

fn integer(source: &mut Peekable<Chars>) -> Token {
    let mut lex = String::new();
    while let Some('0'..='9') = source.peek() {
        lex.push(source.next().unwrap());
    };

    let value = lex.parse().unwrap();
    if value > 0 && lex.starts_with('0') {
        panic!("leading zeros in integer literals are not permitted;");
    };

    return Token {kind: TokenKind::Integer(value)}
}