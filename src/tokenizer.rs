use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq)]
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
    pub kind: TokenKind,
    // line: u64
}

pub fn tokenize(source: &str) -> Vec<Token> {
    tokenize_inner(source.chars().peekable())
}

fn tokenize_inner(mut source: Peekable<Chars>) -> Vec<Token> {
    let mut tokens = vec![];

    let mut is_logical_line = false;
    let mut line_start = true;
    let mut indentation_stack = vec![0];

    while let Some(ch) = source.peek() {
        if !ch.is_ascii_whitespace() && *ch != '#' { is_logical_line = true; }

        let token = match ch {
            // literals
            'A'..='Z' | 'a'..='z' | '_' => identifier(&mut source),
            '"' => string(&mut source),
            '0'..='9' => integer(&mut source),
            // comments
            '#' => {
                comment(&mut source, &mut tokens, &mut is_logical_line, &mut line_start);
                continue;
            }
            // whitespace
            ' ' => {
                spaces(&mut source, &mut tokens, &mut line_start, &mut indentation_stack);
                continue;
            }
            '\t' => panic!("Tabs are not implemented. Please use spaces instead"),
            '\r' | '\n' => {
                // line terminators: linux - \n, windows - \r\n, old mac - \r
                source.next();
                source.next_if_eq(&'\n');

                if is_logical_line { tokens.push(Token { kind: TokenKind::Newline }); };
                is_logical_line = false;

                line_start = true;
                continue;
            }
            _ => tokenize_chars(&mut source)
        };
        line_start = false;

        tokens.push(token);
    };

    while *indentation_stack.last().unwrap() > 0 {
        indentation_stack.pop();
        tokens.push(Token { kind: TokenKind::Dedent });
    };
    if is_logical_line { tokens.push(Token { kind: TokenKind::Newline }); };


    tokens
}

fn spaces(source: &mut Peekable<Chars>, tokens: &mut Vec<Token>, line_start: &mut bool, indentation_stack: &mut Vec<i32>) {
    if !*line_start {
        source.next();
        return;
    };

    let mut indent_level = 0;
    while let Some(' ') = source.next_if_eq(&' ') { indent_level += 1; };

    if indent_level == *indentation_stack.last().unwrap() { return; };

    if indent_level > *indentation_stack.last().unwrap() {
        indentation_stack.push(indent_level);
        tokens.push(Token { kind: TokenKind::Indent });
        return;
    };

    if !indentation_stack.contains(&indent_level) { panic!("Unmatched indentation level"); }
    while indent_level < *indentation_stack.last().unwrap() {
        indentation_stack.pop();
        tokens.push(Token { kind: TokenKind::Dedent });
    };
}

fn comment(source: &mut Peekable<Chars>, tokens: &mut Vec<Token>, is_logical_line: &mut bool, line_start: &mut bool) {
    while let Some(ch) = source.next() {
        // line terminators: linux - \n, windows - \r\n, old mac - \r
        if ch == '\n' { break; };
        if ch == '\r' {
            source.next_if_eq(&'\n');
            break;
        }
    };

    if *is_logical_line { tokens.push(Token { kind: TokenKind::Newline }); };
    *is_logical_line = false;
    *line_start = true;
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
        }
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
        "None" => TokenKind::None,
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
        if ch == '"' { return Token { kind: TokenKind::String(str) }; }
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

    return Token { kind: TokenKind::Integer(value) };
}