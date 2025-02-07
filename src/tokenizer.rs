use core::panic;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::Chars;

use crate::parser::Cursor;

#[derive(Debug, Eq, Hash, PartialEq)]
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
    Class,
    Def,
    Elif,
    Else,
    For,
    Global,
    If,
    In,
    Is,
    NonLocal,
    Not,
    Or,
    Pass,
    Return,
    While,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
}

pub fn tokenize(source: &str) -> Cursor<Token> {
    let mut physical_lines = vec![String::new()];

    let mut chars = source.chars().peekable();
    while let Some(ch) = chars.next() {
        // \r terminator
        if ch == '\r' {
            // \r\n terminator
            let _ = chars.next_if_eq(&'\n').is_some();
            physical_lines.push(String::new());
            continue;
        };

        // \n terminator
        if ch == '\n' {
            physical_lines.push(String::new());
            continue;
        }

        let line = physical_lines.last_mut().unwrap();
        line.push(ch);
    }

    let logical_lines = physical_lines
        .into_iter()
        .enumerate()
        .filter(|(_line_num, line_str)| {
            let line = line_str.trim();
            !line.starts_with("#") && !line.is_empty()
        })
        .collect::<Vec<_>>();

    let tokenized_lines = logical_lines
        .into_iter()
        .map(|(i, line_str)| tokenize_line(i + 1, &line_str))
        .collect::<Vec<_>>();

    let mut dent_stack = vec![0];
    let mut tokens = tokenized_lines
        .into_iter()
        .map(|(mut tokens_line, indent_level)| {
            let stack_top = *dent_stack.last().unwrap();

            if indent_level == stack_top {
                return tokens_line;
            }

            if indent_level > stack_top {
                dent_stack.push(indent_level);

                let cur_line_num = tokens_line[0].line;
                tokens_line.push_front(Token {
                    kind: TokenKind::Indent,
                    line: cur_line_num,
                });
                return tokens_line;
            };

            if !dent_stack.contains(&indent_level) {
                panic!("Unmatched indentation level");
            }
            while indent_level < *dent_stack.last().unwrap() {
                dent_stack.pop();

                let cur_line_num = tokens_line[0].line;
                tokens_line.push_front(Token {
                    kind: TokenKind::Dedent,
                    line: cur_line_num,
                });
            }

            tokens_line
        })
        .fold(vec![], |mut acc, line| {
            acc.append(&mut line.into());
            acc
        });

    // dedents for all non-expclityly dedented indents
    let newline_tok = tokens.pop().unwrap(); // remove newline at end
    while *dent_stack.last().unwrap() > 0 {
        dent_stack.pop();
        tokens.push(Token {
            kind: TokenKind::Dedent,
            line: newline_tok.line,
        });
    }
    tokens.push(newline_tok);

    tokens.iter().for_each(|tok| {
        if tok.kind == TokenKind::Newline {
            print!("{:?} Line: {:?} \n\n", tok.kind, tok.line);
        } else {
            print!("{:?} ", tok.kind)
        };
    });

    Cursor::new(tokens)
}

fn tokenize_line(line_num: usize, line: &str) -> (VecDeque<Token>, usize) {
    let front_trimmed = line.trim_start_matches(' ');
    let indent_level = line.len() - front_trimmed.len();
    let mut source = front_trimmed.chars().peekable();

    let mut tokens = VecDeque::new();
    while let Some(ch) = source.peek() {
        let token = match ch {
            // literals
            'A'..='Z' | 'a'..='z' | '_' => identifier(line_num, &mut source),
            '"' => string(line_num, &mut source),
            '0'..='9' => integer(line_num, &mut source),
            // comments
            '#' => {
                return (tokens, indent_level);
            }
            // whitespace
            ' ' => {
                let _ = source.next();
                continue;
            }
            '\t' => panic!("Tabs are not implemented. Please use spaces instead"),
            _ => tokenize_chars(line_num, &mut source),
        };

        tokens.push_back(token);
    }
    tokens.push_back(Token {
        kind: TokenKind::Newline,
        line: line_num,
    });
    (tokens, indent_level)
}

fn tokenize_chars(line_num: usize, source: &mut Peekable<Chars>) -> Token {
    fn either(
        source: &mut Peekable<Chars>,
        kind1: TokenKind,
        if_next: char,
        kind2: TokenKind,
    ) -> TokenKind {
        let mut kind = kind1;
        if source.next_if_eq(&if_next).is_some() {
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
            let Some('/') = source.next() else {
                panic!("Chocopy does not support float division")
            };
            TokenKind::SlashSlash
        }
        '!' => {
            let Some('=') = source.next() else {
                panic!("! is not recognized. Try `not` for boolean negation")
            };
            TokenKind::BangEqual
        }

        ch => panic!("{:?}: Unrecognized character", ch),
    };

    Token {
        kind,
        line: line_num,
    }
}

fn identifier(line_num: usize, chars: &mut Peekable<Chars>) -> Token {
    let mut lex = String::new();
    while let Some('A'..='Z' | 'a'..='z' | '_' | '0'..='9') = chars.peek() {
        lex.push(chars.next().unwrap());
    }

    match lex.as_str() {
        "as" | "assert" | "async" | "await" | "break" | "continue" | "del" | "except"
        | "finally" | "from" | "import" | "lambda" | "raise" | "try" | "with" | "yield" => {
            panic!("Keyword {} is not supported by Chocopy", lex.as_str())
        }
        _ => (),
    };

    let token_kind = match lex.as_str() {
        "False" => TokenKind::False,
        "None" => TokenKind::None,
        "True" => TokenKind::True,
        "and" => TokenKind::And,
        "class" => TokenKind::Class,
        "def" => TokenKind::Def,
        "elif" => TokenKind::Elif,
        "else" => TokenKind::Else,
        "for" => TokenKind::For,
        "global" => TokenKind::Global,
        "if" => TokenKind::If,
        "in" => TokenKind::In,
        "is" => TokenKind::Is,
        "nonlocal" => TokenKind::NonLocal,
        "not" => TokenKind::Not,
        "or" => TokenKind::Or,
        "pass" => TokenKind::Pass,
        "return" => TokenKind::Return,
        "while" => TokenKind::While,
        _ => TokenKind::Identifier(lex),
    };

    Token {
        kind: token_kind,
        line: line_num,
    }
}

fn string(line_num: usize, source: &mut Peekable<Chars>) -> Token {
    source.next();

    let mut str = String::new();
    for ch in source {
        if ch == '"' {
            return Token {
                kind: TokenKind::String(str),
                line: line_num,
            };
        }
        if !matches!(ch as u8, 32..=162) {
            panic!("Only ASCII 32-126 characters allowed")
        }
        str.push(ch);
    }

    panic!("Unterminated String error")
}

fn integer(line_num: usize, source: &mut Peekable<Chars>) -> Token {
    let mut lex = String::new();
    while let Some('0'..='9') = source.peek() {
        lex.push(source.next().unwrap());
    }

    let value = lex.parse().unwrap();
    if value > 0 && lex.starts_with('0') {
        panic!("leading zeros in integer literals are not permitted;");
    };

    Token {
        kind: TokenKind::Integer(value),
        line: line_num,
    }
}
