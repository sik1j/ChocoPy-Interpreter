/*
Grammar:

expr                      ::= or_op [ 'if' or_op 'else' expr ]?
or_op                     ::= and_op [ 'or' and_op ]*
and_op                    ::= not_op [ 'and' not_op ]*
not_op                    ::= [ 'not' ]* comparison

comparison                ::= term [ [ '==' | '!=' | '<=' | '>=' | '<' | '>' ] term ]*
term                      ::= factor [ [ '+' | '-' ] factor ]*
factor                    ::= negation [ [ '*' | '//' | '%' ] negation ]*
negation                     ::= '-'* accessor

accessor                  ::= base
                                [ . func_call
                                | '[' expr ']'
                                ]*

base                      ::= func_call
                            | literal
                            | '[' comma_sep_exprs ']'
                            | '(' expr ')'
func_call                 ::= ID [ '(' comma_sep_exprs ')' ]?

comma_sep_exprs           ::= [ expr [ , expr ]* ]?

literal                   ::= None
                            | True
                            | False
                            | Integer
                            | IdString | String
*/

// todo: get rid of the panics and '?' whenever sensible
use core::panic;

use crate::tokenizer::{Cursor, Token, TokenKind};

pub fn parse(input: &mut Cursor<Token>) -> Vec<Expr> {
    let mut lits = vec![];
    while let Some(lit) = input.parse() {
        lits.push(lit);
    }
    lits
}

pub trait Parse: Sized {
    fn parse(input: &mut Cursor<Token>) -> Option<Self>;
}

#[derive(Debug)]
pub enum Literal {
    None,
    True,
    False,
    Integer(u32),
    String(String),
    IdString(String),
}

impl Parse for Literal {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let lit = match &input.peek()?.kind {
            TokenKind::None => Literal::None,
            TokenKind::True => Literal::True,
            TokenKind::False => Literal::False,
            TokenKind::Integer(n) => Literal::Integer(*n),
            TokenKind::String(s) => Literal::String(s.clone()),
            _ => return None,
        };
        input.next();

        Some(lit)
    }
}

#[derive(Debug)]
pub struct Expr {
    or_op: OrOp,
    if_expr: Option<(OrOp, Box<Expr>)>,
}

impl Parse for Expr {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let or1 = input.parse()?;
        if input.next_if(|t| t.kind == TokenKind::If).is_none() {
            return Expr {
                or_op: or1,
                if_expr: None,
            }
            .into();
        }

        let or2 = input.parse()?;
        input.next_if(|t| t.kind == TokenKind::Else)?;
        let else_expr = input.parse()?;

        Expr {
            or_op: or1,
            if_expr: (or2, Box::new(else_expr)).into(),
        }
        .into()
    }
}

#[derive(Debug)]
pub struct OrOp(Box<AndOp>, Vec<AndOp>);
impl Parse for OrOp {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let left = input.parse()?;

        let mut rest = vec![];
        while input.next_if(|t| t.kind == TokenKind::And).is_some() {
            rest.push(input.parse()?)
        }

        Some(OrOp(Box::new(left), rest))
    }
}

#[derive(Debug)]
pub struct AndOp(NotOp, Vec<NotOp>);
impl Parse for AndOp {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let left = input.parse()?;

        let mut rest = vec![];
        while input.next_if(|t| t.kind == TokenKind::And).is_some() {
            rest.push(input.parse()?)
        }

        Some(AndOp(left, rest))
    }
}

#[derive(Debug)]
pub enum NotOp {
    Not(Box<NotOp>),
    Comparison(Comparison),
}
impl Parse for NotOp {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if input.next_if(|t| t.kind == TokenKind::Not).is_some() {
            let rest = Box::new(input.parse()?);
            return NotOp::Not(rest).into();
        };

        let comparison = input.parse()?;
        NotOp::Comparison(comparison).into()
    }
}

fn parse_binary<R, O: Copy, N: Parse>(
    input: &mut Cursor<Token>,
    ret_type: fn(N, Vec<(O, N)>) -> R,
    mapping: &[(TokenKind, O)],
) -> Option<R> {
    let left = input.parse()?;

    let mut rest = vec![];
    loop {
        let Some(token) = input.peek() else {
            break;
        };

        let mut operator = None;
        for (kind, op_kind) in mapping {
            if token.kind == *kind {
                operator = Some(*op_kind);
                input.next();
                break;
            };
        }
        if operator.is_none() {
            break;
        }

        let right = input
            .parse()
            .expect("Expected an argument after binary operator");
        rest.push((operator.unwrap(), right));
    }

    ret_type(left, rest).into()
}

#[derive(Debug)]
pub struct Comparison(Term, Vec<(ComparisonOp, Term)>);
#[derive(Debug, Clone, Copy)]
pub enum ComparisonOp {
    Equality,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Is,
}

impl Parse for Comparison {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        parse_binary(
            input,
            Self,
            &[
                (TokenKind::EqualEqual, ComparisonOp::Equality),
                (TokenKind::BangEqual, ComparisonOp::NotEqual),
                (TokenKind::Less, ComparisonOp::Less),
                (TokenKind::LessEqual, ComparisonOp::LessEqual),
                (TokenKind::Greater, ComparisonOp::Greater),
                (TokenKind::GreaterEqual, ComparisonOp::GreaterEqual),
                (TokenKind::Is, ComparisonOp::Is),
            ],
        )
    }
}

#[derive(Debug)]
pub struct Term(Factor, Vec<(TermOp, Factor)>);
#[derive(Debug, Clone, Copy)]
pub enum TermOp {
    Add,
    Subtract,
}
impl Parse for Term {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        parse_binary(
            input,
            Self,
            &[
                (TokenKind::Plus, TermOp::Add),
                (TokenKind::Minus, TermOp::Subtract),
            ],
        )
    }
}

#[derive(Debug)]
pub struct Factor(Negation, Vec<(FactorOp, Negation)>);
#[derive(Debug, Clone, Copy)]
pub enum FactorOp {
    Multiply,
    IntDiv,
    Modulo,
}

impl Parse for Factor {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        parse_binary(
            input,
            Self,
            &[
                (TokenKind::Star, FactorOp::Multiply),
                (TokenKind::SlashSlash, FactorOp::IntDiv),
                (TokenKind::Percent, FactorOp::Modulo),
            ],
        )
    }
}

#[derive(Debug)]
pub struct Negation {
    op_count: usize,
    accessor: Accessor,
}
impl Parse for Negation {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let mut op_count = 0;
        while input.next_if_kind(&TokenKind::Minus).is_some() {
            op_count += 1;
        }

        let accessor = input.parse()?;
        Negation { op_count, accessor }.into()
    }
}

#[derive(Debug)]
pub struct Accessor(Base, Vec<AccessorOp>);
#[derive(Debug)]
pub enum AccessorOp {
    Index(Expr),
    MemberFunc(FuncCall),
}
impl Parse for Accessor {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let left = input.parse()?;

        let mut accessors = vec![];
        loop {
            let Some(tok) = input.peek() else {
                break;
            };

            match tok.kind {
                TokenKind::LeftBracket => {
                    input.next();
                    let expr = input.parse()?;
                    input
                        .next_if_kind(&TokenKind::RightBracket)
                        .expect("Expected closing ]");
                    accessors.push(AccessorOp::Index(expr));
                }
                TokenKind::Period => {
                    input.next();
                    if let Some(func_call) = input.parse() {
                        accessors.push(AccessorOp::MemberFunc(func_call));
                    } else {
                        panic!("Expected an accessor after '.'");
                    };
                }
                _ => break,
            }
        }

        Accessor(left, accessors).into()
    }
}

#[derive(Debug)]
pub enum Base {
    Literal(Literal),
    Array(CommaSepVals<Expr>),
    Grouping(Expr),
    FuncCall(FuncCall),
}

impl Parse for Base {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if let Some(func_call) = input.parse() {
            println!("FuncCall");
            return Base::FuncCall(func_call).into();
        };

        if let Some(lit) = input.parse() {
            println!("Lit");
            return Base::Literal(lit).into();
        };

        if input.next_if_kind(&TokenKind::LeftBracket).is_some() {
            let arr = Base::Array(input.parse()?);
            input
                .next_if_kind(&TokenKind::RightBracket)
                .expect("Expect closing ]");
            return arr.into();
        }

        if input.next_if_kind(&TokenKind::LeftParen).is_some() {
            let grouping = Base::Grouping(input.parse()?);
            input
                .next_if_kind(&TokenKind::RightParen)
                .expect("Expect closing ]");
            return grouping.into();
        }

        None
    }
}

#[derive(Debug)]
pub enum FuncCall {
    FuncCall(Identifier, CommaSepVals<Expr>),
    Identifer(Identifier),
}

impl Parse for FuncCall {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        // println!("FuncParse {:?}", input.post_cursor());
        let iden = input.parse()?;
        // println!("iden: {:?}, pc: {:?}", iden, input.post_cursor());
        if input.next_if_kind(&TokenKind::LeftParen).is_none() {
            return FuncCall::Identifer(iden).into();
        };

        let args = input.parse()?;
        println!("args: {:?}, \n\n pc: {:?}", args, input.post_cursor());
        input
            .next_if_kind(&TokenKind::RightParen)
            .expect("Expected a ')'");

        FuncCall::FuncCall(iden, args).into()
    }
}

#[derive(Debug)]
pub struct Identifier(String);
impl Parse for Identifier {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let res = match &input.peek()?.kind {
            TokenKind::Identifier(name) => Identifier(name.to_string()).into(),
            _ => None,
        };
        input.next();

        res
    }
}

#[derive(Debug)]
pub struct CommaSepVals<T>(Vec<T>);
impl<T: Parse> Parse for CommaSepVals<T> {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let Some(expr) = input.parse() else {
            return CommaSepVals(vec![]).into();
        };

        let mut exprs = vec![expr];
        while input.next_if_kind(&TokenKind::Comma).is_some() {
            exprs.push(input.parse()?);
        }

        CommaSepVals(exprs).into()
    }
}
