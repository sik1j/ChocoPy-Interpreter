/*
Grammar: lit [x], bin_op [x], [expr..=not_op][x]
literal                   ::= None
                            | True
                            | False
                            | Integer
                            | IdString | String

expr                      ::= or_op [ 'if' or_op 'else' expr ]?
or_op                     ::= and_op [ 'or' and_op ]*
and_op                    ::= not_op [ 'and' not_op ]*
not_op                    ::= [ 'not' ]* cexpr


cexpr                     ::= IDENTIFIER
                            | literal
                            | '[' [ expr [,expr]* ]? ']'
                            | '(' expr ')'
                            | member_expr
                            | index_expr
                            | member_expr '(' [ expr [,expr]* ]? ')'
                            | IDENTIFIER '(' [ expr [,expr]* ]? ')'
                            | cexpr bin_op cexpr
                            | - cexpr

bin_op                    ::= '+' | '-' | '*' | '//' | '%' | '=='
                            | '!=' | '<=' | '>=' | '<' | '>' | 'is'

member_expr               ::= cexpr . IDENTIFIER
index_expr                ::= cexpr [ expr ]
target                    ::= IDENTIFIER
                            | member_expr
                            | index_expr

*/

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
pub struct OrOp(AndOp, Vec<AndOp>);
impl Parse for OrOp {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let left = input.parse()?;

        let mut rest = vec![];
        while input.next_if(|t| t.kind == TokenKind::And).is_some() {
            rest.push(input.parse()?)
        }

        Some(OrOp(left, rest))
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
    Cexpr(CExpr),
}
impl Parse for NotOp {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if input.next_if(|t| t.kind == TokenKind::Not).is_some() {
            let rest = Box::new(input.parse()?);
            return NotOp::Not(rest).into();
        };

        let cexpr = input.parse()?;
        NotOp::Cexpr(cexpr).into()
    }
}

#[derive(Debug)]
pub enum CExpr {
    Literal(Literal),
}
impl Parse for CExpr {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        Some(CExpr::Literal(input.parse()?))
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    IntDiv,
    Mod,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    Less,
    Greater,
    Is,
}
impl Parse for BinaryOp {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        todo!()
    }
}
