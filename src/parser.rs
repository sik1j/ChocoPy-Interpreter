/*
Grammar:
Expr    ::= Term
Term    ::= Factor '+' Term | Factor '-' Term | Factor
Factor  ::= Unary '*' Factor | Unary '/' Factor | Unary
Unary   ::= '-' Unary | '!' Unary | Literal
Literal ::= String | Number | Bool
*/

use crate::tokenizer::{Token, TokenType};

pub fn parse(tokens: &[Token]) -> Expr {
    let (t, _) = Expr::parse(tokens).unwrap();
    t
}

trait Parse {
    fn parse(tokens: &[Token]) -> Option<(Self, &[Token])> where Self: Sized;
}

#[derive(Debug)]
pub enum Literal {
    String(String),
    Number(f64),
    Bool(bool),
}

impl Parse for Literal {
    fn parse(tokens: &[Token]) -> Option<(Self, &[Token])> where Self: Sized {
        let literal = match &tokens[0].token_type {
            TokenType::Number(n) => Literal::Number(n.clone()),
            TokenType::String(s) => Literal::String(s.clone()),
            TokenType::Bool(b) => Literal::Bool(b.clone()),
            _ => return None
        };

        Some((literal, &tokens[1..]))
    }
}

#[derive(Debug)]
pub enum Unary {
    NegNum(Box<Unary>),
    NegBool(Box<Unary>),
    Lit(Literal),
}

impl Parse for Unary {
    fn parse(tokens: &[Token]) -> Option<(Self, &[Token])> where Self: Sized {
        match &tokens[0].token_type {
            t @ (TokenType::Minus | TokenType::Bang) =>
                Unary::parse(&tokens[1..])
                    .map(|(unary, rest)| {
                        let unary_type = match t {
                            TokenType::Minus => Unary::NegNum,
                            TokenType::Bang => Unary::NegBool,
                            _ => unreachable!()
                        };

                        (unary_type(Box::new(unary)), rest)
                    }
                    ),
            _ => Literal::parse(tokens)
                .map(|(lit, rest)| (Unary::Lit(lit), rest)),
        }
    }
}

#[derive(Debug)]
pub enum Factor {
    Mul(Unary, Box<Factor>),
    Div(Unary, Box<Factor>),
    Una(Unary),
}

impl Parse for Factor {
    fn parse(tokens: &[Token]) -> Option<(Self, &[Token])> where Self: Sized {
        let (left, rest1) = Unary::parse(tokens)?;
        let op = match rest1[0].token_type {
            TokenType::Star => Factor::Mul,
            TokenType::Slash => Factor::Div,
            _ => return Some((Factor::Una(left), rest1)),
        };
        let (right, rest2) = Factor::parse(&rest1[1..])?;

        Some((op(left, Box::from(right)), rest2))
    }
}

#[derive(Debug)]
pub enum Term {
    Add(Factor, Box<Term>),
    Sub(Factor, Box<Term>),
    Fac(Factor),
}

impl Parse for Term {
    fn parse(tokens: &[Token]) -> Option<(Self, &[Token])> where Self: Sized {
        let (left, rest1) = Factor::parse(tokens)?;
        let op = match rest1[0].token_type {
            TokenType::Plus => Term::Add,
            TokenType::Minus => Term::Sub,
            _ => return Some((Term::Fac(left), rest1)),
        };
        let (right, rest2) = Term::parse(&rest1[1..])?;

        Some((op(left, Box::from(right)), rest2))
    }
}


#[derive(Debug)]
pub struct Expr(pub(crate) Term);

impl Parse for Expr {
    fn parse(tokens: &[Token]) -> Option<(Self, &[Token])> where Self: Sized {
        Term::parse(tokens)
            .map(|(term, rest)| (Expr(term), rest))
    }
}