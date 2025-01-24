/*
Grammar:
Expr      ::= Term

Term      ::= Factor TerTail
TerTail   ::= '+' Factor TerTail | '*' Factor TerTail | Nil

Factor    ::= Unary FacTail
FacTail   ::= '*' Unary FacTail | '+' Unary FacTail | Nil

Unary     ::= '-' Unary | '!' Unary | Literal

Literal   ::= String | Number | Bool | Grouping
Grouping  ::= '(' Literal ')'
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
    Grouping(Box<Expr>)
}

impl Parse for Literal {
    fn parse(tokens: &[Token]) -> Option<(Self, &[Token])> where Self: Sized {
        let literal = match &tokens.get(0)?.token_type {
            TokenType::Number(n) => Literal::Number(n.clone()),
            TokenType::String(s) => Literal::String(s.clone()),
            TokenType::Bool(b) => Literal::Bool(b.clone()),
            TokenType::LeftParen => {
                let (expr, rest) = Expr::parse(&tokens[1..])?;
                if rest.get(0).is_none() || rest.get(0).unwrap().token_type != TokenType::RightParen {
                    panic!("Error: Expected a closing paren");
                };

                return Some((Literal::Grouping(Box::from(expr)), &rest[1..]))
            },
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
        match &tokens.get(0)?.token_type {
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
pub struct  Factor(pub Unary, pub FacTail);
impl Parse for Factor {
    fn parse(tokens: &[Token]) -> Option<(Self, &[Token])> where Self: Sized {
        let (unary, rest1) = Unary::parse(tokens)?;
        let (tail, rest2) = FacTail::parse(rest1)?;

        Some((Factor(unary, tail), rest2))
    }
}

#[derive(Debug)]
pub enum FacTail {
    Mul(Unary, Box<FacTail>),
    Div(Unary, Box<FacTail>),
    Nil
}

impl Parse for FacTail {
    fn parse(tokens: &[Token]) -> Option<(Self, &[Token])> where Self: Sized {
        if tokens.is_empty() {
            return Some((FacTail::Nil, tokens))
        };

        let op = match tokens[0].token_type {
            TokenType::Star => FacTail::Mul,
            TokenType::Slash => FacTail::Div,
            _ => return Some((FacTail::Nil, tokens)),
        };

        let (right, rest) = Unary::parse(&tokens[1..])?;
        let (tail, rest2) = FacTail::parse(rest)?;

        Some((op(right, Box::from(tail)), rest2))
    }
}


#[derive(Debug)]
pub struct  Term(pub Factor, pub TerTail);
impl Parse for Term {
    fn parse(tokens: &[Token]) -> Option<(Self, &[Token])> where Self: Sized {
        let (factor, rest1) = Factor::parse(tokens)?;
        let (tail, rest2) = TerTail::parse(rest1)?;

        Some((Term(factor, tail), rest2))
    }
}

#[derive(Debug)]
pub enum TerTail {
    Add(Factor, Box<TerTail>),
    Sub(Factor, Box<TerTail>),
    Nil
}

impl Parse for TerTail {
    fn parse(tokens: &[Token]) -> Option<(Self, &[Token])> where Self: Sized {
        if tokens.is_empty() {
            return Some((TerTail::Nil, tokens))
        };

        let op = match tokens[0].token_type {
            TokenType::Plus => TerTail::Add,
            TokenType::Minus => TerTail::Sub,
            _ => return Some((TerTail::Nil, tokens)),
        };

        let (right, rest) = Factor::parse(&tokens[1..])?;
        let (tail, rest2) = TerTail::parse(rest)?;

        Some((op(right, Box::from(tail)), rest2))
    }
}


#[derive(Debug)]
pub struct Expr(pub Term);

impl Parse for Expr {
    fn parse(tokens: &[Token]) -> Option<(Self, &[Token])> where Self: Sized {
        let (term, rest) = Term::parse(tokens)?;
        Some((Expr(term), rest))
    }
}