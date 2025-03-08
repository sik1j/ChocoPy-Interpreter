/*
Grammar:


program           ::= [ var_def | func_def | class_def ]* statement*
class_def         ::= 'class' ID '(' ID ')' ':' NEWLINE INDENT class_body DEDENT
class_body        ::= pass NEWLINE
                    | [ var_def | func_def ]+

func_def          ::= 'def' ID ( [ typed_var [ , typed_var ]* ]? ) [ '->' type ]? ':' NEWLINE INDENT func_body DEDENT
func_body         ::= [ global_decl | nonlocal_decl | var_def | func_def ]* statement+

typed_var         ::= ID ':' type
type              ::= ID | IDSTRING | [type]

global_decl       ::= 'global' ID NEWLINE
nonlocal_decl     ::= 'nonlocal' ID NEWLINE
var_def           ::= typed_var '=' literal NEWLINE

statement         ::= simple_statement NEWLINE
                    | 'if' expr ':' block [ 'elif' expr ':' block ]* [ 'else' ':' block ]?
                    | 'while' expr ':' block
                    | 'for' ID 'in' expr : block

simple_statement  ::= 'pass'
                    | [ target '=' ]+ expr
                    | expr
                    | 'return' [ expr ]?

block             ::= NEWLINE INDENT statement+ DEDENT

literal           ::= None
                    | True
                    | False
                    | Integer
                    | IDSTRING | String

expr              ::= or_op [ 'if' or_op 'else' expr ]?
or_op             ::= and_op [ 'or' and_op ]*
and_op            ::= not_op [ 'and' not_op ]*
not_op            ::= [ 'not' ]* comparison

comparison        ::= term [ [ '==' | '!=' | '<=' | '>=' | '<' | '>' ] term ]*
term              ::= factor [ [ '+' | '-' ] factor ]*
factor            ::= negation [ [ '*' | '//' | '%' ] negation ]*
negation          ::= '-'* accessor

accessor          ::= base accessor_op*
accessor_op       ::= . func_call
                    | '[' expr ']'

base              ::= func_call
                    | literal
                    | '[' [ expr [ , expr ]* ]? ']'
                    | '(' expr ')'
func_call         ::= ID [ '(' [ expr [ , expr ]* ]? ')' ]?


target            ::= ID
                    | base accessor_op+
*/

// todo: get rid of the panics and '?' whenever sensible
// todo: review all the code again, and use the newer functions whenever possible
use core::panic;
use std::collections::HashMap;

use crate::tokenizer::{Token, TokenKind};

#[derive(Debug)]
pub struct Span<T> {
    pub item: T,
    pub line: usize,
}
impl<T> Span<T> {
    fn new(item: T, line: usize) -> Self {
        Self { item, line }
    }
}

pub trait GetPosition {
    fn get_line(&self) -> usize;
}

macro_rules! expr_tuple_impl_get_pos {
    ($($name:ty),+) => {
        $(
          impl GetPosition for $name {
              fn get_line(&self) -> usize {
                  self.0.get_line()
              }
          }
        )*
    };
}

expr_tuple_impl_get_pos!(OrOp, AndOp, Comparison, Term, Factor);

impl GetPosition for NotOp {
    fn get_line(&self) -> usize {
        self.comparison.get_line()
    }
}

impl GetPosition for NegInt {
    fn get_line(&self) -> usize {
        self.accessor.get_line()
    }
}

#[derive(Debug)]
pub struct Cursor<T> {
    list: Vec<T>,
    cursor: usize,
}

impl<T> Cursor<T> {
    pub fn new(list: Vec<T>) -> Self {
        Cursor { list, cursor: 0 }
    }

    pub fn at_end(&self) -> bool {
        self.cursor >= self.list.len()
    }

    pub fn peek(&self) -> Option<&T> {
        self.list.get(self.cursor)
    }

    pub fn next(&mut self) -> Option<&T> {
        if !self.at_end() {
            self.cursor += 1;
            return self.list.get(self.cursor - 1);
        };

        None
    }

    pub fn next_if<F: Fn(&T) -> bool>(&mut self, f: F) -> Option<&T> {
        if f(self.peek()?) {
            return self.next();
        };
        None
    }

    pub fn create_checkpoint(&self) -> usize {
        self.cursor
    }

    pub fn set_checkpoint(&mut self, cursor: usize) {
        self.cursor = cursor;
    }

    pub fn post_cursor(&self) -> &[T] {
        &self.list[self.cursor..]
    }
}

impl Cursor<Token> {
    pub fn parse<T: Parse>(&mut self) -> Option<T> {
        let checkpoint = self.create_checkpoint();
        match T::parse(self) {
            None => {
                self.set_checkpoint(checkpoint);
                None
            }
            tok => tok,
        }
    }

    pub fn parse_one_or_more<T>(
        &mut self,
        mut parse_fn: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<OneOrMore<T>> {
        let checkpoint = self.create_checkpoint();
        let Some(guaranteed) = parse_fn(self) else {
            self.set_checkpoint(checkpoint);
            return None;
        };

        let mut rest = vec![];
        loop {
            let checkpoint = self.create_checkpoint();
            let opt_t = parse_fn(self);
            match opt_t {
                Some(t) => rest.push(t),
                None => {
                    self.set_checkpoint(checkpoint);
                    break;
                }
            }
        }

        OneOrMore {
            one: guaranteed,
            more: rest,
        }
        .into()
    }

    pub fn parse_zero_or_more<T>(
        &mut self,
        parse_fn: impl FnMut(&mut Self) -> Option<T>,
    ) -> Vec<T> {
        self.parse_one_or_more(parse_fn)
            .map_or(vec![], |oom| oom.into_iter().collect::<Vec<_>>())
    }

    pub fn next_if_kind(&mut self, kind: &TokenKind) -> Option<&Token> {
        self.next_if(|t| std::mem::discriminant(&t.kind) == std::mem::discriminant(kind))
    }

    pub fn msg_with_line(&self, msg: &str) -> String {
        let line = match self.peek() {
            Some(t) => t.line,
            None => self.list.last().map_or(0, |t| t.line),
        };
        format!("[Line: {line}] {msg}")
    }

    pub fn expect_kind(&mut self, kind: &TokenKind, msg: &str) -> &Token {
        let msg = self.msg_with_line(msg);
        self.next_if_kind(kind).expect(&msg)
    }

    pub fn parse_expect<T: Parse>(&mut self, msg: &str) -> T {
        self.parse().expect(&self.msg_with_line(msg))
    }

    pub fn panic(&self, msg: &str) {
        panic!("{}", self.msg_with_line(msg))
    }
}

#[derive(Debug)]
pub struct OneOrMore<T> {
    pub one: T,
    pub more: Vec<T>,
}

impl<T> OneOrMore<T> {
    pub fn iter(&self) -> std::iter::Chain<std::iter::Once<&T>, std::slice::Iter<'_, T>> {
        self.into_iter()
    }
}

pub trait IteratorOneOrMoreExt: Iterator + Sized {
    fn collect_one_or_more(self) -> Option<OneOrMore<Self::Item>> {
        let mut iter = self;
        let one = iter.next()?;
        let more = iter.collect();
        Some(OneOrMore { one, more })
    }
}

impl<I: Iterator> IteratorOneOrMoreExt for I {}

impl<'a, T: 'a> IntoIterator for &'a OneOrMore<T> {
    type Item = &'a T;

    type IntoIter = std::iter::Chain<std::iter::Once<&'a T>, std::slice::Iter<'a, T>>;

    fn into_iter(self) -> Self::IntoIter {
        std::iter::once(&self.one).chain(&self.more)
    }
}

impl<T> IntoIterator for OneOrMore<T> {
    type Item = T;

    type IntoIter = std::iter::Chain<std::iter::Once<T>, std::vec::IntoIter<T>>;

    fn into_iter(self) -> Self::IntoIter {
        std::iter::once(self.one).chain(self.more)
    }
}

pub fn parse(input: &mut Cursor<Token>) -> Program {
    input.parse_expect("Parser Error")
}

pub trait Parse: Sized {
    fn parse(input: &mut Cursor<Token>) -> Option<Self>;
}

#[derive(Debug)]
pub struct Program {
    pub definitions: Vec<Definition>,
    pub statements: Vec<Statement>,
}

impl Parse for Program {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let definitions = input.parse_zero_or_more(Cursor::parse);
        let statements = input.parse_zero_or_more(Cursor::parse);

        println!("stmt.len(): {}\n\n", statements.len());

        Program {
            definitions,
            statements,
        }
        .into()
    }
}

#[derive(Debug)]
pub enum Definition {
    VarDef(VarDef),
    FuncDef(FuncDef),
    ClassDef(ClassDef),
}

impl Parse for Definition {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if let Some(var_def) = input.parse() {
            return Definition::VarDef(var_def).into();
        };

        if let Some(func_def) = input.parse() {
            return Definition::FuncDef(func_def).into();
        };

        Definition::ClassDef(input.parse()?).into()
    }
}

#[derive(Debug)]
pub struct ClassDef {
    pub name: Identifier,
    pub super_class: Identifier,
    pub body: ClassBody,
}

impl Parse for ClassDef {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        input.next_if_kind(&TokenKind::Class)?;
        let name = input.parse_expect("Expected class name");

        input.expect_kind(&TokenKind::LeftParen, "Expected opening `(`");
        let super_class = input.parse_expect("Expected super class name");

        input.expect_kind(&TokenKind::RightParen, "Expected closing `)`");
        input.expect_kind(&TokenKind::Colon, "Expected `:`");
        input.expect_kind(&TokenKind::Newline, "Expected newline");
        input.expect_kind(&TokenKind::Indent, "Expected an indent after class def");

        let body = input.parse_expect("Expected class body");
        input.expect_kind(&TokenKind::Dedent, "Expected a dedent after class body");

        ClassDef {
            name,
            super_class,
            body,
        }
        .into()
    }
}

#[derive(Debug)]
pub enum ClassBody {
    Pass,
    Definitions(OneOrMore<DefinitionInClass>),
}

impl Parse for ClassBody {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if input.next_if_kind(&TokenKind::Pass).is_some() {
            input.expect_kind(&TokenKind::Newline, "Expected newline");
            return ClassBody::Pass.into();
        };

        ClassBody::Definitions(input.parse_one_or_more(Cursor::parse)?).into()
    }
}

#[derive(Debug)]
pub enum DefinitionInClass {
    VarDef(VarDef),
    FuncDef(FuncDef),
}

impl Parse for DefinitionInClass {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if let Some(var_def) = input.parse() {
            return DefinitionInClass::VarDef(var_def).into();
        }

        DefinitionInClass::FuncDef(input.parse()?).into()
    }
}

#[derive(Debug)]
pub struct FuncDef {
    pub name: Identifier,
    pub params: Vec<TypedVar>,
    pub return_type: Option<Type>,
    pub func_body: FuncBody,
}

impl Parse for FuncDef {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        input.next_if_kind(&TokenKind::Def)?;
        let name = input.parse_expect("Expected a name");
        input.expect_kind(&TokenKind::LeftParen, "Expected an opening `(`");

        let params = if let Some(param1) = input.parse() {
            let mut res = vec![param1];
            res.append(&mut input.parse_zero_or_more(|nput| {
                nput.next_if_kind(&TokenKind::Comma)?;
                nput.parse()
            }));
            res
        } else {
            vec![]
        };
        input.expect_kind(&TokenKind::RightParen, "Expected an closing `)`");

        let return_type = input
            .next_if_kind(&TokenKind::Arrow)
            .is_some()
            .then(|| input.parse_expect::<Type>("Expected a return type"));

        input.expect_kind(&TokenKind::Colon, "Expected a `:`");
        input.expect_kind(&TokenKind::Newline, "Expected a newline_1");
        input.expect_kind(&TokenKind::Indent, "Expected a Indent");

        let func_body = input.parse_expect("Expected a body");
        input.expect_kind(&TokenKind::Dedent, "Expected a Dedent");

        FuncDef {
            name,
            params,
            return_type,
            func_body,
        }
        .into()
    }
}

#[derive(Debug)]
pub struct FuncBody {
    pub declarations: Vec<Declaration>,
    pub statements: OneOrMore<Statement>,
}

impl Parse for FuncBody {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let declarations = input.parse_zero_or_more(Cursor::parse);
        let statements = input.parse_one_or_more(Cursor::parse)?;
        FuncBody {
            declarations,
            statements,
        }
        .into()
    }
}

#[derive(Debug)]
pub enum Declaration {
    GlobalDecl(GlobalDecl),
    NonLocalDecl(NonLocalDecl),
    VarDef(VarDef),
    FuncDef(FuncDef),
}

impl Parse for Declaration {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if let Some(global) = input.parse() {
            return Declaration::GlobalDecl(global).into();
        }

        if let Some(non_local) = input.parse() {
            return Declaration::NonLocalDecl(non_local).into();
        }

        if let Some(var_def) = input.parse() {
            return Declaration::VarDef(var_def).into();
        }

        if let Some(func_def) = input.parse() {
            return Declaration::FuncDef(func_def).into();
        };

        None
    }
}

#[derive(Debug)]
pub struct TypedVar {
    pub name: Identifier,
    pub r#type: Type,
}

impl Parse for TypedVar {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let name = input.parse()?;
        input.next_if_kind(&TokenKind::Colon)?;
        let r#type = input.parse_expect("Expected a type");

        TypedVar { name, r#type }.into()
    }
}

impl GetPosition for TypedVar {
    fn get_line(&self) -> usize {
        self.name.line
    }
}

#[derive(Debug)]
pub enum Type {
    Identifier(Identifier),
    IdString(String),
    Array(Box<Type>),
}

impl Parse for Type {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if input.peek()?.kind == TokenKind::None {
            input.panic("`None` is not valid. Please omit `-> None` if no return value")
        }

        if let Some(Token {
            kind: TokenKind::String(str),
            ..
        }) = input.peek()
        {
            let str = str.to_string();
            input.next();
            return Type::IdString(str).into();
        }

        if let Some(iden) = input.parse() {
            return Type::Identifier(iden).into();
        }

        input.next_if_kind(&TokenKind::LeftBracket)?;
        let my_type = input.parse()?;
        input
            .next_if_kind(&TokenKind::RightBracket)
            .expect("Expected closing ]");

        Type::Array(Box::new(my_type)).into()
    }
}

#[derive(Debug)]
pub struct GlobalDecl(Identifier);
impl Parse for GlobalDecl {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        input.next_if_kind(&TokenKind::Global)?;

        let iden = input.parse()?;
        input.expect_kind(&TokenKind::Newline, "Expected newline");
        GlobalDecl(iden).into()
    }
}

#[derive(Debug)]
pub struct NonLocalDecl(Identifier);
impl Parse for NonLocalDecl {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        input.next_if_kind(&TokenKind::NonLocal)?;

        let iden = input.parse()?;
        input.expect_kind(&TokenKind::Newline, "Expected newline");
        NonLocalDecl(iden).into()
    }
}

#[derive(Debug)]
pub struct VarDef {
    pub typed_var: TypedVar,
    pub value: Literal,
}
impl Parse for VarDef {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let typed_var = input.parse()?;
        input.next_if_kind(&TokenKind::Equal)?;

        let value = input
            .parse()
            .expect("Expected a literal. Declarations do not support expressions");
        input
            .next_if_kind(&TokenKind::Newline)
            .expect("Expected a newline_3. Typed declarations do not support expressions");

        VarDef { typed_var, value }.into()
    }
}
impl GetPosition for VarDef {
    fn get_line(&self) -> usize {
        self.typed_var.get_line()
    }
}

#[derive(Debug)]
pub enum Statement {
    Simple(SimpleStatement),
    IfStmt(IfStmt),
    WhileLoop {
        condition: Expr,
        body: Block,
    },
    ForLoop {
        item: Identifier,
        iterator: Expr,
        body: Block,
    },
}
impl Parse for Statement {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let colon_msg = "Expected a ':'";
        let body_msg = "Expected a body";

        if let Some(simple) = input.parse() {
            input.expect_kind(&TokenKind::Newline, "Expected a newline_2");
            return Statement::Simple(simple).into();
        };

        if input.next_if_kind(&TokenKind::While).is_some() {
            let condition = input.parse()?;
            input.expect_kind(&TokenKind::Colon, &colon_msg);

            return Statement::WhileLoop {
                condition,
                body: input.parse_expect(&body_msg),
            }
            .into();
        };

        if input.next_if_kind(&TokenKind::For).is_some() {
            let item = input.parse()?;

            input.expect_kind(&TokenKind::In, "Expected a 'in'");

            let iterator = input.parse_expect("Expected an iterator");
            input.expect_kind(&TokenKind::Colon, &colon_msg);

            return Statement::ForLoop {
                item,
                iterator,
                body: input.parse_expect(&body_msg),
            }
            .into();
        };

        Statement::IfStmt(input.parse()?).into()
    }
}

#[derive(Debug)]
pub struct IfStmt {
    r#if: (Expr, Block),
    elifs: Vec<(Expr, Block)>,
    r#else: Option<Block>,
}

impl Parse for IfStmt {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let cond_msg = "Expected an expression";
        let colon_msg = "Expected a `:`";
        let body_msg = "Expected a body";

        input.next_if_kind(&TokenKind::If)?;
        let if_cond = input.parse_expect(cond_msg);
        input.expect_kind(&TokenKind::Colon, colon_msg);
        let if_body = input.parse_expect(body_msg);

        let elifs = input.parse_zero_or_more(|nput| {
            nput.next_if_kind(&TokenKind::Elif)?;

            let cond = nput.parse_expect(&cond_msg);
            nput.expect_kind(&TokenKind::Colon, &colon_msg);

            let body = nput.parse_expect(&body_msg);
            (cond, body).into()
        });

        if input.next_if_kind(&TokenKind::Else).is_none() {
            return IfStmt {
                r#if: (if_cond, if_body),
                elifs,
                r#else: None,
            }
            .into();
        }

        input.expect_kind(&TokenKind::Colon, &colon_msg);
        IfStmt {
            r#if: (if_cond, if_body),
            elifs,
            r#else: input.parse_expect::<Block>(&body_msg).into(),
        }
        .into()
    }
}

#[derive(Debug)]
pub enum SimpleStatement {
    Pass,
    Expr(Expr),
    Return(Option<Expr>),
    Assignments {
        targets: OneOrMore<Target>,
        expr: Expr,
    },
}

impl Parse for SimpleStatement {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if input.next_if_kind(&TokenKind::Pass).is_some() {
            return SimpleStatement::Pass.into();
        };

        let targets = input.parse_one_or_more(|nput| {
            let target = nput.parse::<Target>()?;
            nput.next_if_kind(&TokenKind::Equal)?;
            target.into()
        });

        if targets.is_some() {
            return SimpleStatement::Assignments {
                targets: targets?,
                expr: input.parse_expect("Expected an expression after '='"),
            }
            .into();
        }

        if input.next_if_kind(&TokenKind::Return).is_some() {
            return SimpleStatement::Return(input.parse()).into();
        };

        SimpleStatement::Expr(input.parse()?).into()
    }
}

#[derive(Debug)]
pub struct Block(OneOrMore<Box<Statement>>);
impl Parse for Block {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        input.next_if_kind(&TokenKind::Newline)?;
        input.next_if_kind(&TokenKind::Indent)?;

        let statements = input.parse_one_or_more(Cursor::parse)?;
        input.expect_kind(&TokenKind::Dedent, "Expected a dedent");

        Block(
            statements
                .into_iter()
                .map(|t| Box::new(t))
                .collect_one_or_more()?,
        )
        .into()
    }
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
        let lit = match &input.next()?.kind {
            TokenKind::None => Literal::None,
            TokenKind::True => Literal::True,
            TokenKind::False => Literal::False,
            TokenKind::Integer(n) => Literal::Integer(*n),
            TokenKind::String(s) => Literal::String(s.clone()),
            _ => return None,
        };

        Some(lit)
    }
}

#[derive(Debug)]
pub struct Expr {
    pub or_op: OrOp,
    pub if_expr: Option<(OrOp, Box<Expr>)>,
}
impl GetPosition for Expr {
    fn get_line(&self) -> usize {
        self.or_op.get_line()
    }
}

impl Parse for Expr {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let or1 = input.parse()?;
        if input.next_if_kind(&TokenKind::If).is_none() {
            return Expr {
                or_op: or1,
                if_expr: None,
            }
            .into();
        }

        let or2 = input.parse_expect("Expected an expression");
        input.expect_kind(&TokenKind::Else, "Expected `else`");
        let else_expr = input.parse_expect("Expected an expression");

        Expr {
            or_op: or1,
            if_expr: (or2, Box::new(else_expr)).into(),
        }
        .into()
    }
}

#[derive(Debug)]
pub struct OrOp(pub Box<AndOp>, pub Vec<AndOp>);

impl Parse for OrOp {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let left = input.parse()?;

        let mut rest = vec![];
        while let Some(Token { kind: _, line }) = input.next_if_kind(&TokenKind::Or) {
            rest.push(input.parse_expect("Expected expression after `or`"));
        }

        Some(OrOp(Box::new(left), rest))
    }
}

#[derive(Debug)]
pub struct AndOp(pub NotOp, pub Vec<NotOp>);
impl Parse for AndOp {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let left = input.parse()?;

        let mut rest = vec![];
        while input.next_if_kind(&TokenKind::And).is_some() {
            rest.push(input.parse_expect("Expected expression after `and`"));
        }

        Some(AndOp(left, rest))
    }
}

#[derive(Debug)]
pub struct NotOp {
    pub op_count: usize,
    pub comparison: Comparison,
}

impl Parse for NotOp {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let mut op_count = 0;
        while input.next_if_kind(&TokenKind::Not).is_some() {
            op_count += 1;
        }

        let comparison = input.parse()?;
        NotOp {
            op_count,
            comparison,
        }
        .into()
    }
}

fn parse_binary<R, O: Copy, N: Parse>(
    input: &mut Cursor<Token>,
    ret_type: fn(N, Vec<(O, N)>) -> R,
    tok_to_op: HashMap<TokenKind, O>,
) -> Option<R> {
    let left = input.parse()?;

    let rest = input.parse_zero_or_more(|nput| {
        let op_type = *tok_to_op.get(&nput.next()?.kind)?;
        let right = nput.parse_expect("Expected expression after op");
        (op_type, right).into()
    });

    ret_type(left, rest).into()
}

#[derive(Debug)]
pub struct Comparison(pub Term, pub Vec<(ComparisonOp, Term)>);
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ComparisonOp {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Is,
}

// todo: comparions should be non-assocciative
// unlike python3, x < y < z is not valid
impl Parse for Comparison {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        parse_binary(
            input,
            Self,
            HashMap::from([
                (TokenKind::EqualEqual, ComparisonOp::Equal),
                (TokenKind::BangEqual, ComparisonOp::NotEqual),
                (TokenKind::Less, ComparisonOp::Less),
                (TokenKind::LessEqual, ComparisonOp::LessEqual),
                (TokenKind::Greater, ComparisonOp::Greater),
                (TokenKind::GreaterEqual, ComparisonOp::GreaterEqual),
                (TokenKind::Is, ComparisonOp::Is),
            ]),
        )
    }
}

#[derive(Debug)]
pub struct Term(pub Factor, pub Vec<(TermOp, Factor)>);
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TermOp {
    Add,
    Subtract,
}
impl Parse for Term {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        parse_binary(
            input,
            Self,
            HashMap::from([
                (TokenKind::Plus, TermOp::Add),
                (TokenKind::Minus, TermOp::Subtract),
            ]),
        )
    }
}

#[derive(Debug)]
pub struct Factor(pub NegInt, pub Vec<(FactorOp, NegInt)>);
#[derive(Debug, Clone, Copy, PartialEq)]
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
            HashMap::from([
                (TokenKind::Star, FactorOp::Multiply),
                (TokenKind::SlashSlash, FactorOp::IntDiv),
                (TokenKind::Percent, FactorOp::Modulo),
            ]),
        )
    }
}

#[derive(Debug)]
pub struct NegInt {
    pub op_count: usize,
    pub accessor: Accessor,
}
impl Parse for NegInt {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let mut op_count = 0;
        while input.next_if_kind(&TokenKind::Minus).is_some() {
            op_count += 1;
        }

        let accessor = input.parse()?;
        NegInt { op_count, accessor }.into()
    }
}

#[derive(Debug)]
pub enum Accessor {
    Base(Base),
    Accessors(Base, OneOrMore<AccessorOp>),
}
impl GetPosition for Accessor {
    fn get_line(&self) -> usize {
        match self {
            Accessor::Base(base) => base.get_line(),
            Accessor::Accessors(base, ..) => base.get_line(),
        }
    }
}
impl Parse for Accessor {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let base = input.parse()?;
        let Some(accessor_ops) = input
            .parse_zero_or_more(Cursor::parse)
            .into_iter()
            .collect_one_or_more()
        else {
            return Accessor::Base(base).into();
        };

        Accessor::Accessors(base, accessor_ops).into()
    }
}

#[derive(Debug)]
pub enum AccessorOp {
    Index(Expr),
    MemberFunc(FuncCall),
}
impl GetPosition for AccessorOp {
    fn get_line(&self) -> usize {
        match self {
            AccessorOp::Index(expr) => expr.get_line(),
            AccessorOp::MemberFunc(func_call) => func_call.get_line(),
        }
    }
}
impl Parse for AccessorOp {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if input.next_if_kind(&TokenKind::LeftBracket).is_some() {
            let expr = input.parse_expect("Expected an expression");
            input.expect_kind(&TokenKind::RightBracket, "Expected a `]`");
            return AccessorOp::Index(expr).into();
        };

        if input.next_if_kind(&TokenKind::Period).is_some() {
            let func_call = input.parse_expect("Expected an member/method");
            return AccessorOp::MemberFunc(func_call).into();
        };

        None
    }
}

#[derive(Debug)]
pub enum Base {
    Literal(Span<Literal>),
    List(Span<Vec<Expr>>),
    Grouping(Span<Expr>),
    FuncCall(Span<FuncCall>),
}

impl GetPosition for Base {
    fn get_line(&self) -> usize {
        match self {
            Base::Literal(span) => span.line,
            Base::List(span) => span.line,
            Base::Grouping(span) => span.line,
            Base::FuncCall(span) => span.line,
        }
    }
}

impl Parse for Base {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let line = input.peek()?.line;
        if let Some(func_call) = input.parse() {
            return Base::FuncCall(Span::new(func_call, line)).into();
        };

        if let Some(lit) = input.parse() {
            return Base::Literal(Span::new(lit, line)).into();
        };

        if input.next_if_kind(&TokenKind::LeftBracket).is_some() {
            let Some(elem1) = input.parse() else {
                input.expect_kind(&TokenKind::RightBracket, "Expected a closing `]`");
                return Base::List(Span::new(vec![], line)).into();
            };

            let mut elems = vec![elem1];
            elems.append(&mut input.parse_zero_or_more(|nput| {
                nput.next_if_kind(&TokenKind::Comma)?;
                nput.parse()
            }));

            input.expect_kind(&TokenKind::RightBracket, "Expected a closing `]`");
            return Base::List(Span::new(elems, line)).into();
        };

        if input.next_if_kind(&TokenKind::LeftParen).is_some() {
            let grouping = Base::Grouping(Span::new(input.parse()?, line));
            input.expect_kind(&TokenKind::RightParen, "Expect closing `)`");
            return grouping.into();
        };

        None
    }
}

#[derive(Debug)]
pub enum FuncCall {
    FuncCall { name: Identifier, args: Vec<Expr> },
    Identifier(Identifier),
}
impl GetPosition for FuncCall {
    fn get_line(&self) -> usize {
        match self {
            FuncCall::FuncCall { name, .. } => name.get_line(),
            FuncCall::Identifier(identifier) => identifier.get_line(),
        }
    }
}

impl Parse for FuncCall {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let name = input.parse()?;

        if input.next_if_kind(&TokenKind::LeftParen).is_none() {
            return FuncCall::Identifier(name).into();
        };

        let Some(arg1) = input.parse() else {
            input.expect_kind(&TokenKind::RightParen, "Expected an closing `)`");
            return FuncCall::FuncCall { name, args: vec![] }.into();
        };

        let mut args = vec![arg1];
        args.append(&mut input.parse_zero_or_more(|nput| {
            nput.next_if_kind(&TokenKind::Comma);
            nput.parse::<Expr>()
        }));
        input.expect_kind(&TokenKind::RightParen, "Expected an closing `)`");

        FuncCall::FuncCall { name, args }.into()
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct Identifier {
    pub name: String,
    pub line: usize,
}
impl Parse for Identifier {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let Some(Token {
            kind: TokenKind::Identifier(name),
            line,
        }) = input.next()
        else {
            return None;
        };

        Identifier {
            name: name.to_string(),
            line: *line,
        }
        .into()
    }
}
impl GetPosition for Identifier {
    fn get_line(&self) -> usize {
        self.line
    }
}

#[derive(Debug)]
pub enum Target {
    Identifer(Identifier),
    Accesor {
        base: Base,
        accessors: OneOrMore<AccessorOp>,
    },
}

impl Parse for Target {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let base = input.parse()?;
        let opt_accessors = input.parse_one_or_more(Cursor::parse);

        if let Some(accessors) = opt_accessors {
            return Target::Accesor { base, accessors }.into();
        };

        let Base::FuncCall(Span {
            item: FuncCall::Identifier(iden),
            line,
        }) = base
        else {
            return None;
        };

        Target::Identifer(iden).into()
    }
}
