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
                    | expr
                    | 'return' [ expr ]?
                    | [ target '=' ]+ expr

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

use crate::tokenizer::{Cursor, IteratorOneOrMoreExt, OneOrMore, Token, TokenKind};

pub fn parse(input: &mut Cursor<Token>) -> Program {
    input.parse().expect("Parser Error")
}

pub trait Parse: Sized {
    fn parse(input: &mut Cursor<Token>) -> Option<Self>;
}

#[derive(Debug)]
pub struct Program {
    definitions: Vec<Definition>,
    statements: Vec<Statement>,
}

impl Parse for Program {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let definitions = input.parse_zero_or_more(Cursor::parse);
        let statements = input.parse_zero_or_more(Cursor::parse);

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
    name: Identifier,
    super_class: Identifier,
    body: ClassBody,
}

impl Parse for ClassDef {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        input.next_if_kind(&TokenKind::Class)?;
        let name = input.parse().expect("Expected class name");

        input.expect_kind(&TokenKind::LeftParen, "Expected opening `(`");
        let super_class = input.parse().expect("Expected super class name");

        input.expect_kind(&TokenKind::RightParen, "Expected closing `)`");
        input.expect_kind(&TokenKind::Colon, "Expected `:`");
        input.expect_kind(&TokenKind::Newline, "Expected Newline");

        let body = input.parse().expect("Expected class body");

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
            input.expect_kind(&TokenKind::Newline, "Expected Newline");
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
    name: Identifier,
    params: Vec<TypedVar>,
    return_type: Option<Type>,
    func_body: FuncBody,
}

impl Parse for FuncDef {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        input.next_if_kind(&TokenKind::Def)?;
        let name = input.parse().expect("Expected a name");
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

        let return_type = input
            .next_if_kind(&TokenKind::Arrow)
            .is_some()
            .then(|| input.parse::<Type>().expect("Expected a return type"));

        input.expect_kind(&TokenKind::Colon, "Expected a `:`");
        input.expect_kind(&TokenKind::Newline, "Expected a Newline");
        input.expect_kind(&TokenKind::Indent, "Expected a Indent");

        let func_body = input.parse().expect("Expected a body");
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
    declarations: Vec<Declaration>,
    statements: OneOrMore<Statement>,
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
    name: Identifier,
    var_type: Type,
}

impl Parse for TypedVar {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let name = input.parse()?;
        input.expect_kind(&TokenKind::Colon, "Expected a type");

        TypedVar {
            name,
            var_type: input.parse()?,
        }
        .into()
    }
}

#[derive(Debug)]
pub enum Type {
    Identifer(Identifier),
    Array(Box<Type>),
}

impl Parse for Type {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if let Some(iden) = input.parse() {
            return Type::Identifer(iden).into();
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
    typed_var: TypedVar,
    value: Literal,
}
impl Parse for VarDef {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let typed_var = input.parse()?;
        input.next_if_kind(&TokenKind::Equal)?;
        let value = input.parse()?;
        input.next_if_kind(&TokenKind::Newline)?;

        VarDef { typed_var, value }.into()
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
            return Statement::Simple(simple).into();
        };

        if input.next_if_kind(&TokenKind::While).is_some() {
            let condition = input.parse()?;
            input.expect_kind(&TokenKind::Colon, &colon_msg);

            return Statement::WhileLoop {
                condition,
                body: input.parse().expect(&body_msg),
            }
            .into();
        };

        if input.next_if_kind(&TokenKind::For).is_some() {
            let item = input.parse()?;
            input.expect_kind(&TokenKind::In, "Expected a 'in'");

            let iterator = input.parse().expect(&body_msg);
            input.expect_kind(&TokenKind::Colon, &colon_msg);

            return Statement::ForLoop {
                item,
                iterator,
                body: input.parse().expect(&body_msg),
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
        let colon_msg = "Expected a ':'";
        let body_msg = "Expected a body";

        input.next_if_kind(&TokenKind::If)?;
        let if_cond = input.parse().expect(cond_msg);
        input.expect_kind(&TokenKind::Colon, colon_msg);
        let if_body = input.parse().expect(body_msg);

        let elifs = input.parse_zero_or_more(|nput| {
            nput.next_if_kind(&TokenKind::Elif)?;

            let cond = nput.parse().expect(&cond_msg);
            nput.expect_kind(&TokenKind::Colon, &colon_msg);

            let body = nput.parse().expect(&body_msg);
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
            r#else: input.parse::<Block>().expect(&body_msg).into(),
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

        if let Some(expr) = input.parse() {
            return SimpleStatement::Expr(expr).into();
        };

        if input.next_if_kind(&TokenKind::Return).is_some() {
            return SimpleStatement::Return(input.parse()).into();
        };

        let targets = input.parse_one_or_more(|inpt| {
            let target = inpt.parse::<Target>()?;
            inpt.expect_kind(&TokenKind::Equal, "Expected an '='");
            target.into()
        })?;

        SimpleStatement::Assignments {
            targets,
            expr: input.parse().expect("Expected an expression"),
        }
        .into()
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
    or_op: OrOp,
    if_expr: Option<(OrOp, Box<Expr>)>,
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

        let or2 = input.parse().expect("Expected an expression");
        input.expect_kind(&TokenKind::Else, "Expected `else`");
        let else_expr = input.parse().expect("Expected an expression");

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
        while input.next_if_kind(&TokenKind::Or).is_some() {
            rest.push(input.parse().expect("Expected expression after `or`"));
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
        while input.next_if_kind(&TokenKind::And).is_some() {
            rest.push(input.parse().expect("Expected expression after `and`"));
        }

        Some(AndOp(left, rest))
    }
}

#[derive(Debug)]
pub struct NotOp {
    op_count: usize,
    comparison: Comparison,
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
        let right = nput.parse().expect("Expected expression");
        (op_type, right).into()
    });

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
            HashMap::from([
                (TokenKind::EqualEqual, ComparisonOp::Equality),
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
            HashMap::from([
                (TokenKind::Plus, TermOp::Add),
                (TokenKind::Minus, TermOp::Subtract),
            ]),
        )
    }
}

#[derive(Debug)]
pub struct Factor(NegInt, Vec<(FactorOp, NegInt)>);
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
    op_count: usize,
    accessor: Accessor,
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
pub struct Accessor(Base, Vec<AccessorOp>);
impl Parse for Accessor {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let base = input.parse()?;
        let accessor_ops = input.parse_zero_or_more(Cursor::parse);

        Accessor(base, accessor_ops).into()
    }
}

#[derive(Debug)]
pub enum AccessorOp {
    Index(Expr),
    MemberFunc(FuncCall),
}
impl Parse for AccessorOp {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if input.next_if_kind(&TokenKind::LeftBracket).is_some() {
            let expr = input.parse().expect("Expected an expression");
            input.expect_kind(&TokenKind::RightBracket, "Expected a `]`");
            return AccessorOp::Index(expr).into();
        };

        if input.next_if_kind(&TokenKind::Period).is_some() {
            let func_call = input.parse().expect("Expected an member/method");
            return AccessorOp::MemberFunc(func_call).into();
        };

        None
    }
}

#[derive(Debug)]
pub enum Base {
    Literal(Literal),
    Array(Vec<Expr>),
    Grouping(Expr),
    FuncCall(FuncCall),
}

impl Parse for Base {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        if let Some(func_call) = input.parse() {
            return Base::FuncCall(func_call).into();
        };

        if let Some(lit) = input.parse() {
            return Base::Literal(lit).into();
        };

        if input.next_if_kind(&TokenKind::LeftBracket).is_some() {
            let Some(elem1) = input.parse() else {
                input.expect_kind(&TokenKind::RightBracket, "Expected a closing `]`");
                return Base::Array(vec![]).into();
            };

            let mut elems = vec![elem1];
            elems.append(&mut input.parse_zero_or_more(|nput| {
                nput.next_if_kind(&TokenKind::Comma)?;
                nput.parse()
            }));

            return Base::Array(elems).into();
        };

        if input.next_if_kind(&TokenKind::LeftParen).is_some() {
            let grouping = Base::Grouping(input.parse()?);
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

        FuncCall::FuncCall { name, args }.into()
    }
}

#[derive(Debug)]
pub struct Identifier(String);
impl Parse for Identifier {
    fn parse(input: &mut Cursor<Token>) -> Option<Self> {
        let Some(Token {
            kind: TokenKind::Identifier(name),
        }) = input.next()
        else {
            return None;
        };

        Identifier(name.to_string()).into()
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

        let Base::FuncCall(FuncCall::Identifier(iden)) = base else {
            return None;
        };

        Target::Identifer(iden).into()
    }
}
