use crate::parser::{Expr, FacTail, Factor, Literal, Term, TerTail, Unary};


#[derive(Debug)]
pub enum ByteCode {
    NegNum,
    NegBool,
    Add,
    Sub,
    Mul,
    Div,
    Const(f64),
}

pub trait Compile {
    fn compile(&self) -> Vec<ByteCode>;
}

pub fn compile(ast: &dyn Compile) -> Vec<ByteCode> {
    ast.compile()
}

impl Compile for Expr {
    fn compile(&self) -> Vec<ByteCode> {
        compile(&self.0)
    }
}

impl Compile for Term {
    fn compile(&self) -> Vec<ByteCode> {
        let Term(left, tail) = self;
        let mut bytecode = compile(left);
        bytecode.append(&mut compile(tail));

        bytecode
    }
}

impl Compile for TerTail {
    fn compile(&self) -> Vec<ByteCode> {
        let (op, right, tail) = match self {
            TerTail::Nil => return vec![],
            TerTail::Add(right, tail) => (ByteCode::Add, right, tail),
            TerTail::Sub(right, tail) => (ByteCode::Sub, right, tail)
        };

        let mut bytecode = compile(right);
        bytecode.push(op);
        bytecode.append(&mut compile(&**tail));
        bytecode
    }
}

impl Compile for Factor {
    fn compile(&self) -> Vec<ByteCode> {
        let Factor(left, tail) = self;
        let mut bytecode = compile(left);
        bytecode.append(&mut compile(tail));

        bytecode
    }
}

impl Compile for FacTail {
    fn compile(&self) -> Vec<ByteCode> {
        let (op, right, tail) = match self {
            FacTail::Nil => return vec![],
            FacTail::Mul(right, tail) => (ByteCode::Mul, right, tail),
            FacTail::Div(right, tail) => (ByteCode::Div, right, tail)
        };

        let mut bytecode = compile(right);
        bytecode.push(op);
        bytecode.append(&mut compile(&**tail));
        bytecode
    }
}

impl Compile for Unary {
    fn compile(&self) -> Vec<ByteCode> {
        let (op, unary) = match self {
            Unary::NegNum(unary) => (ByteCode::NegNum, unary),
            Unary::NegBool(unary) => (ByteCode::NegBool, unary),
            Unary::Lit(lit) => return compile(lit),
        };

        let mut bytecode = compile(&**unary);
        bytecode.push(op);
        bytecode
    }
}

impl Compile for Literal {
    fn compile(&self) -> Vec<ByteCode> {
        match self {
            Literal::String(_) => todo!(),
            Literal::Number(n) => vec![ByteCode::Const(n.clone())],
            Literal::Bool(_) => todo!(),
            Literal::Grouping(expr) => compile(&**expr)
        }
    }
}