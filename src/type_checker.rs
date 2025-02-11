use core::panic;
use std::{collections::HashMap, fmt::Debug, ops::BitXor, rc::Rc};

use crate::parser::{
    Accessor, AccessorOp, AndOp, Base, ClassDef, Comparison, ComparisonOp, Definition, Expr,
    Factor, FactorOp, FuncCall, GetPosition, Identifier, Literal, NegInt, NotOp, OneOrMore, OrOp,
    Program, SimpleStatement, Span, Term, TermOp, Type as ParserType, TypedVar, VarDef,
};

#[derive(PartialEq, Clone, Debug)]
enum Type {
    I32,
    Bool,
    Str,
    None,
    Empty,
    List(Box<Type>),
    Class(Class),
}
macro_rules! panic_line {
    ($line:expr, $msg:expr) => {
        panic!("[Line: {}] {}", $line, $msg)
    };
}

impl Type {
    fn is_subtype(&self, super_type: &Type) -> bool {
        match (self, super_type) {
            (Type::Class(c1), Type::Class(c2)) => c1.is_subtype(c2),
            (Type::None, Type::I32 | Type::Bool | Type::Str) => false,
            (Type::None, _) => true,
            (Type::Empty, Type::List(_)) => true,
            (Type::List(t1), Type::List(t2)) => **t1 == Type::None && Type::None.is_subtype(t2),
            (t1, t2) => t1 == t2,
        }
    }

    fn subtype_or_panic(&self, super_type: &Type, line: usize, msg: &str) {
        if !self.is_subtype(super_type) {
            panic_line!(line, msg);
        }
    }

    fn panic_if_mismatch(&self, expected: &Type, line: usize) {
        if self != expected {
            panic_line!(
                line,
                &format!("Expected type: {:?} but actual type: {:?}", expected, self)
            );
        }
    }

    fn join(&self, t: &Type) -> &Type {
        match t {
            Type::I32 => todo!(),
            Type::Bool => todo!(),
            Type::Str => todo!(),
            Type::None => todo!(),
            Type::Empty => todo!(),
            Type::List(_) => todo!(),
            Type::Class(class) => todo!(),
        }
    }

    fn panic_undefined_op(&self, name: &str, line: usize) {
        panic_line!(line, &format!("{:?} is undefined for {:?}", self, name));
    }

    fn check_binary_expr<T: Debug + std::cmp::PartialEq, U: TypecheckExpression>(
        type_env: &mut TypeEnv,
        line: usize,
        expected_type: Type,
        valid_ops: &[T],
        rest_of_args: &Vec<(T, U)>,
    ) -> Type {
        for (op, to_check) in rest_of_args {
            to_check
                .check_expression(type_env)
                .panic_if_mismatch(&expected_type, line);

            if !valid_ops.contains(op) {
                panic_line!(
                    line,
                    format!("{:?} is not defined for {:?}", op, expected_type)
                )
            };
        }

        expected_type
    }
}

#[derive(PartialEq, Clone, Debug)]
enum Class {
    Class {
        name: String,
        super_class: Rc<Class>,
    },
    Object,
}

impl Class {
    fn is_subtype(&self, super_type: &Class) -> bool {
        let var_name = match (self, super_type) {
            (_, Class::Object) => true,
            (Class::Object, Class::Class { .. }) => false,
            (
                Class::Class {
                    name: _,
                    super_class: self_super,
                },
                b @ Class::Class { .. },
            ) => **self_super == *b || self_super.is_subtype(b),
        };
        var_name
    }
}

struct TypeEnv {
    local: HashMap<Identifier, Type>,
    method_attr: HashMap<String, Type>,
    class: Option<String>,
    return_type: Option<String>,
}

impl TypeEnv {
    fn new() -> TypeEnv {
        TypeEnv {
            local: HashMap::new(),
            method_attr: HashMap::new(),
            class: None,
            return_type: None,
        }
    }

    fn get_local(&self, iden: &Identifier) -> Option<&Type> {
        self.local.get(iden)
    }
}

trait TypecheckExpression {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type;
}

impl TypecheckExpression for Literal {
    fn check_expression(&self, _type_env: &mut TypeEnv) -> Type {
        match self {
            Literal::None => Type::None,
            Literal::True | Literal::False => Type::Bool,
            Literal::Integer(_) => Type::I32,
            Literal::String(_) => Type::Str,
            Literal::IdString(_) => todo!(),
        }
    }
}

impl TypecheckExpression for Accessor {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        match self {
            Accessor::Base(base) => base.check_expression(type_env),
            Accessor::Accessors(base, one_or_more)
                if base.check_expression(type_env) == Type::Str =>
            {
                let expected = Type::Str;
                for op in one_or_more.iter() {
                    match op {
                        AccessorOp::Index(expr) => expr
                            .check_expression(type_env)
                            .panic_if_mismatch(&expected, self.get_line()),
                        AccessorOp::MemberFunc(func_call) => panic_line!(
                            self.get_line(),
                            "Type `Str` does not support `.` operations"
                        ),
                    }
                }

                expected
            }
            _ => todo!(),
        }
    }
}

impl TypecheckExpression for NegInt {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        if self.op_count == 0 {
            return self.accessor.check_expression(type_env);
        }

        let expected = Type::I32;
        self.accessor
            .check_expression(type_env)
            .panic_if_mismatch(&expected, self.get_line());

        expected
    }
}

impl TypecheckExpression for Factor {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        let Factor(first, rest) = self;
        if rest.is_empty() {
            return first.check_expression(type_env);
        }

        let expected = Type::I32;
        Type::check_binary_expr(
            type_env,
            self.get_line(),
            expected,
            &[FactorOp::Multiply, FactorOp::IntDiv, FactorOp::Modulo],
            rest,
        )
    }
}

impl TypecheckExpression for Term {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        let Self(first, rest) = self;
        if rest.is_empty() {
            return first.check_expression(type_env);
        }

        match first.check_expression(type_env) {
            Type::I32 => Type::check_binary_expr(
                type_env,
                self.get_line(),
                Type::I32,
                &[TermOp::Add, TermOp::Subtract],
                rest,
            ),
            Type::Bool => todo!(),
            Type::Str => {
                Type::check_binary_expr(type_env, self.get_line(), Type::Str, &[TermOp::Add], rest)
            }

            Type::None => todo!(),
            Type::Empty => todo!(),
            Type::List(_) => todo!(),
            Type::Class(class) => todo!(),
        }
    }
}

impl TypecheckExpression for Comparison {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        let Self(first, rest) = self;
        if rest.is_empty() {
            return first.check_expression(type_env);
        }

        let line = self.get_line();
        match first.check_expression(type_env) {
            Type::I32 => Type::check_binary_expr(
                type_env,
                line,
                Type::I32,
                &[
                    ComparisonOp::Equal,
                    ComparisonOp::NotEqual,
                    ComparisonOp::Less,
                    ComparisonOp::LessEqual,
                    ComparisonOp::Greater,
                    ComparisonOp::GreaterEqual,
                ],
                rest,
            ),
            ty @ (Type::Bool | Type::Str) => Type::check_binary_expr(
                type_env,
                line,
                ty,
                &[ComparisonOp::Equal, ComparisonOp::NotEqual],
                rest,
            ),

            t => panic_line!(
                self.get_line(),
                format!("{:?} is undefined for {:?}", rest[0].0, t)
            ),
        }
    }
}

impl TypecheckExpression for NotOp {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        if self.op_count == 0 {
            return self.comparison.check_expression(type_env);
        }

        let expected = Type::Bool;
        self.comparison
            .check_expression(type_env)
            .panic_if_mismatch(&expected, self.get_line());

        expected
    }
}

impl TypecheckExpression for AndOp {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        let Self(first, rest) = self;
        if rest.is_empty() {
            return first.check_expression(type_env);
        }

        let expected = Type::Bool;
        first
            .check_expression(type_env)
            .panic_if_mismatch(&expected, self.get_line());

        for expr in rest {
            expr.check_expression(type_env)
                .panic_if_mismatch(&expected, self.get_line());
        }

        expected
    }
}

impl TypecheckExpression for OrOp {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        let Self(first, rest) = self;
        if rest.is_empty() {
            return first.check_expression(type_env);
        }

        let expected = Type::Bool;
        first
            .check_expression(type_env)
            .panic_if_mismatch(&expected, self.get_line());

        for expr in rest {
            expr.check_expression(type_env)
                .panic_if_mismatch(&expected, self.get_line());
        }

        expected
    }
}

impl TypecheckExpression for Expr {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        let type1 = self.or_op.check_expression(type_env);

        let Some((condition, else_expr)) = &self.if_expr else {
            return type1;
        };

        condition
            .check_expression(type_env)
            .panic_if_mismatch(&Type::Bool, self.get_line());

        let type2 = else_expr.check_expression(type_env);
        // join(e1, e2)
        todo!()
    }
}

impl TypecheckExpression for Base {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        match self {
            Base::Literal(Span { item: literal, .. }) => todo!(),
            Base::List(span) => span.check_expression(type_env),
            Base::Grouping(expr) => todo!(),
            Base::FuncCall(func_call) => todo!(),
        }
    }
}

impl TypecheckExpression for Span<Vec<Expr>> {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        if self.item.is_empty() {
            return Type::Empty;
        };

        self.item
            .iter()
            .map(|e| e.check_expression(type_env))
            .reduce(|acc, e| e.join(&acc).clone())
            .unwrap()
            .clone()
    }
}

impl TypecheckExpression for FuncCall {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        match self {
            FuncCall::FuncCall { name, args } => todo!(),
            FuncCall::Identifier(identifier) => type_env
                .local
                .get(identifier)
                .expect(&format!(
                    "[Line: {}] Missing Variable Definition",
                    identifier.get_line()
                ))
                .clone(),
        }
    }
}

impl TypecheckExpression for ParserType {
    fn check_expression(&self, type_env: &mut TypeEnv) -> Type {
        match self {
            ParserType::Identifier(identifier) => todo!(),
            ParserType::IdString(_) => todo!(),
            ParserType::Array(_) => todo!(),
        }
    }
}

trait TypecheckStatement {
    fn check_statement(&self, type_env: &mut TypeEnv);
}

impl TypecheckStatement for VarDef {
    fn check_statement(&self, type_env: &mut TypeEnv) {
        let VarDef {
            typed_var: TypedVar { name, r#type },
            value,
        } = self;

        let given_type = r#type.check_expression(type_env);
        let literal_type = value.check_expression(type_env);

        literal_type.subtype_or_panic(
            &given_type,
            self.get_line(),
            "Cannot assign {literal_type:?} to {given_type:?}",
        );

        type_env.local.insert(name.clone(), given_type);
    }
}

impl TypecheckStatement for SimpleStatement {
    fn check_statement(&self, type_env: &mut TypeEnv) {
        match self {
            SimpleStatement::Pass => (),
            SimpleStatement::Expr(expr) => {
                expr.check_expression(type_env);
            }
            SimpleStatement::Return(expr) => todo!(),
            SimpleStatement::Assignments { targets, expr } => {
                let OneOrMore { one, more } = targets;
            }
        };
    }
}
