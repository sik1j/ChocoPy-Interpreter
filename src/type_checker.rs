use std::borrow::Borrow;
use std::hash::Hash;
use std::{collections::HashMap, fmt::Debug, rc::Rc};

use crate::parser::{
    self, Accessor, AccessorOp, AndOp, Base, ClassDef, Comparison, ComparisonOp, Definition, Expr,
    Factor, FactorOp, FuncBody, FuncCall, FuncDef, GetPosition, Identifier, Literal, NegInt, NotOp,
    OneOrMore, OrOp, Program, SimpleStatement, Span, Statement, Term, TermOp, TypedVar, VarDef,
};

#[derive(PartialEq, Clone, Debug)]
enum Type {
    Int,
    Bool,
    Str,
    None,
    Empty,
    List(Box<Type>),
    Class(Rc<ClassType>),
    Function(FunctionType),
}

#[derive(PartialEq, Clone, Debug)]
struct FunctionType {
    params: Vec<(String, Type)>,
    return_type: Box<Type>,
}
macro_rules! panic_line {
    ($line:expr, $msg:expr) => {
        panic!("[Line: {}] {}", $line, $msg)
    };
}

impl Type {
    fn is_subtype(&self, super_type: &Type) -> bool {
        let var_name = match (self, super_type) {
            (Type::Class(c1), Type::Class(c2)) => c1.is_subtype(c2),
            (Type::None, Type::Int | Type::Bool | Type::Str) => false,
            (Type::None, _) => true,
            (Type::Empty, Type::List(_)) => true,
            (Type::List(t1), Type::List(t2)) => **t1 == Type::None && Type::None.is_subtype(t2),
            (t1, t2) => t1 == t2,
        };
        var_name
    }

    fn expect_subtype_of(&self, super_type: &Type, line: usize) {
        if !self.is_subtype(super_type) {
            panic_line!(
                line,
                &format!("{:?} is not a subtype of {:?}", self, super_type)
            );
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

    fn expect_type(self, expected: &Type, line: usize) -> Self {
        self.panic_if_mismatch(expected, line);
        self
    }

    fn join(&self, t: &Type) -> &Type {
        match t {
            Type::Int => todo!(),
            Type::Bool => todo!(),
            Type::Str => todo!(),
            Type::None => todo!(),
            Type::Empty => todo!(),
            Type::List(_) => todo!(),
            Type::Class(class) => todo!(),
            Type::Function(..) => todo!(),
        }
    }

    fn panic_undefined_op(&self, name: &str, line: usize) {
        panic_line!(line, &format!("{:?} is undefined for {:?}", self, name));
    }

    // fn binary_operations<OpType, ArgType>(&self, more: &[(OpType, ArgType)]) -> Type {
    //     let (is_op_valid) = match self {
    //         Type::Int => |op| op == TermOp::Add,
    //         Type::Bool => todo!(),
    //         Type::Str => todo!(),
    //         Type::None => todo!(),
    //         Type::Empty => todo!(),
    //         Type::List(_) => todo!(),
    //         Type::Class(class_type) => todo!(),
    //         Type::Function => todo!(),
    //     };

    //     for (op, _) in more {
    //         if !is_op_valid(op) {
    //             panic!()
    //         }
    //     }

    //     self.clone()
    // }

    fn check_binary_expr<'a, T, U>(
        self,
        valid_ops: &[T],
        rest_of_args: &'a Vec<(T, U)>,
        line: usize,
    ) -> Type
    where
        T: Debug + std::cmp::PartialEq,
        U: 'a + GetPosition,
        Type: From<&'a U>,
    {
        for (op, expr) in rest_of_args {
            Type::from(expr).expect_type(&self, expr.get_line());

            if !valid_ops.contains(op) {
                panic_line!(line, format!("{:?} is not defined for {:?}", op, self))
            };
        }

        self
    }

    // fn check_accesor_expression(
    //     &self,
    //     type_env: &mut TypeEnv,
    //     one_or_more: &OneOrMore<AccessorOp>,
    // ) -> Type {
    //     match self {
    //         Type::Str => one_or_more.iter().fold(Type::Str, |_, op| match op {
    //             AccessorOp::Index(expr) => expr
    //                 .check_expression(type_env)
    //                 .expect_type(&Type::I32, expr.get_line()),

    //             AccessorOp::MemberFunc(func_call) => {
    //                 panic_line!(func_call.get_line(), "`str` doesn't implement `.`")
    //             }
    //         }),
    //         _ => todo!(),
    //     }
    // }
}

#[derive(PartialEq, Clone, Debug)]
enum ClassType {
    Class {
        name: String,
        super_class: Rc<ClassType>,
    },
    Object,
}

impl ClassType {
    fn is_subtype(&self, super_type: &ClassType) -> bool {
        let var_name = match (self, super_type) {
            (_, ClassType::Object) => true,
            (ClassType::Object, ClassType::Class { .. }) => false,
            (
                ClassType::Class {
                    name: _,
                    super_class: self_super,
                },
                b @ ClassType::Class { .. },
            ) => **self_super == *b || self_super.is_subtype(b),
        };
        var_name
    }
}

struct LocalEnv {
    scopes: Vec<HashMap<String, Rc<Type>>>,
}

impl LocalEnv {
    fn new() -> LocalEnv {
        LocalEnv {
            scopes: vec![HashMap::new()],
        }
    }

    fn insert(&mut self, name: String, typ: Rc<Type>) -> Option<Rc<Type>> {
        self.scopes
            .last_mut()
            .expect("scopes should never be empty")
            .insert(name, typ)
    }

    fn get<Q>(&self, name: &Q) -> Option<&Rc<Type>>
    where
        Q: Hash + Eq + ?Sized,
        String: Borrow<Q>,
    {
        self.scopes
            .last()
            .expect("scopes should never be empty")
            .get(name)
    }

    fn add_scope(&mut self, FunctionType { params, .. }: &FunctionType) {
        self.scopes.push(HashMap::new());
        for (name, typ) in params {
            self.insert(name.clone(), typ.clone().into());
        }
    }

    fn drop_scope(&mut self) {
        self.scopes.pop();
    }
}

struct TypeEnv {
    local: LocalEnv,
    method_attr: HashMap<String, Rc<Type>>,
    class: Option<Type>,
    return_type: Option<Type>,
}

impl TypeEnv {
    fn new(program: &Program) -> TypeEnv {
        for def in program.definitions {
            /// collect top level functions and classes to foward-reference them
            /// in top level local scope
            match def {
                Definition::FuncDef(FuncDef {
                    name,
                    params,
                    return_type,
                    ..
                }) => Type::Function(FunctionType {
                    params: params
                        .iter()
                        .map(|param| (param.name.name.clone(), self.))
                        .collect(),
                    return_type,
                }),
                Definition::ClassDef(class_def) => todo!(),
                Definition::VarDef(var_def) => continue,
            }
        }

        TypeEnv {
            local: LocalEnv::new(),
            method_attr: HashMap::new(),
            class: None,
            return_type: None,
        }
    }

    // fn var_read<'a>(&'a self, identifier: &Identifier) -> &'a Type {
    //     let Identifier { name, line } = identifier;

    //     let var_type = self.get_local(name).expect("msg");
    //     if var_type == &Type::Function {
    //         panic!();
    //     };
    //     var_type
    // }

    pub fn type_check_program<'a>(&mut self, program: &'a Program) {
        let Program {
            definitions,
            statements,
        } = program;

        for def in definitions {
            self.check_definition(def);
        }

        for stmt in statements {
            self.check_stmt(stmt);
        }
    }

    fn var_assign_stmt<'a>(&'a self) {
        todo!()
    }

    fn check_definition(&mut self, definition: &Definition) {
        match definition {
            Definition::VarDef(var_def) => self.check_var_def(var_def),
            Definition::FuncDef(func_def) => self.check_func_def(func_def),
            Definition::ClassDef(class_def) => self.check_class_def(class_def),
        }
    }

    fn check_var_def(&mut self, var_def: &VarDef) {
        let VarDef {
            typed_var:
                TypedVar {
                    r#type,
                    name: Identifier { name, .. },
                },
            value,
        } = var_def;

        let defined_type = Type::from(r#type);
        let value_type = Type::from(value);

        value_type.expect_subtype_of(&defined_type, var_def.get_line());

        self.local.insert(name.clone(), defined_type.into());
    }

    fn check_func_def(&mut self, func_def: &FuncDef) {
        let FuncDef {
            name,
            params,
            return_type,
            func_body,
        } = func_def;

        let return_type = return_type.as_ref().map_or(Type::None, Type::from).into();
        let params = params
            .iter()
            .map(|param| (param.name.name.clone(), Type::from(&param.r#type)))
            .collect();

        let func_type = FunctionType {
            params,
            return_type,
        };
        self.check_func_body(&func_type, func_body);

        self.local
            .insert(name.name.clone(), Type::Function(func_type).into());
    }

    fn check_func_body(&mut self, func_type: &FunctionType, func_body: &FuncBody) {
        self.local.add_scope(func_type);

        todo!();

        self.local.drop_scope();
    }

    fn check_class_def(&mut self, class_def: &ClassDef) {
        let ClassDef {
            name,
            super_class,
            body,
        } = class_def;

        if name.name == "object" {
            panic!("class cannot be named object");
        }

        let super_class = match &**self
            .local
            .get(&super_class.name)
            .expect("Expected a superclass")
        {
            Type::Class(class) => class.clone(),
            _ => panic!("{:?} is not a class type", &super_class.name),
        };

        todo!()
        // let class_type = ClassType::Class { name: (), super_class: () }
    }

    fn check_simple_stmt(&self, simple_statement: &SimpleStatement) {
        match simple_statement {
            SimpleStatement::Pass => (),
            SimpleStatement::Expr(expr) => {
                let _ = Type::from(expr);
            }
            SimpleStatement::Return(expr) => todo!(),
            SimpleStatement::Assignments { targets, expr } => todo!(),
        }
    }

    fn check_stmt(&self, statement: &Statement) {
        match statement {
            Statement::Simple(simple_statement) => self.check_simple_stmt(simple_statement),
            Statement::IfStmt(if_stmt) => todo!(),
            Statement::WhileLoop { condition, body } => todo!(),
            Statement::ForLoop {
                item,
                iterator,
                body,
            } => todo!(),
        }
    }

    fn convert_parser_type(&self, parser_type: parser::Type) -> Rc<Type> {
        match parser_type {
            parser::Type::Identifier(identifier) => match identifier.name.as_str() {
                "int" => Rc::new(Type::Int),
                "bool" => Rc::new(Type::Bool),
                "str" => Rc::new(Type::Str),
                class_name => self
                    .local
                    .get(class_name)
                    .expect(&format!("Undefined class: {:?}", class_name))
                    .clone(),
            },
            parser::Type::IdString(_) => todo!(),
            parser::Type::Array(_) => todo!(),
        }
    }
}

impl From<&parser::Type> for Type {
    fn from(value: &parser::Type) -> Self {
        todo!()
    }
}

impl From<&Expr> for Type {
    fn from(value: &Expr) -> Self {
        let Expr { or_op, if_expr } = value;
        let Some((if_cond, else_expr)) = if_expr else {
            return Type::from(or_op);
        };

        Type::from(if_cond).expect_type(&Type::Bool, value.get_line());

        let t1 = Type::from(or_op);
        let t2 = Type::from(&**else_expr);

        t1.join(&t2).clone()
    }
}

impl From<&OrOp> for Type {
    fn from(value: &OrOp) -> Self {
        let OrOp(one, more) = value;
        if more.is_empty() {
            return Type::from(&**one);
        };

        let line = value.get_line();
        let expected = Type::Bool;
        Type::from(&**one).expect_type(&expected, line);
        for right in more {
            Type::from(right).expect_type(&expected, line);
        }

        expected
    }
}

impl From<&AndOp> for Type {
    fn from(value: &AndOp) -> Self {
        let AndOp(one, more) = value;
        if more.is_empty() {
            return Type::from(one);
        };

        let line = value.get_line();
        let expected = Type::Bool;
        Type::from(one).expect_type(&expected, line);
        for right in more {
            Type::from(right).expect_type(&expected, line);
        }

        expected
    }
}

impl From<&NotOp> for Type {
    fn from(value: &NotOp) -> Self {
        let NotOp {
            op_count,
            comparison,
        } = value;
        if *op_count == 0 {
            return Type::from(comparison);
        };

        Type::from(comparison).expect_type(&Type::Bool, value.get_line())
    }
}

impl From<&Comparison> for Type {
    fn from(value: &Comparison) -> Self {
        let Comparison(one, more) = value;

        let line = value.get_line();
        match more.len() {
            0 => Type::from(one),
            1 => {
                let expected = Type::Bool;
                let _ = Type::from(one).expect_type(&expected, line);
                let _ = Type::from(&more[0].1).expect_type(&expected, line);

                expected
            }
            _ => panic_line!(line, "Cannot chain comparison operations"),
        }
    }
}

impl From<&Term> for Type {
    fn from(value: &Term) -> Self {
        let Term(one, more) = value;

        if more.is_empty() {
            return Type::from(one);
        };

        let line = value.get_line();
        match Type::from(&more[0].1) {
            Type::Int => Type::Int.check_binary_expr(&[TermOp::Add, TermOp::Subtract], more, line),
            Type::Bool => todo!(),
            Type::Str => Type::Str.check_binary_expr(&[TermOp::Add], more, line),
            Type::None => todo!(),
            Type::Empty => todo!(),
            Type::List(_) => todo!(),
            Type::Class(rc) => todo!(),
            Type::Function(function_type) => todo!(),
        }
    }
}

impl From<&Factor> for Type {
    fn from(value: &Factor) -> Self {
        let Factor(one, more) = value;

        if more.is_empty() {
            return Type::from(one);
        };

        let line = value.get_line();
        match Type::from(&more[0].1) {
            Type::Int => Type::Int.check_binary_expr(
                &[FactorOp::Multiply, FactorOp::IntDiv, FactorOp::Modulo],
                more,
                line,
            ),
            _ => todo!(),
        }
    }
}

impl From<&NegInt> for Type {
    fn from(value: &NegInt) -> Self {
        let NegInt { op_count, accessor } = value;
        if *op_count == 0 {
            return Type::from(accessor);
        };

        Type::from(accessor).expect_type(&Type::Int, value.get_line())
    }
}

impl From<&Accessor> for Type {
    fn from(value: &Accessor) -> Self {
        match value {
            Accessor::Base(base) => Type::from(base),
            Accessor::Accessors(base, one_or_more) => todo!(),
        }
    }
}

impl From<&Base> for Type {
    fn from(value: &Base) -> Self {
        match value {
            Base::Literal(span) => Type::from(&span.item),
            Base::List(span) => span
                .item
                .iter()
                .map(Type::from)
                .reduce(|acc, typ| acc.join(&typ).clone())
                .unwrap_or(Type::Empty),
            Base::Grouping(span) => Type::from(&span.item),
            Base::FuncCall(span) => todo!(),
        }
    }
}

impl From<&Literal> for Type {
    fn from(value: &Literal) -> Self {
        match value {
            Literal::None => Type::None,
            Literal::True | Literal::False => Type::Bool,
            Literal::Integer(_) => Type::Int,
            Literal::String(_) => Type::Str,
            Literal::IdString(_) => todo!(),
        }
    }
}
