use crate::compiler::ByteCode;
use rand::Rng;
use meval::eval_str;

pub struct VirtualMachine {
    stack: Vec<f64>,
}

impl VirtualMachine {
    pub fn new() -> Self {
        VirtualMachine { stack: Vec::new() }
    }

    pub fn run(&mut self, bytecode: &[ByteCode]) -> Result<f64, String> {
        for instruction in bytecode {
            match instruction {
                ByteCode::NegNum => self.negate_number()?,
                ByteCode::NegBool => return Err("NegBool is not supported yet".into()),
                ByteCode::Add => self.add()?,
                ByteCode::Sub => self.sub()?,
                ByteCode::Mul => self.mul()?,
                ByteCode::Div => self.div()?,
                ByteCode::Const(value) => self.push(*value),
            }
        }

        // Ensure the stack ends with exactly one value, which is the result.
        if self.stack.len() == 1 {
            Ok(self.stack.pop().unwrap())
        } else {
            Err("Stack did not end with exactly one value".into())
        }
    }

    // Pushes a value onto the stack.
    fn push(&mut self, value: f64) {
        self.stack.push(value);
    }

    // Pops a value from the stack, returning an error if the stack is empty.
    fn pop(&mut self) -> Result<f64, String> {
        self.stack.pop().ok_or_else(|| "Stack underflow".into())
    }

    // Negates the top value on the stack.
    fn negate_number(&mut self) -> Result<(), String> {
        let value = self.pop()?;
        self.push(-value);
        Ok(())
    }

    // Adds the top two values on the stack.
    fn add(&mut self) -> Result<(), String> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(a + b);
        Ok(())
    }

    // Subtracts the top value from the second value on the stack.
    fn sub(&mut self) -> Result<(), String> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(a - b);
        Ok(())
    }

    // Multiplies the top two values on the stack.
    fn mul(&mut self) -> Result<(), String> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(a * b);
        Ok(())
    }

    // Divides the second value on the stack by the top value.
    fn div(&mut self) -> Result<(), String> {
        let b = self.pop()?;
        if b == 0.0 {
            return Err("Division by zero".into());
        }
        let a = self.pop()?;
        self.push(a / b);
        Ok(())
    }
}

// Function to generate a random arithmetic expression
pub fn generate_random_expression() -> String {
    let mut rng = rand::rng();

    // Randomly decide the number of terms in the expression.
    let num_terms = rng.random_range(2..=10);
    let mut expression = String::new();
    let mut current_value = rng.random_range(-100.0..100.0);

    expression.push_str(&format!("{:.2}", current_value));

    // Build the expression term by term.
    for _ in 1..num_terms {
        // Randomly pick an operator.
        let operator = match rng.random_range(0..4) {
            0 => '+',
            1 => '-',
            2 => '*',
            _ => '/',
        };

        let next_value = rng.random_range(-100.0..100.0);

        // Avoid division by zero in the generated expression.
        if operator == '/' && next_value == 0.0 {
            continue;
        }

        expression.push(' ');
        expression.push(operator);
        expression.push(' ');
        expression.push_str(&format!("{:.2}", next_value));

        // Evaluate the expression step by step in Rust.
        current_value = match operator {
            '+' => current_value + next_value,
            '-' => current_value - next_value,
            '*' => current_value * next_value,
            '/' => current_value / next_value,
            _ => unreachable!(),
        };
    }

    expression
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::{ByteCode, compile};
    use crate::parser::parse;
    use crate::tokenizer::tokenize;

    #[test]
    fn test_simple_addition() {
        let mut vm = VirtualMachine::new();
        let bytecode = vec![
            ByteCode::Const(3.0),
            ByteCode::Const(4.0),
            ByteCode::Add,
        ];
        let result = vm.run(&bytecode);
        assert_eq!(result, Ok(7.0));
    }

    #[test]
    fn test_simple_subtraction() {
        let mut vm = VirtualMachine::new();
        let bytecode = vec![
            ByteCode::Const(10.0),
            ByteCode::Const(4.0),
            ByteCode::Sub,
        ];
        let result = vm.run(&bytecode);
        assert_eq!(result, Ok(6.0));
    }

    #[test]
    fn test_division_by_zero() {
        let mut vm = VirtualMachine::new();
        let bytecode = vec![
            ByteCode::Const(10.0),
            ByteCode::Const(0.0),
            ByteCode::Div,
        ];
        let result = vm.run(&bytecode);
        assert_eq!(result, Err("Division by zero".into()));
    }

    #[test]
    fn test_negate_number() {
        let mut vm = VirtualMachine::new();
        let bytecode = vec![
            ByteCode::Const(5.0),
            ByteCode::NegNum,
        ];
        let result = vm.run(&bytecode);
        assert_eq!(result, Ok(-5.0));
    }

    #[test]
    fn test_complex_expression() {
        let mut vm = VirtualMachine::new();
        let bytecode = vec![
            ByteCode::Const(5.0),
            ByteCode::Const(3.0),
            ByteCode::Mul,
            ByteCode::Const(2.0),
            ByteCode::Add,
        ];
        let result = vm.run(&bytecode);
        assert_eq!(result, Ok(17.0));
    }

    #[test]
    fn test_random_expression_generator() {
        for _ in 1..=1000 {
            let expr = generate_random_expression();
            println!("Generated Expression: {}", expr);

            let mut vm = VirtualMachine::new();
            let Ok(vm_result) = vm.run(&compile(&parse(&tokenize(&expr)))) else {
                return assert!(false);
            };
            let Ok(rust_result) = eval_str(expr) else {
                return assert!(false);
            };

            // println!("{} {}", rust_result, vm_result);
            // assert!((rust_result - vm_result).abs() < 0.0000001)
            assert_eq!(rust_result, vm_result)
        }
    }
}
