use crate::compiler::ByteCode;

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

        if self.stack.len() == 1 {
            Ok(self.stack.pop().unwrap())
        } else {
            Err("Stack did not end with exactly one value".into())
        }
    }

    fn push(&mut self, value: f64) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Result<f64, String> {
        self.stack.pop().ok_or_else(|| "Stack underflow".into())
    }

    fn negate_number(&mut self) -> Result<(), String> {
        let value = self.pop()?;
        self.push(-value);
        Ok(())
    }

    fn add(&mut self) -> Result<(), String> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(a + b);
        Ok(())
    }

    fn sub(&mut self) -> Result<(), String> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(a - b);
        Ok(())
    }

    fn mul(&mut self) -> Result<(), String> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(a * b);
        Ok(())
    }

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::ByteCode;

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
}
