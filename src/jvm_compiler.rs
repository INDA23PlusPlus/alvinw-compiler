use std::collections::HashMap;
use std::iter::Map;
use crate::parser::{AST, Block, Expression, Factor, Statement, Term};

pub fn compile_to_jvm(ast: &AST) -> Vec<u8> {
    let mut jvm_compiler = JvmCompiler::new();
    jvm_compiler.compile_to_jvm(ast);
    jvm_compiler.get_bytes()
}

struct JvmCompiler {
    constants_pool: ConstantsPool,
    local_variables: HashMap<String, u8>,
    next_local_variable_number: u8,
    method_bytecode: Vec<u8>,
}

impl JvmCompiler {
    pub fn new() -> Self {
        Self {
            constants_pool: ConstantsPool::new(),
            local_variables: HashMap::new(),
            next_local_variable_number: 0,
            method_bytecode: Vec::new(),
        }
    }

    pub fn compile_to_jvm(&mut self, ast: &AST) {
        self.compile_block(&ast.root_block);
    }

    pub fn get_bytes(&mut self) -> Vec<u8> {
        let mut bytes = vec![
            0xCA, // magic
            0xFE, // magic
            0xBA, // magic
            0xBE, // magic
            0, // minor version
            0, // minor version
            0,  // major version
            61, // major version
        ];
        let program_string = self.constants_pool.get_string("Program");
        let object_string = self.constants_pool.get_string("java/lang/Object");
        let main_string = self.constants_pool.get_string("main");
        let main_descriptor_string = self.constants_pool.get_string("([Ljava/lang/String;)V");

        let (mut constants_pool_bytes, constants_pool_size) = self.constants_pool.to_bytes();
        bytes.append(&mut constants_pool_bytes);

        let mut index = constants_pool_size;
        let this_class_index = index;
        index += 1;
        bytes.append(&mut vec![
            7, // class
            0, // index of name
            program_string, // index of name
        ]);
        index += 1;
        let super_class_index = index;;
        bytes.append(&mut vec![
            7, // class
            0, // index of name
            object_string, // index of name
        ]);
        index += 1;

        let flags = 0x0001; // public
        bytes.push((flags << 8 & 0xFF) as u8);
        bytes.push((flags & 0xFF) as u8);

        bytes.push(0);
        bytes.push(this_class_index);

        bytes.push(0);
        bytes.push(super_class_index);

        // No implemented interfaces
        bytes.push(0);
        bytes.push(0);

        // No fields
        bytes.push(0);
        bytes.push(0);

        // One method
        bytes.push(0);
        bytes.push(1);

        let method_flags = 0x0001 | 0x0008; // public static
        bytes.push((method_flags >> 8 & 0xFF) as u8);
        bytes.push((method_flags & 0xFF) as u8);

        // Method name
        bytes.push(0);
        bytes.push(main_string);

        // Method descriptor
        bytes.push(0);
        bytes.push(main_descriptor_string);

        bytes
    }

    fn compile_block(&mut self, block: &Block) {
        for statement in &block.statements {
            self.compile_statement(statement);
        }
    }

    fn compile_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::VariableDeclaration(decl) => {
                let variable_index = self.next_local_variable_number;
                self.next_local_variable_number += 1;
                self.local_variables.insert(decl.variable_name.clone(), variable_index);
                self.compile_expression(&decl.value_expr);
                self.method_bytecode.push(opcodes::ISTORE);
                self.method_bytecode.push(variable_index);
            }
            Statement::VariableChange(_) => {}
            Statement::If(_) => {}
            Statement::ExpressionStatement(_) => {}
            Statement::Loop(_) => {}
            Statement::Break => {}
        }
    }

    fn compile_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Term(term) => {
                self.compile_term(term);
            }
            Expression::Add(expr, term) => {
                self.compile_expression(expr);
                self.compile_term(term);
                self.method_bytecode.push(opcodes::IADD);
            }
            Expression::Subtract(expr, term) => {
                self.compile_expression(expr);
                self.compile_term(term);
                self.method_bytecode.push(opcodes::ISUB);
            }
            Expression::Equals(a, b) => {
                self.compile_expression(a);
                self.compile_expression(b);
                let addr = self.method_bytecode.len();
                let load_0_addr = addr + 7;
                let continue_addr = addr + 8;
                self.method_bytecode.push(opcodes::IF_ICMPNE);
                self.method_bytecode.push((load_0_addr & 0xFF00 >> 8) as u8);
                self.method_bytecode.push((load_0_addr & 0xFF) as u8);
                self.method_bytecode.push(opcodes::ICONST_1);
                self.method_bytecode.push(opcodes::GOTO);
                self.method_bytecode.push((continue_addr & 0xFF00 >> 8) as u8);
                self.method_bytecode.push((continue_addr & 0xFF) as u8);
                self.method_bytecode.push(opcodes::ICONST_0);
            }
            Expression::LessThan(a, b) => {
                todo!();
            }
            Expression::MoreThan(a, b) => {
                todo!();
            }
        }
    }

    fn compile_term(&mut self, term: &Term) {
        match term {
            Term::Factor(factor) => {
                self.compile_factor(factor);
            }
            Term::Multiply(term, factor) => {
                self.compile_term(term);
                self.compile_factor(factor);
                self.method_bytecode.push(opcodes::IMUL);
            }
            Term::Divide(term, factor) => {
                self.compile_term(term);
                self.compile_factor(factor);
                self.method_bytecode.push(opcodes::IDIV);
            }
        }
    }

    fn compile_factor(&mut self, factor: &Factor) {
        match factor {
            Factor::Integer(int) => {
                self.method_bytecode.push(opcodes::LDC);
                self.method_bytecode.push(self.constants_pool.get_integer(int.value));
            }
            Factor::Expression(expr) => {
                self.compile_expression(expr);
            }
            Factor::Variable(var) => {
                self.method_bytecode.push(opcodes::ILOAD);
                self.method_bytecode.push(*self.local_variables.get(var).expect("Local variables are valid."));
            }
            Factor::FunctionCall(call) => {
                assert_eq!(call.function_name, "print");
                todo!();
            }
        }
    }
}

struct ConstantsPool {
    current_index: u8,
    integers: HashMap<i32, u8>,
    strings: HashMap<String, u8>,
}

impl ConstantsPool {
    pub fn new() -> Self {
        Self {
            current_index: 0,
            integers: HashMap::new(),
            strings: HashMap::new(),
        }
    }

    pub fn get_integer(&mut self, val: i32) -> u8 {
        if let Some(index) = self.integers.get(&val) {
            return *index;
        }
        let index = self.current_index;
        self.integers.insert(val, index);
        self.current_index += 1;
        index
    }

    pub fn get_string(&mut self, val: &str) -> u8 {
        if let Some(index) = self.strings.get(val) {
            return *index;
        }
        let index = self.current_index;
        self.strings.insert(val.to_string(), index);
        self.current_index += 1;
        index
    }

    pub fn to_bytes(&self) -> (Vec<u8>, u8) {
        let mut bytes = Vec::new();
        let count = self.current_index;
        bytes.push(0);
        bytes.push(count);

        let mut data = Vec::with_capacity(count as usize);
        for _ in 0..count {
            data.push(0);
        }
        for (value, index) in &self.integers {
            data[*index as usize] = *value;
        }
        for value in data {
            bytes.push(3); // integer constant is type 3
            bytes.push(((value >> 24) & 0xFF) as u8);
            bytes.push(((value >> 16) & 0xFF) as u8);
            bytes.push(((value >> 8) & 0xFF) as u8);
            bytes.push((value & 0xFF) as u8);
        }

        let mut data = Vec::with_capacity(count as usize);
        for _ in 0..count {
            data.push("");
        }
        for (value, index) in &self.strings {
            data[*index as usize] = value;
        }
        for value in data {
            bytes.push(1); // UTF-8 constant is type 3
            let mut string_bytes: Vec<u8> = value.bytes().collect();
            let size = string_bytes.len() as u16;
            bytes.push((size > 8 & 0xFF) as u8);
            bytes.push((size & 0xFF) as u8);
            bytes.append(&mut string_bytes);
        }

        return (bytes, count);
    }
}

mod opcodes {
    pub const LDC: u8 = 0x12;
    pub const ILOAD: u8 = 0x15;
    pub const IMUL: u8 = 0x68;
    pub const IDIV: u8 = 0x6c;
    pub const IADD: u8 = 0x60;
    pub const ISUB: u8 = 0x64;
    pub const IF_ICMPNE: u8 = 0xa0;
    pub const ICONST_0: u8 = 0x3;
    pub const ICONST_1: u8 = 0x4;
    pub const GOTO: u8 = 0xa7;
    pub const ISTORE: u8 = 0x36;
}


#[cfg(test)]
mod tests {
    use crate::lexer;
    use crate::parser::Parser;
    use crate::semantic_analyser::validate;
    use super::*;

    fn compile(source: &str) -> Vec<u8> {
        let tokens = lexer::lex(source).expect("Failed to lex.");
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        validate(&ast).expect("Invalid code.");

        compile_to_jvm(&ast)
    }

    #[test]
    fn test1() {
        let jvm_bytes = compile("let a = 1; let b = 2; let c = a + b; print(c);");
        println!("{:02X?}", jvm_bytes);
        panic!();
    }
}