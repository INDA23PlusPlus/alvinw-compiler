use crate::parser::{AST, Block, Expression, Factor, Statement, Term};

pub fn transpile_to_c(ast: &AST) -> String {
    let block_code = transpile_block(&ast.root_block);
    return format!("#include <stdio.h>\nint main() {{ {} }}", block_code);
}

fn transpile_block(block: &Block) -> String {
    let mut str = String::new();
    for statement in &block.statements {
        str.push_str(transpile_statement(&statement).as_str());
    }
    return str;
}

fn transpile_statement(statement: &Statement) -> String {
    match statement {
        Statement::VariableDeclaration(decl) => {
            let expr = transpile_expr(&decl.value_expr);
            format!("int {} = {};", decl.variable_name, expr)
        }
        Statement::VariableChange(assignment) => {
            let expr = transpile_expr(&assignment.value_expr);
            format!("{} = {};", assignment.variable_name, expr)
        }
        Statement::If(if_statement) => {
            let condition = transpile_expr(&if_statement.condition);
            let body = transpile_block(&if_statement.body);
            format!("if ({}) {{ {} }}", condition, body)
        }
        Statement::ExpressionStatement(expr) => {
            format!("{};", transpile_expr(expr.as_ref()))
        }
        Statement::Loop(block) => {
            let block = transpile_block(block.as_ref());
            format!("for (;;) {{ {} }}", block)
        }
        Statement::Break => {
            "break;".to_string()
        }
    }
}

fn transpile_expr(expr: &Expression) -> String {
    match expr {
        Expression::Term(term) => {
            transpile_term(term)
        }
        Expression::Add(expr, term) => {
            format!("{} + {}", transpile_expr(&**expr), transpile_term(term))
        }
        Expression::Subtract(expr, term) => {
            format!("{} - {}", transpile_expr(&**expr), transpile_term(term))
        }
        Expression::Equals(expr1, expr2) => {
            format!("{} == {}", transpile_expr(&**expr1), transpile_expr(&**expr2))
        }
        Expression::LessThan(expr1, expr2) => {
            format!("{} < {}", transpile_expr(&**expr1), transpile_expr(&**expr2))
        }
        Expression::MoreThan(expr1, expr2) => {
            format!("{} > {}", transpile_expr(&**expr1), transpile_expr(&**expr2))
        }
    }
}

fn transpile_term(term: &Term) -> String {
    match term {
        Term::Factor(factor) => {
            transpile_factor(factor)
        }
        Term::Multiply(term, factor) => {
            format!("{} * {}", transpile_term(term.as_ref()), transpile_factor(factor))
        }
        Term::Divide(term, factor) => {
            format!("{} / {}", transpile_term(term.as_ref()), transpile_factor(factor))
        }
    }
}

fn transpile_factor(factor: &Factor) -> String {
    match factor {
        Factor::Integer(integer) => {
            integer.value.to_string()
        }
        Factor::Expression(expr) => {
            transpile_expr(expr.as_ref())
        }
        Factor::Variable(variable_name) => {
            variable_name.to_string()
        }
        Factor::FunctionCall(call) => {
            // will only be "print" as per semantic analysis
            let expr = transpile_expr(&call.arguments[0]);
            format!("printf(\"%d\\n\", {})", expr)
        }
    }
}



#[cfg(test)]
mod tests {
    use crate::lexer;
    use crate::parser::Parser;
    use crate::semantic_analyser::validate;
    use super::*;

    fn transpile(source: &str) -> String {
        let tokens = lexer::lex(source).expect("Failed to lex.");
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        validate(&ast).expect("Invalid code.");

        transpile_to_c(&ast)
    }

    #[test]
    fn test1() {
        assert_eq!(transpile(
            "let a = 2; set a = a + 2; print(a);"
        ),
            "#include <stdio.h>\nint main() { int a = 2;a = a + 2;printf(\"%d\\n\", a); }"
        )
    }
}