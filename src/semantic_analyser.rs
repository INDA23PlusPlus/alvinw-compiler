use std::collections::HashMap;
use crate::parser::{AST, Block, Expression, Factor, Statement, Term};

#[derive(Eq, PartialEq, Debug)]
pub enum SemanticAnalysisError {
    VariableAlreadyDeclared(String),
    UndeclaredVariable(String),
    UndeclaredFunction(String),
    ExpectedBooleanExpression,
    BreakOutsideLoop,
}

type Error = SemanticAnalysisError;
type Res = Result<(), SemanticAnalysisError>;
type Variables = HashMap<String, ()>;

pub fn validate(ast: &AST) -> Res {
    let variables = HashMap::new();
    validate_block(&variables, &ast.root_block, false)
}

fn validate_block(outer_variables: &Variables, block: &Block, is_loop: bool) -> Res {
    let mut variables: Variables = HashMap::new();
    println!("Outer variables: {:?}", outer_variables);
    for (variable_name, _) in outer_variables {
        variables.insert(variable_name.clone(), ());
    }
    for statement in &block.statements {
        match statement {
            Statement::VariableDeclaration(decl) => {
                if variables.contains_key(&decl.variable_name) {
                    return Err(Error::VariableAlreadyDeclared(decl.variable_name.to_string()));
                }
                validate_expr(&variables, &decl.value_expr)?;
                variables.insert(decl.variable_name.to_string(), ());
            }
            Statement::VariableChange(assignment) => {
                if !variables.contains_key(&assignment.variable_name) {
                    return Err(Error::UndeclaredVariable(assignment.variable_name.to_string()));
                }
                validate_expr(&variables, &assignment.value_expr)?;
            }
            Statement::If(if_statement) => {
                validate_expr(&variables, &if_statement.condition)?;
                if !is_boolean_expression(&if_statement.condition) {
                    return Err(Error::ExpectedBooleanExpression);
                }
                validate_block(&variables, &if_statement.body, is_loop)?;
            }
            Statement::ExpressionStatement(expr) => {
                validate_expr(&variables, expr.as_ref())?;
            }
            Statement::Loop(block) => {
                validate_block(&variables, block, true)?;
            },
            Statement::Break => {
                if !is_loop {
                    return Err(Error::BreakOutsideLoop);
                }
            }
        }
    }
    
    Ok(())
}

fn validate_expr(variables: &Variables, expr: &Expression) -> Res {
    match expr {
        Expression::Term(term) => {
            validate_term(variables, term)?;
        }
        Expression::Add(expr, term) => {
            validate_expr(variables, expr.as_ref())?;
            validate_term(variables, term)?;
        }
        Expression::Subtract(expr, term) => {
            validate_expr(variables, expr.as_ref())?;
            validate_term(variables, term)?;
        }
        Expression::Equals(expr1, expr2) => {
            validate_expr(variables, expr1.as_ref())?;
            validate_expr(variables, expr2.as_ref())?;
        }
        Expression::LessThan(expr1, expr2) => {
            validate_expr(variables, expr1.as_ref())?;
            validate_expr(variables, expr2.as_ref())?;
        }
        Expression::MoreThan(expr1, expr2) => {
            validate_expr(variables, expr1.as_ref())?;
            validate_expr(variables, expr2.as_ref())?;
        }
    }

    Ok(())
}

fn is_boolean_expression(expr: &Expression) -> bool {
    match expr {
        Expression::Equals(_, _) => true,
        Expression::LessThan(_, _) => true,
        Expression::MoreThan(_, _) => true,
        _ => false,
    }
}

fn validate_term(variables: &Variables, term: &Term) -> Res {
    match term {
        Term::Factor(factor) => {
            validate_factor(variables, factor)?;
        }
        Term::Multiply(term, factor) => {
            validate_term(variables, term.as_ref())?;
            validate_factor(variables, factor)?;
        }
        Term::Divide(term, factor) => {
            validate_term(variables, term.as_ref())?;
            validate_factor(variables, factor)?;
        }
    }

    Ok(())
}

fn validate_factor(variables: &Variables, factor: &Factor) -> Res {
    match factor {
        Factor::Integer(_) => {
            // integers are ok
        }
        Factor::Expression(expr) => {
            validate_expr(variables, expr)?;
        }
        Factor::Variable(variable_name) => {
            if !variables.contains_key(variable_name) {
                return Err(Error::UndeclaredVariable(variable_name.clone()));
            }
        }
        Factor::FunctionCall(function_call) => {
            match &function_call.function_name[..] {
                "print" => {
                    // ok
                }
                unknown => {
                    return Err(Error::UndeclaredFunction(unknown.to_string()))
                }
            }
            for argument_expr in &function_call.arguments {
                validate_expr(variables, &argument_expr)?;
            }
        }
    }

    Ok(())
}


#[cfg(test)]
mod tests {
    use crate::lexer;
    use crate::parser::Parser;
    use super::*;

    fn analyze(source: &str) -> Res {
        let tokens = lexer::lex(source).expect("Failed to lex.");
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        println!("ast = {:#?}", ast);

        validate(&ast)
    }

    fn analyze_err(source: &str) -> Error {
        let res = analyze(source);
        res.expect_err("Expected to fail.")
    }

    #[test]
    fn test1() {
        analyze("let a = 2; set a = 2;").unwrap();
    }

    #[test]
    fn test2() {
        let err = analyze_err("let a = 2; let a = 2;");
        assert_eq!(err, Error::VariableAlreadyDeclared("a".to_string()));
    }

    #[test]
    fn test3() {
        let err = analyze_err("let a = 2; set b = 2;");
        assert_eq!(err, Error::UndeclaredVariable("b".to_string()));
    }

    #[test]
    fn test4() {
        analyze("let a = 2; print(a + 2);").unwrap();
    }

    #[test]
    fn test5() {
        let err = analyze_err("unknown(2);");
        assert_eq!(err, Error::UndeclaredFunction("unknown".to_string()));
    }

    #[test]
    fn test6() {
        let err = analyze_err("if (1 + 1) {}");
        assert_eq!(err, Error::ExpectedBooleanExpression);
    }
}