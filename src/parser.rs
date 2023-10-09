use crate::lexer::{Token, TokenType};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: TokenType,
        found: Token,
    },
    ExpectedButNothingFound(TokenType),
    UnexpectedEnd,
    InvalidStatement,
    InvalidToken(Token),
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn peek(&mut self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn read(&mut self) -> Option<&Token> {
        let ret = self.tokens.get(self.index);
        println!("Reading {:?}!", ret);
        // println!("{:#?}", backtrace::Backtrace::capture());
        self.index += 1;
        ret
    }

    fn skip(&mut self) {
        self.index += 1
    }

    fn expect_token(&mut self, token_type: TokenType) -> Result<&Token, ParseError> {
        let token = self.read();
        if let Some(token) = token {
            if token.token_type() == token_type {
                Ok(token)
            } else {
                Err(ParseError::UnexpectedToken { expected: token_type, found: (*token).clone() })
            }
        } else {
            Err(ParseError::ExpectedButNothingFound(token_type))
        }
    }

    fn expect_exact_token(&mut self, token_type: TokenType, data: &str) -> Result<&Token, ParseError> {
        let token = self.read();
        if let Some(token) = token {
            if token.is(token_type, data) {
                Ok(token)
            } else {
                Err(ParseError::UnexpectedToken { expected: token_type, found: (*token).clone() })
            }
        } else {
            Err(ParseError::ExpectedButNothingFound(token_type))
        }
    }

    pub fn parse(&mut self) -> Result<AST, ParseError> {
        Ok(AST { root_block: self.block()? })
    }

    fn block(&mut self) -> Result<Block, ParseError> {
        let mut statements = Vec::new();
        loop {
            let index = self.index;
            let statement = self.statement();
            if let Ok(statement) = statement {
                statements.push(statement);
            } else {
                // In case of error, we need to reset the cursor to before attempting to read
                // the statement. An error here does not mean anything is wrong with the source
                // code, it just means what we read was not a statement.
                self.index = index;
                println!("End of block {:#?}", statement);
                break;
            }
        }
        Ok(Block { statements })
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        let token = self.read().ok_or(ParseError::UnexpectedEnd)?;
        if token.is(TokenType::Keyword, "let") {
            let name = self.expect_token(TokenType::Identifier)?.data().unwrap().clone();
            self.expect_exact_token(TokenType::Operator, "=")?;
            let value = self.expr()?;
            self.expect_exact_token(TokenType::Separator, ";")?;
            return Ok(Statement::VariableDeclaration(VariableAssignment {
                variable_name: name,
                value_expr: value,
            }));
        } else if token.is(TokenType::Keyword, "change") {
            let name = self.expect_token(TokenType::Identifier)?.data().unwrap().clone();
            self.expect_exact_token(TokenType::Operator, "=")?;
            let value = self.expr()?;
            self.expect_exact_token(TokenType::Separator, ";")?;
            return Ok(Statement::VariableChange(VariableAssignment {
                variable_name: name,
                value_expr: value,
            }));
        } else if token.is(TokenType::Keyword, "if") {
            self.expect_exact_token(TokenType::Separator, "(")?;
            let condition = self.expr()?;
            self.expect_exact_token(TokenType::Separator, ")")?;
            self.expect_exact_token(TokenType::Separator, "{")?;
            let body = self.block()?;
            self.expect_exact_token(TokenType::Separator, "}")?;
            return Ok(Statement::If(IfStatement { condition, body }));
        }

        Err(ParseError::InvalidStatement)
    }

    fn expr(&mut self) -> Result<Expression, ParseError> {
        let mut expr = Expression::Term(self.term()?);

        while let Some(next) = self.peek() {
            if next.is(TokenType::Operator, "+") {
                self.skip();
                expr = Expression::Add(Box::new(expr), self.term()?);
            } else if next.is(TokenType::Operator, "-") {
                self.skip();
                expr = Expression::Subtract(Box::new(expr), self.term()?);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Term, ParseError> {
        let mut term = Term::Factor(self.factor()?);

        while let Some(next) = self.peek() {
            if next.is(TokenType::Operator, "*") {
                self.skip();
                term = Term::Multiply(Box::new(term), self.factor()?);
            } else if next.is(TokenType::Operator, "/") {
                self.skip();
                term = Term::Divide(Box::new(term), self.factor()?);
            } else {
                break;
            }
        }

        Ok(term)
    }

    fn factor(&mut self) -> Result<Factor, ParseError> {
        let token = self.read().ok_or(ParseError::UnexpectedEnd)?;
        if token.token_type() == TokenType::Integer {
            let value = token.data()
                .expect("Integer tokens cannot be empty")
                .parse()
                .expect("Integer token should be a valid integer.");
            return Ok(Factor::Integer(Integer { value }))
        } else if token.is(TokenType::Separator, "(") {
            let expr = self.expr()?;
            self.expect_exact_token(TokenType::Separator, ")")?;
            return Ok(Factor::Expression(Box::new(expr)));
        }

        return Err(ParseError::InvalidToken((*token).clone()));
    }
}

#[derive(Eq, PartialEq, Debug)]
struct AST {
    root_block: Block,
}

#[derive(Eq, PartialEq, Debug)]
struct Block {
    statements: Vec<Statement>,
}

#[derive(Eq, PartialEq, Debug)]
enum Statement {
    VariableDeclaration(VariableAssignment),
    VariableChange(VariableAssignment),
    If(IfStatement),
}

#[derive(Eq, PartialEq, Debug)]
struct VariableAssignment {
    variable_name: String,
    value_expr: Expression,
}

#[derive(Eq, PartialEq, Debug)]
struct IfStatement {
    condition: Expression,
    body: Block,
}

#[derive(Eq, PartialEq, Debug)]
enum Expression {
    Term(Term),
    Add(Box<Expression>, Term),
    Subtract(Box<Expression>, Term),
}

#[derive(Eq, PartialEq, Debug)]
enum Term {
    Factor(Factor),
    Multiply(Box<Term>, Factor),
    Divide(Box<Term>, Factor),
}

#[derive(Eq, PartialEq, Debug)]
enum Factor {
    Integer(Integer),
    Expression(Box<Expression>),
}

#[derive(Eq, PartialEq, Debug)]
struct Integer {
    value: i32,
}

#[cfg(test)]
mod tests {
    use crate::lexer;
    use super::*;

    fn parse_test(source: &str, ast: AST) {
        let tokens = lexer::lex(source).expect("Failed to lex.");
        println!("{:?}", tokens);

        let mut parser = Parser { tokens, index: 0 };
        let found_ast = parser.parse().unwrap();

        if ast != found_ast {
            panic!("Expected: {:#?}\nFound: {:#?}", ast, found_ast);
        }
    }

    #[test]
    fn test1() {
        parse_test(
            "let first = 1+2*3;",
            AST {
                root_block: Block {
                    statements: vec![
                        Statement::VariableDeclaration(VariableAssignment {
                            variable_name: "first".to_string(),
                            value_expr: Expression::Add(
                                Box::new(Expression::Term(Term::Factor(Factor::Integer(Integer { value: 1 })))),
                                Term::Multiply(
                                    Box::new(Term::Factor(Factor::Integer(Integer { value: 2 }))),
                                    Factor::Integer(Integer { value: 3 })
                                )
                            )
                        })
                    ]
                },
            }
        );
    }

    #[test]
    fn test2() {
        parse_test(
            "let second = 1+2+3+(4+5)*6;",
            AST {
                root_block: Block {
                    statements: vec![
                        Statement::VariableDeclaration(VariableAssignment {
                            variable_name: "second".to_string(),
                            value_expr: Expression::Add(
                                Box::new(Expression::Add(
                                    Box::new(Expression::Add(
                                        Box::new(Expression::Term(Term::Factor(Factor::Integer(Integer { value: 1 })))),
                                        Term::Factor(Factor::Integer(Integer { value: 2 }))
                                    )),
                                    Term::Factor(Factor::Integer(Integer { value: 3 }))
                                )),
                                Term::Multiply(
                                    Box::new(Term::Factor(Factor::Expression(Box::new(Expression::Add(
                                        Box::new(Expression::Term(Term::Factor(Factor::Integer(Integer { value: 4 })))),
                                        Term::Factor(Factor::Integer(Integer { value: 5 }))
                                    ))))),
                                    Factor::Integer(Integer { value: 6 })
                                )
                            )
                        })
                    ]
                },
            }
        );
    }

    #[test]
    fn variable_declaration_and_change() {
        parse_test(
            "let a = 1;\
                    change a = 2;
                    ",
            AST {
                root_block: Block {
                    statements: vec![
                        Statement::VariableDeclaration(VariableAssignment {
                            variable_name: "a".to_string(),
                            value_expr: Expression::Term(Term::Factor(Factor::Integer(Integer { value: 1 })))
                        }),
                        Statement::VariableChange(VariableAssignment {
                            variable_name: "a".to_string(),
                            value_expr: Expression::Term(Term::Factor(Factor::Integer(Integer { value: 2 })))
                        })
                    ]
                },
            }
        );
    }
}