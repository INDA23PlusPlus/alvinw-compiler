use crate::lexer::{Token, TokenType};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: TokenType,
        found: Token,
    },
    ExpectedButNothingFound(TokenType),
    UnexpectedEnd,
    ExpectedEndOfFile,
    InvalidToken(Token),
}

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

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
        let root_block = self.block()?;
        if self.index != self.tokens.len() {
            return Err(ParseError::ExpectedEndOfFile);
        }
        Ok(AST { root_block })
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
        println!("dbu3 {:?}", token);
        if token.is(TokenType::Keyword, "let") {
            let name = self.expect_token(TokenType::Identifier)?.data().unwrap().clone();
            self.expect_exact_token(TokenType::Operator, "=")?;
            let value = self.expr()?;
            self.expect_exact_token(TokenType::Separator, ";")?;
            return Ok(Statement::VariableDeclaration(VariableAssignment {
                variable_name: name,
                value_expr: value,
            }));
        } else if token.is(TokenType::Keyword, "set") {
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
        } else if token.is(TokenType::Keyword, "loop") {
            self.expect_exact_token(TokenType::Separator, "{")?;
            let body = self.block()?;
            self.expect_exact_token(TokenType::Separator, "}")?;
            return Ok(Statement::Loop(Box::new(body)));
        } else if token.is(TokenType::Keyword, "break") {
            self.expect_exact_token(TokenType::Separator, ";")?;
            return Ok(Statement::Break);
        }

        // Maybe this statement is an expression statement? If not it's invalid.
        self.index -= 1;
        let expr = self.expr()?;
        self.expect_exact_token(TokenType::Separator, ";")?;
        return Ok(Statement::ExpressionStatement(Box::new(expr)));
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
            } else if next.is(TokenType::Operator, "=") {
                self.skip();
                expr = Expression::Equals(Box::new(expr), Box::new(self.expr()?));
            } else if next.is(TokenType::Operator, "<") {
                self.skip();
                expr = Expression::LessThan(Box::new(expr), Box::new(self.expr()?));
            } else if next.is(TokenType::Operator, ">") {
                self.skip();
                expr = Expression::MoreThan(Box::new(expr), Box::new(self.expr()?));
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
        let token = self.read().ok_or(ParseError::UnexpectedEnd)?.clone();
        println!("dbu2 {:?}", token);
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
        } else if token.token_type() == TokenType::Identifier {
            let ident = token.data().expect("Identifier must be valid").clone();
            println!("dbu1 {:?}", ident);
            let next = self.peek();
            if let Some(next) = next {
                if next.is(TokenType::Separator, "(") {
                    self.skip();
                    let mut arguments = Vec::new();
                    loop {
                        match self.peek() {
                            Some(token) if token.is(TokenType::Separator, ")") => {
                                // End of arguments
                                break;
                            },
                            _ => {},
                        }
                        let arg = self.expr()?;
                        arguments.push(arg);
                        let next = self.read();
                        match next {
                            Some(token) if token.is(TokenType::Separator, ")") => {
                                // End of arguments
                                break;
                            }
                            Some(token) if token.is(TokenType::Separator, ",") => {
                                // Read another argument
                                continue;
                            }
                            Some(other) => {
                                return Err(ParseError::InvalidToken(other.clone()));
                            }
                            None => {
                                return Err(ParseError::UnexpectedEnd);
                            }
                        }
                    }

                    return Ok(Factor::FunctionCall(FunctionCall {
                        function_name: ident,
                        arguments
                    }));
                }
            }

            // Variable
            return Ok(Factor::Variable(ident));
        }

        return Err(ParseError::InvalidToken(token));
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct AST {
    pub root_block: Block,
}

#[derive(Eq, PartialEq, Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Eq, PartialEq, Debug)]
pub enum Statement {
    VariableDeclaration(VariableAssignment),
    VariableChange(VariableAssignment),
    If(IfStatement),
    ExpressionStatement(Box<Expression>),
    Loop(Box<Block>),
    Break,
}

#[derive(Eq, PartialEq, Debug)]
pub struct VariableAssignment {
    pub variable_name: String,
    pub value_expr: Expression,
}

#[derive(Eq, PartialEq, Debug)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Block,
}

#[derive(Eq, PartialEq, Debug)]
pub enum Expression {
    Term(Term),
    Add(Box<Expression>, Term),
    Subtract(Box<Expression>, Term),
    Equals(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    MoreThan(Box<Expression>, Box<Expression>),
}

#[derive(Eq, PartialEq, Debug)]
pub enum Term {
    Factor(Factor),
    Multiply(Box<Term>, Factor),
    Divide(Box<Term>, Factor),
}

#[derive(Eq, PartialEq, Debug)]
pub enum Factor {
    Integer(Integer),
    Expression(Box<Expression>),
    Variable(String),
    FunctionCall(FunctionCall)
}

#[derive(Eq, PartialEq, Debug)]
pub struct FunctionCall {
    pub function_name: String,
    pub arguments: Vec<Expression>,
}

#[derive(Eq, PartialEq, Debug)]
pub struct Integer {
    pub value: i32,
}

#[cfg(test)]
mod tests {
    use crate::lexer;
    use crate::parser::Statement::ExpressionStatement;
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
                    set a = 2;
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

    #[test]
    fn function_call() {
        parse_test(
            "let a = 1;\
                    print(a + 1);
                    ",
            AST {
                root_block: Block {
                    statements: vec![
                        Statement::VariableDeclaration(VariableAssignment {
                            variable_name: "a".to_string(),
                            value_expr: Expression::Term(Term::Factor(Factor::Integer(Integer { value: 1 })))
                        }),
                        Statement::ExpressionStatement(Box::new(Expression::Term(Term::Factor(Factor::FunctionCall(FunctionCall {
                            function_name: "print".to_string(),
                            arguments: vec![
                                Expression::Add(
                                    Box::new(Expression::Term(Term::Factor(Factor::Variable("a".to_string())))),
                                    Term::Factor(Factor::Integer(Integer { value: 1 }))
                                )
                            ]
                        })))))
                    ]
                }
            }
        );
    }

    #[test]
    fn variable_decl_and_change() {
        parse_test(
            "let a = 2; set a = 2;",
            AST {
                root_block: Block {
                    statements: vec![
                        Statement::VariableDeclaration(
                            VariableAssignment {
                                variable_name: "a".to_string(),
                                value_expr: Expression::Term(Term::Factor(Factor::Integer(
                                    Integer { value: 2 }
                                )))
                            }
                        ),
                        Statement::VariableChange(
                            VariableAssignment {
                                variable_name: "a".to_string(),
                                value_expr: Expression::Term(Term::Factor(Factor::Integer(
                                    Integer { value: 2 }
                                )))
                            }
                        )
                    ]
                }
            }
        )
    }

    #[test]
    fn if_equals() {
        parse_test(
            "if (a = b) {}",
            AST {
                root_block: Block {
                    statements: vec![
                        Statement::If(IfStatement {
                            condition: Expression::Equals(
                                Box::new(Expression::Term(Term::Factor(Factor::Variable("a".to_string())))),
                                Box::new(Expression::Term(Term::Factor(Factor::Variable("b".to_string()))))
                            ),
                            body: Block { statements: vec![] }
                        })
                    ]
                }
            }
        )
    }

    #[test]
    fn loop_test1() {
        parse_test(
            "loop {}",
            AST {
                root_block: Block {
                    statements: vec![
                        Statement::Loop(Box::new(Block { statements: vec![] }))
                    ]
                }
            }
        )
    }
    #[test]
    fn loop_test2() {
        parse_test(
            "loop { print(1); }",
            AST {
                root_block: Block {
                    statements: vec![
                        Statement::Loop(Box::new(Block { statements: vec![
                            ExpressionStatement(Box::new(Expression::Term(Term::Factor(Factor::FunctionCall(
                                FunctionCall { function_name: "print".to_string(), arguments: vec![
                                    Expression::Term(Term::Factor(Factor::Integer(Integer { value: 1 })))
                                ] }
                            )))))
                        ] }))
                    ]
                }
            }
        )
    }
}