use crate::lexer::{Token, TokenType};

enum ParseError {
    UnexpectedToken(TokenType, TokenType),
    ExpectedButNothingFound(TokenType),
    Idk,
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    marks: Vec<usize>,
}

impl Parser {
    fn peek(&mut self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn read(&mut self) -> Option<&Token> {
        let ret = self.tokens.get(self.index);
        self.index += 1;
        ret
    }

    fn mark(&mut self) {
        self.marks.push(self.index);
    }

    fn reset(&mut self) {
        self.index = self.marks.pop().expect("Must call mark before reset");
    }

    fn discard_mark(&mut self) {
        self.marks.pop().expect("Must call mark before discard_mark");
    }

    fn expect(&mut self, token_type: TokenType) -> Result<&Token, ParseError> {
        let token = self.read();
        if let Some(token) = token {
            if token.token_type() == token_type {
                Ok(token)
            } else {
                Err(ParseError::UnexpectedToken(token.token_type(), token_type))
            }
        } else {
            Err(ParseError::ExpectedButNothingFound(token_type))
        }
    }

    fn expect2(&mut self, token_type: TokenType, data: &str) -> Result<&Token, ParseError> {
        let token = self.read();
        if let Some(token) = token {
            if token.is(token_type, data) {
                Ok(token)
            } else {
                Err(ParseError::UnexpectedToken(token.token_type(), token_type))
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
        while let Ok(statement) = self.statement() {
            statements.push(statement);
        }
        Ok(Block { statements })
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        self.mark();
        let token = self.read().ok_or(ParseError::Idk)?;
        if token.is(TokenType::Keyword, "let") {
            let ident = self.expect(TokenType::Identifier)?;
            self.expect2(TokenType::Operator, "=")?;
            let value = self.expr().ok_or(ParseError::Idk)?;
            self.discard_mark();
            return Ok(Statement::VariableDeclaration(VariableDeclaration {
                variable_name: ident.data().clone().unwrap(),
                value_expr: value,
            }));
        }

        self.reset();
        Err(ParseError::Idk)
    }

    fn expr(&mut self) -> Option<Expression> {
        Some(Expression {})
    }
}

#[derive(Eq, PartialEq)]
struct AST {
    root_block: Block,
}

#[derive(Eq, PartialEq)]
struct Block {
    statements: Vec<Statement>,
}

#[derive(Eq, PartialEq)]
enum Statement {
    VariableDeclaration(VariableDeclaration),
}

#[derive(Eq, PartialEq)]
struct VariableDeclaration {
    variable_name: String,
    value_expr: Expression,
}

#[derive(Eq, PartialEq)]
struct Expression {

}

#[cfg(test)]
mod tests {
    use crate::lexer;
    use super::*;

    fn parse_test(source: &str, ast: AST) {
        let tokens = lexer::lex(source).expect("Failed to lex.");

        let parser = Parser { tokens, index: 0, marks: Vec::new() };
    }

    #[test]
    fn test1() {
        parse_test(
            "let first = 0;",
            AST {
                root_block: Block {
                    statements: vec![
                        Statement::VariableDeclaration(VariableDeclaration {
                            variable_name: "first".to_string(),
                            value_expr: Expression {}
                        })
                    ]
                },
            }
        );
    }
}