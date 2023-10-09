use std::fmt::{Debug, Formatter};
use crate::string_reader::{is_word_char, StringReader};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenType {
    Keyword,
    Identifier,
    Operator,
    Separator,
    Integer,
}

const KEYWORDS: &'static [&'static str] = &["let", "if", "loop", "change"];
const OPERATORS: [char; 5] = ['+', '-', '*', '/', '='];
const SEPARATOR: [char; 6] = ['(', ')', '{', '}', '.', ';'];

#[derive(PartialEq, Eq, Clone)]
pub struct Token {
    token_type: TokenType,
    index: usize,
    data: Option<String>,
}

#[derive(Debug)]
pub enum LexError {
    UnexpectedCharacter(char),
}

impl Token {
    pub fn with_data(token_type: TokenType, index: usize, data: String) -> Self {
        Self { token_type, index, data: Some(data) }
    }

    pub fn is(&self, ty: TokenType, data: &str) -> bool {
        self.token_type == ty && self.data.as_ref().is_some_and(|d| d == data)
    }

    pub fn token_type(&self) -> TokenType {
        self.token_type
    }

    pub fn data(&self) -> Option<&String> {
        self.data.as_ref()
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(data) = &self.data {
            write!(f, "{:?}({})", self.token_type, data)
        } else {
            write!(f, "{:?}", self.token_type)
        }
    }
}

pub fn lex(source: &str) -> Result<Vec<Token>, LexError> {
    let mut reader = StringReader::new(source);
    let mut tokens = Vec::new();

    while let Some(c) = reader.read() {
        if c == '/' && reader.peek() == Some('/') {
            todo!("Skip comment");
        } else if c == '/' && reader.peek() == Some('*') {
            todo!("Skip block comment");
        } else if OPERATORS.contains(&c) {
            tokens.push(Token::with_data(TokenType::Operator, reader.index(), String::from(c)));
        } else if SEPARATOR.contains(&c) {
            tokens.push(Token::with_data(TokenType::Separator, reader.index(), String::from(c)));
        } else if is_word_char(c) {
            let word = reader.read_word(c);
            if KEYWORDS.contains(&&word[..]) {
                tokens.push(Token::with_data(TokenType::Keyword, reader.index(), word));
            } else {
                tokens.push(Token::with_data(TokenType::Identifier, reader.index(), word));
            }
        } else if c.is_digit(10) {
            let int = reader.read_int(c);
            tokens.push(Token::with_data(TokenType::Integer, reader.index(), int));
        } else {
            return Err(LexError::UnexpectedCharacter(c));
        }

        reader.skip_whitespace();
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_test(source: &str, token_string: &str) {
        let tokens = lex(source).expect("Failed to lex");
        for (index, expected_token_string) in token_string.split_whitespace().enumerate() {
            let token = tokens.get(index)
                .expect(&format!("Expected token at token index {}", index));
            let found_token_string = format!("{:?}", token);

            if found_token_string != expected_token_string {
                panic!(
                    "Invalid token at token index {}.\nExpected: {}\nFound:    {}",
                    index, expected_token_string, found_token_string
                );
            }
        }
    }

    #[test]
    fn test1() {
        lex_test(
            "let first = 0;",
            "Keyword(let) Identifier(first) Operator(=) Integer(0) Separator(;)"
        );
    }
}