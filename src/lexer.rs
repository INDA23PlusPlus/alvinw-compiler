use std::fmt::{Debug, Formatter};

#[derive(Debug)]
enum TokenType {
    Keyword,
    Identifier,
    Equals,
    Plus,
    Integer,
    Semicolon,
    LeftBrace, // {
    RightBrace, // }
    LeftParen, // (
    RightParen, // )
}

struct Token {
    token_type: TokenType,
    index: usize,
    data: Option<String>,
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

fn lex(source: &str) -> Vec<Token> {
    todo!();
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_test(source: &str, token_string: &str) {
        let tokens = lex(source);
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
            "Keyword(let) Identifier(first) Equals Integer(0) Semicolon"
        );
    }
}