use std::str::Chars;

pub fn is_word_char(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

pub struct StringReader<'a> {
    chars: Chars<'a>,
    index: usize,
    peeked_char: Option<char>,
}

impl<'a> StringReader<'a> {
    pub fn new(value: &'a str) -> Self {
        Self {
            chars: value.chars(),
            index: 0,
            peeked_char: None,
        }
    }

    pub fn index(&self) -> usize { self.index }

    pub fn skip(&mut self) {
        self.index += 1;
        if self.peeked_char.is_some() {
            self.peeked_char = None;
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        if let Some(peeked_char) = self.peeked_char {
            Some(peeked_char)
        } else {
            self.peeked_char = self.chars.next();
            if self.peeked_char.is_some() {
                self.index += 1;
            }
            self.peeked_char
        }
    }

    pub fn read(&mut self) -> Option<char> {
        self.index += 1;
        if let Some(peeked_char) = self.peeked_char {
            self.peeked_char = None;
            Some(peeked_char)
        } else {
            self.chars.next()
        }
    }

    pub fn read_word(&mut self, first_char: char) -> String {
        let mut str = String::new();
        str.push(first_char);
        while let Some(c) = self.peek() {
            if !is_word_char(c) {
                break;
            }
            str.push(c);
            self.skip();
        }
        str
    }

    pub fn read_int(&mut self, first_char: char) -> String {
        let mut str = String::new();
        str.push(first_char);
        while let Some(c) = self.peek() {
            if !c.is_digit(10) {
                break;
            }
            str.push(c);
            self.skip();
        }
        str
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.skip();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peek_read_test() {
        let mut reader = StringReader::new("Hello");
        assert_eq!(reader.read(), Some('H'));
        assert_eq!(reader.peek(), Some('e'));
        assert_eq!(reader.peek(), Some('e'));
        assert_eq!(reader.read(), Some('e'));
        assert_eq!(reader.read(), Some('l'));
        assert_eq!(reader.read(), Some('l'));
        assert_eq!(reader.read(), Some('o'));
        assert_eq!(reader.peek(), None);
        assert_eq!(reader.read(), None);
    }

    #[test]
    fn read_word() {
        let mut reader = StringReader::new("hello world");
        let first = reader.read().unwrap();
        let word = reader.read_word(first);
        assert_eq!("hello", word);
    }
}