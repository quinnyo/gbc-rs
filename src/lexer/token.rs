// Internal Dependencies ------------------------------------------------------
use super::{LexerError, LexerToken, LexerFile, TokenType, InnerToken};


// Types ----------------------------------------------------------------------
pub enum TokenChar {
    Valid(char),
    Delimeter,
    Ignore,
    Invalid
}


// Token Iterator Implementation ----------------------------------------------
pub struct TokenIterator<T: LexerToken> {
    file_index: usize,
    index: usize,
    tokens: std::iter::Peekable<std::vec::IntoIter<T>>
}

impl<T: LexerToken> TokenIterator<T> {

    pub fn new(tokens: Vec<T>) -> Self {
        Self {
            file_index: 0,
            index: 0,
            tokens: tokens.into_iter().peekable()
        }
    }

    pub fn peek(&mut self, typ: TokenType, value: Option<&str>) -> bool {
        match self.tokens.peek() {
            Some(token) => if token.is(typ) {
                if let Some(value) = value {
                    token.has_value(value)

                } else {
                    true
                }

            } else {
                false
            },
            None => false
        }
    }

    pub fn expect<S: Into<String>>(&mut self, typ: TokenType, value: Option<&str>, message: S) -> Result<T, LexerError> {
        match self.next() {
            Some(token) => {
                if token.is(typ)  {
                    if let Some(value) = value {
                        if token.has_value(value) {
                            Ok(token)

                        } else {
                            Err(token.error(format!("Unexpected token value \"{:?}\" {}, expected a value of \"{:?}\" instead.", token.value(), message.into(), value)))
                        }

                    } else {
                        Ok(token)
                    }

                } else {
                    Err(token.error(format!("Unexpected token \"{:?}\" {}, expected a \"{:?}\" token instead.", token.typ(), message.into(), typ)))
                }
            },
            None => Err(LexerError::new(
                self.file_index,
                self.index,
                format!("Unexpected end of input {}, expected a \"{:?}\" token instead.", message.into(), typ)
            ))
        }
    }

    pub fn get<S: Into<String>>(&mut self, message: S) -> Result<T, LexerError> {
        match self.next() {
            Some(token) => Ok(token),
            None => Err(LexerError::new(
                self.file_index,
                self.index,
                format!("Unexpected end of input {}.", message.into())
            ))
        }
    }

    pub fn next(&mut self) -> Option<T> {
        match self.tokens.next() {
            Some(token) => {
                let (file_index, index) = token.index();
                self.file_index = file_index;
                self.index = index;
                Some(token)
            },
            None => None
        }
    }

}


// Token Generator Implementation ---------------------------------------------
pub struct TokenGenerator {
    file_index: usize,
    index: usize,
    start: usize,
    current: char,
    input_exhausted: bool,
    chars: Vec<char>
}

impl TokenGenerator {

    pub fn new(file: &LexerFile, text: &str) -> Self {
        Self {
            file_index: file.index,
            index: 0,
            start: 0,
            current: '\0',
            input_exhausted: false,
            chars: text.chars().collect()
        }
    }

    pub fn error(&self) -> LexerError {
        LexerError::new(
            self.file_index,
            self.index - 1,
            format!("Unexpected character \"{}\".", self.current)
        )
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn assert_char(&self, c: char, message: String) -> Result<(), LexerError> {
        if self.current != c || self.input_exhausted {
            Err(LexerError::new(
                self.file_index,
                self.index,
                message
            ))

        } else {
            Ok(())
        }
    }

    pub fn assert_index_changed(&self, previous: usize, message: String) -> Result<(), LexerError> {
        if self.index == previous {
            Err(LexerError::new(
                self.file_index,
                self.index,
                message
            ))

        } else {
            Ok(())
        }
    }


    pub fn next(&mut self) -> char {
        let c = *self.chars.get(self.index).unwrap_or(&'\0');
        self.current = c;
        self.index += 1;
        c
    }

    pub fn peek(&mut self) -> Option<char> {
        let c = self.chars.get(self.index);
        c.cloned()
    }

    pub fn collect_single(&mut self) -> InnerToken {
        InnerToken::new(
            self.file_index,
            self.index - 1,
            self.index,
            self.current.to_string(),
            self.current.to_string()
        )
    }

    pub fn collect<C: FnMut(char, char) -> TokenChar>(&mut self, inclusive: bool, cb: C) -> Result<InnerToken, LexerError> {
        self.start = self.index - 1;
        if inclusive {
            self.collect_with(vec![self.current], vec![self.current], cb)

        } else {
            self.collect_with(vec![self.current], vec![], cb)
        }
    }

    fn collect_with<C: FnMut(char, char) -> TokenChar>(&mut self, mut raw: Vec<char>, mut parsed: Vec<char>, mut cb: C) -> Result<InnerToken, LexerError> {

        let mut last = '\0';
        let mut end_index = self.index;
        let mut limited = false;
        while self.peek().is_some() {
            self.next();
            match cb(self.current, last) {
                TokenChar::Valid(p) => {
                    raw.push(self.current);
                    end_index += 1;
                    parsed.push(p);
                },
                TokenChar::Ignore => {
                    raw.push(self.current);
                    end_index += 1;
                },
                TokenChar::Delimeter => {
                    raw.push(self.current);
                    end_index += 1;
                    limited = true;
                    break;
                },
                TokenChar::Invalid => {
                    self.index -= 1;
                    break;
                }
            }
            last = self.current;
        }

        if !limited && self.peek().is_none() {
            self.input_exhausted = true;
        }

        Ok(InnerToken::new(
            self.file_index,
            self.start,
            end_index,
            raw.into_iter().collect(),
            parsed.into_iter().collect()
        ))
    }

}

