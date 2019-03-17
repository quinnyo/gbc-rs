// Internal Dependencies ------------------------------------------------------
use super::{LexerToken, TokenType};
use super::super::LexerError;


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

    pub fn peek_typ(&mut self) -> Option<TokenType> {
        self.tokens.peek().map(|t| t.typ())
    }

    pub fn peek_is(&mut self, typ: TokenType, value: Option<&str>) -> bool {
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

    pub fn peek(&mut self) -> Option<&T> {
        self.tokens.peek()
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

                    } else if let Some(value) = value {
                        Err(token.error(format!("Unexpected token \"{:?}\" {}, expected \"{}\" instead.", token.typ(), message.into(), value)))

                    } else {
                        Ok(token)
                    }

                } else {
                    Err(token.error(format!("Unexpected token \"{:?}\" {}, expected a \"{:?}\" token instead.", token.typ(), message.into(), typ)))
                }
            },
            None => if let Some(value) = value {
                Err(LexerError::new(
                    self.file_index,
                    self.index,
                    format!("Unexpected end of input {}, expected \"{}\" instead.", message.into(), value)
                ))

            } else {
                Err(LexerError::new(
                    self.file_index,
                    self.index,
                    format!("Unexpected end of input {}, expected a \"{:?}\" token instead.", message.into(), typ)
                ))
            }
        }
    }

    pub fn get<S: Into<String>>(&mut self, message: S) -> Result<T, LexerError> {
        match self.next() {
            Some(token) => Ok(token),
            None => Err(LexerError::new(
                self.file_index,
                self.index,
                message.into()
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

