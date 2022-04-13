// Internal Dependencies ------------------------------------------------------
use crate::error::SourceError;
use super::{LexerToken, Symbol};


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

    pub fn peek_typ(&mut self) -> Option<T::Typ> {
        self.tokens.peek().map(LexerToken::typ)
    }

    pub fn peek_is(&mut self, typ: T::Typ, symbol: Option<Symbol>) -> bool {
        match self.tokens.peek() {
            Some(token) => if token.is(typ) {
                if let Some(symbol) = symbol {
                    token.is_symbol(symbol)

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

    pub fn expect<S: Into<String>>(&mut self, typ: T::Typ, symbol: Option<Symbol>, message: S) -> Result<T, SourceError> {
        match self.next() {
            Some(token) => {
                if token.is(typ)  {
                    if let Some(symbol) = symbol {
                        if token.is_symbol(symbol.clone()) {
                            Ok(token)

                        } else {
                            Err(token.error(format!("Unexpected token value \"{:?}\" {}, expected a value of \"{:?}\" instead.", token.symbol(), message.into(), symbol)))
                        }

                    } else {
                        Ok(token)
                    }

                } else if let Some(symbol) = symbol {
                    Err(token.error(format!("Unexpected token \"{:?}\" {}, expected \"{}\" instead.", token.typ(), message.into(), symbol)))

                } else {
                    Err(token.error(format!("Unexpected token \"{:?}\" {}, expected a \"{:?}\" token instead.", token.typ(), message.into(), typ)))
                }
            },
            None => if let Some(symbol) = symbol {
                Err(SourceError::new(
                    self.file_index,
                    self.index,
                    format!("Unexpected end of input {}, expected \"{}\" instead.", message.into(), symbol)
                ))

            } else {
                Err(SourceError::new(
                    self.file_index,
                    self.index,
                    format!("Unexpected end of input {}, expected a \"{:?}\" token instead.", message.into(), typ)
                ))
            }
        }
    }

    pub fn get<S: Into<String>>(&mut self, message: S) -> Result<T, SourceError> {
        match self.next() {
            Some(token) => Ok(token),
            None => Err(SourceError::new(
                self.file_index,
                self.index,
                message.into()
            ))
        }
    }

}

impl<T: LexerToken> Iterator for TokenIterator<T>  {
    type Item = T;
    fn next(&mut self) -> Option<T> {
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

