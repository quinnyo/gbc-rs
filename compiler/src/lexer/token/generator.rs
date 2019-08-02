// Internal Dependencies ------------------------------------------------------
use super::InnerToken;
use crate::error::SourceError;
use super::super::LexerFile;


// Types ----------------------------------------------------------------------
pub enum TokenChar {
    Valid(char),
    Delimeter,
    Ignore,
    Invalid
}

#[derive(Debug, Clone)]
pub struct GeneratorState {
    index: usize,
    start: usize,
    current: char,
    input_exhausted: bool,
}


// Token Generator Implementation ---------------------------------------------
pub struct TokenGenerator {
    file_index: usize,
    state: GeneratorState,
    chars: Vec<char>
}

impl TokenGenerator {

    pub fn new(file: &LexerFile, text: &str) -> Self {
        Self {
            file_index: file.index,
            state: GeneratorState {
                index: 0,
                start: 0,
                current: '\0',
                input_exhausted: false,
            },
            chars: text.chars().collect()
        }
    }

    pub fn state(&self) -> GeneratorState {
        self.state.clone()
    }

    pub fn set_state(&mut self, state: GeneratorState) {
        self.state = state;
    }

    pub fn error(&self, message: &str) -> SourceError {
        SourceError::new(
            self.file_index,
            self.state.index - 1,
            format!("{} \"{}\".", message, self.state.current)
        )
    }

    pub fn end_of_input(&self, message: &str) -> SourceError {
        SourceError::new(
            self.file_index,
            self.state.index - 1,
            format!("Unexpected end of input {}.", message)
        )
    }

    pub fn index(&self) -> usize {
        self.state.index
    }

    pub fn assert_char(&self, c: char, message: String) -> Result<(), SourceError> {
        if self.state.current != c || self.state.input_exhausted {
            Err(SourceError::new(
                self.file_index,
                self.state.index,
                message
            ))

        } else {
            Ok(())
        }
    }

    pub fn assert_index_changed(&self, previous: usize, message: String) -> Result<(), SourceError> {
        if self.state.index == previous {
            Err(SourceError::new(
                self.file_index,
                self.state.index,
                message
            ))

        } else {
            Ok(())
        }
    }


    pub fn next(&mut self) -> char {
        let c = *self.chars.get(self.state.index).unwrap_or(&'\0');
        self.state.current = c;
        self.state.index += 1;
        c
    }

    pub fn peek(&mut self) -> Option<char> {
        let c = self.chars.get(self.state.index);
        c.cloned()
    }

    pub fn collect_single(&mut self) -> InnerToken {
        InnerToken::new(
            self.file_index,
            self.state.index - 1,
            self.state.index,
            self.state.current.to_string()
        )
    }

    pub fn collect<C: FnMut(char, char) -> TokenChar>(&mut self, inclusive: bool, cb: C) -> Result<InnerToken, SourceError> {
        self.state.start = self.state.index - 1;
        let mut chars = String::with_capacity(8);
        if inclusive {
            chars.push(self.state.current);
        }
        self.collect_with(chars, cb)
    }

    pub fn skip_with<C: Fn(char) -> bool>(&mut self, cb: C) {
        while self.peek().is_some() {
            self.next();
            if cb(self.state.current) {
                break;
            }
        }
    }

    fn collect_with<C: FnMut(char, char) -> TokenChar>(&mut self, mut parsed: String, mut cb: C) -> Result<InnerToken, SourceError> {

        let mut last = '\0';
        let mut end_index = self.state.index;
        let mut limited = false;
        while self.peek().is_some() {
            self.next();
            match cb(self.state.current, last) {
                TokenChar::Valid(p) => {
                    end_index += 1;
                    parsed.push(p);
                },
                TokenChar::Ignore => {
                    end_index += 1;
                },
                TokenChar::Delimeter => {
                    end_index += 1;
                    limited = true;
                    break;
                },
                TokenChar::Invalid => {
                    self.state.index -= 1;
                    break;
                }
            }
            last = self.state.current;
        }

        if !limited && self.peek().is_none() {
            self.state.input_exhausted = true;
        }

        Ok(InnerToken::new(
            self.file_index,
            self.state.start,
            end_index,
            parsed
        ))
    }

}

