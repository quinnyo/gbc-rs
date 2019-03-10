// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::error::Error;


// Internal Dependencies ------------------------------------------------------
use super::{LexerFile};
use super::macros::MacroCall;


// Lexer Error Abstraction -----------------------------------------------------
#[derive(Debug)]
pub struct LexerError {
    pub file_index: usize,
    pub index: usize,
    pub message: String,
    macro_call_id: Option<usize>
}

impl LexerError {

    pub fn new(file_index: usize, index: usize, message: String) -> Self {
        Self {
            file_index,
            index,
            message,
            macro_call_id: None
        }
    }

    pub fn with_macro_call_id(file_index: usize, index: usize, message: String, macro_call_id: Option<usize>) -> Self {
        Self {
            file_index,
            index,
            message,
            macro_call_id
        }
    }

    pub fn extend_with_location_and_macros(self, files: &[LexerFile], macro_calls: &[MacroCall]) -> LexerError {
        self.extend(files, Some(macro_calls))
    }

    pub fn extend_with_location(self, files: &[LexerFile]) -> LexerError {
        self.extend(files, None)
    }

    fn extend(self, files: &[LexerFile], macro_calls: Option<&[MacroCall]>) -> LexerError {

        let file = &files[self.file_index];
        let (line, col) = file.get_line_and_col(self.index);
        let line_source = file.contents.split(|c| c == '\r' || c == '\n').nth(line).unwrap_or("Unknown Error Location");
        let col_pointer = str::repeat(" ", col);

        // Add file include stacktrace
        let stack = if file.include_stack.len() > 0 {
            format!("\n\n{}", file.include_stack.iter().rev().map(|token| {
                let file = &files[token.file_index];
                let (line, col) = file.get_line_and_col(token.start_index);
                format!("included from file \"{}\" on line {}, column {}", file.path.display(), line + 1, col + 1)

            }).collect::<Vec<String>>().join("\n"))

        } else {
            "".to_string()
        };

        // Add macro call stacktrace
        let macro_call = if let Some(macro_calls) = macro_calls {
            if let Some(macro_call_id) = self.macro_call_id {
                let call_error = macro_calls[macro_call_id].error("Triggered by previous macro invocation".to_string());
                let call_error = call_error.extend(files, Some(macro_calls));
                format!("\n\n{}", call_error)

            } else {
                "".to_string()
            }

        } else {
            "".to_string()
        };

        // TODO show context lines?
        let message = format!(
            "In file \"{}\" on line {}, column {}: {}\n\n{}\n{}^--- Here{}{}",
            file.path.display(),
            line + 1,
            col + 1,
            self.message,
            line_source,
            col_pointer,
            stack,
            macro_call
        );

        LexerError {
            file_index: self.file_index,
            index: self.index,
            macro_call_id: self.macro_call_id,
            message
        }

    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for LexerError {}

