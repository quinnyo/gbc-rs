// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::error::Error;


// Internal Dependencies ------------------------------------------------------
use super::{LexerFile, InnerToken};
use super::stage::macros::MacroCall;


// Lexer Error Abstraction -----------------------------------------------------
#[derive(Debug)]
pub struct LexerError {
    pub file_index: usize,
    pub index: usize,
    pub message: String,
    macro_call_id: Option<usize>,
    reference: Option<(usize, usize, String)>
}

impl LexerError {

    pub fn new(file_index: usize, index: usize, message: String) -> Self {
        Self {
            file_index,
            index,
            message,
            macro_call_id: None,
            reference: None
        }
    }

    pub fn with_macro_call_id(
        file_index: usize,
        index: usize,
        message: String,
        macro_call_id: Option<usize>

    ) -> Self {
        Self {
            file_index,
            index,
            message,
            macro_call_id,
            reference: None
        }
    }

    pub fn with_reference<S: Into<String>>(mut self, token: &InnerToken, message: S) -> Self {
        self.reference =Some((token.file_index, token.start_index, message.into()));
        self
    }

    pub fn extend_with_location_and_macros(self, files: &[LexerFile], macro_calls: &[MacroCall]) -> LexerError {
        self.extend(files, Some(macro_calls))
    }

    pub fn extend_with_location(self, files: &[LexerFile]) -> LexerError {
        self.extend(files, None)
    }

    fn extend(self, files: &[LexerFile], macro_calls: Option<&[MacroCall]>) -> LexerError {

        let file = &files[self.file_index];

        // Add file include stacktrace
        let stack = if !file.include_stack.is_empty() {
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

        // Add reference location
        let reference = if let Some((file_index, index, reference)) = self.reference {
            format!("\n\n{}", Self::format_location(files, file_index, index, reference, true))

        } else {
            "".to_string()
        };

        let location = Self::format_location(
            files,
            self.file_index,
            self.index,
            self.message,
            false
        );

        LexerError {
            file_index: self.file_index,
            index: self.index,
            macro_call_id: self.macro_call_id,
            message: format!(
                "{}{}{}{}",
                location,
                stack,
                reference,
                macro_call,
            ),
            reference: None
        }

    }

    fn format_location(files: &[LexerFile], file_index: usize, index: usize, message: String, prefix_message: bool) -> String {
        let file = &files[file_index];
        let (line, col) = file.get_line_and_col(index);
        let line_source = file.contents.split(|c| c == '\r' || c == '\n').nth(line).unwrap_or("Unknown Error Location");
        let col_pointer = str::repeat(" ", col);
        if prefix_message {
            format!(
                "{} in file \"{}\" on line {}, column {}:\n\n{}\n{}^--- Here",
                message,
                file.path.display(),
                line + 1,
                col + 1,
                line_source,
                col_pointer
            )

        } else {
            format!(
                "In file \"{}\" on line {}, column {}: {}\n\n{}\n{}^--- Here",
                file.path.display(),
                line + 1,
                col + 1,
                message,
                line_source,
                col_pointer
            )
        }
    }

}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for LexerError {}

