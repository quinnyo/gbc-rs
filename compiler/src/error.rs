// STD Dependencies -----------------------------------------------------------
use std::fmt;


// External Dependencies ------------------------------------------------------
use colored::Colorize;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{LexerFile, InnerToken};
use crate::lexer::stage::macros::MacroCall;


// Lexer Error Abstraction -----------------------------------------------------
#[derive(Debug)]
pub struct SourceError {
    pub file_index: usize,
    pub index: usize,
    pub message: String,
    macro_call_id: Option<usize>,
    reference: Option<(usize, usize, String)>
}

impl SourceError {

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
        self.reference = Some((token.file_index, token.start_index, message.into()));
        self
    }

    pub fn with_file<S: Into<String>>(mut self, file_index: usize, message: S) -> Self {
        self.reference = Some((file_index, 0, message.into()));
        self
    }

    pub fn extend_with_location_and_macros(self, files: &[LexerFile], macro_calls: &[MacroCall]) -> SourceError {
        self.extend(files, Some(macro_calls))
    }

    pub fn extend_with_location(self, files: &[LexerFile]) -> SourceError {
        self.extend(files, None)
    }

    pub fn extend_with_basic_location(self, files: &[LexerFile]) -> SourceError {
        let location = Self::format_location(
            files,
            self.file_index,
            self.index,
            self.message,
            true,
            false,
            true
        );
        let reference = if let Some((file_index, index, reference)) = self.reference {
            format!(". {}", Self::format_location(files, file_index, index, reference, true, false, false))

        } else {
            "".to_string()
        };

        SourceError {
            file_index: self.file_index,
            index: self.index,
            macro_call_id: self.macro_call_id,
            message: format!("{}{}", location, reference),
            reference: None
        }
    }

    fn extend(self, files: &[LexerFile], macro_calls: Option<&[MacroCall]>) -> SourceError {

        // Add file include stacktrace
        let stack = if let Some(file) = files.get(self.file_index) {
            if !file.include_stack.is_empty() {
                format!("\n\n{}", file.include_stack.iter().rev().map(|token| {
                    let file = &files[token.file_index];
                    let (line, col) = file.get_line_and_col(token.start_index);
                    format!("included from file \"{}\" on line {}, column {}", error_path(file), line + 1, col + 1)

                }).collect::<Vec<String>>().join("\n"))

            } else {
                "".to_string()
            }
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
            format!("\n\n{}", Self::format_location(files, file_index, index, reference, true, true, true))

        } else {
            "".to_string()
        };

        let location = Self::format_location(
            files,
            self.file_index,
            self.index,
            self.message,
            false,
            true,
            true
        );

        SourceError {
            file_index: self.file_index,
            index: self.index,
            macro_call_id: self.macro_call_id,
            message: format!(
                "{}{}{}{}",
                location,
                stack.bright_yellow(),
                reference,
                macro_call,
            ),
            reference: None
        }

    }

    fn format_location(
        files: &[LexerFile],
        file_index: usize,
        index: usize,
        message: String,
        prefix_message: bool,
        show_source: bool,
        show_line: bool

    ) -> String {
        if let Some(file) = files.get(file_index) {
            let (line, col) = file.get_line_and_col(index);
            let line_source = file.contents.split(|c| c == '\r' || c == '\n').nth(line).unwrap_or("Unknown Error Location");
            let col_pointer = str::repeat(" ", col);
            if prefix_message {
                let location = if show_line {
                    format!(
                        "{} in file \"{}\" on line {}, column {}",
                        message,
                        error_path(file),
                        line + 1,
                        col + 1,
                    )
                } else {
                    format!("{} in file \"{}\".", message, error_path(file))
                };
                if show_source {
                    format!(
                        "{}:\n\n{}\n{}{}",
                        location.bright_red(),
                        line_source,
                        col_pointer,
                        "^--- Here".bright_red()
                    )

                } else {
                    location
                }

            } else {
                let location = format!(
                    "In file \"{}\" on line {}, column {}:",
                    error_path(file),
                    line + 1,
                    col + 1,
                );
                format!(
                    "{} {}\n\n{}\n{}{}",
                    location.bright_red(),
                    message,
                    line_source,
                    col_pointer,
                    "^--- Here".bright_red()
                )
            }
        } else {
            message
        }
    }

}

impl fmt::Display for SourceError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

fn error_path(file: &LexerFile) -> String {
    match file.path.canonicalize() {
        Ok(p) => format!("{}", p.display()),
        Err(_) => format!("{}", file.path.display())
    }
}

