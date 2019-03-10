// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::error::Error;
use std::path::PathBuf;


// Modules --------------------------------------------------------------------
mod include;
mod macros;
mod token;
#[cfg(test)] mod mocks;


// Exports --------------------------------------------------------------------
pub use self::include::IncludeLexer;
pub use self::macros::MacroLexer;
pub use self::token::TokenIterator;


// Lexer Tokens ---------------------------------------------------------------
pub trait LexerToken {

    fn typ(&self) -> TokenType;

    fn inner(&self) -> &InnerToken;

    fn inner_mut(&mut self) -> &mut InnerToken;

    fn into_inner(self) -> InnerToken;

    // fn is_expanded(&self) -> bool {
    //     self.inner().is_expanded
    // }

    fn error(&self, message: String) -> LexerError {
        self.inner().error(message)
    }

    fn index(&self) -> (usize, usize) {
        let inner = self.inner();
        (inner.file_index, inner.start_index)
    }

    fn is(&self, typ: TokenType) -> bool {
        self.typ() == typ
    }

    fn has_value(&self, value: &str) -> bool {
        self.inner().value == value
    }

    fn value(&self) -> &str {
        &self.inner().value
    }

}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum TokenType {
    Newline,
    Name,
    Reserved,
    Parameter,
    Offset,
    NumberLiteral,
    StringLiteral,
    TokenGroup,
    BinaryFile,
    BuiltinCall,
    Comma,
    Point,
    Colon,
    Operator,
    Comment,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct InnerToken {
    file_index: usize,
    // Only used for error locations so we can trace back to the source code in macro expansions
    start_index: usize,
    // Only used for error locations so we can trace back to the source code in macro expansions
    end_index: usize,
    raw_value: String,
    value: String,
    expansion_id: Option<usize>
}

impl InnerToken {
    fn new(file_index: usize, start_index: usize, end_index: usize, raw_value: String, value: String) -> Self {
        Self {
            file_index,
            start_index,
            end_index,
            raw_value,
            value,
            expansion_id: None
        }
    }

    fn set_expansion_id(&mut self, id: usize) {
        self.expansion_id = Some(id);
    }

    fn error(&self, message: String) -> LexerError {
        LexerError {
            file_index: self.file_index,
            index: self.start_index,
            message
        }
    }

}


// Lexer File Abstraction -----------------------------------------------------
pub struct LexerFile {
    index: usize,
    path: PathBuf,
    contents: String,
    include_stack: Vec<InnerToken>
}

impl LexerFile {

    fn new(index: usize, contents: String, path: PathBuf, include_stack: Vec<InnerToken>) -> Self {
        Self {
            index,
            path,
            contents,
            include_stack
        }
    }

    fn get_line_and_col(&self, index: usize) -> (usize, usize) {
        let (mut line, mut col) = (0, 0);
        for (i, c) in self.contents.chars().enumerate() {
            if i == index {
                break;

            } else if c == '\n' || c == '\r' {
                line += 1;
                col = 0;

            } else {
                col += 1;
            }
        }
        (line, col)
    }

    fn error(err: LexerError, files: &[LexerFile]) -> LexerError {

        let file = &files[err.file_index];
        let (line, col) = file.get_line_and_col(err.index);
        let line_source = file.contents.split(|c| c == '\r' || c == '\n').nth(line).unwrap_or("Unknown Error Location");
        let col_pointer = str::repeat(" ", col);

        // Build include stacktrace
        let stack = if file.include_stack.len() > 0 {
            format!("\n\n{}", file.include_stack.iter().rev().map(|token| {
                let file = &files[token.file_index];
                let (line, col) = file.get_line_and_col(token.start_index);
                format!("included from file \"{}\" on line {}, column {}", file.path.display(), line + 1, col + 1)

            }).collect::<Vec<String>>().join("\n"))

        } else {
            "".to_string()
        };

        // TODO show context lines?
        let message = format!(
            "In file \"{}\" on line {}, column {}: {}\n\n{}\n{}^--- Here{}",
            file.path.display(),
            line + 1,
            col + 1,
            err.message,
            line_source,
            col_pointer,
            stack
        );

        LexerError {
            file_index: err.file_index,
            index: err.index,
            message
        }

    }

}


// Lexer Error Abstraction -----------------------------------------------------
#[derive(Debug)]
pub struct LexerError {
    file_index: usize,
    index: usize,
    message: String
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for LexerError {}

