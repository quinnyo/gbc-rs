// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::error::Error;
use std::path::PathBuf;


// Modules --------------------------------------------------------------------
mod include;
mod macros;
mod token;


// Exports --------------------------------------------------------------------
pub use self::include::Lexer as IncludeLexer;
pub use self::macros::MacroLexer;


// Lexer Tokens ---------------------------------------------------------------
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct InnerToken {
    file_index: usize,
    // Only used for error locations so we can trace back to the source code in macro expansions
    start_index: usize,
    // Only used for error locations so we can trace back to the source code in macro expansions
    end_index: usize,
    raw_value: String,
    value: String
}

#[derive(Debug, Eq, PartialEq)]
pub enum LexerToken {
    Newline(InnerToken),
    Name(InnerToken),
    Parameter(InnerToken),
    Offset(InnerToken),
    NumberLiteral(InnerToken),
    StringLiteral(InnerToken),
    TokenGroup(InnerToken, Vec<LexerToken>),
    Comma(InnerToken),
    Point(InnerToken),
    Colon(InnerToken),
    Operator(InnerToken),
    Comment(InnerToken),
    OpenParen(InnerToken),
    CloseParen(InnerToken),
    OpenBracket(InnerToken),
    CloseBracket(InnerToken),
}

impl LexerToken {
    fn index(&self) -> (usize, usize) {
        match self {
            LexerToken::Newline(inner) | LexerToken::Name(inner) | LexerToken::Parameter(inner) | LexerToken::Offset(inner) | LexerToken::NumberLiteral(inner)
            | LexerToken::StringLiteral(inner) | LexerToken::TokenGroup(inner, _)
            | LexerToken::Comma(inner) | LexerToken::Point(inner) | LexerToken::Colon(inner) | LexerToken::Operator(inner) | LexerToken::Comment(inner)
            | LexerToken::OpenParen(inner) | LexerToken::CloseParen(inner) | LexerToken::OpenBracket(inner) | LexerToken::CloseBracket(inner) => {
                (inner.file_index, inner.start_index)
            }
        }
    }
    fn error(&self, message: String) -> LexerError {
        let (file_index, index) = self.index();
        LexerError {
            file_index,
            index,
            message
        }
    }
}

// Lexer File Abstraction -----------------------------------------------------
pub struct LexerFile {
    index: usize,
    path: PathBuf,
    prefix: String,
    contents: String
}

impl LexerFile {

    fn new(index: usize, contents: String, path: PathBuf) -> Self {
        Self {
            index,
            path,
            prefix: format!("F{}#", index),
            contents
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

    fn error(
        err: LexerError,
        current_file_index: usize,
        files: &[LexerFile],
        include_stack: &[InnerToken]

    ) -> LexerError {

        // Only attach error location once in original source file
        if current_file_index != err.file_index {
            return err;
        }

        let file = &files[err.file_index];
        let (line, col) = file.get_line_and_col(err.index);
        let line_source = file.contents.split(|c| c == '\r' || c == '\n').nth(line).unwrap_or("Unknown Error Location");
        let col_pointer = str::repeat(" ", col);

        // Build include stacktrace
        let stack = if include_stack.len() > 1 {
            format!("\n\n{}", include_stack.iter().rev().map(|token| {
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
        write!(f, "LexerError: {}", self.message)
    }
}

impl Error for LexerError {}


