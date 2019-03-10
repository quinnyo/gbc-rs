// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Modules --------------------------------------------------------------------
mod error;
mod include;
mod macros;
mod token;
mod value;
#[cfg(test)] mod mocks;


// Exports --------------------------------------------------------------------
pub use self::include::IncludeLexer;
pub use self::macros::MacroLexer;
pub use self::value::ValueLexer;
pub use self::token::TokenIterator;
pub use self::error::LexerError;


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
    Instruction,
    Parameter,
    Offset,
    NumberLiteral,
    Number,
    StringLiteral,
    String,
    TokenGroup,
    BinaryFile,
    BuiltinCall,
    Comma,
    Point,
    Colon,
    Operator,
    LabelDef,
    LabelRef,
    Comment,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct InnerToken {
    pub file_index: usize,
    // Only used for error locations so we can trace back to the source code in macro expansions
    pub start_index: usize,
    // Only used for error locations so we can trace back to the source code in macro expansions
    end_index: usize,
    raw_value: String,
    value: String,
    pub macro_call_id: Option<usize>
}

impl InnerToken {
    fn new(file_index: usize, start_index: usize, end_index: usize, raw_value: String, value: String) -> Self {
        Self {
            file_index,
            start_index,
            end_index,
            raw_value,
            value,
            macro_call_id: None
        }
    }

    fn set_macro_call_id(&mut self, id: usize) {
        self.macro_call_id = Some(id);
    }

    fn error(&self, message: String) -> LexerError {
        LexerError::with_macro_call_id(
            self.file_index,
            self.start_index,
            message,
            self.macro_call_id
        )
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

}

