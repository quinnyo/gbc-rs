// STD Dependencies -----------------------------------------------------------
use std::error::Error;


// Internal Dependencies ------------------------------------------------------
use super::{IncludeLexer, InnerToken, LexerError, LexerFile, LexerToken};
use super::include::IncludeToken;


// Include Specific Tokens ----------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub enum MacroToken {
    Name(InnerToken),
    Offset(InnerToken),
    NumberLiteral(InnerToken),
    StringLiteral(InnerToken),
    BinaryFile(InnerToken, Vec<u8>),
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

impl From<IncludeToken> for MacroToken {
    fn from(token: IncludeToken) -> Self {
        match token {
            IncludeToken::Name(inner) => MacroToken::Name(inner),
            IncludeToken::Offset(inner) => MacroToken::Offset(inner),
            IncludeToken::NumberLiteral(inner) => MacroToken::NumberLiteral(inner),
            IncludeToken::StringLiteral(inner) => MacroToken::StringLiteral(inner),
            IncludeToken::BinaryFile(inner, bytes) => MacroToken::BinaryFile(inner, bytes),
            IncludeToken::Comma(inner) => MacroToken::Comma(inner),
            IncludeToken::Point(inner) => MacroToken::Point(inner),
            IncludeToken::Colon(inner) => MacroToken::Colon(inner),
            IncludeToken::Operator(inner) => MacroToken::Operator(inner),
            IncludeToken::Comment(inner) => MacroToken::Comment(inner),
            IncludeToken::OpenParen(inner) => MacroToken::OpenParen(inner),
            IncludeToken::CloseParen(inner) => MacroToken::CloseParen(inner),
            IncludeToken::OpenBracket(inner) => MacroToken::OpenBracket(inner),
            IncludeToken::CloseBracket(inner) => MacroToken::CloseBracket(inner),
            _ => unreachable!()
        }
    }
}

impl LexerToken for MacroToken {
    fn index(&self) -> (usize, usize) {
        match self {
            MacroToken::Name(inner) | MacroToken::Offset(inner) | MacroToken::NumberLiteral(inner)
            | MacroToken::StringLiteral(inner) | MacroToken::BinaryFile(inner, _)
            | MacroToken::Comma(inner) | MacroToken::Point(inner) | MacroToken::Colon(inner) | MacroToken::Operator(inner) | MacroToken::Comment(inner)
            | MacroToken::OpenParen(inner) | MacroToken::CloseParen(inner) | MacroToken::OpenBracket(inner) | MacroToken::CloseBracket(inner) => {
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


// Macro Definitions ----------------------------------------------------------


// Macro Level Lexer Implementation -------------------------------------------
pub struct MacroLexer {
    files: Vec<LexerFile>,
    tokens: Vec<MacroToken>
}

impl MacroLexer {

    pub fn try_from(lexer: IncludeLexer) -> Result<MacroLexer, Box<dyn Error>> {

        let mut tokens_without_macro_defs = Vec::new();

        // Drop all newlines
        let mut tokens = lexer.tokens.into_iter().filter(|t| if let IncludeToken::Newline(_) = t {
            false

        } else {
            true
        });

        // Extract Macro Definitions
        while let Some(token) = tokens.next() {
            // TODO check for MACRO <NAME>(@param, ...) [body] ENDMACRO and extract
            // TODO check for balanced closing paren

            if let IncludeToken::Parameter(_) = token {
                // TODO remove simple debug filter

            } else {
                tokens_without_macro_defs.push(token);
            }
        }

        // Recursively expand Macro Calls
        let mut tokens_without_macro_calls = Vec::new();
        loop {
            let mut expanded_macro_calls = 0;
            let mut tokens = tokens_without_macro_defs.into_iter();
            while let Some(token) = tokens.next() {
                // TODO check for MACRO_NAME(@param, ...)
                // TODO check for balanced closing paren
                // TODO expand macro calls by...
                    // TODO creating a copy of the macro tokens
                    // TODO replacing all argument references with the supplied arguments tokens and
                        // flattening groups
                    // TODO inserting the expanded macro tokens into the call site

                // TODO mark all inserted tokens as being expanded
                if let IncludeToken::TokenGroup(_, _) = token {
                    // TODO remove simple debug filter

                } else {
                    tokens_without_macro_calls.push(token);
                }
            }
            if expanded_macro_calls > 0 {
                // Try to expand any newly introduced calls from the current expansion
                tokens_without_macro_defs = tokens_without_macro_calls.drain(0..).collect();

            } else {
                break;
            }
        }

        Ok(Self {
            files: lexer.files,
            tokens: tokens_without_macro_calls.into_iter().map(MacroToken::from).collect()
        })

    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::{MacroLexer, MacroToken, InnerToken};
    use crate::lexer::mocks::include_lex;

    fn tfm<S: Into<String>>(s: S) -> Vec<MacroToken> {
        MacroLexer::try_from(include_lex(s)).expect("MacroLexer failed").tokens
    }

    macro_rules! tk {
        ($tok:ident, $start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            MacroToken::$tok(InnerToken::new(0, $start, $end, $raw.into(), $parsed.into()))
        }
    }

    #[test]
    fn test_empty() {
        assert_eq!(tfm(""), vec![]);
    }

    #[test]
    fn test_passthrough() {
        assert_eq!(tfm("4\n@-2\nhl,.:"), vec![
            // TODO test pass through of all tokens
            tk!(NumberLiteral, 0, 1, "4", "4"),
            tk!(Offset, 2, 5, "@-2", "-2"),
            tk!(Name, 6, 8, "hl", "hl"),
            tk!(Comma, 8, 9, ",", ","),
            tk!(Point, 9, 10, ".", "."),
            tk!(Colon, 10, 11, ":", ":"),
        ]);
    }

}

