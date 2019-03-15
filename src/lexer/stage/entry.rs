// STD Dependencies -----------------------------------------------------------
use std::error::Error;


// Internal Dependencies ------------------------------------------------------
use super::super::{ExpressionLexer, InnerToken, TokenType, LexerToken, LexerFile, LexerError};
use super::expression::ExpressionToken;
use super::macros::MacroCall;


// Types ----------------------------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub enum Mnemonic {

}
// enum Mnemonic {
//     ADD_HL_BC(),
//     META(MetaMnemonic)
// }
//
// enum MetaMnemonic {
//     ADD_W(),
//     VSync()
// }



// Entry Specific Tokens ------------------------------------------------------
lexer_token!(EntryToken, (Debug, Eq, PartialEq), {
    BinaryFile((Vec<u8>))
}, {
    // TODO do const eval first in rom layout computation
    // TODO then do address resolution
    // TODO during parsing store constants and addresses to evaluate

    // Constant + EQU|EQUS + ConstExpression
    Constant {
        // TODO value (Expression)
        // TODO type Number / String
        name => String,
        size => usize
    },
    // GlobalLabelDef + DS|DS8|DS16 + ConstExpression
    // GlobalLabelDef + DB|DW
    Variable {
        // TODO byte / word
        name => String,
        alignment => usize, // Enum
        size => usize // Enum
    },
    Instruction {
        // TODO value(s) for parameters (Expression(s))
        mnemonic => Mnemonic // TODO put values directly into mnemonic enum
        // TODO size get from Mnemonic
    },
    // SECTION EXPR[String]
    SectionDeclaration {
        // TODO offset, bank etc.
        name => String
    },
    GlobalLabelDef {
        name => String
    },
    LocalLabelDef {
        name => String
    }
});

// Entry Level Lexer Implementation -------------------------------------------
pub struct EntryLexer {
    pub files: Vec<LexerFile>,
    pub tokens: Vec<EntryToken>,
    pub macro_calls: Vec<MacroCall>
}

impl EntryLexer {

    pub fn try_from(lexer: ExpressionLexer) -> Result<EntryLexer, Box<dyn Error>> {
        let files = lexer.files;
        let macro_calls = lexer.macro_calls;
        let tokens = Self::from_tokens(lexer.tokens).map_err(|err| {
            err.extend_with_location_and_macros(&files, &macro_calls)
        })?;
        Ok(Self {
            files,
            tokens,
            macro_calls
        })
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    fn from_tokens(tokens: Vec<ExpressionToken>) -> Result<Vec<EntryToken>, LexerError> {
        Ok(Vec::new())
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::{EntryLexer, EntryToken};
    use super::super::mocks::expr_lex;

    fn entry_lexer<S: Into<String>>(s: S) -> EntryLexer {
        EntryLexer::try_from(expr_lex(s)).expect("EntryLexer failed")
    }

    // fn entry_lexer_error<S: Into<String>>(s: S) -> String {
    //     EntryLexer::try_from(expr_lex(s)).err().unwrap().to_string()
    // }

    fn tfe<S: Into<String>>(s: S) -> Vec<EntryToken> {
        entry_lexer(s).tokens
    }

    macro_rules! itk {
        ($start:expr, $end:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $parsed.into())
        }
    }

    // Entry Parsing ----------------------------------------------------------
    #[test]
    fn test_empty() {
        assert_eq!(tfe(""), vec![]);
    }

}

