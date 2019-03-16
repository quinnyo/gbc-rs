// Internal Dependencies ------------------------------------------------------
use super::macros::MacroCall;
use crate::lexer::ExpressionStage;
use super::super::{LexerStage, InnerToken, TokenType, LexerToken, LexerError};


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
pub struct EntryStage;
impl LexerStage for EntryStage {

    type Input = ExpressionStage;
    type Output = EntryToken;
    type Data = ();

    fn from_tokens(
        _tokens: Vec<<Self::Input as LexerStage>::Output>,
        _macro_calls: &mut Vec<MacroCall>,
        _data: &mut Vec<Self::Data>

    ) -> Result<Vec<Self::Output>, LexerError> {
        Ok(Vec::new())
    }

}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use super::{EntryStage, EntryToken};
    use super::super::mocks::expr_lex;

    fn entry_lexer<S: Into<String>>(s: S) -> Lexer<EntryStage> {
        Lexer::<EntryStage>::from_lexer(expr_lex(s)).expect("EntryStage failed")
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

