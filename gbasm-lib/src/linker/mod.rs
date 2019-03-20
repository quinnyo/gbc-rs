// External Dependencies ------------------------------------------------------


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{Lexer, EntryStage};


// Linker Implementation ------------------------------------------------------
pub struct Linker {

}


// TODO enum ExpressionResult String/Integer/Float

impl Linker {

    pub fn from_lexer(lexer: Lexer<EntryStage>) -> Self {
        // TODO filter out all EntryToken::Constant's and put into a HashMap<Name, Result>
        // TODO resolve constant sizes for EntryToken::Data's so all offsets (i.e. label addresses) can be calculated in
        // one sweep instead of having to do multiple passes
        Self {

        }
    }

}

