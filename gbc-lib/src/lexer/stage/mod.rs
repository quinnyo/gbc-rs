// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Modules --------------------------------------------------------------------
pub mod entry;
pub mod expression;
pub mod include;
pub mod macros;
pub mod value;


// Internal Dependencies ------------------------------------------------------
use crate::traits::FileReader;
use crate::error::SourceError;
use super::{LexerFile, LexerToken};
use macros::MacroCall;


// Traits ---------------------------------------------------------------------
pub trait LexerStage {
    type Input: LexerStage;
    type Output: LexerToken;
    type Data;

    fn from_file<R: FileReader>(
        _: &R,
        _: &PathBuf,
        _: &mut Vec<LexerFile>

    ) -> Result<Vec<Self::Output>, SourceError> {
        Ok(Vec::new())
    }

    fn from_tokens(
        _: Vec<<Self::Input as LexerStage>::Output>,
        _: &mut Vec<MacroCall>,
        _: &mut Vec<Self::Data>,
        _: bool

    ) -> Result<Vec<Self::Output>, SourceError> {
        Ok(Vec::new())
    }

}

