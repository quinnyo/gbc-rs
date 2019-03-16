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
use super::{LexerFile, LexerToken, LexerError};
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

    ) -> Result<Vec<Self::Output>, LexerError> {
        Ok(Vec::new())
    }

    fn from_tokens(
        _: Vec<<Self::Input as LexerStage>::Output>,
        _: &mut Vec<MacroCall>,
        _: &mut Vec<Self::Data>

    ) -> Result<Vec<Self::Output>, LexerError> {
        Ok(Vec::new())
    }
}


// Test Mocks -----------------------------------------------------------------
#[cfg(test)]
mod mocks {
    // STD Dependencies -------------------------------------------------------
    use std::path::PathBuf;
    use std::collections::HashMap;
    use std::io::{Error as IOError, ErrorKind};

    // Internal Dependencies --------------------------------------------------
    use crate::traits::{FileReader, FileError};
    use crate::lexer::{Lexer, IncludeStage, MacroStage, ValueStage, ExpressionStage};

    #[derive(Default)]
    pub struct MockFileReader {
        pub base: PathBuf,
        files: HashMap<PathBuf, String>,
        binary_files: HashMap<PathBuf, Vec<u8>>
    }

    impl MockFileReader {
        pub fn add_file<S: Into<String>>(&mut self, path: S, content: S) {
            self.files.insert(PathBuf::from(path.into()), content.into());
        }
        pub fn add_binary_file<S: Into<String>>(&mut self, path: S, bytes: Vec<u8>) {
            self.binary_files.insert(PathBuf::from(path.into()), bytes);
        }
    }

    impl FileReader for MockFileReader {

        fn read_file(&self, parent_path: Option<&PathBuf>, child_path: &PathBuf) -> Result<(PathBuf, String), FileError> {
            let path = Self::resolve_path(&self.base, parent_path, child_path);
            let contents = self.files.get(&path).map(|s| s.to_string()).ok_or_else(|| {
                FileError {
                    io: IOError::new(ErrorKind::NotFound, "No Mock file provided"),
                    path: path.clone()
                }
            })?;
            Ok((path, contents))
        }

        fn read_binary_file(&self, parent_path: Option<&PathBuf>, child_path: &PathBuf) -> Result<(PathBuf, Vec<u8>), FileError> {
            let path = Self::resolve_path(&self.base, parent_path, child_path);
            let contents = self.binary_files.get(&path).map(|b| b.clone()).ok_or_else(|| {
                FileError {
                    io: IOError::new(ErrorKind::NotFound, "No Mock file provided"),
                    path: path.clone()
                }
            })?;
            Ok((path, contents))
        }

    }

    pub fn include_lex<S: Into<String>>(s: S) -> Lexer<IncludeStage> {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).expect("IncludeStage failed");
        assert_eq!(lexer.files.len(), 1);
        lexer
    }

    pub fn macro_lex<S: Into<String>>(s: S) -> Lexer<MacroStage> {
        let lexer = include_lex(s);
        Lexer::<MacroStage>::from_lexer(lexer).expect("MacroStage failed")
    }

    pub fn macro_lex_child<S: Into<String>>(s: S, c: S) -> Lexer<MacroStage> {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        reader.add_file("child.gb.s", c.into().as_str());
        let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).expect("IncludeStage failed");
        assert_eq!(lexer.files.len(), 2);
        Lexer::<MacroStage>::from_lexer(lexer).expect("MacroStage failed")
    }

    pub fn value_lex<S: Into<String>>(s: S) -> Lexer<ValueStage> {
        let lexer = macro_lex(s);
        Lexer::<ValueStage>::from_lexer(lexer).expect("ValueStage failed")
    }

    pub fn expr_lex<S: Into<String>>(s: S) -> Lexer<ExpressionStage> {
        let lexer = value_lex(s);
        Lexer::<ExpressionStage>::from_lexer(lexer).expect("ExpressionStage failed")
    }

}

