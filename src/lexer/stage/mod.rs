// Modules --------------------------------------------------------------------
pub mod entry;
pub mod expression;
pub mod include;
pub mod macros;
pub mod value;

#[cfg(test)]
mod mocks {
    // STD Dependencies -------------------------------------------------------
    use std::path::PathBuf;
    use std::collections::HashMap;
    use std::error::Error;
    use std::io::{Error as IOError, ErrorKind};

    // Internal Dependencies --------------------------------------------------
    use crate::traits::{FileReader, FileError};
    use super::include::{IncludeLexer, IncludeToken};
    use super::macros::MacroLexer;
    use super::value::ValueLexer;
    use super::expression::ExpressionLexer;

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

    pub fn include_lex<S: Into<String>>(s: S) -> IncludeLexer {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let lexer = IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.files.len(), 1);
        lexer
    }

    pub fn macro_lex<S: Into<String>>(s: S) -> MacroLexer {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let lexer = IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.files.len(), 1);
        MacroLexer::try_from(lexer).expect("MacroLexer failed")
    }

    pub fn macro_lex_child<S: Into<String>>(s: S, c: S) -> MacroLexer {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        reader.add_file("child.gb.s", c.into().as_str());
        let lexer = IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.files.len(), 2);
        MacroLexer::try_from(lexer).expect("MacroLexer failed")
    }

    pub fn value_lex<S: Into<String>>(s: S) -> ValueLexer {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let lexer = IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.files.len(), 1);
        let lexer = MacroLexer::try_from(lexer).expect("MacroLexer failed");
        ValueLexer::try_from(lexer).expect("ValueLexer failed")
    }


    pub fn expr_lex<S: Into<String>>(s: S) -> ExpressionLexer {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let lexer = IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.files.len(), 1);
        let lexer = MacroLexer::try_from(lexer).expect("MacroLexer failed");
        let lexer = ValueLexer::try_from(lexer).expect("ValueLexer failed");
        ExpressionLexer::try_from(lexer).expect("ExpressionLexer failed")
    }

    pub fn tfs<S: Into<String>>(s: S) -> Vec<IncludeToken> {
        include_lex(s).tokens
    }

    pub fn tfe<S: Into<String>>(s: S) -> Result<usize, Box<dyn Error>> {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        Ok(IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s"))?.len())
    }

}

