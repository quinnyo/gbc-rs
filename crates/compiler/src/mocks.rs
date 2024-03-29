// STD Dependencies -----------------------------------------------------------
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::io::{Error as IOError, ErrorKind};

// Internal Dependencies ------------------------------------------------------
use file_io::{FileReader, FileWriter, FileError};
use crate::lexer::{Lexer, IncludeStage, MacroStage, ValueStage, ExpressionStage, EntryStage};

pub struct MockFileReader {
    pub base: PathBuf,
    files: HashMap<PathBuf, String>,
    binary_files: HashMap<PathBuf, Vec<u8>>,
    commands: HashMap<(String, Vec<String>), (Vec<u8>, Vec<u8>, Option<String>)>
}

impl MockFileReader {
    pub fn new() -> Self {
        Self {
            base: PathBuf::from("/"),
            files: HashMap::new(),
            binary_files: HashMap::new(),
            commands: HashMap::new()
        }
    }

    pub fn from_base(base: PathBuf) -> Self {
        Self {
            base,
            files: HashMap::new(),
            binary_files: HashMap::new(),
            commands: HashMap::new()
        }
    }

    pub fn add_file<S: Into<String>>(&mut self, path: S, content: S) {
        self.files.insert(PathBuf::from(path.into()), content.into());
    }

    pub fn add_binary_file<S: Into<String>>(&mut self, path: S, bytes: Vec<u8>) {
        self.binary_files.insert(PathBuf::from(path.into()), bytes);
    }

    pub fn get_file<S: Into<String>>(&mut self, path: S) -> Option<String> {
        self.files.remove(&PathBuf::from(path.into()))
    }

    pub fn get_binary_file<S: Into<String>>(&mut self, path: S) -> Option<Vec<u8>> {
        self.binary_files.remove(&PathBuf::from(path.into()))
    }

    pub fn add_command<S: Into<String>>(
        &mut self,
        name: S,
        args: Vec<String>,
        input: Vec<u8>,
        output: Vec<u8>,
        stderr: Option<String>
    ) {
        self.commands.insert((name.into(), args), (input, output, stderr));
    }

}

impl FileReader for MockFileReader {

    fn base_dir(&self) -> &Path {
        &self.base
    }

    fn run_command(&self, name: String, args: Vec<String>, input: &[u8]) -> Result<Vec<u8>, String> {
        let (expected, output, stderr) = self.commands.get(&(name.clone(), args)).cloned().ok_or_else(|| {
            format!("{}: mock command not found", name)
        })?;
        if let Some(stderr) = stderr {
            Err(stderr)

        } else if input != &expected[..] {
            Err(format!("Mock command input does not match expected values: {:?} vs {:?}", input, expected))

        } else {
            Ok(output)
        }
    }

    fn read_file(&self, parent_path: Option<&PathBuf>, child_path: &Path) -> Result<(PathBuf, String), FileError> {
        let path = Self::resolve_path(&self.base, parent_path, child_path);
        let contents = self.files.get(&path).map(|s| s.to_string()).ok_or_else(|| {
            FileError {
                io: IOError::new(ErrorKind::NotFound, "No Mock file provided"),
                path: path.clone()
            }
        })?;
        Ok((path, contents))
    }

    fn read_binary_file(&self, parent_path: Option<&PathBuf>, child_path: &Path) -> Result<(PathBuf, Vec<u8>), FileError> {
        let path = Self::resolve_path(&self.base, parent_path, child_path);
        let contents = self.binary_files.get(&path).cloned().ok_or_else(|| {
            FileError {
                io: IOError::new(ErrorKind::NotFound, "No Mock file provided"),
                path: path.clone()
            }
        })?;
        Ok((path, contents))
    }

}

impl FileWriter for MockFileReader {
    fn write_file(&mut self, path: &Path, data: String) -> Result<(), FileError> {
        self.files.insert(path.to_path_buf(), data);
        Ok(())
    }

    fn write_binary_file(&mut self, path: &Path, data: Vec<u8>) -> Result<(), FileError> {
        self.binary_files.insert(path.to_path_buf(), data);
        Ok(())
    }
}

pub fn include_lex<S: Into<String>>(s: S) -> Lexer<IncludeStage> {
    let mut reader = MockFileReader::new();
    reader.add_file("/main.gbc", s.into().as_str());
    let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gbc")).expect("IncludeStage failed");
    assert_eq!(lexer.files.len(), 1);
    lexer
}

pub fn include_lex_child<S: Into<String>>(s: S, c: S) -> Lexer<IncludeStage> {
    let mut reader = MockFileReader::new();
    reader.add_file("/main.gbc", s.into().as_str());
    reader.add_file("/second.gbc", c.into().as_str());
    Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gbc")).expect("IncludeStage failed")
}

pub fn macro_lex<S: Into<String>>(s: S) -> Lexer<MacroStage> {
    let lexer = include_lex(s);
    Lexer::<MacroStage>::from_lexer(lexer).expect("MacroStage failed")
}

pub fn macro_lex_child<S: Into<String>>(s: S, c: S) -> Lexer<MacroStage> {
    let lexer = include_lex_child(s, c);
    assert_eq!(lexer.files.len(), 2);
    Lexer::<MacroStage>::from_lexer(lexer).expect("MacroStage failed")
}

pub fn macro_lex_child_error<S: Into<String>>(s: S, c: S) -> String {
    let lexer = include_lex_child(s, c);
    assert_eq!(lexer.files.len(), 2);
    Lexer::<MacroStage>::from_lexer(lexer).err().expect("Expected a SourceError").to_string()
}

pub fn value_lex<S: Into<String>>(s: S) -> Lexer<ValueStage> {
    let lexer = macro_lex(s);
    Lexer::<ValueStage>::from_lexer(lexer).expect("ValueStage failed")
}

pub fn expr_lex<S: Into<String>>(s: S) -> Lexer<ExpressionStage> {
    let lexer = value_lex(s);
    Lexer::<ExpressionStage>::from_lexer(lexer).expect("ExpressionStage failed")
}

pub fn entry_lex<S: Into<String>>(s: S) -> Lexer<EntryStage> {
    let lexer = expr_lex(s);
    Lexer::<EntryStage>::from_lexer(lexer).expect("EntryStage failed")
}

pub fn entry_lex_child<S: Into<String>>(s: S, c: S) -> Lexer<EntryStage> {
    let lexer = macro_lex_child(s, c);
    let lexer = Lexer::<ValueStage>::from_lexer(lexer).expect("ValueStage failed");
    let lexer = Lexer::<ExpressionStage>::from_lexer(lexer).expect("ExpressionStage failed");
    Lexer::<EntryStage>::from_lexer(lexer).expect("EntryStage failed")
}

pub fn entry_lex_child_error<S: Into<String>>(s: S, c: S) -> String {
    let lexer = macro_lex_child(s, c);
    let lexer = Lexer::<ValueStage>::from_lexer(lexer).expect("ValueStage failed");
    let lexer = Lexer::<ExpressionStage>::from_lexer(lexer).expect("ExpressionStage failed");
    Lexer::<EntryStage>::from_lexer(lexer).err().expect("Expected a SourceError").to_string()
}

pub fn expr_lex_binary<S: Into<String>>(s: S, b: Vec<u8>) -> Lexer<ExpressionStage> {
    let mut reader = MockFileReader::new();
    reader.add_file("/main.gbc", s.into().as_str());
    reader.add_binary_file("/child.bin", b);
    let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gbc")).expect("IncludeStage failed");
    let lexer = Lexer::<MacroStage>::from_lexer(lexer).expect("MacroStage failed");
    let lexer = Lexer::<ValueStage>::from_lexer(lexer).expect("ValueStage failed");
    Lexer::<ExpressionStage>::from_lexer(lexer).expect("ExpressionStage failed")
}

pub fn entry_lex_binary<S: Into<String>>(s: S, b: Vec<u8>) -> Lexer<EntryStage> {
    let lexer = expr_lex_binary(s, b);
    Lexer::<EntryStage>::from_lexer(lexer).expect("EntryStage failed")
}

