// STD Dependencies -----------------------------------------------------------
use std::error::Error;
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use crate::traits::{FileError, FileReader};
use super::token::{TokenIterator, TokenChar};
use super::{InnerToken, LexerError, LexerFile, LexerToken};


// Include Specific Tokens ----------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub enum IncludeToken {
    Newline(InnerToken),
    Name(InnerToken),
    Parameter(InnerToken),
    Offset(InnerToken),
    NumberLiteral(InnerToken),
    StringLiteral(InnerToken),
    TokenGroup(InnerToken, Vec<IncludeToken>),
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

impl LexerToken for IncludeToken {
    fn index(&self) -> (usize, usize) {
        match self {
            IncludeToken::Newline(inner) | IncludeToken::Name(inner) | IncludeToken::Parameter(inner) | IncludeToken::Offset(inner) | IncludeToken::NumberLiteral(inner)
            | IncludeToken::StringLiteral(inner) | IncludeToken::TokenGroup(inner, _) | IncludeToken::BinaryFile(inner, _)
            | IncludeToken::Comma(inner) | IncludeToken::Point(inner) | IncludeToken::Colon(inner) | IncludeToken::Operator(inner) | IncludeToken::Comment(inner)
            | IncludeToken::OpenParen(inner) | IncludeToken::CloseParen(inner) | IncludeToken::OpenBracket(inner) | IncludeToken::CloseBracket(inner) => {
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


// Types ----------------------------------------------------------------------
struct IncludeLexerState<'a, T: FileReader> {
    file_reader: &'a T,
    files: &'a mut Vec<LexerFile>,
    parent_path: Option<&'a PathBuf>,
    child_path: &'a PathBuf,
    include_stack: Vec<InnerToken>
}

impl<'a, T: FileReader> IncludeLexerState<'a, T> {

    fn split_off_child(mut self, parent_path: &'a PathBuf) -> (Self, Vec<InnerToken>) {
        let include_stack = self.include_stack.clone();
        self.parent_path = Some(parent_path);
        (self, include_stack)
    }

}


// Include Level Lexer Implementation -----------------------------------------
pub struct IncludeLexer {
    files: Vec<LexerFile>,
    tokens: Vec<IncludeToken>
}

impl IncludeLexer {

    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            tokens: Vec::new()
        }
    }

    pub fn lex_file<T: FileReader>(&mut self, file_reader: &T, child_path: &PathBuf) -> Result<usize, Box<dyn Error>>{
        self.tokens = Self::lex_file_child(IncludeLexerState {
            file_reader,
            files: &mut self.files,
            parent_path: None,
            child_path,
            include_stack: Vec::new()
        })?;
        Ok(self.tokens.len())
    }

    fn lex_file_child<T: FileReader>(state: IncludeLexerState<T>) -> Result<Vec<IncludeToken>, Box<dyn Error>>{

        // Read in child file contents
        let (child_path, contents) = state.file_reader.read_file(state.parent_path, state.child_path)?;

        // Create new file abstraction
        let file = LexerFile::new(state.files.len(), contents, child_path.clone());
        let current_file_index = file.index;
        state.files.push(file);

        // Create new lexer state for child file
        let (mut state, include_stack) = state.split_off_child(&child_path);
        let file = state.files.last().unwrap();

        // Lex child tokens
        let child_tokens = Self::tokenize(file, &file.contents).map_err(|err| {
            LexerFile::error(err, current_file_index, state.files, &state.include_stack)
        })?;

        // Resolve any includes in the tokenized file
        Ok(Self::resolve_include_tokens(
            file.index,
            child_tokens,
            &mut state,

        ).map_err(|err| {
            LexerFile::error(err, current_file_index, state.files, &include_stack)
        })?)

    }

    fn resolve_include_tokens<T: FileReader>(
        parent_file_index: usize,
        tokens: Vec<IncludeToken>,
        state: &mut IncludeLexerState<T>,

    ) -> Result<Vec<IncludeToken>, LexerError> {

        let mut serialized = Vec::new();

        let mut tokens = tokens.into_iter();
        while let Some(token) = tokens.next() {
            if let IncludeToken::Name(ref name) = token {
                if name.value == "INCLUDE" {
                    match tokens.next() {
                        Some(IncludeToken::StringLiteral(token)) => {
                            let mut include_stack = state.include_stack.clone();
                            include_stack.push(token.clone());

                            let child_state = IncludeLexerState {
                                file_reader: state.file_reader,
                                files: state.files,
                                parent_path: state.parent_path,
                                child_path: &PathBuf::from(token.value.clone()),
                                include_stack
                            };

                            serialized.append(&mut Self::include_directive(
                                child_state,
                                parent_file_index,
                                token.start_index
                            )?);
                        },
                        Some(other) => return Err(other.error("Expected a StringLiteral instead.".to_string())),
                        None => return Err(token.error("Expected a StringLiteral to follow.".to_string()))
                    }

                } else if name.value == "INCBIN" {
                    match tokens.next() {
                        Some(IncludeToken::StringLiteral(token)) => {
                            let child_state = IncludeLexerState {
                                file_reader: state.file_reader,
                                files: state.files,
                                parent_path: state.parent_path,
                                child_path: &PathBuf::from(token.value.clone()),
                                include_stack: state.include_stack.clone()
                            };
                            serialized.push(Self::incbin_directive(
                                child_state,
                                token
                            )?);
                        },
                        Some(other) => return Err(other.error("Expected a StringLiteral instead.".to_string())),
                        None => return Err(token.error("Expected a StringLiteral to follow.".to_string()))
                    }

                } else {
                    serialized.push(token);
                }

            } else {
                serialized.push(token);
            }
        }

        Ok(serialized)

    }

    fn include_directive<T: FileReader>(
        state: IncludeLexerState<T>,
        file_index: usize,
        index: usize

    ) -> Result<Vec<IncludeToken>, LexerError> {
        Self::lex_file_child(state).map_err(|err| {
            if let Some(err) = err.downcast_ref::<FileError>() {
                LexerError {
                    file_index,
                    index,
                    message: format!("File \"{}\" not found", err.path.display())
                }

            } else if let Some(err) = err.downcast_ref::<LexerError>() {
                LexerError {
                    file_index: err.file_index,
                    index: err.index,
                    message: err.message.clone()
                }

            } else {
                unreachable!();
            }
        })
    }

    fn incbin_directive<T: FileReader>(
        state: IncludeLexerState<T>,
        token: InnerToken

    ) -> Result<IncludeToken, LexerError> {
        let (_, bytes) = state.file_reader.read_binary_file(state.parent_path, state.child_path).map_err(|err| {
            LexerError {
                file_index: token.file_index,
                index: token.start_index,
                message: format!("File \"{}\" not found", err.path.display())
            }
        })?;
        Ok(IncludeToken::BinaryFile(token, bytes))
    }

    fn tokenize(file: &LexerFile, text: &str) -> Result<Vec<IncludeToken>, LexerError> {
        let mut iter = TokenIterator::new(&file, text);
        Self::collect_tokens(&mut iter, false)
    }

    fn collect_tokens(iter: &mut TokenIterator, inside_token_group: bool) -> Result<Vec<IncludeToken>, LexerError> {
        let mut tokens = Vec::new();
        while iter.peek().is_some() {
            let token = match iter.next() {
                // Whitespace
                ' ' | '\t' => continue, // Ignore whitespace,
                '\n' | '\r' => Some(IncludeToken::Newline(iter.collect_single())),
                // Names
                'a'...'z' | 'A'...'Z' | '_' => Some(IncludeToken::Name(Self::collect_inner_name(iter, true)?)),
                // Parameter / Offset
                '@' => {
                    if let Some('+') | Some('-') = iter.peek() {
                        Some(IncludeToken::Offset(iter.collect(false, |c, _| {
                            if let '_' = c {
                                TokenChar::Ignore

                            } else if let '0'...'9' | '+' | '-' = c {
                                TokenChar::Valid(c)

                            } else {
                                TokenChar::Invalid
                            }
                        })?))

                    } else if let Some('a'...'z') | Some('A'...'Z') = iter.peek() {
                        Some(IncludeToken::Parameter(Self::collect_inner_name(iter, false)?))

                    } else {
                        None
                    }
                },
                // NumberLiteral
                '0'...'9' => Some(Self::collect_number_literal(iter)?),
                '-' => if let Some('0'...'9') = iter.peek() {
                    Some(Self::collect_number_literal(iter)?)

                } else {
                    Some(IncludeToken::Operator(iter.collect_single()))
                },
                '$' => if let Some('0'...'9') | Some('a'...'f') | Some('A'...'F') = iter.peek() {
                    Some(IncludeToken::NumberLiteral(iter.collect(true, |c, _| {
                        if let '_' = c {
                            TokenChar::Ignore

                        } else if let '0'...'9' | 'a'...'f' | 'A'...'F' = c {
                            TokenChar::Valid(c)

                        } else {
                            TokenChar::Invalid
                        }
                    })?))

                } else {
                    None
                },
                '%' => if let Some('0'...'1') = iter.peek() {
                    Some(IncludeToken::NumberLiteral(iter.collect(true, |c, _| {
                        if let '_' = c {
                            TokenChar::Ignore

                        } else if let '0'...'1' = c {
                            TokenChar::Valid(c)

                        } else {
                            TokenChar::Invalid
                        }
                    })?))

                } else {
                    Some(IncludeToken::Operator(iter.collect_single()))
                },
                // StringLiteral
                '"' => {
                    Some(IncludeToken::StringLiteral(Self::collect_inner_string(iter, '"')?))
                },
                '\'' => {
                    Some(IncludeToken::StringLiteral(Self::collect_inner_string(iter, '\'')?))
                },
                // Token Groups
                '`' => if inside_token_group{
                    return Ok(tokens);

                } else {
                    let index_token = iter.collect_single();
                    let group_start = iter.index();
                    let tokens = Self::collect_tokens(iter, true)?;
                    iter.assert_char('`', "Unclosed token group.".to_string())?;
                    iter.assert_index_changed(group_start, "Unclosed token group.".to_string())?;
                    Some(IncludeToken::TokenGroup(index_token, tokens))
                },
                // Operator
                '!' | '&' | '*' | '/' | '=' | '|' | '+' | '~' | '<' | '>' => {
                    Some(IncludeToken::Operator(iter.collect_single()))
                },
                // Punctation
                ';' => Some(IncludeToken::Comment(iter.collect(true, |c, _| {
                    if let '\n' | '\r' = c {
                        TokenChar::Invalid

                    } else {
                        TokenChar::Valid(c)
                    }
                })?)),
                ':' => Some(IncludeToken::Colon(iter.collect_single())),
                '.' => Some(IncludeToken::Point(iter.collect_single())),
                ',' => Some(IncludeToken::Comma(iter.collect_single())),
                '(' => Some(IncludeToken::OpenParen(iter.collect_single())),
                ')' => Some(IncludeToken::CloseParen(iter.collect_single())),
                '[' => Some(IncludeToken::OpenBracket(iter.collect_single())),
                ']' => Some(IncludeToken::CloseBracket(iter.collect_single())),
                _ => None
            };
            if let Some(token) = token {
                tokens.push(token);

            } else {
                return Err(iter.error());
            }
        }
        Ok(tokens)
    }

    fn collect_inner_string(iter: &mut TokenIterator, delimeter: char) -> Result<InnerToken, LexerError> {
        let t = iter.collect(false, |c, p| {
            // Ignore escape slashes
            if c == '\\' && p != '\\' {
                TokenChar::Ignore

            // Handle escaped characters
            } else if p == '\\' {
                match c {
                    'n' => TokenChar::Valid('\n'),
                    'r' => TokenChar::Valid('\r'),
                    't' => TokenChar::Valid('\t'),
                    '\'' => {
                        TokenChar::Valid('\'')
                    },
                    '"' => TokenChar::Valid('"'),
                    _ => TokenChar::Ignore
                }

            } else if c == delimeter {
                TokenChar::Delimeter

            } else {
                TokenChar::Valid(c)
            }
        })?;
        iter.assert_char(delimeter, "Unclosed string literal.".to_string())?;
        Ok(t)
    }

    fn collect_inner_name(iter: &mut TokenIterator, inclusive: bool) -> Result<InnerToken, LexerError> {
        Ok(iter.collect(inclusive, |c, _| {
            if let 'a'...'z' | 'A'...'Z' | '_' | '0'...'9' = c {
                TokenChar::Valid(c)

            } else {
                TokenChar::Invalid
            }
        })?)
    }

    fn collect_number_literal(iter: &mut TokenIterator) -> Result<IncludeToken, LexerError> {
        Ok(IncludeToken::NumberLiteral(iter.collect(true, |c, _| {
            if let '_' = c {
                TokenChar::Ignore

            } else if let '0'...'9' = c {
                TokenChar::Valid(c)

            } else {
                TokenChar::Invalid
            }
        })?))
    }

}

// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {

    use std::path::PathBuf;
    use std::collections::HashMap;
    use std::error::Error;
    use std::io::{Error as IOError, ErrorKind};
    use super::{IncludeLexer, IncludeToken, InnerToken, FileReader, FileError};

    #[derive(Default)]
    struct MockFileReader {
        base: PathBuf,
        files: HashMap<PathBuf, String>,
        binary_files: HashMap<PathBuf, Vec<u8>>
    }

    impl MockFileReader {
        fn add_file<S: Into<String>>(&mut self, path: S, content: S) {
            self.files.insert(PathBuf::from(path.into()), content.into());
        }
        fn add_binary_file<S: Into<String>>(&mut self, path: S, bytes: Vec<u8>) {
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

    fn tfs<S: Into<String>>(s: S) -> Vec<IncludeToken> {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let mut lexer = IncludeLexer::new();
        lexer.lex_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.files.len(), 1);
        lexer.tokens
    }

    fn tfe<S: Into<String>>(s: S) -> Result<usize, Box<dyn Error>> {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let mut lexer = IncludeLexer::new();
        lexer.lex_file(&reader, &PathBuf::from("main.gb.s"))
    }

    macro_rules! tk {
        ($tok:ident, $start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            IncludeToken::$tok(InnerToken::new(0, $start, $end, $raw.into(), $parsed.into()))
        }
    }

    macro_rules! tkf {
        ($file:expr, $tok:ident, $start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            IncludeToken::$tok(InnerToken::new($file, $start, $end, $raw.into(), $parsed.into()))
        }
    }

    macro_rules! itk {
        ($start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $raw.into(), $parsed.into())
        }
    }

    #[test]
    fn test_empty() {
        assert_eq!(tfs(""), vec![]);
    }

    #[test]
    fn test_resolve_includes() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "INCLUDE 'foo.gb.s'\nINCLUDE 'extra/bar.gb.s'\nINCLUDE '/abs.gb.s'");
        reader.add_file("src/foo.gb.s", "42");
        reader.add_file("src/extra/bar.gb.s", "BAR");
        reader.add_file("src/abs.gb.s", "ABS");

        let mut lexer = IncludeLexer::new();
        lexer.lex_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            tkf!(1, NumberLiteral, 0, 2, "42", "42"),
            tkf!(0, Newline, 18, 19, "\n", "\n"),
            tkf!(2, Name, 0, 3, "BAR", "BAR"),
            tkf!(0, Newline, 43, 44, "\n", "\n"),
            tkf!(3, Name, 0, 3, "ABS", "ABS"),
        ]);

    }

    #[test]
    fn test_resolve_nested_includes() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "1\nINCLUDE 'one.gb.s'");
        reader.add_file("src/one.gb.s", "2\nINCLUDE 'extra/two.gb.s'\n4");
        reader.add_file("src/extra/two.gb.s", "INCLUDE '/three.gb.s'");
        reader.add_file("src/three.gb.s", "3");

        let mut lexer = IncludeLexer::new();
        lexer.lex_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            tkf!(0, NumberLiteral, 0, 1, "1", "1"),
            tkf!(0, Newline, 1, 2, "\n", "\n"),
            tkf!(1, NumberLiteral, 0, 1, "2", "2"),
            tkf!(1, Newline, 1, 2, "\n", "\n"),
            tkf!(3, NumberLiteral, 0, 1, "3", "3"),
            tkf!(1, Newline, 26, 27, "\n", "\n"),
            tkf!(1, NumberLiteral, 27, 28, "4", "4"),
        ]);

    }

    #[test]
    fn test_resolve_nested_include_io_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "1\nINCLUDE 'one.gb.s'");

        let mut lexer = IncludeLexer::new();
        let err = lexer.lex_file(&reader, &PathBuf::from("main.gb.s")).unwrap_err();
        assert_eq!(err.to_string(), "LexerError: In file \"src/main.gb.s\" on line 2, column 9: File \"src/one.gb.s\" not found\n\nINCLUDE \'one.gb.s\'\n        ^--- Here");

    }

    #[test]
    fn test_resolve_nested_include_lexer_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "1\nINCLUDE 'one.gb.s'");
        reader.add_file("src/one.gb.s", "2\nINCLUDE 'extra/two.gb.s'\n4");
        reader.add_file("src/extra/two.gb.s", "INCLUDE '/three.gb.s'");
        reader.add_file("src/three.gb.s", "@");

        let mut lexer = IncludeLexer::new();
        let err = lexer.lex_file(&reader, &PathBuf::from("main.gb.s")).unwrap_err();
        assert_eq!(err.to_string(), "LexerError: In file \"src/three.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here\n\nincluded from file \"src/extra/two.gb.s\" on line 1, column 9\nincluded from file \"src/one.gb.s\" on line 2, column 9\nincluded from file \"src/main.gb.s\" on line 2, column 9");

    }

    #[test]
    fn test_resolve_include_incomplete() {
        assert_eq!(tfe("INCLUDE 4").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 9: Expected a StringLiteral instead.\n\nINCLUDE 4\n        ^--- Here");
        assert_eq!(tfe("INCLUDE").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 1: Expected a StringLiteral to follow.\n\nINCLUDE\n^--- Here");
    }

    #[test]
    fn test_resolve_incbins() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "INCBIN 'data.bin'\nINCBIN 'second.bin'");
        reader.add_binary_file("src/data.bin", vec![0, 1, 2, 3, 4, 5, 6, 7]);
        reader.add_binary_file("src/second.bin", vec![42]);

        let mut lexer = IncludeLexer::new();
        lexer.lex_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            IncludeToken::BinaryFile(itk!(7, 17, "'data.bin'", "data.bin"), vec![0, 1, 2, 3, 4, 5, 6, 7]),
            tkf!(0, Newline, 17, 18, "\n", "\n"),
            IncludeToken::BinaryFile(itk!(25, 37, "'second.bin'", "second.bin"), vec![42])
        ]);

    }

    #[test]
    fn test_resolve_nested_incbins_io_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "INCBIN 'data.bin'");

        let mut lexer = IncludeLexer::new();
        let err = lexer.lex_file(&reader, &PathBuf::from("main.gb.s")).unwrap_err();
        assert_eq!(err.to_string(), "LexerError: In file \"src/main.gb.s\" on line 1, column 8: File \"src/data.bin\" not found\n\nINCBIN \'data.bin\'\n       ^--- Here");

    }

    #[test]
    fn test_resolve_incbin_incomplete() {
        assert_eq!(tfe("INCBIN 4").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 8: Expected a StringLiteral instead.\n\nINCBIN 4\n       ^--- Here");
        assert_eq!(tfe("INCBIN").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 1: Expected a StringLiteral to follow.\n\nINCBIN\n^--- Here");
    }

    #[test]
    fn test_newlinews() {
        assert_eq!(tfs("\n\r\n\n\r"), vec![
            tk!(Newline, 0, 1, "\n", "\n"),
            tk!(Newline, 1, 2, "\r", "\r"),
            tk!(Newline, 2, 3, "\n", "\n"),
            tk!(Newline, 3, 4, "\n", "\n"),
            tk!(Newline, 4, 5, "\r", "\r"),
        ]);
    }

    #[test]
    fn test_name() {
        assert_eq!(tfs("INCLUDEX"), vec![tk!(Name, 0, 8, "INCLUDEX", "INCLUDEX")]);
        assert_eq!(tfs("hl"), vec![tk!(Name, 0, 2, "hl", "hl")]);
        assert_eq!(tfs("a"), vec![tk!(Name, 0, 1, "a", "a")]);
        assert_eq!(tfs("foo_bar"), vec![tk!(Name, 0, 7, "foo_bar", "foo_bar")]);
        assert_eq!(tfs("_test"), vec![tk!(Name, 0, 5, "_test", "_test")]);
        assert_eq!(tfs("abcdefghijklmnopqrstuvwxyz"), vec![tk!(Name, 0, 26, "abcdefghijklmnopqrstuvwxyz", "abcdefghijklmnopqrstuvwxyz")]);
        assert_eq!(tfs("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), vec![tk!(Name, 0, 26, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "ABCDEFGHIJKLMNOPQRSTUVWXYZ")]);
    }

    #[test]
    fn test_number_literal() {
        assert_eq!(tfs("20_48"), vec![
            tk!(NumberLiteral, 0, 5, "20_48", "2048")
        ]);
        assert_eq!(tfs("-512"), vec![
            tk!(NumberLiteral, 0, 4, "-512", "-512")
        ]);
        assert_eq!(tfs("$1234"), vec![
            tk!(NumberLiteral, 0, 5, "$1234", "$1234")
        ]);
        assert_eq!(tfs("$1234"), vec![
            tk!(NumberLiteral, 0, 5, "$1234", "$1234")
        ]);
        assert_eq!(tfs("$abcdefABCDEF"), vec![
            tk!(NumberLiteral, 0, 13, "$abcdefABCDEF", "$abcdefABCDEF")
        ]);
        assert_eq!(tfs("%1001_0020"), vec![
            tk!(NumberLiteral, 0, 8, "%1001_00", "%100100"),
            tk!(NumberLiteral, 8, 10, "20", "20")
        ]);
    }

    #[test]
    fn test_number_literal_hex_incomplete() {
        assert_eq!(tfe("$").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 1: Unexpected character \"$\".\n\n$\n^--- Here");
    }

    #[test]
    fn test_string_literal() {
        assert_eq!(tfs("'Hello World'"), vec![
            tk!(StringLiteral, 0, 13, "'Hello World'", "Hello World")
        ]);
        assert_eq!(tfs("\"Hello World\""), vec![
            tk!(StringLiteral, 0, 13, "\"Hello World\"", "Hello World")
        ]);
        assert_eq!(tfs("'\"'"), vec![
            tk!(StringLiteral, 0, 3, "'\"'", "\"")
        ]);
        assert_eq!(tfs("\"'\""), vec![
            tk!(StringLiteral, 0, 3, "\"'\"", "'")
        ]);
    }

    #[test]
    fn test_string_literal_unclosed() {
        assert_eq!(tfe("'Hello World").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 13: Unclosed string literal.\n\n'Hello World\n            ^--- Here");
        assert_eq!(tfe("\"Hello World").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 13: Unclosed string literal.\n\n\"Hello World\n            ^--- Here");
        assert_eq!(tfe("'''").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 4: Unclosed string literal.\n\n\'\'\'\n   ^--- Here");
        assert_eq!(tfe("\"\"\"").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 4: Unclosed string literal.\n\n\"\"\"\n   ^--- Here");
    }

    #[test]
    fn test_string_literal_escapes() {
        assert_eq!(tfs("'\\n'"), vec![
            tk!(StringLiteral, 0, 4, "'\\n'", "\n")
        ]);
        assert_eq!(tfs("'\\r'"), vec![
            tk!(StringLiteral, 0, 4, "'\\r'", "\r")
        ]);
        assert_eq!(tfs("'\\t'"), vec![
            tk!(StringLiteral, 0, 4, "'\\t'", "\t")
        ]);
        assert_eq!(tfs("'\\''"), vec![
            tk!(StringLiteral, 0, 4, "'\\\''", "'")
        ]);
        assert_eq!(tfs("\"\\\"\""), vec![
            tk!(StringLiteral, 0, 4, "\"\\\"\"", "\"")
        ]);
    }

    #[test]
    fn test_string_punct() {
        assert_eq!(tfs(","), vec![tk!(Comma, 0, 1, ",", ",")]);
        assert_eq!(tfs("."), vec![tk!(Point, 0, 1, ".", ".")]);
        assert_eq!(tfs(":"), vec![tk!(Colon, 0, 1, ":", ":")]);
        assert_eq!(tfs("()"), vec![
            tk!(OpenParen, 0, 1, "(", "("),
            tk!(CloseParen, 1, 2, ")", ")")
        ]);
        assert_eq!(tfs("[]"), vec![
            tk!(OpenBracket, 0, 1, "[", "["),
            tk!(CloseBracket, 1, 2, "]", "]")
        ]);
        assert_eq!(tfs("=%&|!-+<>~*/"), vec![
            tk!(Operator, 0, 1, "=", "="),
            tk!(Operator, 1, 2, "%", "%"),
            tk!(Operator, 2, 3, "&", "&"),
            tk!(Operator, 3, 4, "|", "|"),
            tk!(Operator, 4, 5, "!", "!"),
            tk!(Operator, 5, 6, "-", "-"),
            tk!(Operator, 6, 7, "+", "+"),
            tk!(Operator, 7, 8, "<", "<"),
            tk!(Operator, 8, 9, ">", ">"),
            tk!(Operator, 9, 10, "~", "~"),
            tk!(Operator, 10, 11, "*", "*"),
            tk!(Operator, 11, 12, "/", "/"),
        ]);
    }

    #[test]
    fn test_param_offset() {
        assert_eq!(tfs("@param"), vec![tk!(Parameter, 0, 6, "@param", "param")]);
        assert_eq!(tfs("@param_foo"), vec![tk!(Parameter, 0, 10, "@param_foo", "param_foo")]);
        assert_eq!(tfs("@+4"), vec![tk!(Offset, 0, 3, "@+4", "+4")]);
        assert_eq!(tfs("@-4"), vec![tk!(Offset, 0, 3, "@-4", "-4")]);
    }

    #[test]
    fn test_param_offset_incomplete() {
        assert_eq!(tfe("@").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here");
    }

    #[test]
    fn test_comments() {
        assert_eq!(tfs("2 ; A Comment"), vec![
            tk!(NumberLiteral, 0, 1, "2", "2"),
            tk!(Comment, 2, 13, "; A Comment", "; A Comment")
        ]);
    }

    #[test]
    fn test_multiline() {
        assert_eq!(tfs("a\n'Text'\n4"), vec![
            tk!(Name, 0, 1, "a", "a"),
            tk!(Newline, 1, 2, "\n", "\n"),
            tk!(StringLiteral, 2, 8, "'Text'", "Text"),
            tk!(Newline, 8, 9, "\n", "\n"),
            tk!(NumberLiteral, 9, 10, "4", "4")
        ]);
        assert_eq!(tfs("; A Comment\n2"), vec![
            tk!(Comment, 0, 11, "; A Comment", "; A Comment"),
            tk!(Newline, 11, 12, "\n", "\n"),
            tk!(NumberLiteral, 12, 13, "2", "2"),
        ]);
    }

    #[test]
    fn test_group() {
        assert_eq!(tfs("``"), vec![
            IncludeToken::TokenGroup(itk!(0, 1, "`", "`"), Vec::new())
        ]);
        assert_eq!(tfs("`a\n'Text'\n4`"), vec![
            IncludeToken::TokenGroup(itk!(0, 1, "`", "`"), vec![
                tk!(Name, 1, 2, "a", "a"),
                tk!(Newline, 2, 3, "\n", "\n"),
                tk!(StringLiteral, 3, 9, "'Text'", "Text"),
                tk!(Newline, 9, 10, "\n", "\n"),
                tk!(NumberLiteral, 10, 11, "4", "4")
            ])
        ]);
        assert_eq!(tfs("`\n\n`"), vec![
            IncludeToken::TokenGroup(itk!(0, 1, "`", "`"), vec![
                tk!(Newline, 1, 2, "\n", "\n"),
                tk!(Newline, 2, 3, "\n", "\n"),
            ])
        ]);
    }

    #[test]
    fn test_group_unclosed() {
        assert_eq!(tfe("`a").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 3: Unclosed token group.\n\n`a\n  ^--- Here");
        assert_eq!(tfe("`a``a").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 6: Unclosed token group.\n\n`a``a\n     ^--- Here");
        assert_eq!(tfe("```").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 4: Unclosed token group.\n\n```\n   ^--- Here");
    }

    #[test]
    fn test_error_location() {
        assert_eq!(tfe(" $").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 2: Unexpected character \"$\".\n\n $\n ^--- Here");
        assert_eq!(tfe("\n$").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 2, column 1: Unexpected character \"$\".\n\n$\n^--- Here");
        assert_eq!(tfe("\n\n$").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 3, column 1: Unexpected character \"$\".\n\n$\n^--- Here");
    }

}

