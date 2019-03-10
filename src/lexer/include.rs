// STD Dependencies -----------------------------------------------------------
use std::error::Error;
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use crate::traits::FileReader;
use super::token::{TokenGenerator, TokenChar};
use super::{InnerToken, LexerError, LexerFile, LexerToken, TokenType};


// Include Specific Tokens ----------------------------------------------------
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IncludeToken {
    Newline(InnerToken),
    Name(InnerToken),
    Reserved(InnerToken),
    Instruction(InnerToken),
    Parameter(InnerToken),
    Offset(InnerToken),
    NumberLiteral(InnerToken),
    StringLiteral(InnerToken),
    TokenGroup(InnerToken, Vec<IncludeToken>),
    BinaryFile(InnerToken, Vec<u8>),
    BuiltinCall(InnerToken, Vec<Vec<IncludeToken>>),
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
    fn typ(&self) -> TokenType {
        match self {
            IncludeToken::Newline(_) => TokenType::Newline,
            IncludeToken::Name(_) => TokenType::Name,
            IncludeToken::Reserved(_) => TokenType::Reserved,
            IncludeToken::Instruction(_) => TokenType::Instruction,
            IncludeToken::Parameter(_) => TokenType::Parameter,
            IncludeToken::Offset(_) => TokenType::Offset,
            IncludeToken::NumberLiteral(_) => TokenType::NumberLiteral,
            IncludeToken::StringLiteral(_) => TokenType::StringLiteral,
            IncludeToken::TokenGroup(_, _) => TokenType::TokenGroup,
            IncludeToken::BinaryFile(_, _) => TokenType::BinaryFile,
            IncludeToken::BuiltinCall(_, _) => TokenType::BuiltinCall,
            IncludeToken::Comma(_) => TokenType::Comma,
            IncludeToken::Point(_) => TokenType::Point,
            IncludeToken::Colon(_) => TokenType::Colon,
            IncludeToken::Operator(_) => TokenType::Operator,
            IncludeToken::Comment(_) => TokenType::Comment,
            IncludeToken::OpenParen(_) => TokenType::OpenParen,
            IncludeToken::CloseParen(_) => TokenType::CloseParen,
            IncludeToken::OpenBracket(_) => TokenType::OpenBracket,
            IncludeToken::CloseBracket(_) => TokenType::CloseBracket
        }
    }

    fn inner(&self) -> &InnerToken {
        match self {
            IncludeToken::Newline(inner) | IncludeToken::Name(inner) | IncludeToken::Reserved(inner) | IncludeToken::Instruction(inner) |IncludeToken::Parameter(inner) | IncludeToken::Offset(inner) | IncludeToken::NumberLiteral(inner)
            | IncludeToken::StringLiteral(inner) | IncludeToken::TokenGroup(inner, _) | IncludeToken::BinaryFile(inner, _) | IncludeToken::BuiltinCall(inner, _)
            | IncludeToken::Comma(inner) | IncludeToken::Point(inner) | IncludeToken::Colon(inner) | IncludeToken::Operator(inner) | IncludeToken::Comment(inner)
            | IncludeToken::OpenParen(inner) | IncludeToken::CloseParen(inner) | IncludeToken::OpenBracket(inner) | IncludeToken::CloseBracket(inner) => {
                &inner
            }
        }
    }

    fn inner_mut(&mut self) -> &mut InnerToken {
        match self {
            IncludeToken::Newline(inner) | IncludeToken::Name(inner) | IncludeToken::Reserved(inner) | IncludeToken::Instruction(inner) | IncludeToken::Parameter(inner) | IncludeToken::Offset(inner) | IncludeToken::NumberLiteral(inner)
            | IncludeToken::StringLiteral(inner) | IncludeToken::TokenGroup(inner, _) | IncludeToken::BinaryFile(inner, _) | IncludeToken::BuiltinCall(inner, _)
            | IncludeToken::Comma(inner) | IncludeToken::Point(inner) | IncludeToken::Colon(inner) | IncludeToken::Operator(inner) | IncludeToken::Comment(inner)
            | IncludeToken::OpenParen(inner) | IncludeToken::CloseParen(inner) | IncludeToken::OpenBracket(inner) | IncludeToken::CloseBracket(inner) => {
                inner
            }
        }
    }

    fn into_inner(self) -> InnerToken {
        match self {
            IncludeToken::Newline(inner) | IncludeToken::Name(inner) | IncludeToken::Reserved(inner) | IncludeToken::Instruction(inner)| IncludeToken::Parameter(inner) | IncludeToken::Offset(inner) | IncludeToken::NumberLiteral(inner)
            | IncludeToken::StringLiteral(inner) | IncludeToken::TokenGroup(inner, _) | IncludeToken::BinaryFile(inner, _) | IncludeToken::BuiltinCall(inner, _)
            | IncludeToken::Comma(inner) | IncludeToken::Point(inner) | IncludeToken::Colon(inner) | IncludeToken::Operator(inner) | IncludeToken::Comment(inner)
            | IncludeToken::OpenParen(inner) | IncludeToken::CloseParen(inner) | IncludeToken::OpenBracket(inner) | IncludeToken::CloseBracket(inner) => {
                inner
            }
        }
    }

}


// Types ----------------------------------------------------------------------
struct IncludeLexerState<'a, T: FileReader> {
    file_reader: &'a T,
    files: &'a mut Vec<LexerFile>,
    parent_path: Option<&'a PathBuf>,
    child_path: &'a PathBuf
}

impl<'a, T: FileReader> IncludeLexerState<'a, T> {

    fn split_off_child(mut self, parent_path: &'a PathBuf) -> Self {
        self.parent_path = Some(parent_path);
        self
    }

}


// Include Level Lexer Implementation -----------------------------------------
pub struct IncludeLexer {
    pub files: Vec<LexerFile>,
    pub tokens: Vec<IncludeToken>
}

impl IncludeLexer {

    pub fn from_file<T: FileReader>(file_reader: &T, child_path: &PathBuf) -> Result<Self, Box<dyn Error>>{

        let mut files = Vec::new();
        let tokens = Self::include_child(IncludeLexerState {
            file_reader,
            files: &mut files,
            parent_path: None,
            child_path

        }, Vec::new(), 0, 0).map_err(|err| LexerFile::error(err, &files))?;

        Ok(Self {
            files,
            tokens
        })
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    fn include_child<T: FileReader>(
        state: IncludeLexerState<T>,
        include_stack: Vec<InnerToken>,
        file_index: usize,
        index: usize

    ) -> Result<Vec<IncludeToken>, LexerError>{

        // Read in child file contents
        let (child_path, contents) = state.file_reader.read_file(state.parent_path, state.child_path).map_err(|err| {
            LexerError {
                file_index,
                index,
                message: format!("File \"{}\" not found", err.path.display())
            }
        })?;

        // Create new file abstraction
        state.files.push(LexerFile::new(state.files.len(), contents, child_path.clone(), include_stack));

        // Create new lexer state for child file
        let mut state = state.split_off_child(&child_path);
        let file = state.files.last().unwrap();

        // Lex child tokens
        let child_tokens = Self::tokenize(file, &file.contents)?;

        // Resolve any includes in the tokenized file
        Ok(Self::resolve_include_tokens(
            file.index,
            child_tokens,
            &mut state,

        )?)

    }

    fn resolve_include_tokens<T: FileReader>(
        parent_file_index: usize,
        tokens: Vec<IncludeToken>,
        state: &mut IncludeLexerState<T>,

    ) -> Result<Vec<IncludeToken>, LexerError> {

        let mut expanded = Vec::new();

        let mut tokens = tokens.into_iter();
        while let Some(token) = tokens.next() {
            if let IncludeToken::Reserved(ref name) = token {
                if name.value == "INCLUDE" {
                    match tokens.next() {
                        Some(IncludeToken::StringLiteral(token)) => {
                            let mut include_stack = state.files[parent_file_index].include_stack.clone();
                            include_stack.push(token.clone());

                            let child_state = IncludeLexerState {
                                file_reader: state.file_reader,
                                files: state.files,
                                parent_path: state.parent_path,
                                child_path: &PathBuf::from(token.value.clone()),
                            };

                            expanded.append(&mut Self::include_directive(
                                child_state,
                                parent_file_index,
                                token.start_index,
                                include_stack
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
                                child_path: &PathBuf::from(token.value.clone())
                            };
                            expanded.push(Self::incbin_directive(
                                child_state,
                                token
                            )?);
                        },
                        Some(other) => return Err(other.error("Expected a StringLiteral instead.".to_string())),
                        None => return Err(token.error("Expected a StringLiteral to follow.".to_string()))
                    }

                } else {
                    expanded.push(token);
                }

            } else {
                expanded.push(token);
            }
        }

        Ok(expanded)

    }

    fn include_directive<T: FileReader>(
        state: IncludeLexerState<T>,
        file_index: usize,
        index: usize,
        include_stack: Vec<InnerToken>

    ) -> Result<Vec<IncludeToken>, LexerError> {
        Self::include_child(state, include_stack, file_index, index)
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
        let mut iter = TokenGenerator::new(&file, text);
        Self::collect_tokens(&mut iter, false)
    }

    fn collect_tokens(iter: &mut TokenGenerator, inside_token_group: bool) -> Result<Vec<IncludeToken>, LexerError> {
        let mut tokens = Vec::new();
        while iter.peek().is_some() {
            let token = match iter.next() {
                // Whitespace
                ' ' | '\t' => continue, // Ignore whitespace,
                '\n' | '\r' => Some(IncludeToken::Newline(iter.collect_single())),
                // Names
                'a'...'z' | 'A'...'Z' | '_' => {
                    let name = Self::collect_inner_name(iter, true)?;
                    match name.value.as_str() {
                        "DB" | "DW" | "BW" | "DS" |
                        "DS8" | "EQU" |
                        "DS16" | "EQUS" | "BANK" |
                        "MACRO" |
                        "INCBIN" | "SECTION" | "INCLUDE" |
                        "ENDMACRO" => {
                            Some(IncludeToken::Reserved(name))
                        },
                        "cp" | "di" | "ei" | "jp" | "jr" | "or" | "rl" | "rr" | "ld" |
                        "adc" | "add" | "and" | "bit" | "ccf" | "cpl" | "daa" | "dec" | "inc" | "ldh" | "nop" | "pop" | "res" | "ret" | "rla" | "rlc" | "rra" | "rrc" | "rst" | "sbc" | "scf" | "set" | "sla" | "sra" | "srl" | "sub" | "xor" | "msg" | "brk" | "mul" | "div" |
                        "incx" | "decx" | "addw" | "subw" | "ldxa" | "halt" | "push" | "call" | "reti" | "ldhl" | "rlca" | "rrca" | "stop" | "retx" | "swap" |
                        "vsync" => {
                            Some(IncludeToken::Instruction(name))
                        },
                        _ => Some(IncludeToken::Name(name))
                    }

                },
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

    fn collect_inner_string(iter: &mut TokenGenerator, delimeter: char) -> Result<InnerToken, LexerError> {
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

    fn collect_inner_name(iter: &mut TokenGenerator, inclusive: bool) -> Result<InnerToken, LexerError> {
        Ok(iter.collect(inclusive, |c, _| {
            if let 'a'...'z' | 'A'...'Z' | '_' | '0'...'9' = c {
                TokenChar::Valid(c)

            } else {
                TokenChar::Invalid
            }
        })?)
    }

    fn collect_number_literal(iter: &mut TokenGenerator) -> Result<IncludeToken, LexerError> {
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
    use super::{IncludeLexer, IncludeToken, InnerToken};
    use crate::lexer::mocks::{MockFileReader, tfs, tfe};

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

    macro_rules! token_types {
        ($tok:ident, $id:expr) => {
            assert_eq!(tfs($id), vec![tk!($tok, 0, $id.len(), $id, $id)]);
        };
        ($tok:ident, $id:expr, $($rest:expr), +) => {
            token_types!($tok, $id);
            token_types!($tok, $($rest),+)
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

        let lexer = IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
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

        let lexer = IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
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

        let err = IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
        assert_eq!(err.to_string(), "In file \"src/main.gb.s\" on line 2, column 9: File \"src/one.gb.s\" not found\n\nINCLUDE \'one.gb.s\'\n        ^--- Here");

    }

    #[test]
    fn test_resolve_nested_include_lexer_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "1\nINCLUDE 'one.gb.s'");
        reader.add_file("src/one.gb.s", "2\nINCLUDE 'extra/two.gb.s'\n4");
        reader.add_file("src/extra/two.gb.s", "INCLUDE '/three.gb.s'");
        reader.add_file("src/three.gb.s", "@");

        let err = IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
        assert_eq!(err.to_string(), "In file \"src/three.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here\n\nincluded from file \"src/extra/two.gb.s\" on line 1, column 9\nincluded from file \"src/one.gb.s\" on line 2, column 9\nincluded from file \"src/main.gb.s\" on line 2, column 9");

    }

    #[test]
    fn test_resolve_include_lexer_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "1\nINCLUDE 'one.gb.s'");
        reader.add_file("src/one.gb.s", "@");

        let err = IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
        assert_eq!(err.to_string(), "In file \"src/one.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here\n\nincluded from file \"src/main.gb.s\" on line 2, column 9");

    }

    #[test]
    fn test_resolve_include_incomplete() {
        assert_eq!(tfe("INCLUDE 4").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 9: Expected a StringLiteral instead.\n\nINCLUDE 4\n        ^--- Here");
        assert_eq!(tfe("INCLUDE").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 1: Expected a StringLiteral to follow.\n\nINCLUDE\n^--- Here");
    }

    #[test]
    fn test_resolve_incbins() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "INCBIN 'data.bin'\nINCBIN 'second.bin'");
        reader.add_binary_file("src/data.bin", vec![0, 1, 2, 3, 4, 5, 6, 7]);
        reader.add_binary_file("src/second.bin", vec![42]);

        let lexer = IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
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
        let err = IncludeLexer::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
        assert_eq!(err.to_string(), "In file \"src/main.gb.s\" on line 1, column 8: File \"src/data.bin\" not found\n\nINCBIN \'data.bin\'\n       ^--- Here");

    }

    #[test]
    fn test_resolve_incbin_incomplete() {
        assert_eq!(tfe("INCBIN 4").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 8: Expected a StringLiteral instead.\n\nINCBIN 4\n       ^--- Here");
        assert_eq!(tfe("INCBIN").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 1: Expected a StringLiteral to follow.\n\nINCBIN\n^--- Here");
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
    fn test_reserved() {
        token_types!(Reserved, "DB", "DW", "BW", "DS8", "DS16", "EQU", "EQUS", "BANK", "MACRO", "SECTION", "ENDMACRO");
    }

    #[test]
    fn test_instructions() {
        token_types!(Instruction, "cp", "di", "ei", "jp", "jr", "or", "rl", "rr", "ld");
        token_types!(Instruction, "adc", "add", "and", "bit", "ccf", "cpl", "daa", "dec", "inc", "ldh", "nop", "pop", "res", "ret", "rla", "rlc", "rra", "rrc", "rst", "sbc", "scf", "set", "sla", "sra", "srl", "sub", "xor", "msg", "brk", "mul", "div");
        token_types!(Instruction, "incx", "decx", "addw", "subw", "ldxa", "halt", "push", "call", "reti", "ldhl", "rlca", "rrca", "stop", "retx", "swap");
        token_types!(Instruction, "vsync");
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
        assert_eq!(tfe("$").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 1: Unexpected character \"$\".\n\n$\n^--- Here");
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
        assert_eq!(tfe("'Hello World").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 13: Unclosed string literal.\n\n'Hello World\n            ^--- Here");
        assert_eq!(tfe("\"Hello World").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 13: Unclosed string literal.\n\n\"Hello World\n            ^--- Here");
        assert_eq!(tfe("'''").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 4: Unclosed string literal.\n\n\'\'\'\n   ^--- Here");
        assert_eq!(tfe("\"\"\"").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 4: Unclosed string literal.\n\n\"\"\"\n   ^--- Here");
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
        assert_eq!(tfe("@").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here");
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
        assert_eq!(tfe("`a").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 3: Unclosed token group.\n\n`a\n  ^--- Here");
        assert_eq!(tfe("`a``a").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 6: Unclosed token group.\n\n`a``a\n     ^--- Here");
        assert_eq!(tfe("```").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 4: Unclosed token group.\n\n```\n   ^--- Here");
    }

    #[test]
    fn test_error_location() {
        assert_eq!(tfe(" $").unwrap_err().to_string(), "In file \"main.gb.s\" on line 1, column 2: Unexpected character \"$\".\n\n $\n ^--- Here");
        assert_eq!(tfe("\n$").unwrap_err().to_string(), "In file \"main.gb.s\" on line 2, column 1: Unexpected character \"$\".\n\n$\n^--- Here");
        assert_eq!(tfe("\n\n$").unwrap_err().to_string(), "In file \"main.gb.s\" on line 3, column 1: Unexpected character \"$\".\n\n$\n^--- Here");
    }

}

