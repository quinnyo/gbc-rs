// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use crate::traits::FileReader;
use super::super::LexerStage;
use super::super::token::{TokenGenerator, TokenChar};
use super::super::{InnerToken, LexerError, LexerFile, LexerToken, TokenType};


// Include Specific Tokens ----------------------------------------------------
lexer_token!(IncludeToken, (Debug, Eq, PartialEq, Clone), {
    Newline(()),
    Name(()),
    Register(()),
    Flag(()),
    Reserved(()),
    Segment(()),
    Instruction(()),
    MetaInstruction(()),
    Parameter(()),
    Offset(()),
    NumberLiteral(()),
    StringLiteral(()),
    TokenGroup((Vec<IncludeToken>)),
    BinaryFile((Vec<u8>)),
    BuiltinCall((Vec<Vec<IncludeToken>>)),
    Comma(()),
    Point(()),
    Colon(()),
    Operator(()),
    Comment(()),
    OpenParen(()),
    CloseParen(()),
    OpenBracket(()),
    CloseBracket(())
});


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
pub struct IncludeStage;
impl LexerStage for IncludeStage {

    type Input = Self;
    type Output = IncludeToken;
    type Data = ();

    fn from_file<R: FileReader>(
        file_reader: &R,
        child_path: &PathBuf,
        files: &mut Vec<LexerFile>

    ) -> Result<Vec<Self::Output>, LexerError> {
        Self::include_child(IncludeLexerState {
            file_reader,
            files,
            parent_path: None,
            child_path

        }, Vec::new(), 0, 0)
    }

}

impl IncludeStage {

    fn include_child<T: FileReader>(
        state: IncludeLexerState<T>,
        include_stack: Vec<InnerToken>,
        file_index: usize,
        index: usize

    ) -> Result<Vec<IncludeToken>, LexerError>{

        // Read in child file contents
        let (child_path, contents) = state.file_reader.read_file(state.parent_path, state.child_path).map_err(|err| {
            LexerError::new(file_index, index, format!("File \"{}\" not found", err.path.display()))
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
            LexerError::new(token.file_index, token.start_index, format!("File \"{}\" not found", err.path.display()))
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
                        // Split into Reserved Words
                        "DB" | "DW" | "BW" | "DS" |
                        "DS8" | "EQU" |
                        "DS16" | "EQUS" | "BANK" |
                        "MACRO" |
                        "INCBIN" | "SECTION" | "INCLUDE" | "SEGMENT" |
                        "ENDMACRO" => {
                            Some(IncludeToken::Reserved(name))
                        },
                        // ROM Segments
                        "ROM0" | "ROMX" | "WRAM0" | "WRAMX" | "HRAM" | "RAM" | "RAMX" => {
                            Some(IncludeToken::Segment(name))
                        },
                        // Registers
                        "af" | "bc" | "de" | "hl" | "a" | "b" | "c" | "d" | "e" | "h" | "l" | "hld" | "hli" | "sp" => {
                            Some(IncludeToken::Register(name))
                        },
                        // Flags
                        "z" | "nz" | "nc" => {
                            Some(IncludeToken::Flag(name))
                        },
                        // LR35902 Instructions
                        "cp" | "di" | "ei" | "jp" | "jr" | "or" | "rl" | "rr" | "ld" |
                        "adc" | "add" | "and" | "bit" | "ccf" | "cpl" | "daa" | "dec" | "inc" |
                        "ldh" | "nop" | "pop" | "res" | "ret" | "rla" | "rlc" | "rra" | "rrc" |
                        "rst" | "sbc" | "scf" | "set" | "sla" | "sra" | "srl" | "sub" | "xor" |
                        "halt" | "push" | "call" | "reti" | "ldhl" | "rlca" | "rrca" | "stop" | "swap" | "ldsp" => {
                            Some(IncludeToken::Instruction(name))
                        },
                        // gbasm "meta" Instructions
                        "msg" | "brk" | "mul" | "div" | "incx" | "decx" | "addw" | "subw" | "ldxa" | "retx" | "vsync" => {
                            Some(IncludeToken::MetaInstruction(name))
                        },
                        // All other names
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
                '!' | '&' | '*' | '/' | '=' | '|' | '+' | '~' | '<' | '>' | '^' => {
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
        let mut float = false;
        Ok(IncludeToken::NumberLiteral(iter.collect(true, |c, _| {
            if let '_' = c {
                TokenChar::Ignore

            } else if let '.' = c {
                if float {
                    TokenChar::Invalid
                } else {
                    float = true;
                    TokenChar::Valid(c)
                }

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
    use crate::lexer::Lexer;
    use crate::lexer::stage::mocks::{MockFileReader, include_lex};
    use super::{IncludeStage, IncludeToken, InnerToken};

    fn include_lexer<S: Into<String>>(s: S) -> Vec<IncludeToken> {
        include_lex(s).tokens
    }

    fn include_lexer_error<S: Into<String>>(s: S) -> String {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap().to_string()
    }

    macro_rules! tk {
        ($tok:ident, $start:expr, $end:expr, $parsed:expr) => {
            IncludeToken::$tok(InnerToken::new(0, $start, $end, $parsed.into()))
        }
    }

    macro_rules! tkf {
        ($file:expr, $tok:ident, $start:expr, $end:expr, $parsed:expr) => {
            IncludeToken::$tok(InnerToken::new($file, $start, $end, $parsed.into()))
        }
    }

    macro_rules! itk {
        ($start:expr, $end:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $parsed.into())
        }
    }

    macro_rules! token_types {
        ($tok:ident, $id:expr) => {
            assert_eq!(include_lexer($id), vec![tk!($tok, 0, $id.len(), $id)]);
        };
        ($tok:ident, $id:expr, $($rest:expr), +) => {
            token_types!($tok, $id);
            token_types!($tok, $($rest),+)
        }
    }

    #[test]
    fn test_empty() {
        assert_eq!(include_lexer(""), vec![]);
    }

    #[test]
    fn test_resolve_includes() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "INCLUDE 'foo.gb.s'\nINCLUDE 'extra/bar.gb.s'\nINCLUDE '/abs.gb.s'");
        reader.add_file("src/foo.gb.s", "42");
        reader.add_file("src/extra/bar.gb.s", "BAR");
        reader.add_file("src/abs.gb.s", "ABS");

        let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            tkf!(1, NumberLiteral, 0, 2, "42"),
            tkf!(0, Newline, 18, 19, "\n"),
            tkf!(2, Name, 0, 3, "BAR"),
            tkf!(0, Newline, 43, 44, "\n"),
            tkf!(3, Name, 0, 3, "ABS"),
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

        let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            tkf!(0, NumberLiteral, 0, 1, "1"),
            tkf!(0, Newline, 1, 2, "\n"),
            tkf!(1, NumberLiteral, 0, 1, "2"),
            tkf!(1, Newline, 1, 2, "\n"),
            tkf!(3, NumberLiteral, 0, 1, "3"),
            tkf!(1, Newline, 26, 27, "\n"),
            tkf!(1, NumberLiteral, 27, 28, "4"),
        ]);

    }

    #[test]
    fn test_resolve_nested_include_io_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "1\nINCLUDE 'one.gb.s'");

        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
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

        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
        assert_eq!(err.to_string(), "In file \"src/three.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here\n\nincluded from file \"src/extra/two.gb.s\" on line 1, column 9\nincluded from file \"src/one.gb.s\" on line 2, column 9\nincluded from file \"src/main.gb.s\" on line 2, column 9");

    }

    #[test]
    fn test_resolve_include_lexer_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "1\nINCLUDE 'one.gb.s'");
        reader.add_file("src/one.gb.s", "@");

        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
        assert_eq!(err.to_string(), "In file \"src/one.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here\n\nincluded from file \"src/main.gb.s\" on line 2, column 9");

    }

    #[test]
    fn test_resolve_include_incomplete() {
        assert_eq!(include_lexer_error("INCLUDE 4"), "In file \"main.gb.s\" on line 1, column 9: Expected a StringLiteral instead.\n\nINCLUDE 4\n        ^--- Here");
        assert_eq!(include_lexer_error("INCLUDE"), "In file \"main.gb.s\" on line 1, column 1: Expected a StringLiteral to follow.\n\nINCLUDE\n^--- Here");
    }

    #[test]
    fn test_resolve_incbins() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "INCBIN 'data.bin'\nINCBIN 'second.bin'");
        reader.add_binary_file("src/data.bin", vec![0, 1, 2, 3, 4, 5, 6, 7]);
        reader.add_binary_file("src/second.bin", vec![42]);

        let lexer = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.tokens, vec![
            IncludeToken::BinaryFile(itk!(7, 17, "data.bin"), vec![0, 1, 2, 3, 4, 5, 6, 7]),
            tkf!(0, Newline, 17, 18, "\n"),
            IncludeToken::BinaryFile(itk!(25, 37, "second.bin"), vec![42])
        ]);

    }

    #[test]
    fn test_resolve_nested_incbins_io_error() {

        let mut reader = MockFileReader::default();
        reader.base = PathBuf::from("src");
        reader.add_file("src/main.gb.s", "INCBIN 'data.bin'");
        let err = Lexer::<IncludeStage>::from_file(&reader, &PathBuf::from("main.gb.s")).err().unwrap();
        assert_eq!(err.to_string(), "In file \"src/main.gb.s\" on line 1, column 8: File \"src/data.bin\" not found\n\nINCBIN \'data.bin\'\n       ^--- Here");

    }

    #[test]
    fn test_resolve_incbin_incomplete() {
        assert_eq!(include_lexer_error("INCBIN 4"), "In file \"main.gb.s\" on line 1, column 8: Expected a StringLiteral instead.\n\nINCBIN 4\n       ^--- Here");
        assert_eq!(include_lexer_error("INCBIN"), "In file \"main.gb.s\" on line 1, column 1: Expected a StringLiteral to follow.\n\nINCBIN\n^--- Here");
    }

    #[test]
    fn test_newlinews() {
        assert_eq!(include_lexer("\n\r\n\n\r"), vec![
            tk!(Newline, 0, 1, "\n"),
            tk!(Newline, 1, 2, "\r"),
            tk!(Newline, 2, 3, "\n"),
            tk!(Newline, 3, 4, "\n"),
            tk!(Newline, 4, 5, "\r"),
        ]);
    }

    #[test]
    fn test_name() {
        assert_eq!(include_lexer("INCLUDEX"), vec![tk!(Name, 0, 8, "INCLUDEX")]);
        assert_eq!(include_lexer("ol"), vec![tk!(Name, 0, 2, "ol")]);
        assert_eq!(include_lexer("q"), vec![tk!(Name, 0, 1, "q")]);
        assert_eq!(include_lexer("foo_bar"), vec![tk!(Name, 0, 7, "foo_bar")]);
        assert_eq!(include_lexer("_test"), vec![tk!(Name, 0, 5, "_test")]);
        assert_eq!(include_lexer("abcdefghijklmnopqrstuvwxyz"), vec![tk!(Name, 0, 26, "abcdefghijklmnopqrstuvwxyz")]);
        assert_eq!(include_lexer("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), vec![tk!(Name, 0, 26, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")]);
    }

    #[test]
    fn test_reserved() {
        token_types!(Reserved, "DB", "DW", "BW", "DS8", "DS16", "EQU", "EQUS", "BANK", "MACRO", "SECTION", "ENDMACRO", "SEGMENT");
    }

    #[test]
    fn test_segment() {
        token_types!(Segment, "ROM0", "ROMX", "WRAM0", "WRAMX", "HRAM", "RAM", "RAMX");
    }

    #[test]
    fn test_registers() {
        token_types!(Register, "af", "bc", "de", "hl", "a", "b", "c", "d", "e", "h", "l", "hli", "hld", "sp");
    }

    #[test]
    fn test_flags() {
        token_types!(Flag, "nc", "z", "nz");
    }

    #[test]
    fn test_instructions() {
        token_types!(Instruction, "cp", "di", "ei", "jp", "jr", "or", "rl", "rr", "ld");
        token_types!(Instruction, "adc", "add", "and", "bit", "ccf", "cpl", "daa", "dec", "inc", "ldh", "nop", "pop", "res", "ret", "rla", "rlc", "rra", "rrc", "rst", "sbc", "scf", "set", "sla", "sra", "srl", "sub", "xor");
        token_types!(Instruction, "halt", "push", "call", "reti", "ldhl", "rlca", "rrca", "stop", "swap", "ldsp");
    }

    #[test]
    fn test_meta_instructions() {
        token_types!(MetaInstruction, "msg", "brk", "mul", "div", "retx", "incx", "decx", "addw", "subw", "ldxa", "vsync");
    }

    #[test]
    fn test_number_literal() {
        assert_eq!(include_lexer("20_48"), vec![
            tk!(NumberLiteral, 0, 5, "2048")
        ]);
        assert_eq!(include_lexer("-512"), vec![
            tk!(NumberLiteral, 0, 4, "-512")
        ]);
        assert_eq!(include_lexer("$1234"), vec![
            tk!(NumberLiteral, 0, 5, "$1234")
        ]);
        assert_eq!(include_lexer("$1234"), vec![
            tk!(NumberLiteral, 0, 5, "$1234")
        ]);
        assert_eq!(include_lexer("$abcdefABCDEF"), vec![
            tk!(NumberLiteral, 0, 13, "$abcdefABCDEF")
        ]);
        assert_eq!(include_lexer("%1001_0020"), vec![
            tk!(NumberLiteral, 0, 8, "%100100"),
            tk!(NumberLiteral, 8, 10, "20")
        ]);
        assert_eq!(include_lexer("2.4"), vec![
            tk!(NumberLiteral, 0, 3, "2.4")
        ]);
        assert_eq!(include_lexer("2.4."), vec![
            tk!(NumberLiteral, 0, 3, "2.4"),
            tk!(Point, 3, 4, ".")
        ]);
        assert_eq!(include_lexer("2.4.1"), vec![
            tk!(NumberLiteral, 0, 3, "2.4"),
            tk!(Point, 3, 4, "."),
            tk!(NumberLiteral, 4, 5, "1"),
        ]);
    }

    #[test]
    fn test_number_literal_hex_incomplete() {
        assert_eq!(include_lexer_error("$"), "In file \"main.gb.s\" on line 1, column 1: Unexpected character \"$\".\n\n$\n^--- Here");
    }

    #[test]
    fn test_string_literal() {
        assert_eq!(include_lexer("'Hello World'"), vec![
            tk!(StringLiteral, 0, 13, "Hello World")
        ]);
        assert_eq!(include_lexer("\"Hello World\""), vec![
            tk!(StringLiteral, 0, 13, "Hello World")
        ]);
        assert_eq!(include_lexer("'\"'"), vec![
            tk!(StringLiteral, 0, 3, "\"")
        ]);
        assert_eq!(include_lexer("\"'\""), vec![
            tk!(StringLiteral, 0, 3, "'")
        ]);
    }

    #[test]
    fn test_string_literal_unclosed() {
        assert_eq!(include_lexer_error("'Hello World"), "In file \"main.gb.s\" on line 1, column 13: Unclosed string literal.\n\n'Hello World\n            ^--- Here");
        assert_eq!(include_lexer_error("\"Hello World"), "In file \"main.gb.s\" on line 1, column 13: Unclosed string literal.\n\n\"Hello World\n            ^--- Here");
        assert_eq!(include_lexer_error("'''"), "In file \"main.gb.s\" on line 1, column 4: Unclosed string literal.\n\n\'\'\'\n   ^--- Here");
        assert_eq!(include_lexer_error("\"\"\""), "In file \"main.gb.s\" on line 1, column 4: Unclosed string literal.\n\n\"\"\"\n   ^--- Here");
    }

    #[test]
    fn test_string_literal_escapes() {
        assert_eq!(include_lexer("'\\n'"), vec![
            tk!(StringLiteral, 0, 4, "\n")
        ]);
        assert_eq!(include_lexer("'\\r'"), vec![
            tk!(StringLiteral, 0, 4, "\r")
        ]);
        assert_eq!(include_lexer("'\\t'"), vec![
            tk!(StringLiteral, 0, 4, "\t")
        ]);
        assert_eq!(include_lexer("'\\''"), vec![
            tk!(StringLiteral, 0, 4, "'")
        ]);
        assert_eq!(include_lexer("\"\\\"\""), vec![
            tk!(StringLiteral, 0, 4, "\"")
        ]);
    }

    #[test]
    fn test_string_punct() {
        assert_eq!(include_lexer(","), vec![tk!(Comma, 0, 1, ",")]);
        assert_eq!(include_lexer("."), vec![tk!(Point, 0, 1, ".")]);
        assert_eq!(include_lexer(":"), vec![tk!(Colon, 0, 1, ":")]);
        assert_eq!(include_lexer("()"), vec![
            tk!(OpenParen, 0, 1, "("),
            tk!(CloseParen, 1, 2, ")")
        ]);
        assert_eq!(include_lexer("[]"), vec![
            tk!(OpenBracket, 0, 1, "["),
            tk!(CloseBracket, 1, 2, "]")
        ]);
        assert_eq!(include_lexer("=%&|!-+<>~*/^"), vec![
            tk!(Operator, 0, 1, "="),
            tk!(Operator, 1, 2, "%"),
            tk!(Operator, 2, 3, "&"),
            tk!(Operator, 3, 4, "|"),
            tk!(Operator, 4, 5, "!"),
            tk!(Operator, 5, 6, "-"),
            tk!(Operator, 6, 7, "+"),
            tk!(Operator, 7, 8, "<"),
            tk!(Operator, 8, 9, ">"),
            tk!(Operator, 9, 10, "~"),
            tk!(Operator, 10, 11, "*"),
            tk!(Operator, 11, 12, "/"),
            tk!(Operator, 12, 13, "^"),
        ]);
    }

    #[test]
    fn test_param_offset() {
        assert_eq!(include_lexer("@param"), vec![tk!(Parameter, 0, 6, "param")]);
        assert_eq!(include_lexer("@param_foo"), vec![tk!(Parameter, 0, 10, "param_foo")]);
        assert_eq!(include_lexer("@+4"), vec![tk!(Offset, 0, 3, "+4")]);
        assert_eq!(include_lexer("@-4"), vec![tk!(Offset, 0, 3, "-4")]);
    }

    #[test]
    fn test_param_offset_incomplete() {
        assert_eq!(include_lexer_error("@"), "In file \"main.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here");
    }

    #[test]
    fn test_comments() {
        assert_eq!(include_lexer("2 ; A Comment"), vec![
            tk!(NumberLiteral, 0, 1, "2"),
            tk!(Comment, 2, 13, "; A Comment")
        ]);
    }

    #[test]
    fn test_multiline() {
        assert_eq!(include_lexer("q\n'Text'\n4"), vec![
            tk!(Name, 0, 1, "q"),
            tk!(Newline, 1, 2, "\n"),
            tk!(StringLiteral, 2, 8, "Text"),
            tk!(Newline, 8, 9, "\n"),
            tk!(NumberLiteral, 9, 10, "4")
        ]);
        assert_eq!(include_lexer("; A Comment\n2"), vec![
            tk!(Comment, 0, 11, "; A Comment"),
            tk!(Newline, 11, 12, "\n"),
            tk!(NumberLiteral, 12, 13, "2"),
        ]);
    }

    #[test]
    fn test_group() {
        assert_eq!(include_lexer("``"), vec![
            IncludeToken::TokenGroup(itk!(0, 1, "`"), Vec::new())
        ]);
        assert_eq!(include_lexer("`q\n'Text'\n4`"), vec![
            IncludeToken::TokenGroup(itk!(0, 1, "`"), vec![
                tk!(Name, 1, 2, "q"),
                tk!(Newline, 2, 3, "\n"),
                tk!(StringLiteral, 3, 9, "Text"),
                tk!(Newline, 9, 10, "\n"),
                tk!(NumberLiteral, 10, 11, "4")
            ])
        ]);
        assert_eq!(include_lexer("`\n\n`"), vec![
            IncludeToken::TokenGroup(itk!(0, 1, "`"), vec![
                tk!(Newline, 1, 2, "\n"),
                tk!(Newline, 2, 3, "\n"),
            ])
        ]);
    }

    #[test]
    fn test_group_unclosed() {
        assert_eq!(include_lexer_error("`a"), "In file \"main.gb.s\" on line 1, column 3: Unclosed token group.\n\n`a\n  ^--- Here");
        assert_eq!(include_lexer_error("`a``a"), "In file \"main.gb.s\" on line 1, column 6: Unclosed token group.\n\n`a``a\n     ^--- Here");
        assert_eq!(include_lexer_error("```"), "In file \"main.gb.s\" on line 1, column 4: Unclosed token group.\n\n```\n   ^--- Here");
    }

    #[test]
    fn test_error_location() {
        assert_eq!(include_lexer_error(" $"), "In file \"main.gb.s\" on line 1, column 2: Unexpected character \"$\".\n\n $\n ^--- Here");
        assert_eq!(include_lexer_error("\n$"), "In file \"main.gb.s\" on line 2, column 1: Unexpected character \"$\".\n\n$\n^--- Here");
        assert_eq!(include_lexer_error("\n\n$"), "In file \"main.gb.s\" on line 3, column 1: Unexpected character \"$\".\n\n$\n^--- Here");
    }

}

