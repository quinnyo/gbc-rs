// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::error::Error;
use std::path::PathBuf;


// Modules --------------------------------------------------------------------
mod token;


// Internal Dependencies ------------------------------------------------------
use crate::traits::FileReader;
use self::token::{TokenIterator, TokenChar};


// Lexer Tokens ---------------------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub struct InnerToken {
    file_index: usize,
    // Only used for error locations so we can trace back to the source code in macro expansions
    start_index: usize,
    // Only used for error locations so we can trace back to the source code in macro expansions
    end_index: usize,
    raw_value: String,
    value: String
}

#[derive(Debug, Eq, PartialEq)]
pub enum LexerToken {
    Newline(InnerToken),
    Name(InnerToken),
    Parameter(InnerToken),
    Offset(InnerToken),
    NumberLiteral(InnerToken),
    StringLiteral(InnerToken),
    TokenGroup(Vec<LexerToken>),
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

// Lexer File Abstraction -----------------------------------------------------
pub struct LexerFile {
    index: usize,
    path: PathBuf,
    prefix: String,
    contents: String
}

impl LexerFile {

    fn new(index: usize, contents: String, path: PathBuf) -> Self {
        Self {
            index,
            path,
            prefix: format!("F{}#", index),
            contents
        }
    }

    fn get_line_and_col(&self, index: usize) -> (usize, usize) {
        let (mut line, mut col) = (0, 0);
        for (i, c) in self.contents.chars().enumerate() {
            if i == index {
                break;

            } else if c == '\n' || c == '\r' {
                line += 1;
                col = 0;

            } else {
                col += 1;
            }
        }
        (line, col)
    }

    fn error(&self, err: LexerError) -> LexerError {
        let (line, col) = self.get_line_and_col(err.index);
        LexerError {
            file_index: err.file_index,
            index: err.index,
            message: format!(
                "In file \"{}\" on line {}, column {}: {}",
                self.path.display(),
                line + 1,
                col + 1,
                err.message
            )
        }
    }

}


// Lexer Error Abstraction -----------------------------------------------------
#[derive(Debug)]
pub struct LexerError {
    file_index: usize,
    index: usize,
    message: String
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LexerError: {}", self.message)
    }
}

impl Error for LexerError {}


// Low Level Lexer Implementation ---------------------------------------------
pub struct Lexer {
    files: Vec<LexerFile>,
    tokens: Vec<LexerToken>
}

impl Lexer {

    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            tokens: Vec::new()
        }
    }

    pub fn lex_file<T: FileReader>(&mut self, file_reader: &T, child_path: &PathBuf) -> Result<usize, Box<dyn Error>>{
        self.lex_file_child(file_reader, None, child_path, 0)
        // TODO resolve includes directly in lexer
        // TODO show include paths in error
        // TODO expand macros in the initial parser
    }

    fn lex_file_child<T: FileReader>(&mut self, file_reader: &T, parent_path: Option<&PathBuf>, child_path: &PathBuf, insert_at: usize) -> Result<usize, Box<dyn Error>>{
        let (full_path, contents) = file_reader.read_file(parent_path, child_path)?;
        let file = LexerFile::new(self.files.len(), contents, full_path);
        let mut insert_tokens = Lexer::tokenize(&file, &file.contents).map_err(|err| {
            file.error(err)
        })?;
        let mut after_tokens = self.tokens.split_off(insert_at);
        self.tokens.append(&mut insert_tokens);
        self.tokens.append(&mut after_tokens);
        self.files.push(file);
        Ok(self.tokens.len())
    }

    fn tokenize(file: &LexerFile, text: &str) -> Result<Vec<LexerToken>, LexerError> {
        let mut iter = TokenIterator::new(&file, text);
        Self::collect_tokens(&mut iter, false)
    }

    fn collect_tokens(iter: &mut TokenIterator, inside_token_group: bool) -> Result<Vec<LexerToken>, LexerError> {
        let mut tokens = Vec::new();
        while iter.peek().is_some() {
            let token = match iter.next() {
                // Whitespace
                ' ' | '\t' => continue, // Ignore whitespace,
                '\n' | '\r' => Some(LexerToken::Newline(iter.collect_single())),
                // Names
                'a'...'z' | 'A'...'Z' | '_' => Some(LexerToken::Name(Lexer::collect_inner_name(iter, true)?)),
                // Parameter / Offset
                '@' => {
                    if let Some('+') | Some('-') = iter.peek() {
                        Some(LexerToken::Offset(iter.collect(false, |c, _| {
                            if let '_' = c {
                                TokenChar::Ignore

                            } else if let '0'...'9' | '+' | '-' = c {
                                TokenChar::Valid(c)

                            } else {
                                TokenChar::Invalid
                            }
                        })?))

                    } else if let Some('a'...'z') | Some('A'...'Z') = iter.peek() {
                        Some(LexerToken::Parameter(Lexer::collect_inner_name(iter, false)?))

                    } else {
                        None
                    }
                },
                // NumberLiteral
                '0'...'9' => Some(Lexer::collect_number_literal(iter)?),
                '-' => if let Some('0'...'9') = iter.peek() {
                    Some(Lexer::collect_number_literal(iter)?)

                } else {
                    Some(LexerToken::Operator(iter.collect_single()))
                },
                '$' => if let Some('0'...'9') | Some('a'...'f') | Some('A'...'F') = iter.peek() {
                    Some(LexerToken::NumberLiteral(iter.collect(true, |c, _| {
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
                    Some(LexerToken::NumberLiteral(iter.collect(true, |c, _| {
                        if let '_' = c {
                            TokenChar::Ignore

                        } else if let '0'...'1' = c {
                            TokenChar::Valid(c)

                        } else {
                            TokenChar::Invalid
                        }
                    })?))

                } else {
                    Some(LexerToken::Operator(iter.collect_single()))
                },
                // StringLiteral
                '"' => {
                    Some(LexerToken::StringLiteral(Lexer::collect_inner_string(iter, '"')?))
                },
                '\'' => {
                    Some(LexerToken::StringLiteral(Lexer::collect_inner_string(iter, '\'')?))
                },
                // Token Groups
                '`' => if inside_token_group{
                    return Ok(tokens);

                } else {
                    let group_start = iter.index();
                    let tokens = Self::collect_tokens(iter, true)?;
                    iter.assert_char('`', "Unclosed token group.".to_string())?;
                    iter.assert_index_changed(group_start, "Unclosed token group.".to_string())?;
                    Some(LexerToken::TokenGroup(tokens))
                },
                // Operator
                '!' | '&' | '*' | '/' | '=' | '|' | '+' | '~' | '<' | '>' => {
                    Some(LexerToken::Operator(iter.collect_single()))
                },
                // Punctation
                ';' => Some(LexerToken::Comment(iter.collect(true, |c, _| {
                    if let '\n' | '\r' = c {
                        TokenChar::Invalid

                    } else {
                        TokenChar::Valid(c)
                    }
                })?)),
                ':' => Some(LexerToken::Colon(iter.collect_single())),
                '.' => Some(LexerToken::Point(iter.collect_single())),
                ',' => Some(LexerToken::Comma(iter.collect_single())),
                '(' => Some(LexerToken::OpenParen(iter.collect_single())),
                ')' => Some(LexerToken::CloseParen(iter.collect_single())),
                '[' => Some(LexerToken::OpenBracket(iter.collect_single())),
                ']' => Some(LexerToken::CloseBracket(iter.collect_single())),
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

    fn collect_number_literal(iter: &mut TokenIterator) -> Result<LexerToken, LexerError> {
        Ok(LexerToken::NumberLiteral(iter.collect(true, |c, _| {
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
    use super::{Lexer, LexerToken, InnerToken, FileReader};

    #[derive(Default)]
    struct MockFileReader {
        base: PathBuf,
        files: HashMap<PathBuf, String>
    }

    impl MockFileReader {
        fn add_file<S: Into<String>>(&mut self, path: S, content: S) {
            self.files.insert(PathBuf::from(path.into()), content.into());
        }
    }

    impl FileReader for MockFileReader {
        fn read_file(&self, parent_path: Option<&PathBuf>, child_path: &PathBuf) -> Result<(PathBuf, String), IOError> {
            let full_path = Self::resolve_path(&self.base, parent_path, child_path);
            let contents = self.files.get(child_path).map(|s| s.to_string()).ok_or_else(|| {
                IOError::new(ErrorKind::NotFound, "No Mock file provided")
            })?;
            Ok((full_path, contents))
        }
    }

    fn tfs<S: Into<String>>(s: S) -> Vec<LexerToken> {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let mut lexer = Lexer::new();
        lexer.lex_file(&reader, &PathBuf::from("main.gb.s")).expect("Lexer failed");
        assert_eq!(lexer.files.len(), 1);
        lexer.tokens
    }

    fn tfe<S: Into<String>>(s: S) -> Result<usize, Box<dyn Error>> {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let mut lexer = Lexer::new();
        lexer.lex_file(&reader, &PathBuf::from("main.gb.s"))
    }

    macro_rules! tk {
        ($tok:ident, $start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            LexerToken::$tok(InnerToken {
                file_index: 0,
                start_index: $start,
                end_index: $end,
                raw_value: $raw.into(),
                value: $parsed.into()
            })
        }
    }

    #[test]
    fn test_tokens_empty() {
        assert_eq!(tfs(""), vec![]);
    }

    #[test]
    fn text_tokens_newlinews() {
        assert_eq!(tfs("\n\r\n\n\r"), vec![
            tk!(Newline, 0, 1, "\n", "\n"),
            tk!(Newline, 1, 2, "\r", "\r"),
            tk!(Newline, 2, 3, "\n", "\n"),
            tk!(Newline, 3, 4, "\n", "\n"),
            tk!(Newline, 4, 5, "\r", "\r"),
        ]);
    }

    #[test]
    fn text_tokens_name() {
        assert_eq!(tfs("INCLUDE"), vec![tk!(Name, 0, 7, "INCLUDE", "INCLUDE")]);
        assert_eq!(tfs("hl"), vec![tk!(Name, 0, 2, "hl", "hl")]);
        assert_eq!(tfs("a"), vec![tk!(Name, 0, 1, "a", "a")]);
        assert_eq!(tfs("foo_bar"), vec![tk!(Name, 0, 7, "foo_bar", "foo_bar")]);
        assert_eq!(tfs("_test"), vec![tk!(Name, 0, 5, "_test", "_test")]);
        assert_eq!(tfs("abcdefghijklmnopqrstuvwxyz"), vec![tk!(Name, 0, 26, "abcdefghijklmnopqrstuvwxyz", "abcdefghijklmnopqrstuvwxyz")]);
        assert_eq!(tfs("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), vec![tk!(Name, 0, 26, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "ABCDEFGHIJKLMNOPQRSTUVWXYZ")]);
    }

    #[test]
    fn text_tokens_number_literal() {
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
    fn text_tokens_number_literal_hex_incomplete() {
        assert_eq!(tfe("$").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 1: Unexpected character \"$\".");
    }

    #[test]
    fn text_tokens_string_literal() {
        assert_eq!(tfs("'Hello World'"), vec![
            tk!(StringLiteral, 0, 13, "'Hello World'", "Hello World")
        ]);
        assert_eq!(tfs("\"Hello World\""), vec![
            tk!(StringLiteral, 0, 13, "\"Hello World\"", "Hello World")
        ]);
    }

    #[test]
    fn text_tokens_string_literal_unclosed() {
        assert_eq!(tfe("'Hello World").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 13: Unclosed string literal.");
        assert_eq!(tfe("\"Hello World").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 13: Unclosed string literal.");
    }

    #[test]
    fn text_tokens_string_literal_escapes() {
        assert_eq!(tfs("'\\n'"), vec![
            tk!(StringLiteral, 0, 4, "'\\n'", "\n")
        ]);
        assert_eq!(tfs("'\\r'"), vec![
            tk!(StringLiteral, 0, 4, "'\\r'", "\r")
        ]);
        assert_eq!(tfs("'\\t'"), vec![
            tk!(StringLiteral, 0, 4, "'\\t'", "\t")
        ]);
    }

    #[test]
    fn text_tokens_string_punct() {
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
    fn text_tokens_param_offset() {
        assert_eq!(tfs("@param"), vec![tk!(Parameter, 0, 6, "@param", "param")]);
        assert_eq!(tfs("@param_foo"), vec![tk!(Parameter, 0, 10, "@param_foo", "param_foo")]);
        assert_eq!(tfs("@+4"), vec![tk!(Offset, 0, 3, "@+4", "+4")]);
        assert_eq!(tfs("@-4"), vec![tk!(Offset, 0, 3, "@-4", "-4")]);
    }

    #[test]
    fn text_tokens_param_offset_incomplete() {
        assert_eq!(tfe("@").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 1: Unexpected character \"@\".");
    }

    #[test]
    fn text_tokens_comments() {
        assert_eq!(tfs("2 ; A Comment"), vec![
            tk!(NumberLiteral, 0, 1, "2", "2"),
            tk!(Comment, 2, 13, "; A Comment", "; A Comment")
        ]);
    }

    #[test]
    fn text_tokens_multiline() {
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
    fn text_tokens_group() {
        assert_eq!(tfs("``"), vec![
            LexerToken::TokenGroup(Vec::new())
        ]);
        assert_eq!(tfs("`a\n'Text'\n4`"), vec![
            LexerToken::TokenGroup(vec![
                tk!(Name, 1, 2, "a", "a"),
                tk!(Newline, 2, 3, "\n", "\n"),
                tk!(StringLiteral, 3, 9, "'Text'", "Text"),
                tk!(Newline, 9, 10, "\n", "\n"),
                tk!(NumberLiteral, 10, 11, "4", "4")
            ])
        ]);
        assert_eq!(tfs("`\n\n`"), vec![
            LexerToken::TokenGroup(vec![
                tk!(Newline, 1, 2, "\n", "\n"),
                tk!(Newline, 2, 3, "\n", "\n"),
            ])
        ]);
    }

    #[test]
    fn text_tokens_group_unclosed() {
        assert_eq!(tfe("`a").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 3: Unclosed token group.");
        assert_eq!(tfe("`a``a").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 6: Unclosed token group.");
        assert_eq!(tfe("```").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 4: Unclosed token group.");
    }

    #[test]
    fn text_tokens_error_location() {
        assert_eq!(tfe(" $").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 1, column 2: Unexpected character \"$\".");
        assert_eq!(tfe("\n$").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 2, column 1: Unexpected character \"$\".");
        assert_eq!(tfe("\n\n$").unwrap_err().to_string(), "LexerError: In file \"main.gb.s\" on line 3, column 1: Unexpected character \"$\".");
    }

}

