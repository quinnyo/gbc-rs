// STD Dependencies -----------------------------------------------------------
use std::error::Error;


// External Dependencies ------------------------------------------------------
use ordered_float::OrderedFloat;


// Internal Dependencies ------------------------------------------------------
use super::{MacroLexer, InnerToken, TokenIterator, TokenType, LexerToken, LexerFile, LexerError};
use super::macros::MacroToken;


// Value Specific Tokens ------------------------------------------------------
#[allow(unused)]
#[derive(Debug, Eq, PartialEq)]
pub enum ValueToken {
    Name(InnerToken),
    Reserved(InnerToken),
    Instruction(InnerToken),
    BinaryFile(InnerToken, Vec<u8>),
    Comma(InnerToken),
    OpenParen(InnerToken),
    CloseParen(InnerToken),
    OpenBracket(InnerToken),
    CloseBracket(InnerToken),
    BuiltinCall(InnerToken, Vec<Vec<ValueToken>>),
    Offset {
        inner: InnerToken,
        value: i32
    },
    Float {
        inner: InnerToken,
        value: OrderedFloat<f32>
    },
    Integer {
        inner: InnerToken,
        value: i32
    },
    String {
        inner: InnerToken,
        value: String
    },
    LabelDef {
        inner: InnerToken,
        name: String,
        local: bool
    },
    LabelRef {
        inner: InnerToken,
        name: String,
        local: bool
    },
    Operator {
        inner: InnerToken,
        op: ValueOperator
    }
}

impl LexerToken for ValueToken {

    fn typ(&self) -> TokenType {
        match self {
            ValueToken::Name(_) => TokenType::Name,
            ValueToken::Reserved(_) => TokenType::Reserved,
            ValueToken::Instruction(_) => TokenType::Instruction,
            ValueToken::BinaryFile(_, _) => TokenType::BinaryFile,
            ValueToken::BuiltinCall(_, _) => TokenType::BuiltinCall,
            ValueToken::Comma(_) => TokenType::Comma,
            ValueToken::OpenParen(_) => TokenType::OpenParen,
            ValueToken::CloseParen(_) => TokenType::CloseParen,
            ValueToken::OpenBracket(_) => TokenType::OpenBracket,
            ValueToken::CloseBracket(_) => TokenType::CloseBracket,
            ValueToken::Offset { .. } => TokenType::Offset,
            ValueToken::Float { .. } => TokenType::Float,
            ValueToken::Integer { .. } => TokenType::Integer,
            ValueToken::String { .. } => TokenType::String,
            ValueToken::LabelDef { .. } => TokenType::LabelDef,
            ValueToken::LabelRef { .. } => TokenType::LabelRef,
            ValueToken::Operator { .. } => TokenType::Operator,
        }
    }

    fn inner(&self) -> &InnerToken {
        match self {
            ValueToken::Name(inner) | ValueToken::Reserved(inner) | ValueToken::Instruction(inner)
            | ValueToken::BinaryFile(inner, _) | ValueToken::BuiltinCall(inner, _) | ValueToken::Comma(inner)
            | ValueToken::OpenParen(inner) | ValueToken::CloseParen(inner) | ValueToken::OpenBracket(inner) | ValueToken::CloseBracket(inner) => {
                &inner
            },
            ValueToken::Offset { inner, .. } |
            ValueToken::Float { inner, .. } |
            ValueToken::Integer { inner, .. } |
            ValueToken::String { inner, .. } |
            ValueToken::LabelDef { inner, .. } |
            ValueToken::LabelRef { inner, .. } |
            ValueToken::Operator { inner, .. } => {
                &inner
            }
        }
    }

    fn inner_mut(&mut self) -> &mut InnerToken {
        match self {
            ValueToken::Name(inner) | ValueToken::Reserved(inner) | ValueToken::Instruction(inner)
            | ValueToken::BinaryFile(inner, _) | ValueToken::BuiltinCall(inner, _) | ValueToken::Comma(inner)
            | ValueToken::OpenParen(inner) | ValueToken::CloseParen(inner) | ValueToken::OpenBracket(inner) | ValueToken::CloseBracket(inner) => {
                inner
            },
            ValueToken::Offset { inner, .. } |
            ValueToken::Integer { inner, .. } |
            ValueToken::Float { inner, .. } |
            ValueToken::String { inner, .. } |
            ValueToken::LabelDef { inner, .. } |
            ValueToken::LabelRef { inner, .. } |
            ValueToken::Operator { inner, .. } => {
                inner
            }
        }
    }

    fn into_inner(self) -> InnerToken {
        match self {
            ValueToken::Name(inner) | ValueToken::Reserved(inner) | ValueToken::Instruction(inner)
            | ValueToken::BinaryFile(inner, _) | ValueToken::BuiltinCall(inner, _) | ValueToken::Comma(inner)
            | ValueToken::OpenParen(inner) | ValueToken::CloseParen(inner) | ValueToken::OpenBracket(inner) | ValueToken::CloseBracket(inner) => {
                inner
            },
            ValueToken::Offset { inner, .. } |
            ValueToken::Integer { inner, .. } |
            ValueToken::Float { inner, .. } |
            ValueToken::String { inner, .. } |
            ValueToken::LabelDef { inner, .. } |
            ValueToken::LabelRef { inner, .. } |
            ValueToken::Operator { inner, .. } => {
                inner
            }
        }
    }

}

#[derive(Debug, Eq, PartialEq)]
pub enum ValueOperator {
    // Plus,
    // Minus,
    // Modulo,
    // Divide,
}

// Value Level Lexer Implementation -------------------------------------------
pub struct ValueLexer {
    pub files: Vec<LexerFile>,
    pub tokens: Vec<ValueToken>
}

impl ValueLexer {

    pub fn try_from(lexer: MacroLexer) -> Result<ValueLexer, Box<dyn Error>> {
        let files = lexer.files;
        let macro_calls = lexer.macro_calls;
        let tokens = Self::from_tokens(lexer.tokens).map_err(|err| {
            err.extend_with_location_and_macros(&files, &macro_calls)
        })?;
        // TODO parse expressions here too
        Ok(Self {
            files,
            tokens
        })
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    // TODO also use to convert BuiltinCalls Tokens
    fn from_tokens(tokens: Vec<MacroToken>) -> Result<Vec<ValueToken>, LexerError> {
        let mut value_tokens = Vec::new();
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {
            let value_token = match token {

                // Pass through
                MacroToken::Reserved(inner) => ValueToken::Reserved(inner),
                MacroToken::Instruction(inner) => ValueToken::Instruction(inner),
                MacroToken::BinaryFile(inner, bytes) => ValueToken::BinaryFile(inner, bytes),
                MacroToken::Comma(inner) => ValueToken::Comma(inner),
                MacroToken::OpenParen(inner) => ValueToken::OpenParen(inner),
                MacroToken::CloseParen(inner) => ValueToken::CloseParen(inner),
                MacroToken::OpenBracket(inner) => ValueToken::OpenBracket(inner),
                MacroToken::CloseBracket(inner)=> ValueToken::CloseBracket(inner),

                // Drop Comments
                MacroToken::Comment(inner) => continue,

                // Values
                MacroToken::BuiltinCall(inner, tokens) => {
                    unimplemented!();
                },
                MacroToken::Name(inner) => {
                    // TODO check if next is Colon and create LabelDef
                    // TODO Check for duplicate global labels
                    // TODO rest local labels list
                    ValueToken::Name(inner)
                },
                MacroToken::Offset(inner)  => {
                    ValueToken::Offset {
                        value: Self::parse_integer(&inner, 0, 10)?,
                        inner
                    }
                },
                MacroToken::NumberLiteral(inner) => {
                    match inner.value.chars().next().unwrap() {
                        '$' => {
                            ValueToken::Integer {
                                value: Self::parse_integer(&inner, 1, 16)?,
                                inner
                            }
                        },
                        '%' => {
                            ValueToken::Integer {
                                value: Self::parse_integer(&inner, 1, 2)?,
                                inner
                            }
                        },
                        _ => if inner.value.contains(".") {
                            ValueToken::Float {
                                value: OrderedFloat::from(Self::parse_float(&inner)?),
                                inner
                            }

                        } else {
                            ValueToken::Integer {
                                value: Self::parse_integer(&inner, 0, 10)?,
                                inner
                            }
                        }
                    }
                },
                MacroToken::StringLiteral(inner) => {
                    ValueToken::String {
                        value: inner.value.clone(),
                        inner
                    }
                },
                MacroToken::Point(inner) => {
                    // TODO Create local labels
                    // TODO Check for duplicate local labels
                    unimplemented!();
                },
                MacroToken::Colon(inner) => {
                    return Err(inner.error(format!("Unexpected standalone \"{}\", expected a \"Name\" token to preceed it.", inner.value)))
                },
                MacroToken::Operator(inner) => {
                    // TODO Combine and match Operators Tokens into actual Operators
                    unimplemented!();
                }
            };
            value_tokens.push(value_token);
        }

        Ok(value_tokens)
    }

    fn parse_integer(inner: &InnerToken, from: usize, radix: u32) -> Result<i32, LexerError> {
        i32::from_str_radix(&inner.value[from..], radix).map_err(|_| {
            inner.error("Failed to parse integer value.".to_string())
        })
    }

    fn parse_float(inner: &InnerToken) -> Result<f32, LexerError> {
        inner.value.parse::<f32>().map_err(|_| {
            inner.error("Failed to parse float value.".to_string())
        })
    }


}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use ordered_float::OrderedFloat;
    use super::{ValueLexer, ValueToken, InnerToken};
    use crate::lexer::mocks::macro_lex;

    fn value_lexer<S: Into<String>>(s: S) -> ValueLexer {
        ValueLexer::try_from(macro_lex(s)).expect("ValueLexer failed")
    }

    fn value_lexer_error<S: Into<String>>(s: S) -> String {
        ValueLexer::try_from(macro_lex(s)).err().unwrap().to_string()
    }

    fn tfv<S: Into<String>>(s: S) -> Vec<ValueToken> {
        value_lexer(s).tokens
    }

    macro_rules! itk {
        ($start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $raw.into(), $parsed.into())
        }
    }

    macro_rules! vtk {
        ($tok:ident, $start:expr, $end:expr, $raw:expr, $parsed:expr) => {
            ValueToken::$tok(itk!($start, $end, $raw, $parsed))
        }
    }

    // Value Parsing ----------------------------------------------------------
    #[test]
    fn test_empty() {
        assert_eq!(tfv(""), vec![]);
    }

    #[test]
    fn test_strip_comments() {
        assert_eq!(tfv("; Foo"), vec![]);
    }

    #[test]
    fn test_passthrough() {
        assert_eq!(tfv("name\nDS\ncp\n,()[]"), vec![
            vtk!(Name, 0, 4, "name", "name"),
            vtk!(Reserved, 5, 7, "DS", "DS"),
            vtk!(Instruction, 8, 10, "cp", "cp"),
            vtk!(Comma, 11, 12, ",", ","),
            vtk!(OpenParen, 12, 13, "(", "("),
            vtk!(CloseParen, 13, 14, ")", ")"),
            vtk!(OpenBracket, 14, 15, "[", "["),
            vtk!(CloseBracket, 15, 16, "]", "]"),
        ]);
    }

    #[test]
    fn test_offset() {
        assert_eq!(tfv("@+4\n@-4"), vec![
            ValueToken::Offset {
                inner: itk!(0, 3, "@+4", "+4"),
                value: 4
            },
            ValueToken::Offset {
                inner: itk!(4, 7, "@-4", "-4"),
                value: -4
            }
        ]);
    }

    #[test]
    fn test_number() {
        assert_eq!(tfv("123\n-123\n%0000_1010\n$80\n1.24\n-2.48"), vec![
            ValueToken::Integer {
                inner: itk!(0, 3, "123", "123"),
                value: 123
            },
            ValueToken::Integer {
                inner: itk!(4, 8, "-123", "-123"),
                value: -123
            },
            ValueToken::Integer {
                inner: itk!(9, 19, "%0000_1010", "%00001010"),
                value: 10
            },
            ValueToken::Integer {
                inner: itk!(20, 23, "$80", "$80"),
                value: 128
            },
            ValueToken::Float {
                inner: itk!(24, 28, "1.24", "1.24"),
                value: OrderedFloat::from(1.24)
            },
            ValueToken::Float {
                inner: itk!(29, 34, "-2.48", "-2.48"),
                value: OrderedFloat::from(-2.48)
            }
        ]);
    }

    #[test]
    fn test_string() {
        assert_eq!(tfv("'Hello World'\n\"Hello World\""), vec![
            ValueToken::String {
                inner: itk!(0, 13, "'Hello World'", "Hello World"),
                value: "Hello World".to_string()
            },
            ValueToken::String {
                inner: itk!(14, 27, "\"Hello World\"", "Hello World"),
                value: "Hello World".to_string()
            }
        ]);
    }

    // Value Errors -----------------------------------------------------------
    #[test]
    fn test_error_colon_standlone() {
        assert_eq!(value_lexer_error(":"), "In file \"main.gb.s\" on line 1, column 1: Unexpected standalone \":\", expected a \"Name\" token to preceed it.\n\n:\n^--- Here");
    }

}

