// Internal Dependencies ------------------------------------------------------
use super::macros::MacroCall;
use crate::lexer::ExpressionStage;
use super::expression::{ExpressionToken, Expression};
use super::super::{LexerStage, InnerToken, TokenIterator, TokenType, LexerToken, LexerError};


// Types ----------------------------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub enum Mnemonic {

}

type DataExpression = (usize, Expression);
type OptionalDataExpression = Option<DataExpression>;

#[derive(Debug, Eq, PartialEq)]
pub enum DataEndianess {
    /// LL HH
    Little,
    /// HH LL
    Big
}

#[derive(Debug, Eq, PartialEq)]
pub enum DataAlignment {
    /// Anywhere
    Byte,
    /// Low byte must be 00
    Word,
    /// May not cross a word boundary
    WithinWord
}

#[derive(Debug, Eq, PartialEq)]
pub enum DataStorage {
    /// DB
    Byte,
    /// DW
    Word,
    /// INCBIN "..."
    File(Vec<u8>),
    /// DB 1[, 2, 3]
    Bytes(Vec<DataExpression>),
    /// DW 1[, 2, 3]
    Words(Vec<DataExpression>),
    /// DS 1
    ByteRange(DataExpression)
}

// Entry Specific Tokens ------------------------------------------------------
lexer_token!(EntryToken, (Debug, Eq, PartialEq), {
    Instruction((usize)),
    InstructionWithArg((usize, DataExpression)),
    InstructionEx((usize)),
    InstructionExWithArg((usize, DataExpression))

}, {
    GlobalLabelDef {
        name => String
    },
    LocalLabelDef {
        name => String
    },
    //
    // Constant + EQU|EQUS + ConstExpression
    Constant {
        name => String,
        is_string => bool,
        value => DataExpression
    },
    // DS|DS8|DS16 + Size: ConstExpression [, fill value? (only in ROM segements)]
    // DB|DW|BW (Const)Expression, ...
    Data {
        alignment => DataAlignment,
        endianess => DataEndianess,
        storage => DataStorage
    },
    // SECTION EXPR[String]
    SectionDeclaration {
        name => OptionalDataExpression,
        segment_name => String,
        segment_offset => OptionalDataExpression,
        segment_size => OptionalDataExpression,
        bank_index => OptionalDataExpression
    }
});

// Entry Level Lexer Implementation -------------------------------------------
pub struct EntryStage;
impl LexerStage for EntryStage {

    type Input = ExpressionStage;
    type Output = EntryToken;
    type Data = ();

    fn from_tokens(
        tokens: Vec<<Self::Input as LexerStage>::Output>,
        _macro_calls: &mut Vec<MacroCall>,
        _data: &mut Vec<Self::Data>

    ) -> Result<Vec<Self::Output>, LexerError> {
        Self::parse_entry_tokens(tokens)
    }

}

impl EntryStage {

    fn parse_entry_tokens(tokens: Vec<ExpressionToken>) -> Result<Vec<EntryToken>, LexerError> {
        let mut entry_tokens = Vec::with_capacity(tokens.len());
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {
            let entry = match token {

                // Passthrough Label Defs
                ExpressionToken::GlobalLabelDef { inner, name } => EntryToken::GlobalLabelDef {
                    inner,
                    name
                },
                ExpressionToken::LocalLabelDef { inner, name } => EntryToken::LocalLabelDef {
                    inner,
                    name
                },

                // Constant Declarations
                ExpressionToken::Constant(inner) => {
                    if tokens.peek_is(TokenType::Reserved, Some("EQU")) {
                        tokens.expect(TokenType::Reserved, None, "when parsing constant declaration")?;
                        if let ExpressionToken::ConstExpression(_, id, expr) = tokens.expect(TokenType::ConstExpression, None, "when parsing constant declaration")? {
                            EntryToken::Constant {
                                name: inner.value.clone(),
                                inner,
                                is_string: false,
                                value: (id, expr)
                            }

                        } else {
                            unreachable!();
                        }

                    } else if tokens.peek_is(TokenType::Reserved, Some("EQUS")) {
                        tokens.expect(TokenType::Reserved, None, "when parsing constant declaration")?;
                        if let ExpressionToken::ConstExpression(_, id, expr) = tokens.expect(TokenType::ConstExpression, None, "when parsing constant declaration")? {
                            EntryToken::Constant {
                                name: inner.value.clone(),
                                inner,
                                is_string: true,
                                value: (id, expr)
                            }
                        } else {
                            unreachable!();
                        }

                    } else {
                        unreachable!("Expression lexer failed to return \"Constant\" token only if followed by EQU / EQUS");
                    }
                },

                // Instructions
                ExpressionToken::Instruction(_) => {
                    // TODO handle flag, register, comma, OpenBracket, CloseBracket
                    continue;
                },

                ExpressionToken::MetaInstruction(_) => {
                    // TODO handle flag, register, comma, OpenBracket, CloseBracket
                    continue;
                },

                // Binary Data Declarations
                ExpressionToken::BinaryFile(inner, bytes) => {
                    EntryToken::Data {
                        inner,
                        alignment: DataAlignment::Byte,
                        endianess: DataEndianess::Little,
                        storage: DataStorage::File(bytes)
                    }
                },

                // Other Directives
                ExpressionToken::Reserved(inner) => {
                    match inner.value.as_str() {
                        "SECTION" => {

                            // Check for optional section name
                            let name = if tokens.peek_is(TokenType::ConstExpression, None) {
                                let name = tokens.expect(TokenType::ConstExpression, None, "when parsing section declaration")?;
                                tokens.expect(TokenType::Comma, None, "after section name")?;
                                if let ExpressionToken::ConstExpression(_, id, expr) = name {
                                    Some((id, expr))

                                } else {
                                    None
                                }

                            } else {
                                None
                            };

                            // Required Segment
                            let segment_name = tokens.expect(TokenType::Segment, None, "when parsing section declaration")?.into_inner().value;

                            // Check for optional offset
                            let segment_offset = if tokens.peek_is(TokenType::OpenBracket, None) {
                                Self::parse_bracket_expr(&mut tokens, "when parsing section offset", true)?

                            } else {
                                None
                            };

                            // Check for optional size
                            let segment_size = if tokens.peek_is(TokenType::OpenBracket, None) {
                                Self::parse_bracket_expr(&mut tokens, "when parsing section size", false)?

                            } else {
                                None
                            };

                            // Check for optional bank
                            let bank_index = if tokens.peek_is(TokenType::Comma, None) {
                                tokens.expect(TokenType::Comma, None, "when parsing section bank")?;
                                tokens.expect(TokenType::Reserved, Some("BANK"), "when parsing section bank")?;
                                Self::parse_bracket_expr(&mut tokens, "when parsing section bank", false)?

                            } else {
                                None
                            };

                            EntryToken::SectionDeclaration {
                                inner,
                                name,
                                segment_name,
                                segment_offset,
                                segment_size,
                                bank_index
                            }
                        },
                        "DB" => {
                            Self::parse_data_directive_db(&mut tokens, inner)?
                        },
                        "DW" => {
                            Self::parse_data_directive_dw(&mut tokens, inner)?
                        },
                        "BW" => {
                            Self::parse_data_directive_bw(&mut tokens, inner)?
                        },
                        "DS" => {
                            Self::parse_data_directive_ds(&mut tokens, inner, DataAlignment::Byte)?
                        },
                        "DS8" => {
                            Self::parse_data_directive_ds(&mut tokens, inner, DataAlignment::WithinWord)?
                        },
                        "DS16" => {
                            Self::parse_data_directive_ds(&mut tokens, inner, DataAlignment::Word)?
                        },
                        _=> {
                            return Err(inner.error(format!(
                                "Unexpected reserved keyword \"{}\", expected either SECTION, DB, BW, DS, DS8 or DS16 instead.",
                                inner.value
                            )));
                        }
                    }
                },
                token => {
                    return Err(token.error(
                        format!("Unexpected \"{}\", expected either a constant declaration, directive or instruction instead.", token.value())
                    ));
                }

            };
            entry_tokens.push(entry);
        }
        Ok(entry_tokens)
    }

    fn parse_bracket_expr(tokens: &mut TokenIterator<ExpressionToken>, msg: &str, optional_value: bool) -> Result<OptionalDataExpression, LexerError> {
        tokens.expect(TokenType::OpenBracket, Some("["), msg)?;
        if optional_value && tokens.peek_is(TokenType::CloseBracket, None) {
            tokens.expect(TokenType::CloseBracket, Some("]"), msg)?;
            Ok(None)

        } else {
            let value = tokens.expect(TokenType::ConstExpression, None, msg)?;
            tokens.expect(TokenType::CloseBracket, Some("]"), msg)?;
            if let ExpressionToken::ConstExpression(_, id, expr) = value {
                Ok(Some((id, expr)))

            } else {
                Ok(None)
            }
        }
    }

    fn parse_data_directive_db(
        tokens: &mut TokenIterator<ExpressionToken>,
        inner: InnerToken

    ) -> Result<EntryToken, LexerError> {
        match Self::parse_expression_list(tokens)? {
            Some(e) => Ok(EntryToken::Data {
                inner,
                alignment: DataAlignment::Byte,
                endianess: DataEndianess::Little,
                storage: DataStorage::Bytes(e)

            }),
            None => Ok(EntryToken::Data {
                inner,
                alignment: DataAlignment::Byte,
                endianess: DataEndianess::Little,
                storage: DataStorage::Byte
            })
        }
    }

    fn parse_data_directive_dw(
        tokens: &mut TokenIterator<ExpressionToken>,
        inner: InnerToken

    ) -> Result<EntryToken, LexerError> {
        match Self::parse_expression_list(tokens)? {
            Some(e) => Ok(EntryToken::Data {
                inner,
                alignment: DataAlignment::Byte,
                endianess: DataEndianess::Little,
                storage: DataStorage::Words(e)

            }),
            None => Ok(EntryToken::Data {
                inner,
                alignment: DataAlignment::Byte,
                endianess: DataEndianess::Little,
                storage: DataStorage::Word
            })
        }
    }

    fn parse_data_directive_bw(
        tokens: &mut TokenIterator<ExpressionToken>,
        inner: InnerToken

    ) -> Result<EntryToken, LexerError> {
        match Self::parse_expression_list(tokens)? {
            Some(e) => Ok(EntryToken::Data {
                inner,
                alignment: DataAlignment::Byte,
                endianess: DataEndianess::Big,
                storage: DataStorage::Words(e)

            }),
            None => Ok(EntryToken::Data {
                inner,
                alignment: DataAlignment::Byte,
                endianess: DataEndianess::Big,
                storage: DataStorage::Word
            })
        }
    }

    fn parse_data_directive_ds(
        tokens: &mut TokenIterator<ExpressionToken>,
        inner: InnerToken,
        alignment: DataAlignment

    ) -> Result<EntryToken, LexerError> {
        let token = tokens.expect(TokenType::ConstExpression, None, "when parsing data storage directive")?;
        if let ExpressionToken::ConstExpression(_, id, expr) = token {
            Ok(EntryToken::Data {
                inner,
                alignment,
                endianess: DataEndianess::Little,
                storage: DataStorage::ByteRange((id, expr))
            })
        } else {
            unreachable!();
        }
    }

    fn parse_expression_list(tokens: &mut TokenIterator<ExpressionToken>) -> Result<Option<Vec<DataExpression>>, LexerError> {
        if tokens.peek_is(TokenType::Expression, None) || tokens.peek_is(TokenType::ConstExpression, None) {
            let mut expressions = Vec::new();
            while tokens.peek_is(TokenType::Expression, None) || tokens.peek_is(TokenType::ConstExpression, None) {
                let expr = tokens.get("when parsing expression list")?;
                match expr {
                    ExpressionToken::ConstExpression(_, id, expr) | ExpressionToken::Expression(_, id, expr) => {
                        expressions.push((id, expr));
                    },
                    _ => unreachable!()
                }
                if tokens.peek_is(TokenType::Comma, None) {
                    tokens.expect(TokenType::Comma, None, "when parsing expression list")?;

                } else {
                    break;
                }
            }
            Ok(Some(expressions))

        } else {
            Ok(None)
        }
    }

}

// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use crate::lexer::Lexer;
    use super::{EntryStage, EntryToken, InnerToken, DataEndianess, DataAlignment, DataStorage};
    use super::super::expression::{Expression, ExpressionValue};
    use super::super::value::Operator;
    use super::super::mocks::{expr_lex, expr_lex_binary};

    fn entry_lexer<S: Into<String>>(s: S) -> Lexer<EntryStage> {
        Lexer::<EntryStage>::from_lexer(expr_lex(s)).expect("EntryStage failed")
    }

    fn entry_lexer_binary<S: Into<String>>(s: S, b: Vec<u8>) -> Lexer<EntryStage> {
        let lexer = expr_lex_binary(s, b);
        Lexer::<EntryStage>::from_lexer(lexer).expect("EntryStage failed")
    }

    fn entry_lexer_error<S: Into<String>>(s: S) -> String {
        Lexer::<EntryStage>::from_lexer(expr_lex(s)).err().unwrap().to_string()
    }

    fn tfe<S: Into<String>>(s: S) -> Vec<EntryToken> {
        entry_lexer(s).tokens
    }

    macro_rules! itk {
        ($start:expr, $end:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $parsed.into())
        }
    }

    // Entry Parsing ----------------------------------------------------------
    #[test]
    fn test_empty() {
        assert_eq!(tfe(""), vec![]);
    }

    #[test]
    fn test_passthrough_global_label_def() {
        assert_eq!(tfe("global_label:"), vec![EntryToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label"),
            name: "global_label".to_string()
        }]);
    }

    #[test]
    fn test_passthrough_local_label_def() {
        assert_eq!(tfe("global_label:\n.local_label:"), vec![EntryToken::GlobalLabelDef {
            inner: itk!(0, 13, "global_label"),
            name: "global_label".to_string()

        }, EntryToken::LocalLabelDef {
            inner: itk!(14, 27, "."),
            name: "local_label".to_string()
        }]);
    }

    #[test]
    fn test_error_unexpected() {
        assert_eq!(entry_lexer_error("2 + 2"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"2\", expected either a constant declaration, directive or instruction instead.\n\n2 + 2\n^--- Here");
        assert_eq!(entry_lexer_error("EQU"), "In file \"main.gb.s\" on line 1, column 1: Unexpected reserved keyword \"EQU\", expected either SECTION, DB, BW, DS, DS8 or DS16 instead.\n\nEQU\n^--- Here");
        assert_eq!(entry_lexer_error("EQUS"), "In file \"main.gb.s\" on line 1, column 1: Unexpected reserved keyword \"EQUS\", expected either SECTION, DB, BW, DS, DS8 or DS16 instead.\n\nEQUS\n^--- Here");
        assert_eq!(entry_lexer_error("SEGMENT"), "In file \"main.gb.s\" on line 1, column 1: Unexpected reserved keyword \"SEGMENT\", expected either SECTION, DB, BW, DS, DS8 or DS16 instead.\n\nSEGMENT\n^--- Here");
        assert_eq!(entry_lexer_error("BANK"), "In file \"main.gb.s\" on line 1, column 1: Unexpected reserved keyword \"BANK\", expected either SECTION, DB, BW, DS, DS8 or DS16 instead.\n\nBANK\n^--- Here");
        assert_eq!(entry_lexer_error(","), "In file \"main.gb.s\" on line 1, column 1: Unexpected \",\", expected either a constant declaration, directive or instruction instead.\n\n,\n^--- Here");
        assert_eq!(entry_lexer_error("["), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"[\", expected either a constant declaration, directive or instruction instead.\n\n[\n^--- Here");
        assert_eq!(entry_lexer_error("]"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"]\", expected either a constant declaration, directive or instruction instead.\n\n]\n^--- Here");
        assert_eq!(entry_lexer_error("hl"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"hl\", expected either a constant declaration, directive or instruction instead.\n\nhl\n^--- Here");
        assert_eq!(entry_lexer_error("nz"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"nz\", expected either a constant declaration, directive or instruction instead.\n\nnz\n^--- Here");
        assert_eq!(entry_lexer_error("DS"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input when parsing data storage directive, expected a \"ConstExpression\" token instead.\n\nDS\n^--- Here");
        assert_eq!(entry_lexer_error("DS8"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input when parsing data storage directive, expected a \"ConstExpression\" token instead.\n\nDS8\n^--- Here");
        assert_eq!(entry_lexer_error("DS16"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input when parsing data storage directive, expected a \"ConstExpression\" token instead.\n\nDS16\n^--- Here");
        assert_eq!(entry_lexer_error("DS16"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input when parsing data storage directive, expected a \"ConstExpression\" token instead.\n\nDS16\n^--- Here");
        assert_eq!(entry_lexer_error("ROMX"), "In file \"main.gb.s\" on line 1, column 1: Unexpected \"ROMX\", expected either a constant declaration, directive or instruction instead.\n\nROMX\n^--- Here");
        assert_eq!(entry_lexer_error("DS 1 1"), "In file \"main.gb.s\" on line 1, column 6: Unexpected \"1\", expected either a constant declaration, directive or instruction instead.\n\nDS 1 1\n     ^--- Here");
    }

    // Constant Declarations --------------------------------------------------
    #[test]
    fn test_const_declaration_equ() {
        assert_eq!(tfe("foo EQU 2"), vec![EntryToken::Constant {
            inner: itk!(0, 3, "foo"),
            name: "foo".to_string(),
            is_string: false,
            value: (0, Expression::Value(ExpressionValue::Integer(2)))
        }]);
        assert_eq!(tfe("foo EQU bar"), vec![EntryToken::Constant {
            inner: itk!(0, 3, "foo"),
            name: "foo".to_string(),
            is_string: false,
            value: (0, Expression::Value(ExpressionValue::ConstantValue(
                itk!(8, 11, "bar"),
                "bar".to_string()
            )))
        }]);
    }

    #[test]
    fn test_const_declaration_equs() {
        assert_eq!(tfe("foo EQUS 'test'"), vec![EntryToken::Constant {
            inner: itk!(0, 3, "foo"),
            name: "foo".to_string(),
            is_string: true,
            value: (0, Expression::Value(ExpressionValue::String("test".to_string())))
        }]);
        assert_eq!(tfe("foo EQUS bar"), vec![EntryToken::Constant {
            inner: itk!(0, 3, "foo"),
            name: "foo".to_string(),
            is_string: true,
            value: (0, Expression::Value(ExpressionValue::ConstantValue(
                itk!(9, 12, "bar"),
                "bar".to_string()
            )))
        }]);
    }

    #[test]
    fn test_error_const_declaration_no_expr() {
        assert_eq!(entry_lexer_error("foo EQU"), "In file \"main.gb.s\" on line 1, column 5: Unexpected end of input when parsing constant declaration, expected a \"ConstExpression\" token instead.\n\nfoo EQU\n    ^--- Here");
        assert_eq!(entry_lexer_error("foo EQUS"), "In file \"main.gb.s\" on line 1, column 5: Unexpected end of input when parsing constant declaration, expected a \"ConstExpression\" token instead.\n\nfoo EQUS\n    ^--- Here");
        assert_eq!(entry_lexer_error("foo EQU DB"), "In file \"main.gb.s\" on line 1, column 9: Unexpected token \"Reserved\" when parsing constant declaration, expected a \"ConstExpression\" token instead.\n\nfoo EQU DB\n        ^--- Here");
        assert_eq!(entry_lexer_error("global_label:\nfoo EQU global_label"), "In file \"main.gb.s\" on line 2, column 9: Unexpected token \"Expression\" when parsing constant declaration, expected a \"ConstExpression\" token instead.\n\nfoo EQU global_label\n        ^--- Here");
    }

    // Data Declarations ------------------------------------------------------
    #[test]
    fn test_data_db() {
        assert_eq!(tfe("DB"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DB"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::Byte
        }]);
        assert_eq!(tfe("DB 2"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DB"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::Bytes(vec![(0, Expression::Value(ExpressionValue::Integer(2)))])
        }]);
        assert_eq!(tfe("DB 2 + 3, 1"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DB"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::Bytes(vec![
                (0, Expression::Binary {
                    inner: itk!(5, 6, "+"),
                    op: Operator::Plus,
                    left: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
                }),
                (1, Expression::Value(ExpressionValue::Integer(1)))
            ])
        }]);
        assert_eq!(tfe("DB 2, 3, 4, 5"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DB"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::Bytes(vec![
                (0, Expression::Value(ExpressionValue::Integer(2))),
                (1, Expression::Value(ExpressionValue::Integer(3))),
                (2, Expression::Value(ExpressionValue::Integer(4))),
                (3, Expression::Value(ExpressionValue::Integer(5)))
            ])
        }]);
    }

    #[test]
    fn test_data_dw() {
        assert_eq!(tfe("DW"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DW"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::Word
        }]);
        assert_eq!(tfe("DW 2000"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DW"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::Words(vec![(0, Expression::Value(ExpressionValue::Integer(2000)))])
        }]);
        assert_eq!(tfe("DW 2 + 3, 1"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DW"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::Words(vec![
                (0, Expression::Binary {
                    inner: itk!(5, 6, "+"),
                    op: Operator::Plus,
                    left: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
                }),
                (1, Expression::Value(ExpressionValue::Integer(1)))
            ])
        }]);
        assert_eq!(tfe("DW 2000, 3000, 4000, 5000"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DW"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::Words(vec![
                (0, Expression::Value(ExpressionValue::Integer(2000))),
                (1, Expression::Value(ExpressionValue::Integer(3000))),
                (2, Expression::Value(ExpressionValue::Integer(4000))),
                (3, Expression::Value(ExpressionValue::Integer(5000)))
            ])
        }]);
    }

    #[test]
    fn test_data_bw() {
        assert_eq!(tfe("BW"), vec![EntryToken::Data {
            inner: itk!(0, 2, "BW"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Big,
            storage: DataStorage::Word
        }]);
        assert_eq!(tfe("BW 2000"), vec![EntryToken::Data {
            inner: itk!(0, 2, "BW"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Big,
            storage: DataStorage::Words(vec![(0, Expression::Value(ExpressionValue::Integer(2000)))])
        }]);
        assert_eq!(tfe("BW 2 + 3, 1"), vec![EntryToken::Data {
            inner: itk!(0, 2, "BW"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Big,
            storage: DataStorage::Words(vec![
                (0, Expression::Binary {
                    inner: itk!(5, 6, "+"),
                    op: Operator::Plus,
                    left: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
                }),
                (1, Expression::Value(ExpressionValue::Integer(1)))
            ])
        }]);
        assert_eq!(tfe("BW 2000, 3000, 4000, 5000"), vec![EntryToken::Data {
            inner: itk!(0, 2, "BW"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Big,
            storage: DataStorage::Words(vec![
                (0, Expression::Value(ExpressionValue::Integer(2000))),
                (1, Expression::Value(ExpressionValue::Integer(3000))),
                (2, Expression::Value(ExpressionValue::Integer(4000))),
                (3, Expression::Value(ExpressionValue::Integer(5000)))
            ])
        }]);
    }

    #[test]
    fn test_data_ds() {
        assert_eq!(tfe("DS 2 + 3"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DS"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::ByteRange((0, Expression::Binary {
                inner: itk!(5, 6, "+"),
                op: Operator::Plus,
                left: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
            }))
        }]);
    }

    #[test]
    fn test_data_ds8() {
        assert_eq!(tfe("DS8 2 + 3"), vec![EntryToken::Data {
            inner: itk!(0, 3, "DS8"),
            alignment: DataAlignment::WithinWord,
            endianess: DataEndianess::Little,
            storage: DataStorage::ByteRange((0, Expression::Binary {
                inner: itk!(6, 7, "+"),
                op: Operator::Plus,
                left: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
            }))
        }]);
    }

    #[test]
    fn test_data_ds16() {
        assert_eq!(tfe("DS16 2 + 3"), vec![EntryToken::Data {
            inner: itk!(0, 4, "DS16"),
            alignment: DataAlignment::Word,
            endianess: DataEndianess::Little,
            storage: DataStorage::ByteRange((0, Expression::Binary {
                inner: itk!(7, 8, "+"),
                op: Operator::Plus,
                left: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
            }))
        }]);
    }

    #[test]
    fn test_data_incbin() {
        let tokens = entry_lexer_binary("INCBIN 'child.bin'", vec![1, 2, 3]).tokens;
        assert_eq!(tokens, vec![EntryToken::Data {
            inner: itk!(7, 18, "child.bin"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::File(vec![1, 2, 3])
        }]);
    }


    // Section Declarations ---------------------------------------------------
    #[test]
    fn test_section_without_name() {
        assert_eq!(tfe("SECTION ROM0"), vec![EntryToken::SectionDeclaration {
            inner: itk!(0, 7, "SECTION"),
            name: None,
            segment_name: "ROM0".to_string(),
            segment_offset: None,
            segment_size: None,
            bank_index: None
        }]);
    }

    #[test]
    fn test_section_with_name() {
        assert_eq!(tfe("SECTION 'Foo',ROM0"), vec![EntryToken::SectionDeclaration {
            inner: itk!(0, 7, "SECTION"),
            name: Some((0, Expression::Value(ExpressionValue::String("Foo".to_string())))),
            segment_name: "ROM0".to_string(),
            segment_offset: None,
            segment_size: None,
            bank_index: None
        }]);
    }

    #[test]
    fn test_section_with_offset() {
        assert_eq!(tfe("SECTION ROM0[$0000]"), vec![EntryToken::SectionDeclaration {
            inner: itk!(0, 7, "SECTION"),
            name: None,
            segment_name: "ROM0".to_string(),
            segment_offset: Some((0, Expression::Value(ExpressionValue::Integer(0)))),
            segment_size: None,
            bank_index: None
        }]);
    }

    #[test]
    fn test_section_with_offset_and_size() {
        assert_eq!(tfe("SECTION ROM0[$0000][$800]"), vec![EntryToken::SectionDeclaration {
            inner: itk!(0, 7, "SECTION"),
            name: None,
            segment_name: "ROM0".to_string(),
            segment_offset: Some((0, Expression::Value(ExpressionValue::Integer(0)))),
            segment_size: Some((1, Expression::Value(ExpressionValue::Integer(2048)))),
            bank_index: None
        }]);
    }

    #[test]
    fn test_section_without_offset_and_size() {
        assert_eq!(tfe("SECTION ROM0[][$800]"), vec![EntryToken::SectionDeclaration {
            inner: itk!(0, 7, "SECTION"),
            name: None,
            segment_name: "ROM0".to_string(),
            segment_offset: None,
            segment_size: Some((0, Expression::Value(ExpressionValue::Integer(2048)))),
            bank_index: None
        }]);
    }

    #[test]
    fn test_section_with_bank() {
        assert_eq!(tfe("SECTION ROM0,BANK[1]"), vec![EntryToken::SectionDeclaration {
            inner: itk!(0, 7, "SECTION"),
            name: None,
            segment_name: "ROM0".to_string(),
            segment_offset: None,
            segment_size: None,
            bank_index: Some((0, Expression::Value(ExpressionValue::Integer(1)))),
        }]);
    }

    #[test]
    fn test_error_section() {
        assert_eq!(entry_lexer_error("SECTION"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input when parsing section declaration, expected a \"Segment\" token instead.\n\nSECTION\n^--- Here");
        assert_eq!(entry_lexer_error("SECTION ROM0,"), "In file \"main.gb.s\" on line 1, column 13: Unexpected end of input when parsing section bank, expected \"BANK\" instead.\n\nSECTION ROM0,\n            ^--- Here");
        assert_eq!(entry_lexer_error("SECTION ROM0["), "In file \"main.gb.s\" on line 1, column 13: Unexpected end of input when parsing section offset, expected a \"ConstExpression\" token instead.\n\nSECTION ROM0[\n            ^--- Here");
        assert_eq!(entry_lexer_error("SECTION ROM0,BANK"), "In file \"main.gb.s\" on line 1, column 14: Unexpected end of input when parsing section bank, expected \"[\" instead.\n\nSECTION ROM0,BANK\n             ^--- Here");
        assert_eq!(entry_lexer_error("SECTION foo"), "In file \"main.gb.s\" on line 1, column 9: Unexpected end of input after section name, expected a \"Comma\" token instead.\n\nSECTION foo\n        ^--- Here");
        assert_eq!(entry_lexer_error("SECTION foo,bar"), "In file \"main.gb.s\" on line 1, column 13: Unexpected token \"ConstExpression\" when parsing section declaration, expected a \"Segment\" token instead.\n\nSECTION foo,bar\n            ^--- Here");
    }

}

