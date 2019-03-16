// Internal Dependencies ------------------------------------------------------
use super::macros::MacroCall;
use crate::lexer::ExpressionStage;
use super::expression::{ExpressionToken, Expression};
use super::super::{LexerStage, InnerToken, TokenIterator, TokenType, LexerToken, LexerError};


// Types ----------------------------------------------------------------------
#[derive(Debug, Eq, PartialEq)]
pub enum Mnemonic {

}

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
    Bytes(Vec<Expression>),
    /// DW 1[, 2, 3]
    Words(Vec<Expression>),
    /// DS 1
    ByteRange(Expression)
}

// Entry Specific Tokens ------------------------------------------------------
lexer_token!(EntryToken, (Debug, Eq, PartialEq), {}, {
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
        value => Expression
    },
    // DS|DS8|DS16 + Size: ConstExpression [, fill value? (only in ROM segements)]
    // DB|DW|BW (Const)Expression, ...
    Data {
        alignment => DataAlignment,
        endianess => DataEndianess,
        storage => DataStorage
    },
    Instruction {
        // TODO value(s) for parameters (Expression(s))
        mnemonic => Mnemonic // TODO put values directly into mnemonic enum
        // TODO size get from Mnemonic
    },
    // SECTION EXPR[String]
    SectionDeclaration {
        // TODO offset, bank etc.
        name => String
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
                        if let ExpressionToken::ConstExpression(_, _, expr) = tokens.expect(TokenType::ConstExpression, None, "when parsing constant declaration")? {
                            EntryToken::Constant {
                                name: inner.value.clone(),
                                inner,
                                is_string: false,
                                value: expr
                            }

                        } else {
                            unreachable!();
                        }

                    } else if tokens.peek_is(TokenType::Reserved, Some("EQUS")) {
                        tokens.expect(TokenType::Reserved, None, "when parsing constant declaration")?;
                        if let ExpressionToken::ConstExpression(_, _, expr) = tokens.expect(TokenType::ConstExpression, None, "when parsing constant declaration")? {
                            EntryToken::Constant {
                                name: inner.value.clone(),
                                inner,
                                is_string: true,
                                value: expr
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

                            // SECTION "Optional Name",SEGMENT,BANK[ID]
                            // SECTION "Optional Name",SEGMENT[Offset][SIZE],BANK[ID]

                            // Check for optional section name
                            if tokens.peek_is(TokenType::ConstExpression, None) {
                                let _expr = tokens.expect(TokenType::ConstExpression, None, "when parsing section declaration")?;
                                tokens.expect(TokenType::Comma, None, "after section name")?;
                            }

                            // Required Segment
                            let _segment = tokens.expect(TokenType::Segment, None, "when parsing section declaration")?;

                            // Check for optional offset
                            if tokens.peek_is(TokenType::OpenBracket, None) {
                                tokens.expect(TokenType::OpenBracket, None, "when parsing section offset")?;
                                let _offset = tokens.expect(TokenType::ConstExpression, None, "when parsing section offset")?;
                                tokens.expect(TokenType::CloseBracket, None, "when parsing section offset")?;
                            }

                            // Check for optional size
                            if tokens.peek_is(TokenType::OpenBracket, None) {
                                tokens.expect(TokenType::OpenBracket, None, "when parsing section size")?;
                                let _size = tokens.expect(TokenType::ConstExpression, None, "when parsing section size")?;
                                tokens.expect(TokenType::CloseBracket, None, "when parsing section size")?;
                            }

                            // Check for optional bank
                            if tokens.peek_is(TokenType::Comma, None) {
                                tokens.expect(TokenType::Comma, None, "when parsing section bank")?;
                                tokens.expect(TokenType::Reserved, Some("BANK"), "when parsing section bank")?;
                                tokens.expect(TokenType::OpenBracket, None, "when parsing section bank")?;
                                let _bank = tokens.expect(TokenType::ConstExpression, None, "when parsing section bank")?;
                                tokens.expect(TokenType::CloseBracket, None, "when parsing section bank")?;
                            }

                            continue;
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
        if let ExpressionToken::ConstExpression(_, _, expr) = token {
            Ok(EntryToken::Data {
                inner,
                alignment,
                endianess: DataEndianess::Little,
                storage: DataStorage::ByteRange(expr)
            })
        } else {
            unreachable!();
        }
    }

    fn parse_expression_list(tokens: &mut TokenIterator<ExpressionToken>) -> Result<Option<Vec<Expression>>, LexerError> {
        if tokens.peek_is(TokenType::Expression, None) || tokens.peek_is(TokenType::ConstExpression, None) {
            let mut expressions = Vec::new();
            while tokens.peek_is(TokenType::Expression, None) || tokens.peek_is(TokenType::ConstExpression, None) {
                let expr = tokens.get("when parsing expression list")?;
                match expr {
                    ExpressionToken::ConstExpression(_, _, expr) | ExpressionToken::Expression(_, _, expr) => {
                        expressions.push(expr);
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
        let lexer = expr_lex_binary("INCBIN 'child.bin'", b);
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
    }

    // Constant Declarations --------------------------------------------------
    #[test]
    fn test_const_declaration_equ() {
        assert_eq!(tfe("foo EQU 2"), vec![EntryToken::Constant {
            inner: itk!(0, 3, "foo"),
            name: "foo".to_string(),
            is_string: false,
            value: Expression::Value(ExpressionValue::Integer(2))
        }]);
        assert_eq!(tfe("foo EQU bar"), vec![EntryToken::Constant {
            inner: itk!(0, 3, "foo"),
            name: "foo".to_string(),
            is_string: false,
            value: Expression::Value(ExpressionValue::ConstantValue(
                itk!(8, 11, "bar"),
                "bar".to_string()
            ))
        }]);
    }

    #[test]
    fn test_const_declaration_equs() {
        assert_eq!(tfe("foo EQUS 'test'"), vec![EntryToken::Constant {
            inner: itk!(0, 3, "foo"),
            name: "foo".to_string(),
            is_string: true,
            value: Expression::Value(ExpressionValue::String("test".to_string()))
        }]);
        assert_eq!(tfe("foo EQUS bar"), vec![EntryToken::Constant {
            inner: itk!(0, 3, "foo"),
            name: "foo".to_string(),
            is_string: true,
            value: Expression::Value(ExpressionValue::ConstantValue(
                itk!(9, 12, "bar"),
                "bar".to_string()
            ))
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
            storage: DataStorage::Bytes(vec![Expression::Value(ExpressionValue::Integer(2))])
        }]);
        assert_eq!(tfe("DB 2 + 3, 1"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DB"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::Bytes(vec![
                Expression::Binary {
                    inner: itk!(5, 6, "+"),
                    op: Operator::Plus,
                    left: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
                },
                Expression::Value(ExpressionValue::Integer(1))
            ])
        }]);
        assert_eq!(tfe("DB 2, 3, 4, 5"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DB"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::Bytes(vec![
                Expression::Value(ExpressionValue::Integer(2)),
                Expression::Value(ExpressionValue::Integer(3)),
                Expression::Value(ExpressionValue::Integer(4)),
                Expression::Value(ExpressionValue::Integer(5))
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
            storage: DataStorage::Words(vec![Expression::Value(ExpressionValue::Integer(2000))])
        }]);
        assert_eq!(tfe("DW 2 + 3, 1"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DW"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::Words(vec![
                Expression::Binary {
                    inner: itk!(5, 6, "+"),
                    op: Operator::Plus,
                    left: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
                },
                Expression::Value(ExpressionValue::Integer(1))
            ])
        }]);
        assert_eq!(tfe("DW 2000, 3000, 4000, 5000"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DW"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::Words(vec![
                Expression::Value(ExpressionValue::Integer(2000)),
                Expression::Value(ExpressionValue::Integer(3000)),
                Expression::Value(ExpressionValue::Integer(4000)),
                Expression::Value(ExpressionValue::Integer(5000))
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
            storage: DataStorage::Words(vec![Expression::Value(ExpressionValue::Integer(2000))])
        }]);
        assert_eq!(tfe("BW 2 + 3, 1"), vec![EntryToken::Data {
            inner: itk!(0, 2, "BW"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Big,
            storage: DataStorage::Words(vec![
                Expression::Binary {
                    inner: itk!(5, 6, "+"),
                    op: Operator::Plus,
                    left: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                    right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
                },
                Expression::Value(ExpressionValue::Integer(1))
            ])
        }]);
        assert_eq!(tfe("BW 2000, 3000, 4000, 5000"), vec![EntryToken::Data {
            inner: itk!(0, 2, "BW"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Big,
            storage: DataStorage::Words(vec![
                Expression::Value(ExpressionValue::Integer(2000)),
                Expression::Value(ExpressionValue::Integer(3000)),
                Expression::Value(ExpressionValue::Integer(4000)),
                Expression::Value(ExpressionValue::Integer(5000))
            ])
        }]);
    }

    #[test]
    fn test_data_ds() {
        assert_eq!(tfe("DS 2 + 3"), vec![EntryToken::Data {
            inner: itk!(0, 2, "DS"),
            alignment: DataAlignment::Byte,
            endianess: DataEndianess::Little,
            storage: DataStorage::ByteRange(Expression::Binary {
                inner: itk!(5, 6, "+"),
                op: Operator::Plus,
                left: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
            })
        }]);
    }

    #[test]
    fn test_data_ds8() {
        assert_eq!(tfe("DS8 2 + 3"), vec![EntryToken::Data {
            inner: itk!(0, 3, "DS8"),
            alignment: DataAlignment::WithinWord,
            endianess: DataEndianess::Little,
            storage: DataStorage::ByteRange(Expression::Binary {
                inner: itk!(6, 7, "+"),
                op: Operator::Plus,
                left: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
            })
        }]);
    }

    #[test]
    fn test_data_ds16() {
        assert_eq!(tfe("DS16 2 + 3"), vec![EntryToken::Data {
            inner: itk!(0, 4, "DS16"),
            alignment: DataAlignment::Word,
            endianess: DataEndianess::Little,
            storage: DataStorage::ByteRange(Expression::Binary {
                inner: itk!(7, 8, "+"),
                op: Operator::Plus,
                left: Box::new(Expression::Value(ExpressionValue::Integer(2))),
                right: Box::new(Expression::Value(ExpressionValue::Integer(3)))
            })
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
        assert_eq!(tfe("SECTION ROM0"), vec![]);
    }

    #[test]
    fn test_section_with_name() {
        assert_eq!(tfe("SECTION 'Foo',ROM0"), vec![]);
    }

    #[test]
    fn test_section_with_offset() {
        assert_eq!(tfe("SECTION ROM0[$0000]"), vec![]);
    }

    #[test]
    fn test_section_with_offset_and_size() {
        assert_eq!(tfe("SECTION ROM0[$0000][$800]"), vec![]);
    }

    #[test]
    fn test_section_with_bank() {
        assert_eq!(tfe("SECTION ROM0,BANK[0]"), vec![]);
    }

    #[test]
    fn test_error_section() {
        assert_eq!(entry_lexer_error("SECTION"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input when parsing section declaration, expected a \"Segment\" token instead.\n\nSECTION\n^--- Here");
        assert_eq!(entry_lexer_error("SECTION ROM0,"), "In file \"main.gb.s\" on line 1, column 13: Unexpected end of input when parsing section bank, expected a \"Reserved\" token instead.\n\nSECTION ROM0,\n            ^--- Here");
        assert_eq!(entry_lexer_error("SECTION ROM0["), "In file \"main.gb.s\" on line 1, column 13: Unexpected end of input when parsing section offset, expected a \"ConstExpression\" token instead.\n\nSECTION ROM0[\n            ^--- Here");
        assert_eq!(entry_lexer_error("SECTION ROM0,BANK"), "In file \"main.gb.s\" on line 1, column 14: Unexpected end of input when parsing section bank, expected a \"OpenBracket\" token instead.\n\nSECTION ROM0,BANK\n             ^--- Here");
    }

}

