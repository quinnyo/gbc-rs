// External Dependencies ------------------------------------------------------
use gbasm_cpu::{Register, Flag, LexerArgument, InstructionLayouts, self};


// Internal Dependencies ------------------------------------------------------
use super::macros::MacroCall;
use crate::lexer::ExpressionStage;
use super::expression::{ExpressionToken, Expression, ExpressionValue, TEMPORARY_EXPRESSION_ID};
use super::super::{LexerStage, InnerToken, TokenIterator, TokenType, LexerToken, LexerError};


// Types ----------------------------------------------------------------------
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
    DebugInstruction((usize)),
    DebugInstructionWithArg((usize, DataExpression)),
    GlobalLabelDef((usize)),
    LocalLabelDef((usize))

}, {
    // Constant + EQU|EQUS + ConstExpression
    Constant {
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
        let layouts = gbasm_cpu::instruction_layouts();
        Self::parse_entry_tokens(tokens, &layouts)
    }

}

impl EntryStage {

    fn parse_entry_tokens(
        tokens: Vec<ExpressionToken>,
        layouts: &InstructionLayouts

    ) -> Result<Vec<EntryToken>, LexerError> {
        let mut entry_tokens = Vec::with_capacity(tokens.len());
        let mut tokens = TokenIterator::new(tokens);
        while let Some(token) = tokens.next() {
            let entry = match token {

                // Passthrough Label Defs
                ExpressionToken::GlobalLabelDef(inner, id) => EntryToken::GlobalLabelDef(inner, id),
                ExpressionToken::LocalLabelDef(inner, id) => EntryToken::LocalLabelDef(inner, id),

                // Constant Declarations
                ExpressionToken::Constant(inner) => {
                    if tokens.peek_is(TokenType::Reserved, Some("EQU")) {
                        tokens.expect(TokenType::Reserved, None, "when parsing constant declaration")?;
                        if let ExpressionToken::ConstExpression(_, id, expr) = tokens.expect(TokenType::ConstExpression, None, "when parsing constant declaration")? {
                            EntryToken::Constant {
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
                ExpressionToken::Instruction(inner) => Self::parse_instruction(&mut tokens, layouts, inner)?,
                ExpressionToken::MetaInstruction(inner) => {
                    entry_tokens.append(&mut Self::parse_meta_instruction(&mut tokens, inner)?);
                    continue;
                }

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

    fn parse_instruction(
        tokens: &mut TokenIterator<ExpressionToken>,
        layouts: &InstructionLayouts,
        inner: InnerToken

    ) -> Result<EntryToken, LexerError> {

        let max_arg_count = gbasm_cpu::instruction_max_arg_count(&inner.value);
        let mut expression: OptionalDataExpression = None;

        let mut layout = Vec::new();
        let mut arg_count = 0;
        let mut past_comma = false;
        let mut trailing_comma = None;

        // Parse Instruction Arguments Structure
        while arg_count < max_arg_count && arg_count < 2 {

            // Register arguments
            if tokens.peek_is(TokenType::Register, None) {
                trailing_comma = None;
                let reg = tokens.get("while parsing instruction register argument")?;
                if let ExpressionToken::Register { name, .. } = reg {

                    // Special casing for conditional instructions where "c" is the carry flag
                    // instead of a register if infront of the comma
                    if !past_comma && gbasm_cpu::instruction_is_conditional(&inner.value) && name == Register::C{
                        layout.push(LexerArgument::Flag(Flag::Carry));

                    } else {
                        layout.push(LexerArgument::Register(name));
                    }

                } else {
                    unreachable!();
                }

            // Flag must always be infront of a comma
            } else if tokens.peek_is(TokenType::Flag, None) && !past_comma {
                trailing_comma = None;
                let flag = tokens.get("while parsing instruction flag argument")?;
                if let ExpressionToken::Flag { typ, .. } = flag {
                    layout.push(LexerArgument::Flag(typ));

                } else {
                    unreachable!();
                }

            // Memory Locations must contain an expression or register
            } else if tokens.peek_is(TokenType::OpenBracket, None) {
                trailing_comma = None;
                tokens.expect(TokenType::OpenBracket, None, "while parsing instruction memory argument")?;
                if tokens.peek_is(TokenType::Register, None) {
                    let reg = tokens.expect(TokenType::Register, None, "while parsing instruction memory argument")?;
                    if let ExpressionToken::Register { name, .. } = reg {
                        layout.push(LexerArgument::MemoryLookupRegister(name));

                    } else {
                        unreachable!();
                    }

                } else if tokens.peek_is(TokenType::ConstExpression, None) {
                    let expr = tokens.expect(TokenType::ConstExpression, None, "while parsing instruction memory argument")?;
                    if let ExpressionToken::ConstExpression(_, id, expr) = expr {
                        layout.push(LexerArgument::MemoryLookupValue);
                        expression = Some((id, expr));

                    } else {
                        unreachable!();
                    }

                } else {
                    let expr = tokens.expect(TokenType::Expression, None, "while parsing instruction memory argument")?;
                    if let ExpressionToken::Expression(_, id, expr) = expr {
                        layout.push(LexerArgument::MemoryLookupValue);
                        expression = Some((id, expr));

                    } else {
                        unreachable!();
                    }
                }
                tokens.expect(TokenType::CloseBracket, None, "while parsing instruction memory argument")?;

            // Expression arguments
            } else if tokens.peek_is(TokenType::Expression, None) | tokens.peek_is(TokenType::ConstExpression, None) {
                trailing_comma = None;
                let expr = tokens.get("while parsing instruction register argument")?;
                if let ExpressionToken::ConstExpression(_, id, expr) | ExpressionToken::Expression(_, id, expr) = expr {
                    layout.push(LexerArgument::Value);
                    expression = Some((id, expr));

                } else {
                    unreachable!();
                }
            }

            arg_count += 1;

            // Check for a single following comma between arguments
            if tokens.peek_is(TokenType::Comma, None) && !past_comma {
                let inner = tokens.expect(TokenType::Comma, None, "while parsing instruction register argument")?.into_inner();
                past_comma = true;
                trailing_comma = Some(inner);
            }
        }

        if let Some(comma) = trailing_comma {
            Err(comma.error(
                format!("Unexpected trailing comma in \"{}\" instruction.", inner.value)
            ))

        } else if let Some(op_code) = layouts.get(&(inner.value.clone(), layout)) {
            if let Some(expression) = expression {
                Ok(EntryToken::InstructionWithArg(inner, *op_code, expression))

            } else {
                Ok(EntryToken::Instruction(inner, *op_code))
            }

        } else {
            Err(inner.error(
                format!("Unknown or invalid instruction \"{}\".", inner.value)
            ))
        }

    }

    fn parse_meta_instruction(
        tokens: &mut TokenIterator<ExpressionToken>,
        inner: InnerToken

    ) -> Result<Vec<EntryToken>, LexerError> {
        Ok(match inner.value.as_str() {

            // BGB debugging support
            "msg" => {
                let expr = tokens.get("Unexpected end of input while parsing instruction argument.")?;
                if let ExpressionToken::ConstExpression(_, _, Expression::Value(ExpressionValue::String(s))) = expr {
                    // TODO RomStage or something should drop debug instructions without -D flag
                    // TODO enforce string literal only expression directly?
                    /*
                    const arg = s.get('STRING');

                    // Only generate when `-d` flag is passed to compiler
                    if (this.debug) {

                        if (arg.value.length > 128 - 4) {
                            // TODO error for text that is too long
                        }

                        // TODO genrate data entry instead
                        const opcodes = [
                            0x52, // ld      d,d
                            0x18, // jr @+4+LEN(message)
                            4 + arg.value.length,
                            0x64, // DW $6464
                            0x64,
                            0x00, // DW $0000
                            0x00
                        ];

                        for(let i = 0, l = arg.value.length; i < l; i++) {
                            opcodes.push(arg.value.charCodeAt(i));
                        }
                        this.instruction(4 + 12, opcodes);

                    }*/
                    vec![

                    ]

                } else {
                    return Err(expr.error(
                        format!("Unexpected \"{}\", expected a string literal argument.", expr.value())
                    ));
                }
            },
            "brk" => {
                // TODO RomStage or something should drop debug instructions without -D flag
                // ld b,b
                vec![EntryToken::DebugInstruction(inner, 0x40)]
            },

            // Mulitply / Divide Shorthands
            "mul" => Self::parse_meta_div_mul(tokens, 288)?, // sla b
            "div" => Self::parse_meta_div_mul(tokens, 312)?, // srl b

            // Increment Memory Address Shorthands
            "incx" => {
                // decx [expr]
                let expr = Self::parse_meta_bracket_label(tokens)?;
                vec![
                    // ld a,[someLabel]
                    EntryToken::InstructionWithArg(inner.clone(), 0xFA, expr.clone()),
                    // inc a
                    EntryToken::Instruction(inner.clone(), 0x3C),
                    // ld [someLabel],a
                    EntryToken::InstructionWithArg(inner.clone(), 0xEA, expr),
                ]
            },
            "decx" => {
                // decx [expr]
                let expr = Self::parse_meta_bracket_label(tokens)?;
                vec![
                    // ld a,[someLabel]
                    EntryToken::InstructionWithArg(inner.clone(), 0xFA, expr.clone()),
                    // dec a
                    EntryToken::Instruction(inner.clone(), 0x3D),
                    // ld [someLabel],a
                    EntryToken::InstructionWithArg(inner.clone(), 0xEA, expr),
                ]
            },

            // 16 Bit Addition / Subtraction Shorthands
            "addw" => {
                // TODO bc, de, hl
                // addw xx,a
                // addw xx,$FF
                unreachable!();
            },
            "subw" => {
                // TODO bc, de, hl
                // subw xx,a
                // subw xx,$FF
                unreachable!();
            },

            // Extended Memory Loads using the Accumulator as an intermediate
            "ldxa" => {
                // TODO
                // ldxa   [hli],b
                // ldxa   [hli],c
                // ldxa   [hli],d
                // ldxa   [hli],e
                // ldxa   [hli],h
                // ldxa   [hli],l
                // ldxa   [hld],b
                // ldxa   [hld],c
                // ldxa   [hld],d
                // ldxa   [hld],e
                // ldxa   [hld],h
                // ldxa   [hld],l
                // ldxa   [someLabel],hl (h = low + l = high)
                // ldxa   [someLabel],de
                // ldxa   [someLabel],bc
                // ldxa   [someLabel],i8
                // ldxa   [someLabel],b
                // ldxa   [someLabel],c
                // ldxa   [someLabel],d
                // ldxa   [someLabel],e
                // ldxa   [someLabel],h
                // ldxa   [someLabel],l
                // ldxa   [someLabel],[hli]
                // ldxa   [someLabel],[hld]
                // ldxa   [someLabel],[someLabel]
                unreachable!();

            },

            // Return Shorthands
            "retx" => {
                // TODO
                // retx a,[hl]
                // retx a,[bc]
                // retx a,[de]
                // retx a,[someLabel]
                //
                // retx b
                // retx c
                // retx d
                // retx e
                // retx h
                // retx l
                // retx $ff
                unreachable!();
            },

            // VBlank Wait Shorthand
            "vsync" => {
                vec![
                    // ld      a,[$FF41]
                    EntryToken::InstructionWithArg(inner.clone(), 0xFA, (TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::Integer(0xFF41)))),
                    // and     %00000010
                    EntryToken::InstructionWithArg(inner.clone(), 0xE6, (TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::Integer(0b0000_0010)))),
                    // jr      nz,@-4
                    EntryToken::InstructionWithArg(inner.clone(), 0x20, (TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::OffsetAddress(inner, -4)))),
                ]
            },
            _ => unreachable!()
        })
    }

    fn parse_meta_div_mul(tokens: &mut TokenIterator<ExpressionToken>, op_base: usize) -> Result<Vec<EntryToken>, LexerError> {
        let reg = Self::parse_byte_register(tokens)?;
        tokens.expect(TokenType::Comma, None, "while parsing instruction arguments")?;
        let expr = tokens.get("Unexpected end of input while parsing instruction arguments.")?;
        if let ExpressionToken::ConstExpression(_, _, Expression::Value(ExpressionValue::Integer(i))) = expr {
            if i > 0 && (i as u32).is_power_of_two() && i < 128 {
                // TODO generate extended instruction
                let op = op_base + reg.instruction_offset();
                // TODO generate mulitple instructions based on power of two above
                Ok(vec![])

            } else {
                Err(expr.error(
                    format!("Unexpected \"{}\", expected a integer argument that is a power 2 and <= 128.", expr.value())
                ))
            }

        } else {
            Err(expr.error(
                format!("Unexpected \"{}\", expected a integer argument that is a power 2 and <= 128.", expr.value())
            ))
        }
    }

    fn parse_byte_register(tokens: &mut TokenIterator<ExpressionToken>) -> Result<Register, LexerError> {
        let reg = tokens.expect(TokenType::Register, None, "while parsing instruction arguments")?;
        if let ExpressionToken::Register { inner, name } = reg {
            if name.width() == 1 {
                Ok(name)

            } else {
                Err(inner.error(
                    format!("Unexpected \"{}\", expected one of the following registers: a, b, c, d, e, h, l.", inner.value)
                ))
            }

        } else {
            unreachable!();
        }
    }

    fn parse_meta_bracket_label(tokens: &mut TokenIterator<ExpressionToken>) -> Result<DataExpression, LexerError> {
        tokens.expect(TokenType::OpenBracket, Some("["), "while parsing instruction label argument")?;
        let expr = tokens.get("Unexpected end of input while parsing instruction label argument.")?;
        if let ExpressionToken::ConstExpression(_, id, expr) | ExpressionToken::Expression(_, id, expr) = expr {
            tokens.expect(TokenType::CloseBracket, Some("]"), "while parsing instruction label argument")?;
            Ok((id, expr))

        } else {
            Err(expr.error(format!("Unexpected \"{}\", expected a expression as the label argument instead.", expr.value())))
        }
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
    use super::super::expression::{Expression, ExpressionValue, TEMPORARY_EXPRESSION_ID};
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
        assert_eq!(tfe("global_label:"), vec![EntryToken::GlobalLabelDef(
            itk!(0, 13, "global_label"),
            1
        )]);
    }

    #[test]
    fn test_passthrough_local_label_def() {
        assert_eq!(tfe("global_label:\n.local_label:"), vec![EntryToken::GlobalLabelDef(
            itk!(0, 13, "global_label"),
            1

        ), EntryToken::LocalLabelDef(
            itk!(14, 27, "local_label"),
            2
        )]);
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
            is_string: false,
            value: (0, Expression::Value(ExpressionValue::Integer(2)))
        }]);
        assert_eq!(tfe("foo EQU bar"), vec![EntryToken::Constant {
            inner: itk!(0, 3, "foo"),
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
            is_string: true,
            value: (0, Expression::Value(ExpressionValue::String("test".to_string())))
        }]);
        assert_eq!(tfe("foo EQUS bar"), vec![EntryToken::Constant {
            inner: itk!(0, 3, "foo"),
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

    // Instructions -----------------------------------------------------------
    macro_rules! op {
        ($mnemonic:expr, $op:expr, $arg:expr) => {
            EntryToken::InstructionWithArg(
                InnerToken::new(0, 0, $mnemonic.len(), $mnemonic.into()),
                $op,
                (0, Expression::Value(ExpressionValue::Integer($arg)))
            )
        };
        ($mnemonic:expr, $op:expr) => {
            EntryToken::Instruction(
                InnerToken::new(0, 0, $mnemonic.len(), $mnemonic.into()),
                $op
            )
        }
    }

    macro_rules! assert_op {
        ($op:expr, $layout:expr, $arg:expr) => {
            assert_eq!(tfe($layout), vec![op!($layout.split(" ").next().unwrap(), $op, $arg)]);
        };
        ($op:expr, $layout:expr) => {
            assert_eq!(tfe($layout), vec![op!($layout.split(" ").next().unwrap(), $op)]);
        }
    }

    #[test]
    fn test_instructions() {
        assert_op!(0, "nop");
        assert_op!(1, "ld bc,$1234", 4660);
        assert_op!(2, "ld [bc],a");
        assert_op!(3, "inc bc");
        assert_op!(4, "inc b");
        assert_op!(5, "dec b");
        assert_op!(6, "ld b,$20", 32);
        assert_op!(7, "rlca");
        assert_op!(8, "ld [$1234],sp", 4660);
        assert_op!(9, "add hl,bc");
        assert_op!(10, "ld a,[bc]");
        assert_op!(11, "dec bc");
        assert_op!(12, "inc c");
        assert_op!(13, "dec c");
        assert_op!(14, "ld c,$20", 32);
        assert_op!(15, "rrca");
        assert_op!(16, "stop");
        assert_op!(17, "ld de,$1234", 4660);
        assert_op!(18, "ld [de],a");
        assert_op!(19, "inc de");
        assert_op!(20, "inc d");
        assert_op!(21, "dec d");
        assert_op!(22, "ld d,$20", 32);
        assert_op!(23, "rla");
        assert_op!(24, "jr $20", 32);
        assert_op!(25, "add hl,de");
        assert_op!(26, "ld a,[de]");
        assert_op!(27, "dec de");
        assert_op!(28, "inc e");
        assert_op!(29, "dec e");
        assert_op!(30, "ld e,$20", 32);
        assert_op!(31, "rra");
        assert_op!(32, "jr nz,$20", 32);
        assert_op!(33, "ld hl,$1234", 4660);
        assert_op!(34, "ld [hli],a");
        assert_op!(35, "inc hl");
        assert_op!(36, "inc h");
        assert_op!(37, "dec h");
        assert_op!(38, "ld h,$20", 32);
        assert_op!(39, "daa");
        assert_op!(40, "jr z,$20", 32);
        assert_op!(41, "add hl,hl");
        assert_op!(42, "ld a,[hli]");
        assert_op!(43, "dec hl");
        assert_op!(44, "inc l");
        assert_op!(45, "dec l");
        assert_op!(46, "ld l,$20", 32);
        assert_op!(47, "cpl");
        assert_op!(48, "jr nc,$20", 32);
        assert_op!(49, "ld sp,$1234", 4660);
        assert_op!(50, "ld [hld],a");
        assert_op!(51, "inc sp");
        assert_op!(52, "inc [hl]");
        assert_op!(53, "dec [hl]");
        assert_op!(54, "ld [hl],$20", 32);
        assert_op!(55, "scf");
        assert_op!(56, "jr c,$20", 32);
        assert_op!(57, "add hl,sp");
        assert_op!(58, "ld a,[hld]");
        assert_op!(59, "dec sp");
        assert_op!(60, "inc a");
        assert_op!(61, "dec a");
        assert_op!(62, "ld a,$20", 32);
        assert_op!(63, "ccf");
        assert_op!(64, "ld b,b");
        assert_op!(65, "ld b,c");
        assert_op!(66, "ld b,d");
        assert_op!(67, "ld b,e");
        assert_op!(68, "ld b,h");
        assert_op!(69, "ld b,l");
        assert_op!(70, "ld b,[hl]");
        assert_op!(71, "ld b,a");
        assert_op!(72, "ld c,b");
        assert_op!(73, "ld c,c");
        assert_op!(74, "ld c,d");
        assert_op!(75, "ld c,e");
        assert_op!(76, "ld c,h");
        assert_op!(77, "ld c,l");
        assert_op!(78, "ld c,[hl]");
        assert_op!(79, "ld c,a");
        assert_op!(80, "ld d,b");
        assert_op!(81, "ld d,c");
        assert_op!(82, "ld d,d");
        assert_op!(83, "ld d,e");
        assert_op!(84, "ld d,h");
        assert_op!(85, "ld d,l");
        assert_op!(86, "ld d,[hl]");
        assert_op!(87, "ld d,a");
        assert_op!(88, "ld e,b");
        assert_op!(89, "ld e,c");
        assert_op!(90, "ld e,d");
        assert_op!(91, "ld e,e");
        assert_op!(92, "ld e,h");
        assert_op!(93, "ld e,l");
        assert_op!(94, "ld e,[hl]");
        assert_op!(95, "ld e,a");
        assert_op!(96, "ld h,b");
        assert_op!(97, "ld h,c");
        assert_op!(98, "ld h,d");
        assert_op!(99, "ld h,e");
        assert_op!(100, "ld h,h");
        assert_op!(101, "ld h,l");
        assert_op!(102, "ld h,[hl]");
        assert_op!(103, "ld h,a");
        assert_op!(104, "ld l,b");
        assert_op!(105, "ld l,c");
        assert_op!(106, "ld l,d");
        assert_op!(107, "ld l,e");
        assert_op!(108, "ld l,h");
        assert_op!(109, "ld l,l");
        assert_op!(110, "ld l,[hl]");
        assert_op!(111, "ld l,a");
        assert_op!(112, "ld [hl],b");
        assert_op!(113, "ld [hl],c");
        assert_op!(114, "ld [hl],d");
        assert_op!(115, "ld [hl],e");
        assert_op!(116, "ld [hl],h");
        assert_op!(117, "ld [hl],l");
        assert_op!(118, "halt");
        assert_op!(119, "ld [hl],a");
        assert_op!(120, "ld a,b");
        assert_op!(121, "ld a,c");
        assert_op!(122, "ld a,d");
        assert_op!(123, "ld a,e");
        assert_op!(124, "ld a,h");
        assert_op!(125, "ld a,l");
        assert_op!(126, "ld a,[hl]");
        assert_op!(127, "ld a,a");
        assert_op!(128, "add b");
        assert_op!(129, "add c");
        assert_op!(130, "add d");
        assert_op!(131, "add e");
        assert_op!(132, "add h");
        assert_op!(133, "add l");
        assert_op!(134, "add [hl]");
        assert_op!(135, "add a");
        assert_op!(136, "adc b");
        assert_op!(137, "adc c");
        assert_op!(138, "adc d");
        assert_op!(139, "adc e");
        assert_op!(140, "adc h");
        assert_op!(141, "adc l");
        assert_op!(142, "adc [hl]");
        assert_op!(143, "adc a");
        assert_op!(144, "sub b");
        assert_op!(145, "sub c");
        assert_op!(146, "sub d");
        assert_op!(147, "sub e");
        assert_op!(148, "sub h");
        assert_op!(149, "sub l");
        assert_op!(150, "sub [hl]");
        assert_op!(151, "sub a");
        assert_op!(152, "sbc b");
        assert_op!(153, "sbc c");
        assert_op!(154, "sbc d");
        assert_op!(155, "sbc e");
        assert_op!(156, "sbc h");
        assert_op!(157, "sbc l");
        assert_op!(158, "sbc [hl]");
        assert_op!(159, "sbc a");
        assert_op!(160, "and b");
        assert_op!(161, "and c");
        assert_op!(162, "and d");
        assert_op!(163, "and e");
        assert_op!(164, "and h");
        assert_op!(165, "and l");
        assert_op!(166, "and [hl]");
        assert_op!(167, "and a");
        assert_op!(168, "xor b");
        assert_op!(169, "xor c");
        assert_op!(170, "xor d");
        assert_op!(171, "xor e");
        assert_op!(172, "xor h");
        assert_op!(173, "xor l");
        assert_op!(174, "xor [hl]");
        assert_op!(175, "xor a");
        assert_op!(176, "or b");
        assert_op!(177, "or c");
        assert_op!(178, "or d");
        assert_op!(179, "or e");
        assert_op!(180, "or h");
        assert_op!(181, "or l");
        assert_op!(182, "or [hl]");
        assert_op!(183, "or a");
        assert_op!(184, "cp b");
        assert_op!(185, "cp c");
        assert_op!(186, "cp d");
        assert_op!(187, "cp e");
        assert_op!(188, "cp h");
        assert_op!(189, "cp l");
        assert_op!(190, "cp [hl]");
        assert_op!(191, "cp a");
        assert_op!(192, "ret nz");
        assert_op!(193, "pop bc");
        assert_op!(194, "jp nz,$1234", 4660);
        assert_op!(195, "jp $1234", 4660);
        assert_op!(196, "call nz,$1234", 4660);
        assert_op!(197, "push bc");
        assert_op!(198, "add $20", 32);
        assert_op!(199, "rst $00", 0x00);
        assert_op!(200, "ret z");
        assert_op!(201, "ret");
        assert_op!(202, "jp z,$1234", 4660);
        // assert_op!(203, "prefix cb");
        assert_op!(204, "call z,$1234", 4660);
        assert_op!(205, "call $1234", 4660);
        assert_op!(206, "adc $20", 32);
        assert_op!(199, "rst $08", 0x08);
        assert_op!(208, "ret nc");
        assert_op!(209, "pop de");
        assert_op!(210, "jp nc,$1234", 4660);
        // assert_op!(211, "invalid");
        assert_op!(212, "call nc,$1234", 4660);
        assert_op!(213, "push de");
        assert_op!(214, "sub $20", 32);
        assert_op!(199, "rst $10", 0x10);
        assert_op!(216, "ret c");
        assert_op!(217, "reti");
        assert_op!(218, "jp c,$1234", 4660);
        // assert_op!(219, "invalid");
        assert_op!(220, "call c,$1234", 4660);
        // assert_op!(221, "invalid");
        assert_op!(222, "sbc $20", 32);
        assert_op!(199, "rst $18", 0x18);
        assert_op!(224, "ldh [$20],a", 32);
        assert_op!(225, "pop hl");
        assert_op!(226, "ld [c],a");
        // assert_op!(227, "invalid");
        // assert_op!(228, "invalid");
        assert_op!(229, "push hl");
        assert_op!(230, "and $20", 32);
        assert_op!(199, "rst $20", 0x20);
        assert_op!(232, "add sp,$20", 32);
        assert_op!(233, "jp [hl]");
        assert_op!(234, "ld [$1234],a", 4660);
        // assert_op!(235, "invalid");
        // assert_op!(236, "invalid");
        // assert_op!(237, "invalid");
        assert_op!(238, "xor $20", 32);
        assert_op!(199, "rst $28", 0x28);
        assert_op!(240, "ldh a,[$20]", 32);
        assert_op!(241, "pop af");
        assert_op!(242, "ld a,[c]");
        assert_op!(243, "di");
        // assert_op!(244, "invalid");
        assert_op!(245, "push af");
        assert_op!(246, "or $20", 32);
        assert_op!(199, "rst $30", 0x30);
        assert_op!(248, "ldsp hl,$20", 32);
        assert_op!(249, "ld sp,hl");
        assert_op!(250, "ld a,[$1234]", 4660);
        assert_op!(251, "ei");
        // assert_op!(252, "invalid");
        // assert_op!(253, "invalid");
        assert_op!(254, "cp $20", 32);
        assert_op!(199, "rst $38", 0x38);

    }

    #[test]
    fn test_instructions_ex() {
        assert_op!(256, "rlc b");
        assert_op!(257, "rlc c");
        assert_op!(258, "rlc d");
        assert_op!(259, "rlc e");
        assert_op!(260, "rlc h");
        assert_op!(261, "rlc l");
        assert_op!(262, "rlc [hl]");
        assert_op!(263, "rlc a");
        assert_op!(264, "rrc b");
        assert_op!(265, "rrc c");
        assert_op!(266, "rrc d");
        assert_op!(267, "rrc e");
        assert_op!(268, "rrc h");
        assert_op!(269, "rrc l");
        assert_op!(270, "rrc [hl]");
        assert_op!(271, "rrc a");
        assert_op!(272, "rl b");
        assert_op!(273, "rl c");
        assert_op!(274, "rl d");
        assert_op!(275, "rl e");
        assert_op!(276, "rl h");
        assert_op!(277, "rl l");
        assert_op!(278, "rl [hl]");
        assert_op!(279, "rl a");
        assert_op!(280, "rr b");
        assert_op!(281, "rr c");
        assert_op!(282, "rr d");
        assert_op!(283, "rr e");
        assert_op!(284, "rr h");
        assert_op!(285, "rr l");
        assert_op!(286, "rr [hl]");
        assert_op!(287, "rr a");
        assert_op!(288, "sla b");
        assert_op!(289, "sla c");
        assert_op!(290, "sla d");
        assert_op!(291, "sla e");
        assert_op!(292, "sla h");
        assert_op!(293, "sla l");
        assert_op!(294, "sla [hl]");
        assert_op!(295, "sla a");
        assert_op!(296, "sra b");
        assert_op!(297, "sra c");
        assert_op!(298, "sra d");
        assert_op!(299, "sra e");
        assert_op!(300, "sra h");
        assert_op!(301, "sra l");
        assert_op!(302, "sra [hl]");
        assert_op!(303, "sra a");
        assert_op!(304, "swap b");
        assert_op!(305, "swap c");
        assert_op!(306, "swap d");
        assert_op!(307, "swap e");
        assert_op!(308, "swap h");
        assert_op!(309, "swap l");
        assert_op!(310, "swap [hl]");
        assert_op!(311, "swap a");
        assert_op!(312, "srl b");
        assert_op!(313, "srl c");
        assert_op!(314, "srl d");
        assert_op!(315, "srl e");
        assert_op!(316, "srl h");
        assert_op!(317, "srl l");
        assert_op!(318, "srl [hl]");
        assert_op!(319, "srl a");

        assert_op!(320, "bit 0,b", 0);
        assert_op!(321, "bit 0,c", 0);
        assert_op!(322, "bit 0,d", 0);
        assert_op!(323, "bit 0,e", 0);
        assert_op!(324, "bit 0,h", 0);
        assert_op!(325, "bit 0,l", 0);
        assert_op!(326, "bit 0,[hl]", 0);
        assert_op!(327, "bit 0,a", 0);

        assert_op!(320, "bit 1,b", 1);
        assert_op!(321, "bit 1,c", 1);
        assert_op!(322, "bit 1,d", 1);
        assert_op!(323, "bit 1,e", 1);
        assert_op!(324, "bit 1,h", 1);
        assert_op!(325, "bit 1,l", 1);
        assert_op!(326, "bit 1,[hl]", 1);
        assert_op!(327, "bit 1,a", 1);

        assert_op!(320, "bit 2,b", 2);
        assert_op!(321, "bit 2,c", 2);
        assert_op!(322, "bit 2,d", 2);
        assert_op!(323, "bit 2,e", 2);
        assert_op!(324, "bit 2,h", 2);
        assert_op!(325, "bit 2,l", 2);
        assert_op!(326, "bit 2,[hl]", 2);
        assert_op!(327, "bit 2,a", 2);

        assert_op!(320, "bit 3,b", 3);
        assert_op!(321, "bit 3,c", 3);
        assert_op!(322, "bit 3,d", 3);
        assert_op!(323, "bit 3,e", 3);
        assert_op!(324, "bit 3,h", 3);
        assert_op!(325, "bit 3,l", 3);
        assert_op!(326, "bit 3,[hl]", 3);
        assert_op!(327, "bit 3,a", 3);

        assert_op!(320, "bit 4,b", 4);
        assert_op!(321, "bit 4,c", 4);
        assert_op!(322, "bit 4,d", 4);
        assert_op!(323, "bit 4,e", 4);
        assert_op!(324, "bit 4,h", 4);
        assert_op!(325, "bit 4,l", 4);
        assert_op!(326, "bit 4,[hl]", 4);
        assert_op!(327, "bit 4,a", 4);

        assert_op!(320, "bit 5,b", 5);
        assert_op!(321, "bit 5,c", 5);
        assert_op!(322, "bit 5,d", 5);
        assert_op!(323, "bit 5,e", 5);
        assert_op!(324, "bit 5,h", 5);
        assert_op!(325, "bit 5,l", 5);
        assert_op!(326, "bit 5,[hl]", 5);
        assert_op!(327, "bit 5,a", 5);

        assert_op!(320, "bit 6,b", 6);
        assert_op!(321, "bit 6,c", 6);
        assert_op!(322, "bit 6,d", 6);
        assert_op!(323, "bit 6,e", 6);
        assert_op!(324, "bit 6,h", 6);
        assert_op!(325, "bit 6,l", 6);
        assert_op!(326, "bit 6,[hl]", 6);
        assert_op!(327, "bit 6,a", 6);

        assert_op!(320, "bit 7,b", 7);
        assert_op!(321, "bit 7,c", 7);
        assert_op!(322, "bit 7,d", 7);
        assert_op!(323, "bit 7,e", 7);
        assert_op!(324, "bit 7,h", 7);
        assert_op!(325, "bit 7,l", 7);
        assert_op!(326, "bit 7,[hl]", 7);
        assert_op!(327, "bit 7,a", 7);

        assert_op!(384, "res 0,b", 0);
        assert_op!(385, "res 0,c", 0);
        assert_op!(386, "res 0,d", 0);
        assert_op!(387, "res 0,e", 0);
        assert_op!(388, "res 0,h", 0);
        assert_op!(389, "res 0,l", 0);
        assert_op!(390, "res 0,[hl]", 0);
        assert_op!(391, "res 0,a", 0);

        assert_op!(384, "res 1,b", 1);
        assert_op!(385, "res 1,c", 1);
        assert_op!(386, "res 1,d", 1);
        assert_op!(387, "res 1,e", 1);
        assert_op!(388, "res 1,h", 1);
        assert_op!(389, "res 1,l", 1);
        assert_op!(390, "res 1,[hl]", 1);
        assert_op!(391, "res 1,a", 1);

        assert_op!(384, "res 2,b", 2);
        assert_op!(385, "res 2,c", 2);
        assert_op!(386, "res 2,d", 2);
        assert_op!(387, "res 2,e", 2);
        assert_op!(388, "res 2,h", 2);
        assert_op!(389, "res 2,l", 2);
        assert_op!(390, "res 2,[hl]", 2);
        assert_op!(391, "res 2,a", 2);

        assert_op!(384, "res 3,b", 3);
        assert_op!(385, "res 3,c", 3);
        assert_op!(386, "res 3,d", 3);
        assert_op!(387, "res 3,e", 3);
        assert_op!(388, "res 3,h", 3);
        assert_op!(389, "res 3,l", 3);
        assert_op!(390, "res 3,[hl]", 3);
        assert_op!(391, "res 3,a", 3);

        assert_op!(384, "res 4,b", 4);
        assert_op!(385, "res 4,c", 4);
        assert_op!(386, "res 4,d", 4);
        assert_op!(387, "res 4,e", 4);
        assert_op!(388, "res 4,h", 4);
        assert_op!(389, "res 4,l", 4);
        assert_op!(390, "res 4,[hl]", 4);
        assert_op!(391, "res 4,a", 4);

        assert_op!(384, "res 5,b", 5);
        assert_op!(385, "res 5,c", 5);
        assert_op!(386, "res 5,d", 5);
        assert_op!(387, "res 5,e", 5);
        assert_op!(388, "res 5,h", 5);
        assert_op!(389, "res 5,l", 5);
        assert_op!(390, "res 5,[hl]", 5);
        assert_op!(391, "res 5,a", 5);

        assert_op!(384, "res 6,b", 6);
        assert_op!(385, "res 6,c", 6);
        assert_op!(386, "res 6,d", 6);
        assert_op!(387, "res 6,e", 6);
        assert_op!(388, "res 6,h", 6);
        assert_op!(389, "res 6,l", 6);
        assert_op!(390, "res 6,[hl]", 6);
        assert_op!(391, "res 6,a", 6);

        assert_op!(384, "res 7,b", 7);
        assert_op!(385, "res 7,c", 7);
        assert_op!(386, "res 7,d", 7);
        assert_op!(387, "res 7,e", 7);
        assert_op!(388, "res 7,h", 7);
        assert_op!(389, "res 7,l", 7);
        assert_op!(390, "res 7,[hl]", 7);
        assert_op!(391, "res 7,a", 7);

        assert_op!(448, "set 0,b", 0);
        assert_op!(449, "set 0,c", 0);
        assert_op!(450, "set 0,d", 0);
        assert_op!(451, "set 0,e", 0);
        assert_op!(452, "set 0,h", 0);
        assert_op!(453, "set 0,l", 0);
        assert_op!(454, "set 0,[hl]", 0);
        assert_op!(455, "set 0,a", 0);

        assert_op!(448, "set 1,b", 1);
        assert_op!(449, "set 1,c", 1);
        assert_op!(450, "set 1,d", 1);
        assert_op!(451, "set 1,e", 1);
        assert_op!(452, "set 1,h", 1);
        assert_op!(453, "set 1,l", 1);
        assert_op!(454, "set 1,[hl]", 1);
        assert_op!(455, "set 1,a", 1);

        assert_op!(448, "set 2,b", 2);
        assert_op!(449, "set 2,c", 2);
        assert_op!(450, "set 2,d", 2);
        assert_op!(451, "set 2,e", 2);
        assert_op!(452, "set 2,h", 2);
        assert_op!(453, "set 2,l", 2);
        assert_op!(454, "set 2,[hl]", 2);
        assert_op!(455, "set 2,a", 2);

        assert_op!(448, "set 3,b", 3);
        assert_op!(449, "set 3,c", 3);
        assert_op!(450, "set 3,d", 3);
        assert_op!(451, "set 3,e", 3);
        assert_op!(452, "set 3,h", 3);
        assert_op!(453, "set 3,l", 3);
        assert_op!(454, "set 3,[hl]", 3);
        assert_op!(455, "set 3,a", 3);

        assert_op!(448, "set 4,b", 4);
        assert_op!(449, "set 4,c", 4);
        assert_op!(450, "set 4,d", 4);
        assert_op!(451, "set 4,e", 4);
        assert_op!(452, "set 4,h", 4);
        assert_op!(453, "set 4,l", 4);
        assert_op!(454, "set 4,[hl]", 4);
        assert_op!(455, "set 4,a", 4);

        assert_op!(448, "set 5,b", 5);
        assert_op!(449, "set 5,c", 5);
        assert_op!(450, "set 5,d", 5);
        assert_op!(451, "set 5,e", 5);
        assert_op!(452, "set 5,h", 5);
        assert_op!(453, "set 5,l", 5);
        assert_op!(454, "set 5,[hl]", 5);
        assert_op!(455, "set 5,a", 5);

        assert_op!(448, "set 6,b", 6);
        assert_op!(449, "set 6,c", 6);
        assert_op!(450, "set 6,d", 6);
        assert_op!(451, "set 6,e", 6);
        assert_op!(452, "set 6,h", 6);
        assert_op!(453, "set 6,l", 6);
        assert_op!(454, "set 6,[hl]", 6);
        assert_op!(455, "set 6,a", 6);

        assert_op!(448, "set 7,b", 7);
        assert_op!(449, "set 7,c", 7);
        assert_op!(450, "set 7,d", 7);
        assert_op!(451, "set 7,e", 7);
        assert_op!(452, "set 7,h", 7);
        assert_op!(453, "set 7,l", 7);
        assert_op!(454, "set 7,[hl]", 7);
        assert_op!(455, "set 7,a", 7);
    }

    #[test]
    fn test_error_instructions() {
        assert_eq!(entry_lexer_error("ld"), "In file \"main.gb.s\" on line 1, column 1: Unknown or invalid instruction \"ld\".\n\nld\n^--- Here");
        assert_eq!(entry_lexer_error("ld 4,["), "In file \"main.gb.s\" on line 1, column 6: Unexpected end of input while parsing instruction memory argument, expected a \"Expression\" token instead.\n\nld 4,[\n     ^--- Here");
        assert_eq!(entry_lexer_error("ld 4,[3"), "In file \"main.gb.s\" on line 1, column 7: Unexpected end of input while parsing instruction memory argument, expected a \"CloseBracket\" token instead.\n\nld 4,[3\n      ^--- Here");
        assert_eq!(entry_lexer_error("stop 4"), "In file \"main.gb.s\" on line 1, column 6: Unexpected \"4\", expected either a constant declaration, directive or instruction instead.\n\nstop 4\n     ^--- Here");
        assert_eq!(entry_lexer_error("ld a,"), "In file \"main.gb.s\" on line 1, column 5: Unexpected trailing comma in \"ld\" instruction.\n\nld a,\n    ^--- Here");
    }

    // Meta Instructions ------------------------------------------------------
    #[test]
    fn test_meta_instruction_msg() {
        assert_eq!(tfe("msg 'Hello World'"), vec![]);
    }

    #[test]
    fn test_error_meta_instruction_msg() {
        assert_eq!(entry_lexer_error("msg"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input while parsing instruction argument.\n\nmsg\n^--- Here");
        assert_eq!(entry_lexer_error("msg 4"), "In file \"main.gb.s\" on line 1, column 5: Unexpected \"4\", expected a string literal argument.\n\nmsg 4\n    ^--- Here");
    }

    #[test]
    fn test_meta_instruction_brk() {
        assert_eq!(tfe("brk"), vec![
            EntryToken::DebugInstruction(itk!(0, 3, "brk"), 64)
        ]);
    }

    #[test]
    fn test_meta_instruction_mul() {
        assert_eq!(tfe("mul a,2"), vec![]);
        assert_eq!(tfe("mul b,2"), vec![]);
        assert_eq!(tfe("mul c,2"), vec![]);
        assert_eq!(tfe("mul d,2"), vec![]);
        assert_eq!(tfe("mul e,2"), vec![]);
        assert_eq!(tfe("mul h,2"), vec![]);
        assert_eq!(tfe("mul l,2"), vec![]);
    }

    #[test]
    fn test_error_meta_instruction_mul() {
        assert_eq!(entry_lexer_error("mul"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input while parsing instruction arguments, expected a \"Register\" token instead.\n\nmul\n^--- Here");
        assert_eq!(entry_lexer_error("mul hl"), "In file \"main.gb.s\" on line 1, column 5: Unexpected \"hl\", expected one of the following registers: a, b, c, d, e, h, l.\n\nmul hl\n    ^--- Here");
        assert_eq!(entry_lexer_error("mul a"), "In file \"main.gb.s\" on line 1, column 5: Unexpected end of input while parsing instruction arguments, expected a \"Comma\" token instead.\n\nmul a\n    ^--- Here");
        assert_eq!(entry_lexer_error("mul a, 'Foo'"), "In file \"main.gb.s\" on line 1, column 8: Unexpected \"Foo\", expected a integer argument that is a power 2 and <= 128.\n\nmul a, \'Foo\'\n       ^--- Here");
        assert_eq!(entry_lexer_error("mul a, -2"), "In file \"main.gb.s\" on line 1, column 8: Unexpected \"-2\", expected a integer argument that is a power 2 and <= 128.\n\nmul a, -2\n       ^--- Here");
        assert_eq!(entry_lexer_error("mul a, 3"), "In file \"main.gb.s\" on line 1, column 8: Unexpected \"3\", expected a integer argument that is a power 2 and <= 128.\n\nmul a, 3\n       ^--- Here");
        assert_eq!(entry_lexer_error("mul a, 0"), "In file \"main.gb.s\" on line 1, column 8: Unexpected \"0\", expected a integer argument that is a power 2 and <= 128.\n\nmul a, 0\n       ^--- Here");
        assert_eq!(entry_lexer_error("mul a, 256"), "In file \"main.gb.s\" on line 1, column 8: Unexpected \"256\", expected a integer argument that is a power 2 and <= 128.\n\nmul a, 256\n       ^--- Here");
    }

    #[test]
    fn test_meta_instruction_div() {
        assert_eq!(tfe("div a,2"), vec![]);
        assert_eq!(tfe("div a,2"), vec![]);
        assert_eq!(tfe("div b,2"), vec![]);
        assert_eq!(tfe("div c,2"), vec![]);
        assert_eq!(tfe("div d,2"), vec![]);
        assert_eq!(tfe("div e,2"), vec![]);
        assert_eq!(tfe("div h,2"), vec![]);
        assert_eq!(tfe("div l,2"), vec![]);
    }

    #[test]
    fn test_error_meta_instruction_div() {
        assert_eq!(entry_lexer_error("div"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input while parsing instruction arguments, expected a \"Register\" token instead.\n\ndiv\n^--- Here");
        assert_eq!(entry_lexer_error("div hl"), "In file \"main.gb.s\" on line 1, column 5: Unexpected \"hl\", expected one of the following registers: a, b, c, d, e, h, l.\n\ndiv hl\n    ^--- Here");
        assert_eq!(entry_lexer_error("div a"), "In file \"main.gb.s\" on line 1, column 5: Unexpected end of input while parsing instruction arguments, expected a \"Comma\" token instead.\n\ndiv a\n    ^--- Here");
        assert_eq!(entry_lexer_error("div a, 'Foo'"), "In file \"main.gb.s\" on line 1, column 8: Unexpected \"Foo\", expected a integer argument that is a power 2 and <= 128.\n\ndiv a, \'Foo\'\n       ^--- Here");
        assert_eq!(entry_lexer_error("div a, -2"), "In file \"main.gb.s\" on line 1, column 8: Unexpected \"-2\", expected a integer argument that is a power 2 and <= 128.\n\ndiv a, -2\n       ^--- Here");
        assert_eq!(entry_lexer_error("div a, 3"), "In file \"main.gb.s\" on line 1, column 8: Unexpected \"3\", expected a integer argument that is a power 2 and <= 128.\n\ndiv a, 3\n       ^--- Here");
        assert_eq!(entry_lexer_error("div a, 0"), "In file \"main.gb.s\" on line 1, column 8: Unexpected \"0\", expected a integer argument that is a power 2 and <= 128.\n\ndiv a, 0\n       ^--- Here");
        assert_eq!(entry_lexer_error("div a, 256"), "In file \"main.gb.s\" on line 1, column 8: Unexpected \"256\", expected a integer argument that is a power 2 and <= 128.\n\ndiv a, 256\n       ^--- Here");
    }

    #[test]
    fn test_meta_instruction_incx() {
        assert_eq!(tfe("incx [$1234]"), vec![
            EntryToken::InstructionWithArg(itk!(0, 4, "incx"), 0xFA, (0, Expression::Value(ExpressionValue::Integer(0x1234)))),
            EntryToken::Instruction(itk!(0, 4, "incx"), 0x3C),
            EntryToken::InstructionWithArg(itk!(0, 4, "incx"), 0xEA, (0, Expression::Value(ExpressionValue::Integer(0x1234))))
        ]);
    }

    #[test]
    fn test_error_meta_instruction_incx() {
        assert_eq!(entry_lexer_error("incx"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input while parsing instruction label argument, expected \"[\" instead.\n\nincx\n^--- Here");
        assert_eq!(entry_lexer_error("incx ["), "In file \"main.gb.s\" on line 1, column 6: Unexpected end of input while parsing instruction label argument.\n\nincx [\n     ^--- Here");
        assert_eq!(entry_lexer_error("incx []"), "In file \"main.gb.s\" on line 1, column 7: Unexpected \"]\", expected a expression as the label argument instead.\n\nincx []\n      ^--- Here");
        assert_eq!(entry_lexer_error("incx [$1234"), "In file \"main.gb.s\" on line 1, column 7: Unexpected end of input while parsing instruction label argument, expected \"]\" instead.\n\nincx [$1234\n      ^--- Here");
    }

    #[test]
    fn test_meta_instruction_decx() {
        assert_eq!(tfe("decx [$1234]"), vec![
            EntryToken::InstructionWithArg(itk!(0, 4, "decx"), 0xFA, (0, Expression::Value(ExpressionValue::Integer(0x1234)))),
            EntryToken::Instruction(itk!(0, 4, "decx"), 0x3D),
            EntryToken::InstructionWithArg(itk!(0, 4, "decx"), 0xEA, (0, Expression::Value(ExpressionValue::Integer(0x1234))))
        ]);
    }

    #[test]
    fn test_error_meta_instruction_decx() {
        assert_eq!(entry_lexer_error("decx"), "In file \"main.gb.s\" on line 1, column 1: Unexpected end of input while parsing instruction label argument, expected \"[\" instead.\n\ndecx\n^--- Here");
        assert_eq!(entry_lexer_error("decx ["), "In file \"main.gb.s\" on line 1, column 6: Unexpected end of input while parsing instruction label argument.\n\ndecx [\n     ^--- Here");
        assert_eq!(entry_lexer_error("decx []"), "In file \"main.gb.s\" on line 1, column 7: Unexpected \"]\", expected a expression as the label argument instead.\n\ndecx []\n      ^--- Here");
        assert_eq!(entry_lexer_error("decx [$1234"), "In file \"main.gb.s\" on line 1, column 7: Unexpected end of input while parsing instruction label argument, expected \"]\" instead.\n\ndecx [$1234\n      ^--- Here");
    }

    #[test]
    fn test_meta_instruction_vsync() {
        assert_eq!(tfe("vsync"), vec![
            EntryToken::InstructionWithArg(itk!(0, 5, "vsync"), 0xFA, (TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::Integer(0xFF41)))),
            EntryToken::InstructionWithArg(itk!(0, 5, "vsync"), 0xE6, (TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::Integer(0b0000_0010)))),
            EntryToken::InstructionWithArg(itk!(0, 5, "vsync"), 0x20, (TEMPORARY_EXPRESSION_ID, Expression::Value(ExpressionValue::OffsetAddress(itk!(0, 5, "vsync"), -4)))),
        ]);
    }

}

