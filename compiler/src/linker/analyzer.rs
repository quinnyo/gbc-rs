// Internal Dependencies ------------------------------------------------------
use crate::lexer::InnerToken;
use crate::expression::OptionalDataExpression;
use super::section::entry::{EntryData, SectionEntry};


// Types ----------------------------------------------------------------------
pub type AnalyzerNotes = Vec<(InnerToken, String)>;


// Low Level Instruction Analyzer ---------------------------------------------
pub fn analyze_section_entries(entries: &Vec<SectionEntry>, notes: &mut AnalyzerNotes) {
    fn get_instruction(entries: &[SectionEntry], i: usize) -> Option<(u16, i32, &OptionalDataExpression, &[u8], &InnerToken)> {
        if let Some(entry) = entries.get(i) {
            if let EntryData::Instruction { ref op_code, ref bytes, ref expression, volatile, .. } = entry.data {
                if volatile {
                    None

                } else {
                    Some((*op_code, (entry.offset + entry.size) as i32, &expression, bytes, &entry.inner))
                }

            } else {
                None
            }

        } else {
            None
        }
    }

    for i in 0..entries.len() {
        if let Some((op_code, offset, expression, bytes, inner)) = get_instruction(&entries, i) {
            let b = get_instruction(&entries, i + 1);
            let c = get_instruction(&entries, i + 2);
            analyze_instructions(
                notes,
                op_code,
                offset,
                expression,
                bytes,
                inner,
                b,
                c
            );
        }
    }
}

fn analyze_instructions(
    notes: &mut AnalyzerNotes,
    op_code: u16,
    _end_of_instruction: i32,
    _expression: &OptionalDataExpression,
    bytes: &[u8],
    _inner: &InnerToken,
    b: Option<(u16, i32, &OptionalDataExpression, &[u8], &InnerToken)>,
    c: Option<(u16, i32, &OptionalDataExpression, &[u8], &InnerToken)>

) {
    fn reg(v: u16, bytes: &[u8]) -> String {
        match v {
            0x0 => "b".to_string(),
            0x1 => "c".to_string(),
            0x2 => "d".to_string(),
            0x3 => "e".to_string(),
            0x4 => "h".to_string(),
            0x5 => "l".to_string(),
            0x6 => "[hl]".to_string(),
            0x7 => "a".to_string(),
            _ => format!("{}", bytes[1])
        }
    }

    println!("foo {:?}", b);
    if let Some(value) = match op_code {
        // ADD
        //0x80..=0x87 | 0xC6 => Some("+="),
        // SUB
        //0x90..=0x97 | 0xD6 => Some("-="),
        // AND
        //0xA0..=0xA7 | 0xE6 => Some("&="),
        // OR
        //0xB0..=0xB7 | 0xF6 => Some("|="),
        // ADC
        //0x88..=0x8F | 0xCE => Some("+="),
        // SBC
        //0x98..=0x9F | 0xDE => Some("-="),
        // XOR
        //0xA8..=0xAF | 0xEE => Some("^="),
        // CP
        0xB8..=0xBF | 0xFE => Some(reg(op_code - 0xB8, bytes)),
        _ => None
    } {
        if let Some((op_code, _, _, _, inner)) = b {
            match op_code {
                // jp c
                0xDA | 0x38 | 0xD8 => {
                    notes.push((inner.clone(), format!("a < {}", value)));
                    if let Some((0xCA | 0x28, _, _, _, inner)) = c {
                        notes.push((inner.clone(), format!("a == {}", value)));
                    }
                },
                // jp nc
                0xD2 | 0x30 | 0xD0 => {
                    notes.push((inner.clone(), format!("a >= {}", value)));
                },
                // jp z
                0xCA | 0x28 | 0xC8 => {
                    notes.push((inner.clone(), format!("a == {}", value)));
                    if let Some((0xDA | 0x38, _, _, _, inner)) = c {
                        notes.push((inner.clone(), format!("a < {}", value)));
                    }
                },
                // jp nz
                0xC2 | 0x20 | 0xC0 => {
                    notes.push((inner.clone(), format!("a != {}", value)));
                },
                _ => {}
            }
        }
    }
}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::super::test::linker;
    use super::EntryData;
    use crate::lexer::InnerToken;
    use crate::expression::{Expression, ExpressionValue};

    macro_rules! itk {
        ($start:expr, $end:expr, $parsed:expr) => {
            InnerToken::new(0, $start, $end, $parsed.into())
        }
    }

    #[test]
    fn test_cp_integer() {
        let l = linker("SECTION ROM0\ncp b\njp c,global\njp z,global\nglobal:");
        let c = l.context();
        assert_eq!(c.analyzer_notes, &[]);
    }

}

