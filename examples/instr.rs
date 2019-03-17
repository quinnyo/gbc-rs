// STD Dependencies -----------------------------------------------------------
use std::fs::File;
use std::io::{Read, Write};
use gbasm::cpu::{FlagModifier, FlagState, Instruction, Argument};

// Instruction Data Parser ----------------------------------------------------
fn main() {
    convert_html_dump("examples/base.instr.html", "src/cpu/instructions.rs");
}

fn instr_to_string(instr: Instruction) -> String {
    format!("        {:?},", instr).replace("args: [", "args: vec![").replace("\", cycles", "\".to_string(), cycles")
}

// "base.instr.html"
fn convert_html_dump(source: &str, target: &str) {
    let mut file = File::open(source).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    let mut iter = htmlstream::tag_iter(&contents);
    let mut index = 0;
    let mut lines = vec![
        "use crate::cpu::{FlagModifier, FlagState, Instruction, Argument, Register, Flag};".to_string(),
        "".to_string(),
        "// Auto Generated".to_string(),
        "pub fn instructions() -> Vec<Instruction> {".to_string(),
        "    vec![".to_string()
    ];
    while let Some((_, tag)) = iter.next() {
        if tag.name == "td" && tag.state == htmlstream::HTMLTagState::Opening {

            let layout = iter.next().unwrap().1;
            index += 1;

            // Handle Empty cells
            let next = iter.next().unwrap().1;
            if next.name == "td" {
                let instr = Instruction {
                    op_code: index - 1,
                    prefix: None,
                    mnemonic: "invalid".to_string(),
                    cycles: 0,
                    cycles_min: None,
                    args: vec![],
                    flags: FlagState {
                        zero: FlagModifier::Untouched,
                        negative: FlagModifier::Untouched,
                        carry: FlagModifier::Untouched,
                        half_carry: FlagModifier::Untouched,
                    }
                };
                lines.push(instr_to_string(instr));
                continue;
            }
            let cycles = iter.next().unwrap().1;
            iter.next();
            let flags = iter.next().unwrap().1;
            let close = iter.next().unwrap().1;

            // d8 = any unsigned, r8 = any signed
            // d16 = any unsigned
            // 30h = expected hex value
            let layout = layout.html.replace("HL+", "hli").replace("HL-", "hld").replace("(", "[").replace(")", "]").to_ascii_lowercase().split(" ").map(|s| s.to_string()).collect::<Vec<String>>();

            let cycles = cycles.html.replace("&nbsp;", " ").replace("  ", " ").replace("/", " ").split(" ").skip(1).map(|s| s.to_string()).collect::<Vec<String>>();
            let flags = flags.html.to_ascii_lowercase().split(" ").map(|s| s.to_string()).collect::<Vec<String>>();

            let args = if layout.len() > 1 {
                let args = layout[1].split(",").map(|s| s.to_string()).collect::<Vec<String>>();
                args.into_iter().filter_map(|a| Argument::from(&a, &layout[0])).collect()

            } else {
                Vec::new()
            };

            let instr = Instruction {
                op_code: index - 1,
                prefix: None,
                mnemonic: layout[0].clone(),
                cycles: cycles[0].parse().unwrap(),
                cycles_min: if cycles.len() > 1 { cycles[1].parse().ok() } else { None  },
                args,
                flags: FlagState {
                    zero: FlagModifier::from(flags[0].as_str()),
                    negative: FlagModifier::from(flags[1].as_str()),
                    carry: FlagModifier::from(flags[2].as_str()),
                    half_carry: FlagModifier::from(flags[3].as_str()),
                }
            };
            lines.push(instr_to_string(instr));

            if close.name != "td" || close.state != htmlstream::HTMLTagState::Closing {
                panic!("Expected closing td");
            }

        }
    }
    lines.push("    ]".to_string());
    lines.push("}".to_string());

    let mut file = File::create(target).unwrap();
    file.write_all(&lines.join("\n").into_bytes()).unwrap();
}

