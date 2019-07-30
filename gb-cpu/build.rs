// STD Dependencies -----------------------------------------------------------
use std::env;
use std::cmp;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use serde::Deserialize;
use gb_types::{Argument, LexerArgument};


// Types ----------------------------------------------------------------------
pub type InstructionLayouts = HashMap<(String, Vec<LexerArgument>), usize>;

// ConstantValue > OpCode
pub type InstructionMappings = HashMap<(String, Vec<LexerArgument>), Vec<(usize, usize)>>;


// Build Script ---------------------------------------------------------------
fn main() {
    let cargo_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let ops_path = Path::new(&cargo_dir).join("data").join("dmgops.json");
    let mut json = String::new();
    let mut f = File::open(&ops_path).expect("Failed to load OpCode JSON file.");
    f.read_to_string(&mut json).unwrap();

    let mut layouts = HashMap::new();
    let mut mappings = HashMap::new();
    let mut max_arg_count: HashMap<String, usize> = HashMap::new();
    let op_code_map: OpCodeMap = serde_json::from_str(&json).expect("Failed to parse OpCode JSON.");

    let mut instructions = parse_instructions(&op_code_map.unprefixed, None, &mut layouts, &mut mappings, &mut max_arg_count);
    instructions.append(&mut parse_instructions(&op_code_map.cb_prefixed, Some(0xCB), &mut layouts, &mut mappings, &mut max_arg_count));

    // Assign constant list to all affected instructions
    for (key, values) in mappings {
        let base_index = *layouts.get(&key).unwrap();
        instructions[base_index].offsets = Some([
            values[0],
            values[1],
            values[2],
            values[3],
            values[4],
            values[5],
            values[6],
            values[7]
        ])
    }

    // Generate Instruction Layout File
    let mut lines = vec![
        "// Auto generated Instruction Data".to_string(),
        "pub fn instructions() -> Vec<Instruction> {".to_string(),
        "    vec![".to_string()
    ];

    lines.push(instructions.into_iter().map(|i| {
        format!("        {:?},", i)

    }).collect::<Vec<String>>().join("\n"));
    lines.push("    ]".to_string());
    lines.push("}\n".to_string());

    // Generate match statement for maximum number of arguments for each mnemonic
    let mut max_arg_counts: Vec<(String, usize)> = max_arg_count.into_iter().collect();
    max_arg_counts.sort_by(|a, b| a.cmp(&b));
    lines.push("pub fn instruction_max_arg_count(mnemonic: &str) -> usize {".to_string());
    lines.push("    match mnemonic {".to_string());
    for (mnemonic, max_arg_count) in max_arg_counts.into_iter() {
        lines.push(format!("        \"{}\" => {},", mnemonic, max_arg_count));
    }
    lines.push("        _ => 0".to_string());
    lines.push("    }".to_string());
    lines.push("}".to_string());

    // println!("{}", lines.join("\n"));
    // panic!("foo");

    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("instructions.rs");
    let mut f = File::create(&dest_path).unwrap();
    f.write_all(&lines.join("\n").into_bytes()).unwrap();
}


// OpCode Parser --------------------------------------------------------------
fn parse_instructions(
    op_codes: &[OpCode],
    prefix: Option<u8>,
    layouts: &mut InstructionLayouts,
    mappings: &mut InstructionMappings,
    max_arg_count: &mut HashMap<String, usize>

) -> Vec<Instruction> {
    assert_eq!(op_codes.len(), 256);
    op_codes.iter().enumerate().map(|(index, op_code)| {
        op_code.parse(
            index,
            prefix,
            layouts,
            mappings,
            max_arg_count
        )

    }).collect()
}


// Structs --------------------------------------------------------------------
#[derive(Debug, Deserialize)]
struct OpCodeMap {
    #[serde(rename="Unprefixed")]
    unprefixed: Vec<OpCode>,

    #[serde(rename="CBPrefixed")]
    cb_prefixed: Vec<OpCode>
}

#[derive(Debug, Deserialize)]
struct OpCode {
    #[serde(rename="Name")]
    name: String,

    #[serde(rename="Length")]
    size: usize,

    #[serde(rename="TCyclesBranch")]
    cycles_branch: usize,

    #[serde(rename="TCyclesNoBranch")]
    cycles_no_branch: usize
}

impl OpCode {
    fn parse(
        &self,
        index: usize,
        prefix: Option<u8>,
        layouts: &mut InstructionLayouts,
        mappings: &mut InstructionMappings,
        max_arg_count: &mut HashMap<String, usize>

    ) -> Instruction {

        let code = index + if prefix.is_some() { 256 } else { 0 };

        // Parse general instruction layout
        let layout = self.name
            .replace("LD HL,SP", "LDSP HL,SP")
            .replace("LD (FF00+u8),A", "LDH (FF00+u8),A")
            .replace("LD A,(FF00+u8)", "LDH A,(FF00+u8)")
            .replace("HL+", "hli")
            .replace("HL-", "hld")
            .replace("(", "[")
            .replace(")", "]")
            .replace("ADD A,", "ADD ")
            .replace("CP A,", "CP ")
            .replace("AND A,", "AND ")
            .replace("OR A,", "OR ")
            .replace("XOR A,", "XOR ")
            .replace("ADC A,", "ADC ")
            .replace("SBC A,", "SBC ")
            .replace("SUB A,", "SUB ")
            .to_ascii_lowercase().split(" ").map(|s| s.to_string()).collect::<Vec<String>>();

        // Parse instruction arguments
        let args = if layout.len() > 1 {
            let args = layout[1].split(",").map(|s| s.to_string()).collect::<Vec<String>>();
            args.into_iter().filter_map(|a| Argument::from(&a, &layout[0])).collect()

        } else {
            Vec::new()
        };

        // Parse Mnemonic
        let mut mnemonic = layout[0].clone();
        if mnemonic != "prefix" && mnemonic != "unused" {
            // Remember maximum args for all mnemonics
            let arg_count = max_arg_count.entry(mnemonic.clone()).or_insert(0);
            *arg_count = cmp::max(*arg_count, args.len());

        } else {
            mnemonic = "invalid".to_string();
        }

        // Extract user argument to instruction
        let user_argument: Vec<Argument> = args.clone().into_iter().filter(|a| a.is_user_provided()).collect();
        if user_argument.len() > 1 {
            panic!("Instruction with more than one user provided argument!");
        }
        let user_argument = user_argument.into_iter().next();

        // Map similiar op-code with constant argument values to the same op code base
        let arg_layout = args.clone().into_iter().map(|a| a.into()).collect();
        let key: (String, Vec<LexerArgument>) = (mnemonic.clone(), arg_layout);
        if !layouts.contains_key(&key) {
            layouts.insert(key.clone(), code);
            if let Some(Argument::ConstantValue(ref c)) = user_argument {
                mappings.insert(key, vec![(*c, code)]);
            }

        } else {
            if let Some(Argument::ConstantValue(ref c)) = user_argument {
                mappings.get_mut(&key).unwrap().push((*c, code));
            }
        };

        Instruction {
            code,
            prefix,
            name: mnemonic,
            size: self.size,
            value: None,
            argument: user_argument,
            layout: [
                args.get(0).unwrap_or(&Argument::Unused).clone(),
                args.get(1).unwrap_or(&Argument::Unused).clone()
            ],
            offsets: None
        }

    }
}

#[derive(Debug)]
struct Instruction {
    code: usize,
    prefix: Option<u8>,
    name: String,
    size: usize,
    value: Option<u16>,
    layout: [Argument; 2],
    argument: Option<Argument>,
    offsets: Option<[(usize, usize); 8]>,
}


