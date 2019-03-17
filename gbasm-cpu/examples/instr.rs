// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::cmp;
use std::fs::File;
use std::io::{Read, Write};
use std::hash::{Hash, Hasher};
use std::collections::HashMap;

// External Dependencies ------------------------------------------------------
//use gbasm::cpu::{FlagModifier, FlagState, Instruction, Argument};

// Instruction Data Parser ----------------------------------------------------
fn main() {
    convert_html_dump(&[
        ("examples/base.instr.html", None),
        //("examples/ex.instr.html", Some(0xCB))

    ], "src/cpu/instructions.rs");
}

fn instr_to_string(instr: Instruction) -> String {
    format!("        {:?},", instr).replace("layout: [", "layout: vec![").replace("\", size", "\".to_string(), size")
}

fn layout_to_assert(index: usize, layout: String) -> String {

    if layout.contains("a16") {
        format!("assert_op!({}, \"{}\", 4660);", index, layout.replace("a16", "$1234"))

    } else if layout.contains("d16") {
        format!("assert_op!({}, \"{}\", 4660);", index, layout.replace("d16", "$1234"))

    } else if layout.contains("a8") {
        format!("assert_op!({}, \"{}\", 32);", index, layout.replace("a8", "$20"))

    } else if layout.contains("d8") {
        format!("assert_op!({}, \"{}\", 32);", index, layout.replace("d8", "$20"))

    } else if layout.contains("sp+r8") {
        format!("assert_op!({}, \"{}\", 32);", index, layout.replace("sp+r8", "$20"))

    } else if layout.contains("r8") {
        format!("assert_op!({}, \"{}\", 32);", index, layout.replace("r8", "$20"))

    } else if layout.contains("38h") {
        format!("assert_op!({}, \"{}\", 0x38);", index, layout.replace("38h", "$38"))

    } else if layout.contains("30h") {
        format!("assert_op!({}, \"{}\", 0x30);", index, layout.replace("30h", "$30"))

    } else if layout.contains("28h") {
        format!("assert_op!({}, \"{}\", 0x28);", index, layout.replace("28h", "$28"))

    } else if layout.contains("20h") {
        format!("assert_op!({}, \"{}\", 0x20);", index, layout.replace("20h", "$20"))

    } else if layout.contains("18h") {
        format!("assert_op!({}, \"{}\", 0x18);", index, layout.replace("18h", "$18"))

    } else if layout.contains("10h") {
        format!("assert_op!({}, \"{}\", 0x10);", index, layout.replace("10h", "$10"))

    } else if layout.contains("08h") {
        format!("assert_op!({}, \"{}\", 0x08);", index, layout.replace("08h", "$08"))

    } else if layout.contains("00h") {
        format!("assert_op!({}, \"{}\", 0x00);", index, layout.replace("00h", "$00"))

    } else {
        format!("assert_op!({}, \"{}\");", index, layout)
    }

}


// "base.instr.html"
fn convert_html_dump(sources: &[(&str, Option<usize>)], target: &str) {

    let mut lines = vec![
        "use crate::{FlagModifier, FlagState, Instruction, Register, Flag, Argument};\n".to_string(),
    ];

    // Parse all sources
    let mut max_arg_count: HashMap<String, usize> = HashMap::new();
    for (source, prefix) in sources {
        let mut file = File::open(source).unwrap();
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();
        let instructions = parse_instructions_from_string(contents, *prefix, &mut max_arg_count);
        lines.push(instructions_to_vec("instructions", instructions));
    }

    let mut counts: Vec<(String, usize)> = max_arg_count.into_iter().collect();
    counts.sort_by(|a, b| a.cmp(&b));

    lines.push("pub fn instruction_max_arg_count(mnemonic: &str) -> usize {".to_string());
    lines.push("    match mnemonic {".to_string());
    for (mnemonic, max_arg_count) in counts.into_iter() {
        lines.push(format!("        \"{}\" => {},", mnemonic, max_arg_count));
    }
    lines.push("        _ => 0".to_string());
    lines.push("    }".to_string());
    lines.push("}".to_string());

    // println!("{}", lines.join("\n"));

    // Write out
    let mut file = File::create(target).unwrap();
    file.write_all(&lines.join("\n").into_bytes()).unwrap();

}

fn instructions_to_vec(fn_name: &str, instructions: Vec<Instruction>) -> String {
    let mut lines = vec![
        "// Auto generated Instruction Data".to_string(),
        format!("pub fn {}() -> Vec<Instruction> {{", fn_name),
        "    vec![".to_string()
    ];
    for i in instructions {
        lines.push(instr_to_string(i));
    }
    lines.push("    ]".to_string());
    lines.push("}".to_string());
    lines.join("\n")
}

fn parse_instructions_from_string(
    contents: String ,
    prefix: Option<usize>,
    max_arg_count: &mut HashMap<String, usize>

) -> Vec<Instruction> {
    let mut iter = htmlstream::tag_iter(&contents);
    let mut index = 0;
    let mut instructions = Vec::new();
    while let Some((_, tag)) = iter.next() {
        if tag.name == "td" && tag.state == htmlstream::HTMLTagState::Opening {
            let layout = iter.next().unwrap().1;
            let next = iter.next().unwrap().1;
            let (layout, cycles, flags) = if next.name == "td" {
                ("invalid".to_string(), "0 0".to_string(), "- - - -".to_string())

            } else {
                let cycles = iter.next().unwrap().1;
                iter.next();
                let flags = iter.next().unwrap().1;
                let close = iter.next().unwrap().1;
                if close.name != "td" || close.state != htmlstream::HTMLTagState::Closing {
                    panic!("Expected closing td");
                }
                (layout.html, cycles.html, flags.html)
            };

            index += 1;
            instructions.push(parse_instruction(
                index,
                prefix,
                &layout,
                &cycles,
                &flags,
                max_arg_count
            ));
        }
    }
    instructions
}

fn parse_instruction(
    index: usize,
    prefix: Option<usize>,
    layout: &str,
    cycles: &str,
    flags: &str,
    max_arg_count: &mut HashMap<String, usize>

) -> Instruction {

    // Clean up HTML source
    let layout = layout
        .replace("HL+", "hli")
        .replace("HL-", "hld")
        .replace("(", "[")
        .replace(")", "]")
        .replace("ADD A,", "ADD ")
        .replace("ADC A,", "ADC ")
        .replace("SBC A,", "SBC ")
        .to_ascii_lowercase().split(" ").map(|s| s.to_string()).collect::<Vec<String>>();

    println!("{}", layout_to_assert(index - 1, layout.join(" ").replace("stop 0", "stop")));

    // Clean up Cycle Info
    let cycles = cycles
        .replace("&nbsp;", " ")
        .replace("  ", " ")
        .replace("/", " ")
        .split(" ").map(|s| s.to_string()).collect::<Vec<String>>();

    // Split Flag Information
    let flags = flags.to_ascii_lowercase().split(" ").map(|s| s.to_string()).collect::<Vec<String>>();

    // Parse Arguments
    let args = if layout.len() > 1 {
        let args = layout[1].split(",").map(|s| s.to_string()).collect::<Vec<String>>();
        args.into_iter().filter_map(|a| Argument::from(&a, &layout[0])).collect()

    } else {
        Vec::new()
    };

    // Update mnemonic info
    let mnemonic = layout[0].clone();
    if mnemonic != "prefix" && mnemonic != "invalid" {
        let arg_count = max_arg_count.entry(mnemonic.clone()).or_insert(0);
        *arg_count = cmp::max(*arg_count, args.len());
    }

    // Return Instruction Data
    let argument: Vec<Argument> = args.clone().into_iter().filter(|a| a.is_value()).collect();
    if argument.len() > 1 {
        panic!("Instruction with more than one user provided argument!");
    }
    let argument = argument.into_iter().next();

    Instruction {
        code: index - 1,
        prefix,
        name: mnemonic.clone(),
        size: cycles[0].parse().unwrap(),
        cycles: cycles[1].parse().unwrap(),
        cycles_min: if cycles.len() > 2 { cycles[2].parse().ok() } else { None  },
        layout: args,
        argument,
        flags: FlagState {
            z: FlagModifier::from(flags[0].as_str()),
            n: FlagModifier::from(flags[1].as_str()),
            c: FlagModifier::from(flags[2].as_str()),
            h: FlagModifier::from(flags[3].as_str()),
        }
    }

}



// Flags ----------------------------------------------------------------------
#[derive(Eq, PartialEq, Hash, Clone)]
pub enum Flag {
    Zero,
    NoZero,
    Carry,
    NoCarry
}

impl From<&str> for Flag {
    fn from(s: &str) -> Self {
        match s {
            "z" => Flag::Zero,
            "nz" => Flag::NoZero,
            "c" => Flag::Carry,
            "nc" => Flag::NoCarry,
            f => unreachable!("Invalid flag: {}", f)
        }
    }
}

impl fmt::Debug for Flag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Flag::Zero => "Flag::Zero",
            Flag::NoZero => "Flag::NoZero",
            Flag::Carry => "Flag::Carry",
            Flag::NoCarry => "Flag::NoCarry",
        })
    }
}

#[derive(Eq, PartialEq)]
pub enum FlagModifier {
    Keep,
    Set,
    Clear,
    Result
}

impl From<&str> for FlagModifier {
    fn from(s: &str) -> Self {
        match s {
            "0" => FlagModifier::Clear,
            "1" => FlagModifier::Set,
            "-" => FlagModifier::Keep,
            _ => FlagModifier::Result
        }
    }
}

impl fmt::Debug for FlagModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FlagModifier::Keep => write!(f, "FlagModifier::Keep"),
            FlagModifier::Set => write!(f, "FlagModifier::Set"),
            FlagModifier::Clear => write!(f, "FlagModifier::Clear"),
            FlagModifier::Result => write!(f, "FlagModifier::Result")
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FlagState {
    pub z: FlagModifier,
    pub n: FlagModifier,
    pub c: FlagModifier,
    pub h: FlagModifier
}


// Registers ------------------------------------------------------------------
#[derive(Eq, PartialEq, Hash, Clone)]
pub enum Register {
    Accumulator,
    B,
    C,
    D,
    E,
    H,
    L,
    AF,
    BC,
    DE,
    HL,
    HLIncrement,
    HLDecrement,
    SP
}

impl From<&str> for Register {
    fn from(s: &str) -> Self {
        match s {
            "a" => Register::Accumulator,
            "b" => Register::B,
            "c" => Register::C,
            "d" => Register::D,
            "e" => Register::E,
            "h" => Register::H,
            "l" => Register::L,
            "af" => Register::AF,
            "bc" => Register::BC,
            "de" => Register::DE,
            "hl" => Register::HL,
            "hli" => Register::HLIncrement,
            "hld" => Register::HLDecrement,
            "sp" => Register::SP,
            r => unreachable!("Invalid Register: {}", r)
        }
    }
}

impl fmt::Debug for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Register::Accumulator => "Register::Accumulator",
            Register::B => "Register::B",
            Register::C => "Register::C",
            Register::D => "Register::D",
            Register::E => "Register::E",
            Register::H => "Register::H",
            Register::L => "Register::L",
            Register::AF => "Register::AF",
            Register::BC => "Register::BC",
            Register::DE => "Register::DE",
            Register::HL => "Register::HL",
            Register::HLIncrement => "Register::HLIncrement",
            Register::HLDecrement => "Register::HLDecrement",
            Register::SP => "Register::SP"
        })
    }
}


// Instructions ---------------------------------------------------------------
#[derive(Debug)]
pub struct Instruction {
    pub code: usize,
    pub prefix: Option<usize>,
    pub name: String,
    pub size: usize,
    pub cycles: usize,
    pub cycles_min: Option<usize>,
    pub layout: Vec<Argument>,
    pub argument: Option<Argument>,
    pub flags: FlagState
}

#[derive(Hash, Eq, PartialEq)]
pub enum ParserArgument {
    MemoryLookupValue,
    MemoryLookupRegister(Register),
    Value,
    Register(Register),
    Flag(Flag)
}

#[derive(Eq, PartialEq, Clone)]
pub enum Argument {
    MemoryLookupByteValue,
    MemoryLookupWordValue,
    MemoryLookupRegister(Register),
    ByteValue,
    SignedByteValue,
    WordValue,
    ConstantValue(usize),
    Register(Register),
    Flag(Flag)
}

impl Argument {
    fn is_value(&self) -> bool {
        match self {
            Argument::MemoryLookupByteValue => true,
            Argument::MemoryLookupWordValue => true,
            Argument::MemoryLookupRegister(_) => false,
            Argument::ByteValue => true,
            Argument::SignedByteValue => true,
            Argument::WordValue => true,
            Argument::ConstantValue(_) => true,
            Argument::Register(_) => false,
            Argument::Flag(_) => false
        }
    }
}

impl Hash for Argument {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // We employ a customer hash so that we can store more detailed argument layouts for code
        // gen but keep the ability to ignore data types during the entry lexerstage
        let p = match self {
            Argument::MemoryLookupByteValue => ParserArgument::MemoryLookupValue,
            Argument::MemoryLookupWordValue => ParserArgument::MemoryLookupValue,
            Argument::MemoryLookupRegister(r) => ParserArgument::MemoryLookupRegister(r.clone()),
            Argument::ByteValue => ParserArgument::Value,
            Argument::SignedByteValue => ParserArgument::Value,
            Argument::WordValue => ParserArgument::Value,
            Argument::ConstantValue(_) => ParserArgument::Value,
            Argument::Register(r) => ParserArgument::Register(r.clone()),
            Argument::Flag(f) => ParserArgument::Flag(f.clone()),
        };
        p.hash(state);
    }
}

impl fmt::Debug for Argument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Argument::MemoryLookupByteValue => write!(f, "Argument::MemoryLookupByteValue"),
            Argument::MemoryLookupWordValue => write!(f, "Argument::MemoryLookupWordValue"),
            Argument::MemoryLookupRegister(r) => write!(f, "Argument::MemoryLookupRegister({:?})", r),
            Argument::ByteValue => write!(f, "Argument::ByteValue"),
            Argument::SignedByteValue => write!(f, "Argument::SignedByteValue"),
            Argument::WordValue => write!(f, "Argument::WordValue"),
            Argument::ConstantValue(v) => write!(f, "Argument::ConstantValue({})", v),
            Argument::Register(r) => write!(f, "Argument::Register({:?})", r),
            Argument::Flag(r) => write!(f, "Argument::Flag({:?})", r)
        }
    }
}

impl Argument {
    pub fn from(a: &str, mnemonic: &str) -> Option<Self> {
        let cond = mnemonic == "jr" || mnemonic == "jp" || mnemonic == "call" || mnemonic == "ret";
        match a {
            "00h" => Some(Argument::ConstantValue(0)),
            "08h" => Some(Argument::ConstantValue(8)),
            "10h" => Some(Argument::ConstantValue(16)),
            "18h" => Some(Argument::ConstantValue(24)),
            "20h" => Some(Argument::ConstantValue(32)),
            "28h" => Some(Argument::ConstantValue(40)),
            "30h" => Some(Argument::ConstantValue(48)),
            "38h" => Some(Argument::ConstantValue(56)),

            "sp+r8" => Some(Argument::SignedByteValue),
            "r8" => Some(Argument::SignedByteValue),

            // Unsigned
            "a8" => Some(Argument::ByteValue),
            "a16" => Some(Argument::WordValue),
            "d8" => Some(Argument::ByteValue),
            "d16" => Some(Argument::WordValue),
            "[c]" => Some(Argument::MemoryLookupRegister("c".into())),
            "[a16]" => Some(Argument::MemoryLookupWordValue),
            "[a8]" => Some(Argument::MemoryLookupByteValue),

            // Registers
            "a" | "b" | "d" | "e" | "h" | "l" |
            "af" | "bc" | "de" | "hl" | "sp" => Some(Argument::Register(a.into())),
            "c" if !cond => Some(Argument::Register(a.into())),
            "[bc]" | "[de]" | "[hl]" => Some(Argument::MemoryLookupRegister(a[1..3].into())),
            "[hli]" => Some(Argument::MemoryLookupRegister("hli".into())),
            "[hld]" => Some(Argument::MemoryLookupRegister("hld".into())),

            // Flags
            "c" if cond => Some(Argument::Flag(a.into())),
            "nz" | "nc" | "z" => Some(Argument::Flag(a.into())),

            // Ignored
            "0" => None,
            "cb" => None,
            a => panic!("Unknown argument type: {}", a)
        }
    }
}


