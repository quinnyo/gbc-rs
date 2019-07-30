// External Dependencies ------------------------------------------------------
pub use gb_types::{Argument, Flag, Instruction, LexerArgument, Register};


// Functions ------------------------------------------------------------------
pub fn instruction_list() -> Vec<Instruction> {
    instructions()
}

pub fn instruction_is_conditional(mnemonic: &str) -> bool {
    match mnemonic {
        "jr" | "jp" | "call" | "ret" => true,
        _ => false
    }
}

// Include parsed instructions from build script
include!(concat!(env!("OUT_DIR"), "/instructions.rs"));

