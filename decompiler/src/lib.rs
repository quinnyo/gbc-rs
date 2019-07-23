// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::path::PathBuf;


// External Dependencies ------------------------------------------------------
use colored::Colorize;
use gb_cpu::{Instruction, self};
use file_io::{FileReader, FileWriter};


// GameBoy Instruction Decompiler ---------------------------------------------
pub struct Decompiler {
    instructions: Vec<Instruction>,
    silent: bool,
    output: Vec<String>
}

impl Decompiler {

    pub fn new() -> Self {
        Self {
            instructions: gb_cpu::instruction_list(),
            silent: false,
            output: Vec::new()
        }
    }

    pub fn set_silent(&mut self) {
        self.silent = true;
    }

    pub fn decompile_file<T: FileReader + FileWriter>(
        &mut self,
        io: &mut T,
        file: PathBuf

    ) -> Result<String, (String, DecompilationError)> {
        let rom_buffer = self.read_rom_file(io, &file).map_err(|e| self.error("instruction parsing", e))?;
        // TODO analyze from all standard interrupt / rst locations
        let instructions = self.analyze_from(&rom_buffer, 0x100, None).map_err(|e| self.error("instruction parsing", e))?;
        Ok(self.output.join("\n"))
    }

    fn read_rom_file<T: FileReader + FileWriter>(&self, io: &mut T, file: &PathBuf) -> Result<Vec<u8>, RomError>{
        let (_, contents) = io.read_binary_file(None, &file).map_err(|err| {
            RomError::new(0, format!("Failed to read ROM file \"{}\": {}", err.path.display(), err.io))
        })?;
        Ok(contents)
    }

    fn analyze_from(
        &mut self,
        rom_buffer: &[u8],
        offset: usize,
        max_instructions: Option<usize>

    ) -> Result<usize, RomError> {
        Ok(offset)
    }

    fn decode_instruction_at(
        &self,
        rom_buffer: &[u8],
        offset: usize

    ) -> Result<Option<Instruction>, RomError> {
        Ok(None)
    }

    // Helpers ----------------------------------------------------------------
    // TODO dry with compiler
    fn log<S: Into<String>>(&mut self, s: S) {
        if !self.silent  {
            self.output.push(s.into());
        }
    }

    fn warning<S: Into<String>>(&mut self, s: S) {
        if !self.silent {
            self.output.push(format!("     {} {}", "Warning".bright_yellow(), s.into()));
        }
    }

    fn info<S: Into<String>>(&mut self, s: S) {
        if !self.silent {
            self.output.push(format!("        {} {}", "Info".bright_blue(), s.into()));
        }
    }

    fn error(&self, stage: &str, error: RomError) -> (String, DecompilationError) {
        (self.output.join("\n"), DecompilationError::new(stage, error))
    }

}


// Decompiler Error Abstraction -----------------------------------------------
#[derive(Debug)]
pub struct RomError {
    pub rom_offset: usize,
    pub message: String
}

impl RomError {

    pub fn new(rom_offset: usize, message: String) -> Self {
        Self {
            rom_offset,
            message
        }
    }

}

impl fmt::Display for RomError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ROM ${:0>4X} {}", self.rom_offset, self.message)
    }
}

#[derive(Debug)]
pub struct DecompilationError {
    stage: String,
    error: Option<RomError>,
    message: Option<String>
}

impl DecompilationError {

    fn new(stage: &str, error: RomError) -> Self {
        Self {
            stage: stage.to_string(),
            error: Some(error),
            message: None
        }
    }

    fn from_string<S: Into<String>>(message: S) -> Self {
        Self {
            stage: "instruction parsing".to_string(),
            error: None,
            message: Some(message.into())
        }
    }

}

impl fmt::Display for DecompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(source) = self.error.as_ref() {
            write!(f, "       {} De-compilation failed during {} phase!\n\n{}", "Error".bright_red(), self.stage, source)

        } else if let Some(message) = self.message.as_ref() {
            write!(f, "       {} Compilation failed during {} phase!\n\n{}", "Error".bright_red(), self.stage, message)

        } else {
            write!(f, "       {} Compilation failed during {} phase!", "Error".bright_red(), self.stage)
        }
    }
}

