// STD Dependencies -----------------------------------------------------------
use std::fmt;


// External Dependencies ------------------------------------------------------
use colored::Colorize;


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

    pub fn new(stage: &str, error: RomError) -> Self {
        Self {
            stage: stage.to_string(),
            error: Some(error),
            message: None
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

