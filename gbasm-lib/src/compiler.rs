// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::path::PathBuf;
use std::time::Instant;


// Internal Dependencies ------------------------------------------------------
use crate::generator::Generator;
use crate::error::SourceError;
use crate::linker::Linker;
use crate::lexer::{Lexer, IncludeStage, MacroStage, ValueStage, ExpressionStage, EntryStage};
use crate::traits::FileReader;


// Compiler Pipeline Implementation -------------------------------------------
pub struct Compiler {
    silent: bool,
    strip_debug_code: bool,
    optimize_instructions: bool,
    print_segment_map: bool,
    generate_symbols: Option<PathBuf>,
    generata_rom: Option<PathBuf>,
    output: Vec<String>
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            silent: false,
            strip_debug_code: true,
            optimize_instructions: true,
            print_segment_map: false,
            generate_symbols: None,
            generata_rom: None,
            output: Vec::new()
        }
    }

    pub fn set_silent(&mut self) {
        self.silent = true;
    }

    pub fn compile<T: FileReader>(&mut self, reader: T, entry: PathBuf) -> Result<String, (String, CompilerError)> {
        self.log(format!("Compiling \"{}\"...", entry.display()));
        let entry_lexer = self.parse(reader, entry)?;
        let linker = self.link(entry_lexer)?;
        self.generate(linker)?;
        Ok(self.output.join("\n"))
    }
}

impl Compiler {

    fn parse<T: FileReader>(&mut self, reader: T, entry: PathBuf) -> Result<Lexer<EntryStage>, (String, CompilerError)> {
        let start = Instant::now();
        let include_lexer = Lexer::<IncludeStage>::from_file(&reader, &entry).map_err(|e| self.error("file inclusion", e))?;
        let macro_lexer = Lexer::<MacroStage>::from_lexer(include_lexer).map_err(|e| self.error("macro expansion", e))?;
        let value_lexer = Lexer::<ValueStage>::from_lexer(macro_lexer).map_err(|e| self.error("value construction", e))?;
        let expr_lexer = Lexer::<ExpressionStage>::from_lexer(value_lexer).map_err(|e| self.error("expression construction", e))?;
        let entry_lexer = Lexer::<EntryStage>::from_lexer(expr_lexer).map_err(|e| self.error("entry construction", e))?;
        self.log(format!("Parsing completed in {}ms.", start.elapsed().as_millis()));
        Ok(entry_lexer)
    }

    fn link(&mut self, entry_lexer: Lexer<EntryStage>) -> Result<Linker, (String, CompilerError)> {
        let start = Instant::now();
        let linker = Linker::from_lexer(
            entry_lexer,
            self.strip_debug_code,
            self.optimize_instructions

        ).map_err(|e| self.error("section linking", e))?;
        self.log(format!("Linking completed in {}ms.", start.elapsed().as_millis()));

        // Report Segment Usage
        if !self.silent && self.print_segment_map {
            // TODO
            let _usage = linker.to_usage_list();
        }

        // Generate symbol file for BGB debugger
        if let Some(_output_file) = self.generate_symbols.take() {
            // TODO write to file
            let _symbols = linker.to_symbol_list();
        }
        Ok(linker)
    }

    fn generate(&mut self, linker: Linker) -> Result<(), (String, CompilerError)> {
        let start = Instant::now();
        let generator = Generator::from_linker(linker);
        self.log(format!("ROM contents generated in {}ms.", start.elapsed().as_millis()));

        match generator.validate_rom() {
            Ok(_warnings) => if !self.silent {
                // TODO push warnings intout output
            },
            Err(err) => return Err((self.output.join("\n"), CompilerError::from_string(err)))
        }

        // TODO print ROM info

        if let Some(_output_file) = self.generata_rom.take() {
            // TODO write buffer to file
        }
        Ok(())
    }

    fn log<S: Into<String>>(&mut self, s: S) {
        if !self.silent {
            self.output.push(s.into());
        }
    }

    fn error(&self, stage: &str, error: SourceError) -> (String, CompilerError) {
        (self.output.join("\n"), CompilerError::new(stage, error))
    }

}


// Compiler Error Abstraction -------------------------------------------------
#[derive(Debug)]
pub struct CompilerError {
    stage: String,
    source: Option<SourceError>,
    message: Option<String>
}

impl CompilerError {

    fn new(stage: &str, source: SourceError) -> Self {
        Self {
            stage: stage.to_string(),
            source: Some(source),
            message: None
        }
    }

    fn from_string(message: String) -> Self {
        Self {
            stage: "rom generation".to_string(),
            source: None,
            message: Some(message)
        }
    }

}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(source) = self.source.as_ref() {
            write!(f, "Compilation failed during {} phase!\n\n{}", self.stage, source)

        } else if let Some(message) = self.message.as_ref() {
            write!(f, "Compilation failed during {} phase!\n\n{}", self.stage, message)

        } else {
            write!(f, "Compilation failed during {} phase!", self.stage)
        }
    }
}

// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use regex::Regex;
    use std::path::PathBuf;

    use crate::lexer::stage::mocks::MockFileReader;
    use super::Compiler;

    fn compiler<S: Into<String>>(mut compiler: Compiler, s: S) -> String {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let re = Regex::new(r"([0-9+]ms)").unwrap();
        let output = compiler.compile(reader, PathBuf::from("main.gb.s")).expect("Compilation failed");
        re.replace_all(output.as_str(), "XXms").to_string()
    }

    fn compiler_error<S: Into<String>>(mut compiler: Compiler, s: S) -> (String, String) {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let re = Regex::new(r"([0-9+]ms)").unwrap();
        let err = compiler.compile(reader, PathBuf::from("main.gb.s")).err().expect("Expected a CompilerError");
        (re.replace_all(err.0.as_str(), "XXms").to_string(), err.1.to_string())
    }

    // Logs -------------------------------------------------------------------
    #[test]
    fn test_silent() {
        let mut c = Compiler::new();
        c.set_silent();
        assert_eq!(compiler(c, ""), "");
    }

    #[test]
    fn test_empty_input() {
        let c = Compiler::new();
        assert_eq!(compiler(c, ""), "Compiling \"main.gb.s\"...\nParsing completed in XXms.\nLinking completed in XXms.\nROM contents generated in XXms.");
    }

    // Errors -----------------------------------------------------------------
    #[test]
    fn test_error_lexer() {
        let c = Compiler::new();
        assert_eq!(compiler_error(c, "@"), (
            "Compiling \"main.gb.s\"...".to_string(),
            "Compilation failed during file inclusion phase!\n\nIn file \"main.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here".to_string()
        ));
    }

    #[test]
    fn test_error_linking() {
        let c = Compiler::new();
        assert_eq!(compiler_error(c, "ld a,a"), (
            "Compiling \"main.gb.s\"...\nParsing completed in XXms.".to_string(),
            "Compilation failed during section linking phase!\n\nIn file \"main.gb.s\" on line 1, column 1: Unexpected ROM entry before any section declaration\n\nld a,a\n^--- Here".to_string()
        ));
    }

}

