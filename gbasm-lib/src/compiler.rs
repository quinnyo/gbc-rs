// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::path::PathBuf;
use std::time::Instant;


// Internal Dependencies ------------------------------------------------------
use crate::generator::{Generator, ROMInfo};
use crate::error::SourceError;
use crate::linker::{Linker, SegmentUsage};
use crate::lexer::{Lexer, IncludeStage, MacroStage, ValueStage, ExpressionStage, EntryStage};
use crate::traits::{FileReader, FileWriter};


// Compiler Pipeline Implementation -------------------------------------------
pub struct Compiler {
    silent: bool,
    strip_debug_code: bool,
    optimize_instructions: bool,
    print_segment_map: bool,
    print_rom_info: bool,
    generate_symbols: Option<PathBuf>,
    generate_rom: Option<PathBuf>,
    output: Vec<String>
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            silent: false,
            strip_debug_code: false,
            optimize_instructions: false,
            print_segment_map: false,
            print_rom_info: false,
            generate_symbols: None,
            generate_rom: None,
            output: Vec::new()
        }
    }

    pub fn set_silent(&mut self) {
        self.silent = true;
    }

    pub fn set_print_segment_map(&mut self) {
        self.print_segment_map = true;
    }

    pub fn set_print_rom_info(&mut self) {
        self.print_rom_info = true;
    }

    pub fn set_generate_rom(&mut self, path: PathBuf) {
        self.generate_rom = Some(path);
    }

    pub fn set_generate_symbols(&mut self, path: PathBuf) {
        self.generate_symbols = Some(path);
    }

    pub fn set_strip_debug_code(&mut self) {
        self.strip_debug_code = true;
    }

    pub fn set_optimize_instructions(&mut self) {
        self.optimize_instructions = true;
    }

    pub fn compile<T: FileReader + FileWriter>(&mut self, files: &mut T, entry: PathBuf) -> Result<String, (String, CompilerError)> {
        self.log(format!("Compiling \"{}\"...", entry.display()));
        let entry_lexer = self.parse(files, entry)?;
        let linker = self.link(files, entry_lexer)?;
        self.generate(files, linker)?;
        Ok(self.output.join("\n"))
    }
}

impl Compiler {

    fn parse<T: FileReader>(&mut self, reader: &T, entry: PathBuf) -> Result<Lexer<EntryStage>, (String, CompilerError)> {
        let start = Instant::now();
        let include_lexer = Lexer::<IncludeStage>::from_file(reader, &entry).map_err(|e| self.error("file inclusion", e))?;
        let macro_lexer = Lexer::<MacroStage>::from_lexer(include_lexer).map_err(|e| self.error("macro expansion", e))?;
        let value_lexer = Lexer::<ValueStage>::from_lexer(macro_lexer).map_err(|e| self.error("value construction", e))?;
        let expr_lexer = Lexer::<ExpressionStage>::from_lexer(value_lexer).map_err(|e| self.error("expression construction", e))?;
        let entry_lexer = Lexer::<EntryStage>::from_lexer(expr_lexer).map_err(|e| self.error("entry construction", e))?;
        self.log(format!("Parsing completed in {}ms.", start.elapsed().as_millis()));
        Ok(entry_lexer)
    }

    fn link<T: FileWriter>(&mut self, writer: &mut T, entry_lexer: Lexer<EntryStage>) -> Result<Linker, (String, CompilerError)> {
        let start = Instant::now();
        let linker = Linker::from_lexer(
            entry_lexer,
            self.strip_debug_code,
            self.optimize_instructions

        ).map_err(|e| self.error("section linking", e))?;
        self.log(format!("Linking completed in {}ms.", start.elapsed().as_millis()));

        // Report Segment Usage
        if !self.silent && self.print_segment_map {
            self.print_segment_usage(linker.to_usage_list());
        }

        // Generate symbol file for BGB debugger
        if let Some(output_file) = self.generate_symbols.take() {
            let symbols = linker.to_symbol_list().into_iter().map(|(bank, address, name)| {
                format!("{:0>2}:{:0>4x} {}", bank, address, name)

            }).collect::<Vec<String>>().join("\n");
            writer.write_file(&output_file, symbols).map_err(|err| {
                (self.output.join("\n"), CompilerError::from_string(
                    format!("Failed to write symbols to file \"{}\"", err.path.display())
                ))
            })?;
        }
        Ok(linker)
    }

    fn generate<T: FileWriter>(&mut self, writer: &mut T, linker: Linker) -> Result<(), (String, CompilerError)> {
        let start = Instant::now();
        let mut generator = Generator::from_linker(linker);

        // Validate ROM
        match generator.validate_rom() {
            Ok(_warnings) => {
                // TODO push warnings into output
            },
            Err(err) => return Err((self.output.join("\n"), CompilerError::from_string(err)))
        }

        // Apply checksum etc.
        generator.finalize_rom();
        self.log(format!("ROM validated in {}ms.", start.elapsed().as_millis()));

        if !self.silent && self.print_rom_info {
            self.print_rom_info(generator.rom_info());
        }

        if let Some(output_file) = self.generate_rom.take() {
            let start = Instant::now();
            writer.write_binary_file(&output_file, generator.buffer).map_err(|err| {
                (self.output.join("\n"), CompilerError::from_string(
                    format!("Failed to write ROM to file \"{}\"", err.path.display())
                ))
            })?;
            self.log(format!("ROM written in {}ms.", start.elapsed().as_millis()));
        }
        Ok(())
    }

    fn print_rom_info(&mut self, _info: ROMInfo) {
        // println!("{:?}", info);
        // TODO print rom info
        // [gbasm] Title: SPRITE
        // [gbasm] Mapper: MBC1
        // [gbasm] ROM: 32768 bytes ( standard ROM only, 32768 bytes )
        // [gbasm] RAM: 10240 bytes ( internal 8192 bytes, 2048 bytes in 1 bank(s) )
        // [gbasm] BATTERY: None
    }

    fn print_segment_usage(&mut self, segments: Vec<SegmentUsage>) {
        self.output.push("".to_string());
        self.output.push("Segment Usage Report:".to_string());
        self.output.push("".to_string());
        for s in segments {
            let size = s.end_address - s.start_address;
            self.output.push(format!(
                "  {: <10} @ ${:0>4x} ({: >5} of {: >5} bytes used) ({: >6} free)",
                s.name,
                s.start_address ,
                s.bytes_in_use,
                size,
                size - s.bytes_in_use
            ));
            self.output.push("".to_string());
            for (used, name, start, end) in s.ranges {
                let display_name = if let Some(name) = name {
                    format!(" ({})", name)

                } else {
                    "".to_string()
                };
                let marker = if used {
                    "########"

                } else {
                    "........"
                };
                self.output.push(format!(
                    "    ${:0>4x}..=${:0>4x} {} ({: >5} bytes){}",
                    start,
                    end - 1,
                    marker,
                    end - start,
                    display_name
                ));
            }
            self.output.push("".to_string());
            self.output.push("".to_string());
        }
        self.output.pop();
    }


    // Helpers ----------------------------------------------------------------
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

    fn from_string<S: Into<String>>(message: S) -> Self {
        Self {
            stage: "rom generation".to_string(),
            source: None,
            message: Some(message.into())
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
        let re = Regex::new(r"([0-9]+)ms").unwrap();
        let output = compiler.compile(&mut reader, PathBuf::from("main.gb.s")).expect("Compilation failed");
        re.replace_all(output.as_str(), "XXms").to_string()
    }

    fn compiler_writer<S: Into<String>>(mut compiler: Compiler, s: S) -> MockFileReader {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let re = Regex::new(r"([0-9]+)ms").unwrap();
        compiler.set_silent();
        compiler.set_generate_rom(PathBuf::from("rom.gb"));
        let output = compiler.compile(&mut reader, PathBuf::from("main.gb.s")).expect("Compilation failed");
        re.replace_all(output.as_str(), "XXms").to_string();
        reader
    }

    fn compiler_error<S: Into<String>>(mut compiler: Compiler, s: S) -> (String, String) {
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let re = Regex::new(r"([0-9]+)ms").unwrap();
        let err = compiler.compile(&mut reader, PathBuf::from("main.gb.s")).err().expect("Expected a CompilerError");
        (re.replace_all(err.0.as_str(), "XXms").to_string(), err.1.to_string())
    }

    // STDOUT -----------------------------------------------------------------
    #[test]
    fn test_silent() {
        let mut c = Compiler::new();
        c.set_silent();
        assert_eq!(compiler(c, ""), "");
    }

    #[test]
    fn test_empty_input() {
        let c = Compiler::new();
        assert_eq!(compiler(c, ""), "Compiling \"main.gb.s\"...\nParsing completed in XXms.\nLinking completed in XXms.\nROM validated in XXms.");
    }

    #[test]
    fn test_section_map() {
        let mut c = Compiler::new();
        c.set_print_segment_map();
        let output = compiler(c, "SECTION 'Data',ROM0\nDS 256\nDS 32\nSECTION 'Data1',ROM0\nDS 8\nDS 4\nSECTION 'Data2',ROM0\nDS 16\nSECTION ROM0\nDS 48\nSECTION 'Extra',ROM0[$200]\nDS 5\nSECTION ROMX\nDS 128\nSECTION 'X',ROMX\nDS 32\nSECTION 'Vars',WRAM0\nvar: DB\nSECTION 'Buffer',WRAM0\nbuffer: DS 512\nSECTION 'Vars',HRAM\nfoo: DS 128");
        assert_eq!(
            output,
            r#"Compiling "main.gb.s"...
Parsing completed in XXms.
Linking completed in XXms.

Segment Usage Report:

  ROM0       @ $0000 (  369 of 16384 bytes used) ( 16015 free)

    $0000..=$011f ######## (  288 bytes) (Data)
    $0120..=$012b ######## (   12 bytes) (Data1)
    $012c..=$016b ######## (   64 bytes) (Data2)
    $016c..=$01ff ........ (  148 bytes)
    $0200..=$0204 ######## (    5 bytes) (Extra)
    $0205..=$3fff ........ (15867 bytes)


  ROMX[1]    @ $4000 (  160 of 16384 bytes used) ( 16224 free)

    $4000..=$407f ######## (  128 bytes)
    $4080..=$409f ######## (   32 bytes) (X)
    $40a0..=$7fff ........ (16224 bytes)


  WRAM0      @ $c000 (  513 of  4096 bytes used) (  3583 free)

    $c000..=$c000 ######## (    1 bytes) (Vars)
    $c001..=$c200 ######## (  512 bytes) (Buffer)
    $c201..=$cfff ........ ( 3583 bytes)


  HRAM       @ $ff80 (  128 of   128 bytes used) (     0 free)

    $ff80..=$ffff ######## (  128 bytes) (Vars)

ROM validated in XXms."#
        );
    }

    // Symbol Map -------------------------------------------------------------
    #[test]
    fn test_symbol_map() {
        let mut c = Compiler::new();
        c.set_generate_symbols(PathBuf::from("rom.sym"));
        let mut writer = compiler_writer(c, "SECTION ROM0[$150]\nglobal:\nld a,a\n.local:\n");
        let file = writer.get_file("rom.sym").expect("Expected symbol file to be written");
        assert_eq!(file, "00:0150 global\n00:0151 global.local");
    }

    // Debug Stripping --------------------------------------------------------
    #[test]
    fn test_no_debug_strip() {
        let c = Compiler::new();
        let mut writer = compiler_writer(c, "SECTION ROM0[$150]\nbrk");
        let file = writer.get_binary_file("rom.gb").expect("Expected ROM file to be written");
        assert_eq!(file[336..340].to_vec(), vec![64, 0, 0, 0])
    }

    #[test]
    fn test_debug_strip() {
        let mut c = Compiler::new();
        c.set_strip_debug_code();
        let mut writer = compiler_writer(c, "SECTION ROM0[$150]\nbrk");
        let file = writer.get_binary_file("rom.gb").expect("Expected ROM file to be written");
        assert_eq!(file[336..340].to_vec(), vec![0, 0, 0, 0])
    }

    // Optimizations ----------------------------------------------------------
    #[test]
    fn test_no_optimize_instructions() {
        let c = Compiler::new();
        let mut writer = compiler_writer(c, "SECTION ROM0[$150]\nld a,[$ff41]");
        let file = writer.get_binary_file("rom.gb").expect("Expected ROM file to be written");
        assert_eq!(file[336..340].to_vec(), vec![250, 65, 255, 0])
    }

    #[test]
    fn test_optimize_instructions() {
        let mut c = Compiler::new();
        c.set_optimize_instructions();
        let mut writer = compiler_writer(c, "SECTION ROM0[$150]\nld a,[$ff41]");
        let file = writer.get_binary_file("rom.gb").expect("Expected ROM file to be written");
        assert_eq!(file[336..340].to_vec(), vec![240, 65, 0, 0])
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

