// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::path::PathBuf;
use std::time::Instant;


// External Dependencies ------------------------------------------------------
use colored::Colorize;


// Internal Dependencies ------------------------------------------------------
use crate::generator::{Generator, ROMInfo};
use crate::error::SourceError;
use crate::linker::{Linker, SegmentUsage};
use crate::lexer::{Lexer, IncludeStage, MacroStage, ValueStage, ExpressionStage, EntryStage};
use crate::traits::{FileReader, FileWriter};


// Structs --------------------------------------------------------------------
#[derive(Debug)]
pub struct Lint {
    error: SourceError
}

impl Lint {
    pub fn new(error: SourceError) -> Self {
        Self {
            error
        }
    }
}


// Compiler Pipeline Implementation -------------------------------------------
pub struct Compiler {
    silent: bool,
    no_color: bool,
    strip_debug_code: bool,
    optimize_instructions: bool,
    linter_enabled: bool,
    print_segment_map: bool,
    print_rom_info: bool,
    generate_symbols: Option<PathBuf>,
    generate_rom: Option<PathBuf>,
    output: Vec<String>
}

impl Compiler {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            silent: false,
            no_color: false,
            strip_debug_code: false,
            optimize_instructions: false,
            linter_enabled: false,
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

    pub fn set_no_color(&mut self) {
        self.no_color = true;
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

    pub fn set_linter_enabled(&mut self) {
        self.linter_enabled = true;
    }

    pub fn compile<T: FileReader + FileWriter>(&mut self, files: &mut T, entry: PathBuf) -> Result<String, (String, CompilerError)> {
        colored::control::set_override(!self.no_color);
        self.log(format!("{} \"{}\" ...", "   Compiling".bright_green(), entry.display()));
        let entry_lexer = self.parse(files, entry)?;
        let linker = self.link(files, entry_lexer)?;
        if !self.linter_enabled {
            self.generate(files, linker)?;
        }
        Ok(self.output.join("\n"))
    }
}

impl Compiler {

    fn parse<T: FileReader>(&mut self, io: &T, entry: PathBuf) -> Result<Lexer<EntryStage>, (String, CompilerError)> {
        let start = Instant::now();
        let include_lexer = Lexer::<IncludeStage>::from_file(io, &entry).map_err(|e| self.error("file inclusion", e))?;
        self.log(format!("  {} completed in {}ms.", "   File IO".bright_green(), start.elapsed().as_millis()));
        let start = Instant::now();
        let macro_lexer = Lexer::<MacroStage>::from_lexer(include_lexer, self.linter_enabled).map_err(|e| self.error("macro expansion", e))?;
        let value_lexer = Lexer::<ValueStage>::from_lexer(macro_lexer, self.linter_enabled).map_err(|e| self.error("value construction", e))?;
        let expr_lexer = Lexer::<ExpressionStage>::from_lexer(value_lexer, self.linter_enabled).map_err(|e| self.error("expression construction", e))?;
        let entry_lexer = Lexer::<EntryStage>::from_lexer(expr_lexer, self.linter_enabled).map_err(|e| self.error("entry construction", e))?;
        self.log(format!("  {} completed in {}ms.", "   Parsing".bright_green(), start.elapsed().as_millis()));
        Ok(entry_lexer)
    }

    fn link<T: FileReader + FileWriter>(&mut self, io: &mut T, entry_lexer: Lexer<EntryStage>) -> Result<Linker, (String, CompilerError)> {
        let start = Instant::now();
        let linker = Linker::from_lexer(
            io,
            entry_lexer,
            self.strip_debug_code,
            self.optimize_instructions,
            self.linter_enabled

        ).map_err(|e| self.error("section linking", e))?;

        // Report linter warnings only
        if self.linter_enabled {
            self.print_linter_warnings(linker.to_lint_list());
            return Ok(linker);
        }

        self.log(format!("  {} completed in {}ms.", "   Linking".bright_green(), start.elapsed().as_millis()));

        // Report Segment Usage
        if self.print_segment_map {
            self.print_segment_usage(linker.to_usage_list());
        }


        // Generate symbol file for BGB debugger
        if let Some(output_file) = self.generate_symbols.take() {
            let symbols = linker.to_symbol_list().into_iter().map(|(bank, address, name)| {
                format!("{:0>2}:{:0>4x} {}", bank, address, name)

            }).collect::<Vec<String>>().join("\n");
            io.write_file(&output_file, symbols).map_err(|err| {
                (self.output.join("\n"), CompilerError::from_string(
                    format!("Failed to write symbol map to file \"{}\"", err.path.display())
                ))
            })?;
            self.log(format!("     {} symbol map to \"{}\".", "Written".bright_green(), output_file.display()));
        }
        Ok(linker)
    }

    fn generate<T: FileWriter>(&mut self, io: &mut T, linker: Linker) -> Result<(), (String, CompilerError)> {
        let start = Instant::now();
        let mut generator = Generator::from_linker(linker);

        // Validate ROM
        match generator.validate_rom() {
            Ok(warnings) => {
                for w in warnings {
                    self.warning(w);
                }
            },
            Err(err) => return Err((self.output.join("\n"), CompilerError::from_string(err)))
        }

        // Apply checksum etc.
        generator.finalize_rom();
        self.log(format!("{} ROM verified in {}ms.", "   Validated".bright_green(), start.elapsed().as_millis()));

        let info = generator.rom_info();
        if let Some(output_file) = self.generate_rom.take() {
            io.write_binary_file(&output_file, generator.buffer).map_err(|err| {
                (self.output.join("\n"), CompilerError::from_string(
                    format!("Failed to write ROM to file \"{}\"", err.path.display())
                ))
            })?;
            self.log(format!("     {} ROM to \"{}\".", "Written".bright_green(), output_file.display()));
        }

        if self.print_rom_info {
            self.print_rom_info(info);
        }

        Ok(())
    }

    fn print_rom_info(&mut self, info: ROMInfo) {
        self.info(format!("ROM Title: {}", info.title));
        self.info(format!("ROM Version: {}", info.mask_rom_version));
        self.info(format!("ROM Checksum: ${:0>2X} / ${:0>4X}", info.checksum_header, info.checksum_rom));
        self.info(format!("ROM Size: {} bytes", u32::from(info.rom_size) * 1024));
        if info.ram_size > 0 {
            self.info(format!("RAM Size: {} bytes", u32::from(info.ram_size) * 1024));
        }

        if let Some(cart_type) = info.cart_type {
            self.info(format!("ROM Mapper: {}", cart_type.mapper));

        } else {
            self.warning("ROM Mapper: Unknown");
        }
    }

    fn print_segment_usage(&mut self, segments: Vec<SegmentUsage>) {

        self.output.push("".to_string());
        self.info("Segment usage");
        self.output.push("".to_string());
        for s in segments {
            let size = s.end_address - s.start_address;
            let info = format!(
                "({: >5} of {: >5} bytes used) ({: >6} free)",
                s.bytes_in_use,
                size,
                size - s.bytes_in_use
            );
            let offset = format!("${:0>4x}", s.start_address);
            self.output.push(format!("{: >12} {} {}", s.name.bright_blue(), offset.bright_yellow(), info.bright_green()));
            self.output.push("".to_string());
            for (used, name, start, end) in s.ranges {
                let display_name = if let Some(name) = name {
                    format!("  ({})", name)

                } else {
                    "".to_string()
                };
                let marker = if used {
                    "  ======".to_string()

                } else {
                    "  ~~~~~~".to_string()
                };
                let line = format!(
                    "             ${:0>4x}{}=${:0>4x} {}  {: >5} bytes{}",
                    start,
                    "..".bright_black(),
                    end - 1,
                    marker,
                    end - start,
                    display_name
                );
                self.output.push(if used {
                    line

                } else {
                    line.bright_black().to_string()
                });
            }
            self.output.push("".to_string());
            self.output.push("".to_string());
        }
        self.output.pop();
    }

    fn print_linter_warnings(&mut self, lints: Vec<Lint>) {
        self.info("Linter Report");
        for lint in lints {
            self.warning(format!("{}", lint.error));
        }
        self.output.push("".to_string());
    }

    // Helpers ----------------------------------------------------------------
    fn log<S: Into<String>>(&mut self, s: S) {
        if !self.silent && !self.linter_enabled {
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
            write!(f, "       {} Compilation failed during {} phase!\n\n{}", "Error".bright_red(), self.stage, source)

        } else if let Some(message) = self.message.as_ref() {
            write!(f, "       {} Compilation failed during {} phase!\n\n{}", "Error".bright_red(), self.stage, message)

        } else {
            write!(f, "       {} Compilation failed during {} phase!", "Error".bright_red(), self.stage)
        }
    }
}

// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use regex::Regex;
    use std::path::PathBuf;

    use crate::mocks::MockFileReader;
    use super::Compiler;

    fn compiler<S: Into<String>>(mut compiler: Compiler, s: S) -> String {
        compiler.set_no_color();
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let re = Regex::new(r"([0-9]+)ms").unwrap();
        let output = compiler.compile(&mut reader, PathBuf::from("main.gb.s")).expect("Compilation failed");
        re.replace_all(output.as_str(), "XXms").to_string()
    }

    fn compiler_writer<S: Into<String>>(mut compiler: Compiler, s: S) -> (String, MockFileReader) {
        compiler.set_no_color();
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let re = Regex::new(r"([0-9]+)ms").unwrap();
        compiler.set_generate_rom(PathBuf::from("rom.gb"));
        let output = compiler.compile(&mut reader, PathBuf::from("main.gb.s")).expect("Compilation failed");
        let output = re.replace_all(output.as_str(), "XXms").to_string();
        (output, reader)
    }

    fn compiler_error<S: Into<String>>(mut compiler: Compiler, s: S) -> (String, String) {
        compiler.set_no_color();
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let re = Regex::new(r"([0-9]+)ms").unwrap();
        let err = compiler.compile(&mut reader, PathBuf::from("main.gb.s")).err().expect("Expected a CompilerError");
        (re.replace_all(err.0.as_str(), "XXms").to_string(), err.1.to_string())
    }

    fn compiler_lint<S: Into<String>>(mut compiler: Compiler, s: S) -> String {
        compiler.set_no_color();
        compiler.set_linter_enabled();
        let mut reader = MockFileReader::default();
        reader.add_file("main.gb.s", s.into().as_str());
        let re = Regex::new(r"([0-9]+)ms").unwrap();
        let output = compiler.compile(&mut reader, PathBuf::from("main.gb.s")).expect("Compilation failed");
        re.replace_all(output.as_str(), "XXms").to_string()
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
        assert_eq!(compiler(c, ""), "   Compiling \"main.gb.s\" ...\n     File IO completed in XXms.\n     Parsing completed in XXms.\n     Linking completed in XXms.\n   Validated ROM verified in XXms.");
    }

    #[test]
    fn test_section_map() {
        let mut c = Compiler::new();
        c.set_print_segment_map();
        let output = compiler(c, "SECTION 'Data',ROM0\nDS 256\nDS 32\nSECTION 'Data1',ROM0\nDS 8\nDS 4\nSECTION 'Data2',ROM0\nDS 16\nSECTION ROM0\nDS 48\nSECTION 'Extra',ROM0[$200]\nDS 5\nSECTION ROMX\nDS 128\nSECTION 'X',ROMX\nDS 32\nSECTION 'Vars',WRAM0\nvar: DB\nSECTION 'Buffer',WRAM0\nbuffer: DS 512\nSECTION 'Vars',HRAM\nfoo: DS 128");
        assert_eq!(
            output,
            r#"   Compiling "main.gb.s" ...
     File IO completed in XXms.
     Parsing completed in XXms.
     Linking completed in XXms.

        Info Segment usage

        ROM0 $0000 (  369 of 16384 bytes used) ( 16015 free)

             $0000..=$011f   ======    288 bytes  (Data)
             $0120..=$012b   ======     12 bytes  (Data1)
             $012c..=$016b   ======     64 bytes  (Data2)
             $016c..=$01ff   ~~~~~~    148 bytes
             $0200..=$0204   ======      5 bytes  (Extra)
             $0205..=$3fff   ~~~~~~  15867 bytes


     ROMX[1] $4000 (  160 of 16384 bytes used) ( 16224 free)

             $4000..=$407f   ======    128 bytes
             $4080..=$409f   ======     32 bytes  (X)
             $40a0..=$7fff   ~~~~~~  16224 bytes


       WRAM0 $c000 (  513 of  4096 bytes used) (  3583 free)

             $c000..=$c000   ======      1 bytes  (Vars)
             $c001..=$c200   ======    512 bytes  (Buffer)
             $c201..=$cfff   ~~~~~~   3583 bytes


        HRAM $ff80 (  128 of   128 bytes used) (     0 free)

             $ff80..=$ffff   ======    128 bytes  (Vars)

   Validated ROM verified in XXms."#
        );
    }

    // Symbol Map -------------------------------------------------------------
    #[test]
    fn test_symbol_map() {
        let mut c = Compiler::new();
        c.set_generate_symbols(PathBuf::from("rom.sym"));
        let (output, mut writer) = compiler_writer(c, "SECTION ROM0[$150]\nglobal:\nld a,a\n.local:\n");
        let file = writer.get_file("rom.sym").expect("Expected symbol file to be written");
        assert_eq!(file, "00:0150 global\n00:0151 global.local");
        assert_eq!(output, "   Compiling \"main.gb.s\" ...\n     File IO completed in XXms.\n     Parsing completed in XXms.\n     Linking completed in XXms.\n     Written symbol map to \"rom.sym\".\n   Validated ROM verified in XXms.\n     Written ROM to \"rom.gb\".");
    }

    // Debug Stripping --------------------------------------------------------
    #[test]
    fn test_no_debug_strip() {
        let mut c = Compiler::new();
        c.set_silent();
        let (_, mut writer) = compiler_writer(c, "SECTION ROM0[$150]\nbrk");
        let file = writer.get_binary_file("rom.gb").expect("Expected ROM file to be written");
        assert_eq!(file[336..340].to_vec(), vec![64, 0, 0, 0])
    }

    #[test]
    fn test_debug_strip() {
        let mut c = Compiler::new();
        c.set_silent();
        c.set_strip_debug_code();
        let (_, mut writer) = compiler_writer(c, "SECTION ROM0[$150]\nbrk");
        let file = writer.get_binary_file("rom.gb").expect("Expected ROM file to be written");
        assert_eq!(file[336..340].to_vec(), vec![0, 0, 0, 0])
    }

    // Optimizations ----------------------------------------------------------
    #[test]
    fn test_no_optimize_instructions() {
        let mut c = Compiler::new();
        c.set_silent();
        let (_, mut writer) = compiler_writer(c, "SECTION ROM0[$150]\nld a,[$ff41]");
        let file = writer.get_binary_file("rom.gb").expect("Expected ROM file to be written");
        assert_eq!(file[336..340].to_vec(), vec![250, 65, 255, 0])
    }

    #[test]
    fn test_optimize_instructions() {
        let mut c = Compiler::new();
        c.set_silent();
        c.set_optimize_instructions();
        let (_, mut writer) = compiler_writer(c, "SECTION ROM0[$150]\nld a,[$ff41]");
        let file = writer.get_binary_file("rom.gb").expect("Expected ROM file to be written");
        assert_eq!(file[336..340].to_vec(), vec![240, 65, 0, 0])
    }

    // ROM Info ----------------------------------------------------------
    #[test]
    fn test_rom_info_defaults() {
        let mut c = Compiler::new();
        c.set_print_rom_info();
        assert_eq!(compiler(c, ""), "   Compiling \"main.gb.s\" ...\n     File IO completed in XXms.\n     Parsing completed in XXms.\n     Linking completed in XXms.\n   Validated ROM verified in XXms.\n        Info ROM Title: \n        Info ROM Version: 0\n        Info ROM Checksum: $E7 / $162D\n        Info ROM Size: 32768 bytes\n        Info ROM Mapper: ROM");
    }

    #[test]
    fn test_rom_info_mbc5() {
        let header = r#"
            SECTION 'Rom Title',ROM0[$134]
            DS 12 'ABCDEFGHIJKL'

            SECTION 'Core Rom Header',ROM0[$143]
            DB $80                      ; $143
            DS 2 'BD'                   ; $144 - Licensee code (not important)
            DB 0                        ; $146 - SGB Support indicator
            DB $19                      ; $147 - Cart type
            DB 1                        ; $148 - ROM Size
            DB 2                        ; $149 - RAM Size
            DB 1                        ; $14a - Destination code
            DB $33                      ; $14b - Old licensee code
            DB 0                        ; $14c - Mask ROM version
            DB 0                        ; $14d - Header checksum (important)
            DW 0                        ; $14e - Global checksum (not important)
        "#;

        let output = r#"   Compiling "main.gb.s" ...
     File IO completed in XXms.
     Parsing completed in XXms.
     Linking completed in XXms.
   Validated ROM verified in XXms.
        Info ROM Title: ABCDEFGHIJK
        Info ROM Version: 0
        Info ROM Checksum: $43 / $1A2D
        Info ROM Size: 65536 bytes
        Info RAM Size: 8192 bytes
        Info ROM Mapper: MBC5"#;

        let mut c = Compiler::new();
        c.set_print_rom_info();
        assert_eq!(compiler(c, header), output)

    }

    // Errors -----------------------------------------------------------------
    #[test]
    fn test_error_lexer() {
        let c = Compiler::new();
        assert_eq!(compiler_error(c, "@"), (
            "   Compiling \"main.gb.s\" ...".to_string(),
            "       Error Compilation failed during file inclusion phase!\n\nIn file \"main.gb.s\" on line 1, column 1: Unexpected character \"@\".\n\n@\n^--- Here".to_string()
        ));
    }

    #[test]
    fn test_error_linking() {
        let c = Compiler::new();
        assert_eq!(compiler_error(c, "ld a,a"), (
            "   Compiling \"main.gb.s\" ...\n     File IO completed in XXms.\n     Parsing completed in XXms.".to_string(),
            "       Error Compilation failed during section linking phase!\n\nIn file \"main.gb.s\" on line 1, column 1: Unexpected ROM entry before any section declaration\n\nld a,a\n^--- Here".to_string()
        ));
    }

    // Lints ------------------------------------------------------------------
    #[test]
    fn test_linting_unused_constant_direct() {
        let c = Compiler::new();
        assert_eq!(
            compiler_lint(c, "SECTION ROM0\nUNUSED EQU 2\nUSED EQU 1\nDB USED"),
            "        Info Linter Report\n     Warning Constant \"UNUSED\" is never used, declared in file \"main.gb.s\" on line 2, column 1\n".to_string()
        );
    }

    #[test]
    fn test_linting_unused_constant_indirect() {
        let c = Compiler::new();
        assert_eq!(
            compiler_lint(c, "SECTION ROM0\nUNUSED EQU INDIRECT_UNUSED\nINDIRECT_UNUSED EQU 2\nINDIRECT_USED EQU 1\nUSED EQU INDIRECT_USED\nDB USED"),
            "        Info Linter Report\n     Warning Constant \"INDIRECT_UNUSED\" is never used, declared in file \"main.gb.s\" on line 3, column 1\n     Warning Constant \"UNUSED\" is never used, declared in file \"main.gb.s\" on line 2, column 1\n".to_string()
        );
    }

    #[test]
    fn test_linting_unused_label_direct() {
        let c = Compiler::new();
        assert_eq!(
            compiler_lint(c, "SECTION ROM0\nused_label:\nunused_label:\njp used_label"),
            "        Info Linter Report\n     Warning Label \"unused_label\" is never referenced, declared in file \"main.gb.s\" on line 3, column 1\n".to_string()
        );
    }

    #[test]
    fn test_linting_replace_fixed_value_with_constant() {
        let c = Compiler::new();
        assert_eq!(
            compiler_lint(c, "SECTION ROM0\nUSED EQU 2048\nDW USED\nDW 2048"),
            "        Info Linter Report\n     Warning Fixed integer value ($0800) could be replaced with the constant \"USED\" that shares the same value in file \"main.gb.s\" on line 4, column 4\n".to_string()
        );
        // TODO test multiple files
    }

    #[test]
    fn test_linting_constant_global_not_used_globally() {
        let c = Compiler::new();
        assert_eq!(
            compiler_lint(c, "SECTION ROM0\nGLOBAL USED EQU 2048\nDW USED"),
            "        Info Linter Report\n     Warning Constant \"USED\" should be made non-global or default, since it is never used outside its declaration in file \"main.gb.s\" on line 2, column 8\n".to_string()
        );
    }

}

