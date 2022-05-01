// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::path::PathBuf;
use std::time::Instant;


// External Dependencies ------------------------------------------------------
use colored::Colorize;
use file_io::{FileReader, FileWriter, Logger};
use lsp_types::{Diagnostic, DiagnosticSeverity, Url, Range, Position, Location, SymbolKind};


// Internal Dependencies ------------------------------------------------------
use crate::generator::{Generator, ROMInfo};
use crate::error::SourceError;
use crate::linker::{AnalysisLint, Linker, SegmentUsage};
use crate::lexer::{Lexer, IncludeStage, MacroStage, ValueStage, ExpressionStage, EntryStage, IntegerMap, MacroDefinition};


// Compiler Pipeline Implementation -------------------------------------------
pub struct Compiler {
    no_color: bool,
    strip_debug_code: bool,
    optimize_instructions: bool,
    print_segment_map: bool,
    print_rom_info: bool,
    generate_source_map: Option<PathBuf>,
    generate_symbol_map: Option<PathBuf>,
    generate_rom: Option<PathBuf>
}

impl Compiler {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            no_color: false,
            strip_debug_code: false,
            optimize_instructions: false,
            print_segment_map: false,
            print_rom_info: false,
            generate_source_map: None,
            generate_symbol_map: None,
            generate_rom: None
        }
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

    pub fn set_generate_symbol_map(&mut self, path: PathBuf) {
        self.generate_symbol_map = Some(path);
    }

    pub fn set_generate_source_map(&mut self, path: PathBuf) {
        self.generate_source_map = Some(path);
    }

    pub fn set_strip_debug_code(&mut self) {
        self.strip_debug_code = true;
    }

    pub fn set_optimize_instructions(&mut self) {
        self.optimize_instructions = true;
    }

    pub fn compile<T: FileReader + FileWriter>(
        &mut self,
        logger: &mut Logger,
        io: &mut T,
        file: PathBuf

    ) -> Result<Linker, CompilationError> {
        colored::control::set_override(!self.no_color);
        self.status(logger, "Compiling", format!("\"{}\" ...", file.display()));
        let (entry_lexer, macro_defs, integers) = self.parse(logger, io, file)?;
        let linker = self.link(logger, io, entry_lexer, macro_defs, integers)?;
        self.generate(logger, io, &linker)?;
        Ok(linker)
    }

    pub fn create_linker<T: FileReader + FileWriter>(
        &mut self,
        logger: &mut Logger,
        io: &mut T,
        file: PathBuf

    ) -> Result<Linker, CompilationError> {
        colored::control::set_override(false);
        let (entry_lexer, macro_defs, integers) = self.parse(logger, io, file)?;
        self.link(logger, io, entry_lexer, macro_defs, integers)
    }
}

impl Compiler {
    fn parse<T: FileReader>(
        &mut self,
        logger: &mut Logger,
        io: &T,
        file: PathBuf

    ) -> Result<(Lexer<EntryStage>, Vec<MacroDefinition>, IntegerMap), CompilationError> {
        let start = Instant::now();
        let include_lexer = Lexer::<IncludeStage>::from_file(io, &file).map_err(|e| CompilationError::new("file inclusion", e))?;
        self.status(logger, "File IO", format!("completed in {}ms.", start.elapsed().as_millis()));
        let start = Instant::now();
        let mut macro_lexer = Lexer::<MacroStage>::from_lexer(include_lexer).map_err(|e| CompilationError::new("macro expansion", e))?;
        let macro_defs = macro_lexer.data();
        let value_lexer = Lexer::<ValueStage>::from_lexer(macro_lexer).map_err(|e| CompilationError::new("value construction", e))?;
        let mut expr_lexer = Lexer::<ExpressionStage>::from_lexer(value_lexer).map_err(|e| CompilationError::new("expression construction", e))?;
        let integers = expr_lexer.data().remove(0);
        let entry_lexer = Lexer::<EntryStage>::from_lexer(expr_lexer).map_err(|e| CompilationError::new("entry construction", e))?;
        self.status(logger, "Parsing", format!("completed in {}ms.", start.elapsed().as_millis()));
        Ok((entry_lexer, macro_defs, integers))
    }

    fn link<T: FileReader + FileWriter>(
        &mut self,
        logger: &mut Logger,
        io: &mut T,
        entry_lexer: Lexer<EntryStage>,
        macro_defs: Vec<MacroDefinition>,
        integers: IntegerMap

    ) -> Result<Linker, CompilationError> {
        let start = Instant::now();
        let linker = Linker::from_lexer(
            io,
            entry_lexer,
            macro_defs,
            integers,
            self.strip_debug_code,
            self.optimize_instructions

        ).map_err(|e| CompilationError::new("section linking", e))?;

        logger.status("Linking", format!("completed in {}ms.", start.elapsed().as_millis()));

        // Report Segment Usage
        if self.print_segment_map {
            self.print_segment_usage(logger, linker.usage_list());
        }

        // Generate symbol file for debuggers
        if let Some(output_file) = self.generate_symbol_map.take() {
            let symbols = linker.symbol_list().into_iter().map(|(bank, address, name)| {
                let mut size = None;
                for s in &linker.analysis.symbols {
                    if s.name == *name {
                        if s.kind == SymbolKind::VARIABLE && s.width > 0 {
                            size = Some(s.width);
                        }
                        break;
                    }
                }
                if let Some(s) = size {
                    format!("{:0>2}:{:0>4x} {}:{}", bank, address, name, s)

                } else {
                    format!("{:0>2}:{:0>4x} {}", bank, address, name)
                }

            }).collect::<Vec<String>>().join("\n");
            io.write_file(&output_file, symbols).map_err(|err| {
                CompilationError::from_string(
                    format!("Failed to write symbol map to file \"{}\"", err.path.display())
                )
            })?;
            logger.status("Written", format!("symbol map to \"{}\".", output_file.display()));
        }

        // Generate source map file for debuggers
        if let Some(output_file) = self.generate_source_map.take() {
            let mut addresses: Vec<(usize, Location)> = linker.address_location_map().into_iter().collect();
            addresses.sort_unstable_by(|a, b| a.0.cmp(&b.0));

            let base_dir = io.base_dir();
            let mut locations = addresses.into_iter().map(|(offset, location)| {
                let p = PathBuf::from(location.uri.path());
                let relative = p.strip_prefix(base_dir).unwrap_or(&p);
                format!(
                    "a {:0>2}:{:0>4x} {:?},{},{}",
                    offset / 0x4000,
                    offset % 0x4000,
                    relative.display(),
                    location.range.start.line + 1,
                    location.range.start.character + 1
                )

            }).collect::<Vec<String>>();
            let mut scopes = linker.analysis.scopes.iter().map(|s| {
                format!(
                    "s {:0>2}:{:0>4X}-{:0>2}:{:0>4X} {} {}",
                    s.address_range.0 / 0x4000,
                    s.address_range.0 % 0x4000,
                    s.address_range.1 / 0x4000,
                    s.address_range.1 % 0x4000,
                    s.io_references.join(","),
                    s.symbol_references.join(",")
                )

            }).collect::<Vec<String>>();
            locations.append(&mut scopes);
            io.write_file(&output_file, locations.join("\n")).map_err(|err| {
                CompilationError::from_string(
                    format!("Failed to write source map to file \"{}\"", err.path.display())
                )
            })?;
            logger.status("Written", format!("source map to \"{}\".", output_file.display()));
        }

        // Report Warnings
        for lint in &linker.analysis.lints {
            logger.warning(format!(
                "{}: `{}` (in file \"{}\" on line {}, column {})",
                lint.detail.message,
                lint.context,
                lint.uri.path(),
                lint.detail.range.start.line + 1,
                lint.detail.range.start.character + 1
            ));
        }
        Ok(linker)
    }

    fn generate<T: FileWriter>(
        &mut self,
        logger: &mut Logger,
        io: &mut T,
        linker: &Linker

    ) -> Result<(), CompilationError> {
        let start = Instant::now();
        let mut generator = Generator::from_linker(linker);

        // Validate ROM
        match generator.validate_rom() {
            Ok(warnings) => {
                for w in warnings {
                    logger.warning(w);
                }
            },
            Err(err) => return Err(CompilationError::from_string(err))
        }

        // Apply checksum etc.
        generator.finalize_rom();
        logger.status("Validated", format!("ROM verified in {}ms.", start.elapsed().as_millis()));

        let info = generator.rom_info();
        if let Some(output_file) = self.generate_rom.take() {
            io.write_binary_file(&output_file, generator.buffer).map_err(|err| {
                CompilationError::from_string(
                    format!("Failed to write ROM to file \"{}\"", err.path.display())
                )
            })?;
            logger.status("Written", format!("ROM to \"{}\".", output_file.display()));
        }

        if self.print_rom_info {
            self.print_rom_info(logger, info);
        }

        Ok(())
    }

    fn print_rom_info(&mut self, logger: &mut Logger, info: ROMInfo) {
        logger.info(format!("ROM Title: {}", info.title));
        logger.info(format!("ROM Version: {}", info.mask_rom_version));
        logger.info(format!("ROM Checksum: ${:0>2X} / ${:0>4X}", info.checksum_header, info.checksum_rom));
        logger.info(format!("ROM Size: {} bytes", u32::from(info.rom_size) * 1024));
        if info.ram_size > 0 {
            logger.info(format!("RAM Size: {} bytes", u32::from(info.ram_size) * 1024));
        }

        if let Some(cart_type) = info.cart_type {
            logger.info(format!("ROM Mapper: {}", cart_type.mapper));

        } else {
            logger.warning("ROM Mapper: Unknown");
        }
    }

    fn print_segment_usage(&mut self, logger: &mut Logger, segments: Vec<SegmentUsage>) {
        logger.newline();
        logger.info("Segment usage");
        logger.newline();
        for s in segments {
            let size = s.end_address - s.start_address;
            let info = format!(
                "({: >5} of {: >5} bytes used) ({: >6} free)",
                s.bytes_in_use,
                size,
                size - s.bytes_in_use
            );
            let offset = format!("${:0>4x}", s.start_address);
            logger.log(format!("{: >12} {} {}", s.name.bright_blue(), offset.bright_yellow(), info.bright_green()));
            logger.newline();
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
                logger.log(if used {
                    line

                } else {
                    line.bright_black().to_string()
                });
            }
            logger.newline();
            logger.newline();
        }
        logger.clearline();
    }

    fn status(&self, logger: &mut Logger, p: &str, s: String) {
        logger.status(p, s);
    }
}


// Compiler Error Abstraction -------------------------------------------------
#[derive(Debug)]
pub struct CompilationError {
    stage: String,
    source: Option<SourceError>,
    message: Option<String>
}

impl CompilationError {

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

    pub fn into_lint(self) -> Option<AnalysisLint> {
        if let Some(source) = self.source {
            if let Some((path, line, col)) = source.location {
                Some(AnalysisLint {
                    uri: Url::from_file_path(path).unwrap(),
                    context: "".to_string(),
                    detail: Diagnostic {
                        range: Range {
                            start: Position {
                                line: line as u32,
                                character: col as u32
                            },
                            end: Position {
                                line: line as u32,
                                character: col as u32
                            }
                        },
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: source.raw_message,
                        code: None,
                        code_description: None,
                        source: None,
                        related_information: None,
                        tags: None,
                        data: None
                    }
                })

            } else {
                None
            }

        } else {
            None
        }
    }
}

impl fmt::Display for CompilationError {
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
    use super::{Compiler, Logger};

    fn compiler<S: Into<String>>(mut logger: Logger, mut compiler: Compiler, s: S) -> String {
        compiler.set_no_color();
        let mut reader = MockFileReader::new();
        reader.add_file("/main.gbc", s.into().as_str());
        let re = Regex::new(r"([0-9]+)ms").unwrap();
        compiler.compile(&mut logger, &mut reader, PathBuf::from("/main.gbc")).expect("Compilation failed");
        re.replace_all(logger.to_string().as_str(), "XXms").to_string()
    }

    fn compiler_writer<S: Into<String>>(mut logger: Logger, mut compiler: Compiler, s: S) -> (String, MockFileReader) {
        compiler.set_no_color();
        let mut reader = MockFileReader::new();
        reader.add_file("/main.gbc", s.into().as_str());
        let re = Regex::new(r"([0-9]+)ms").unwrap();
        compiler.set_generate_rom(PathBuf::from("rom.gb"));
        compiler.compile(&mut logger, &mut reader, PathBuf::from("/main.gbc")).expect("Compilation failed");
        let output = re.replace_all(logger.to_string().as_str(), "XXms").to_string();
        (output, reader)
    }

    fn compiler_error<S: Into<String>>(mut logger: Logger, mut compiler: Compiler, s: S) -> (String, String) {
        compiler.set_no_color();
        let mut reader = MockFileReader::new();
        reader.add_file("/main.gbc", s.into().as_str());
        let re = Regex::new(r"([0-9]+)ms").unwrap();
        let err = compiler.compile(&mut logger, &mut reader, PathBuf::from("/main.gbc")).err().expect("Expected a CompilationError");
        (re.replace_all(logger.to_string().as_str(), "XXms").to_string(), err.to_string())
    }

    // STDOUT -----------------------------------------------------------------
    #[test]
    fn test_silent() {
        let mut l = Logger::new();
        l.set_silent();
        let c = Compiler::new();
        assert_eq!(compiler(l, c, ""), "");
    }

    #[test]
    fn test_empty_input() {
        let l = Logger::new();
        let c = Compiler::new();
        assert_eq!(compiler(l, c, ""), "   Compiling \"/main.gbc\" ...\n     File IO completed in XXms.\n     Parsing completed in XXms.\n     Linking completed in XXms.\n   Validated ROM verified in XXms.");
    }

    #[test]
    fn test_section_map() {
        let l = Logger::new();
        let mut c = Compiler::new();
        c.set_print_segment_map();
        let output = compiler(l, c, "SECTION 'Data',ROM0\nDS 256\nDS 32\nSECTION 'Data1',ROM0\nDS 8\nDS 4\nSECTION 'Data2',ROM0\nDS 16\nSECTION ROM0\nDS 48\nSECTION 'Extra',ROM0[$200]\nDS 5\nSECTION ROMX\nDS 128\nSECTION 'X',ROMX\nDS 32\nSECTION 'Vars',WRAM0\n_var: DB\nSECTION 'Buffer',WRAM0\n_buffer: DS 512\nSECTION 'Vars',HRAM\n_foo: DS 128");
        assert_eq!(
            output,
            r#"   Compiling "/main.gbc" ...
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
        let l = Logger::new();
        let mut c = Compiler::new();
        c.set_generate_symbol_map(PathBuf::from("rom.sym"));
        let (output, mut writer) = compiler_writer(l, c, "SECTION HRAM\nvariable: DB\nSECTION ROM0[$150]\n_global:\nld a,a\n.local:\nld a,[variable]");
        let file = writer.get_file("rom.sym").expect("Expected symbol file to be written");
        assert_eq!(file, "00:0150 _global\n00:0151 _global.local\n00:ff80 variable:1");
        assert_eq!(output, "   Compiling \"/main.gbc\" ...\n     File IO completed in XXms.\n     Parsing completed in XXms.\n     Linking completed in XXms.\n     Written symbol map to \"rom.sym\".\n   Validated ROM verified in XXms.\n     Written ROM to \"rom.gb\".");
    }

    // Source Map -------------------------------------------------------------
    #[test]
    fn test_source_map() {
        let l = Logger::new();
        let mut c = Compiler::new();
        c.set_generate_source_map(PathBuf::from("rom.map"));
        let (output, mut writer) = compiler_writer(l, c, "CONST rSCX $FF42\nSECTION HRAM\nvariable: DB\nSECTION ROM0[$150]\n_global:\nld a,a\n.local:\nld a,[variable]\nld a,[$FF40]\nld [rSCX],a\n\ncall _global");
        let file = writer.get_file("rom.map").expect("Expected source map to be written");
        assert_eq!(file, "a 00:0150 \"main.gbc\",6,1\na 00:0151 \"main.gbc\",8,1\na 00:0154 \"main.gbc\",9,1\na 00:0157 \"main.gbc\",10,1\na 00:015a \"main.gbc\",12,1\ns 00:0150-00:015A FF40,FF42 _global,variable");
        assert_eq!(output, "   Compiling \"/main.gbc\" ...\n     File IO completed in XXms.\n     Parsing completed in XXms.\n     Linking completed in XXms.\n     Written source map to \"rom.map\".\n   Validated ROM verified in XXms.\n     Written ROM to \"rom.gb\".");
    }

    // Debug Stripping --------------------------------------------------------
    #[test]
    fn test_no_debug_strip() {
        let mut l = Logger::new();
        l.set_silent();
        let c = Compiler::new();
        let (_, mut writer) = compiler_writer(l, c, "SECTION ROM0[$150]\nbrk");
        let file = writer.get_binary_file("rom.gb").expect("Expected ROM file to be written");
        assert_eq!(file[336..340].to_vec(), vec![64, 0, 0, 0])
    }

    #[test]
    fn test_debug_strip() {
        let mut l = Logger::new();
        l.set_silent();
        let mut c = Compiler::new();
        c.set_strip_debug_code();
        let (_, mut writer) = compiler_writer(l, c, "SECTION ROM0[$150]\nbrk");
        let file = writer.get_binary_file("rom.gb").expect("Expected ROM file to be written");
        assert_eq!(file[336..340].to_vec(), vec![0, 0, 0, 0])
    }

    // Optimizations ----------------------------------------------------------
    #[test]
    fn test_no_optimize_instructions() {
        let mut l = Logger::new();
        l.set_silent();
        let c = Compiler::new();
        let (_, mut writer) = compiler_writer(l, c, "SECTION ROM0[$150]\nld a,[$ff41]");
        let file = writer.get_binary_file("rom.gb").expect("Expected ROM file to be written");
        assert_eq!(file[336..340].to_vec(), vec![250, 65, 255, 0])
    }

    #[test]
    fn test_optimize_instructions() {
        let mut l = Logger::new();
        l.set_silent();
        let mut c = Compiler::new();
        c.set_optimize_instructions();
        let (_, mut writer) = compiler_writer(l, c, "SECTION ROM0[$150]\nld a,[$ff41]");
        let file = writer.get_binary_file("rom.gb").expect("Expected ROM file to be written");
        assert_eq!(file[336..340].to_vec(), vec![240, 65, 0, 0])
    }

    // ROM Info ----------------------------------------------------------
    #[test]
    fn test_rom_info_defaults() {
        let l = Logger::new();
        let mut c = Compiler::new();
        c.set_print_rom_info();
        assert_eq!(compiler(l, c, ""), "   Compiling \"/main.gbc\" ...\n     File IO completed in XXms.\n     Parsing completed in XXms.\n     Linking completed in XXms.\n   Validated ROM verified in XXms.\n        Info ROM Title: \n        Info ROM Version: 0\n        Info ROM Checksum: $E7 / $162D\n        Info ROM Size: 32768 bytes\n        Info ROM Mapper: ROM");
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

        let output = r#"   Compiling "/main.gbc" ...
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

        let l = Logger::new();
        let mut c = Compiler::new();
        c.set_print_rom_info();
        assert_eq!(compiler(l, c, header), output)

    }

    // Warnings ---------------------------------------------------------------
    #[test]
    fn test_warning_unused_label() {
        let l = Logger::new();
        let c = Compiler::new();
        assert_eq!(compiler(l, c, "SECTION 'Test',ROM0\nlabel:"), (
            "   Compiling \"/main.gbc\" ...\n     File IO completed in XXms.\n     Parsing completed in XXms.\n     Linking completed in XXms.\n     Warning unused label: `label` (in file \"/main.gbc\" on line 2, column 1)\n   Validated ROM verified in XXms."
        ));
    }

    #[test]
    fn test_warning_global_label_only_used_locally() {
        let l = Logger::new();
        let c = Compiler::new();
        assert_eq!(compiler(l, c, "SECTION 'Test',ROM0\nDW label\nGLOBAL label:"), (
            "   Compiling \"/main.gbc\" ...\n     File IO completed in XXms.\n     Parsing completed in XXms.\n     Linking completed in XXms.\n     Warning global label only ever used locally: `label` (in file \"/main.gbc\" on line 3, column 8)\n   Validated ROM verified in XXms."
        ));
    }

    // Errors -----------------------------------------------------------------
    #[test]
    fn test_error_lexer() {
        let l = Logger::new();
        let c = Compiler::new();
        assert_eq!(compiler_error(l, c, "@"), (
            "   Compiling \"/main.gbc\" ...".to_string(),
            "       Error Compilation failed during file inclusion phase!\n\nIn file \"/main.gbc\" on line 1, column 1: Expected a valid jump offset value \"@\".\n\n@\n^--- Here".to_string()
        ));
    }

    #[test]
    fn test_error_linking() {
        let l = Logger::new();
        let c = Compiler::new();
        assert_eq!(compiler_error(l, c, "ld a,a"), (
            "   Compiling \"/main.gbc\" ...\n     File IO completed in XXms.\n     Parsing completed in XXms.".to_string(),
            "       Error Compilation failed during section linking phase!\n\nIn file \"/main.gbc\" on line 1, column 1: Unexpected ROM entry before any section declaration\n\nld a,a\n^--- Here".to_string()
        ));
    }
}

