// STD Dependencies -----------------------------------------------------------
use std::fs::{DirBuilder, File};
use std::path::PathBuf;
use std::io::{Error as IOError, Read, ErrorKind};
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use serde::Deserialize;
use file_io::Logger;
use compiler::linker::{Completion, Lookup};
use compiler::compiler::{Compiler, CompilationError};
use compiler::lexer::{LexerFile, stage::include::IncludeToken};


// Internal Dependencies ------------------------------------------------------
use crate::reader::ProjectReader;


// GBC Project Configuration --------------------------------------------------
#[derive(Debug, Deserialize)]
pub struct ProjectConfig {
    pub rom: RomConfig,
    pub emulator: HashMap<String, EmulatorConfig>,
    #[serde(default)]
    pub report: ReportConfig
}

impl ProjectConfig {
    pub fn load(logger: &mut Logger, reader: &ProjectReader) -> ProjectConfig {
        match Self::try_load(logger, reader) {
            Ok(project) => project,
            Err(err) => {
                logger.fail(Logger::format_error(
                    format!("Failed when trying to parse project configuration file!\n\n{}", err.to_string())
                ))
            }
        }
    }

    pub fn complete(project: &ProjectConfig, logger: &mut Logger, local_file: &str) -> Result<Vec<Completion>, CompilationError> {
        let mut reader = ProjectReader::from_relative(project.rom.input.clone());
        let main_file = PathBuf::from(project.rom.input.file_name().unwrap());
        let local_file = PathBuf::from(local_file);
        let mut compiler = Compiler::new();
        compiler.set_optimize_instructions();
        compiler.set_strip_debug_code();
        compiler.complete(logger, &mut reader, main_file, local_file)
    }

    pub fn lookup(project: &ProjectConfig, logger: &mut Logger, name: String) -> Result<Option<Lookup>, CompilationError> {
        let mut reader = ProjectReader::from_relative(project.rom.input.clone());
        let main_file = PathBuf::from(project.rom.input.file_name().unwrap());
        let mut compiler = Compiler::new();
        compiler.set_optimize_instructions();
        compiler.set_strip_debug_code();
        compiler.set_linter_enabled();
        compiler.lookup(logger, &mut reader, main_file, name)
    }

    pub fn tokenize(project: &ProjectConfig, logger: &mut Logger, local_file: &str, line: usize, col: usize) -> Result<Option<(IncludeToken, LexerFile)>, CompilationError> {
        let mut reader = ProjectReader::from_relative(project.rom.input.clone());
        let mut compiler = Compiler::new();
        let local_file = PathBuf::from(local_file);
        let local_file = local_file.strip_prefix(reader.base_dir()).unwrap();
        compiler.tokenize(logger, &mut reader, local_file.to_path_buf(), line, col)
    }

    pub fn build(project: &ProjectConfig, logger: &mut Logger, release: bool, lint: bool) {
        let mut reader = ProjectReader::from_relative(project.rom.input.clone());
        let main_file = PathBuf::from(project.rom.input.file_name().unwrap());

        let mut compiler = Compiler::new();
        if project.report.segments {
            compiler.set_print_segment_map();
        }

        if project.report.info {
            compiler.set_print_rom_info();
        }

        compiler.set_optimize_instructions();

        if lint {
            compiler.set_linter_enabled();
        }

        // Remove debug code in release builds
        if release {
            compiler.set_strip_debug_code();

        // But generate symbols in debug builds
        } else {
            let mut symbol_file = project.rom.output.clone();
            symbol_file.set_extension("sym");
            compiler.set_generate_symbols(symbol_file);
        }

        compiler.set_generate_rom(project.rom.output.clone());

        // Create output directory
        let mut output_dir = project.rom.output.clone();
        output_dir.set_file_name("");

        let mut builder = DirBuilder::new();
        builder.recursive(true);
        builder.create(output_dir).expect("Failed to create output directory");

        let result = compiler.compile(logger, &mut reader, main_file);
        logger.finish(result);
    }

    pub fn try_load(logger: &mut Logger, reader: &ProjectReader) -> Result<ProjectConfig, IOError> {
        let mut dir = reader.base_dir().clone();
        loop {
            let mut config_file = dir.clone();
            config_file.push("gbc.toml");

            // Check if config file exists at this level
            if let Ok(mut file) = File::open(config_file.clone()) {
                let mut text = String::new();
                file.read_to_string(&mut text)?;
                let mut project = toml::from_str::<ProjectConfig>(&text).map_err(|err| {
                    IOError::new(ErrorKind::Other, err.to_string())
                })?;

                // Update paths
                let mut input_file = dir.clone();
                input_file.push(project.rom.input);
                project.rom.input = input_file;

                let mut output_file = dir.clone();
                output_file.push(project.rom.output);
                project.rom.output = output_file;

                logger.info(format!("Loaded project configuration from {}", config_file.display()));
                return Ok(project);

            } else if let Some(parent) = dir.parent() {
                dir = parent.into();

            } else {
                return Err(IOError::new(ErrorKind::NotFound, "No project configuration file found."));
            }
        }
    }

}

#[derive(Debug, Deserialize)]
pub struct RomConfig {
    input: PathBuf,
    pub output: PathBuf,
}

#[derive(Debug, Default, Deserialize)]
pub struct ReportConfig {
    #[serde(default)]
    info: bool,
    #[serde(default)]
    segments: bool
}

#[derive(Debug, Clone, Deserialize)]
pub struct EmulatorConfig {
    pub command: String,
    #[serde(default)]
    pub debug: bool
}

