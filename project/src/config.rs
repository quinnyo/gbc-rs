// STD Dependencies -----------------------------------------------------------
use std::fs::{DirBuilder, File};
use std::path::PathBuf;
use std::io::{Error as IOError, Read, ErrorKind};
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use serde::Deserialize;
use file_io::Logger;
use compiler::linker::Linker;
use compiler::compiler::{Compiler, CompilationError};


// Internal Dependencies ------------------------------------------------------
use crate::reader::ProjectReader;


// GBC Project Configuration --------------------------------------------------
#[derive(Debug, Deserialize)]
pub struct ProjectConfig {
    #[serde(default)]
    file: PathBuf,
    pub rom: RomConfig,
    pub emulator: HashMap<String, EmulatorConfig>,
    #[serde(default)]
    pub report: ReportConfig
}

impl ProjectConfig {
    pub fn load(logger: &mut Logger, reader: &ProjectReader) -> ProjectConfig {
        match Self::try_load(reader) {
            Ok(project) => {
                logger.info(format!("Loaded project configuration from {}", project.file.display()));
                project
            },
            Err(err) => {
                logger.error(Logger::format_error(
                    format!("Failed when trying to parse project configuration file!\n\n{}", err.to_string())
                ));
                std::process::exit(1);
            }
        }
    }

    pub fn link(project: &ProjectConfig, logger: &mut Logger) -> Result<Linker, CompilationError> {
        let mut reader = ProjectReader::from_relative(project.rom.input.clone());
        let main_file = PathBuf::from(project.rom.input.file_name().unwrap());
        let mut compiler = Compiler::new();
        compiler.set_optimize_instructions();
        compiler.set_strip_debug_code();
        compiler.create_linker(logger, &mut reader, main_file)
    }

    pub fn build(project: &ProjectConfig, logger: &mut Logger, release: bool) -> Result<Linker, CompilationError> {
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

        match compiler.compile(logger, &mut reader, main_file) {
            Ok(linker) => {
                logger.flush();
                Ok(linker)
            },
            Err(err) => {
                logger.error(err.to_string());
                Err(err)
            }
        }
    }

    pub fn try_load(reader: &ProjectReader) -> Result<ProjectConfig, IOError> {
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
    pub input: PathBuf,
    pub output: PathBuf,
}

#[derive(Debug, Default, Deserialize)]
pub struct ReportConfig {
    #[serde(default)]
    pub info: bool,
    #[serde(default)]
    pub segments: bool
}

#[derive(Debug, Clone, Deserialize)]
pub struct EmulatorConfig {
    pub command: String,
    #[serde(default)]
    pub debug: bool
}

