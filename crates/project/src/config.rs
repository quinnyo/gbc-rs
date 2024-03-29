// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;
use std::fs::{DirBuilder, File};
use std::io::{Error as IOError, ErrorKind, Read};
use std::path::PathBuf;
use std::str::FromStr;

use ::compiler::compiler::SymbolFileFormat;
// External Dependencies ------------------------------------------------------
use compiler::compiler::{CompilationError, Compiler};
use compiler::linker::Linker;
use file_io::{FileReader, Logger};
use serde::Deserialize;

// Internal Dependencies ------------------------------------------------------
use crate::reader::ProjectReader;

// GBC Project Configuration --------------------------------------------------
#[derive(Debug, Deserialize)]
pub struct ProjectConfig {
    #[serde(default)]
    file: PathBuf,
    pub rom: RomConfig,
    pub runner: HashMap<String, RunnerConfig>,
    #[serde(default)]
    pub map: MapConfig,
    #[serde(default)]
    pub report: ReportConfig,
}

impl ProjectConfig {
    pub fn load(logger: &mut Logger, reader: &ProjectReader) -> ProjectConfig {
        match Self::try_load(reader) {
            Ok(project) => {
                logger.info(format!(
                    "Loaded project configuration from {}",
                    project.file.display()
                ));
                project
            }
            Err(err) => {
                logger.error(Logger::format_error(format!(
                    "Failed when trying to parse project configuration file!\n\n{}",
                    err
                )));
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

    pub fn build(
        project: &ProjectConfig,
        logger: &mut Logger,
        reader: Option<ProjectReader>,
        release: bool,
    ) -> Result<Linker, CompilationError> {
        let mut reader =
            reader.unwrap_or_else(|| ProjectReader::from_relative(project.rom.input.clone()));
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
        }

        // Generate symbols if configured
        if let Some(fmt) = &project.map.symbols {
            if let Ok(fmt) = SymbolFileFormat::from_str(&fmt) {
                let mut map_file = project.rom.output.clone();
                map_file.set_extension("sym");
                compiler.set_generate_symbol_map(map_file);
                compiler.set_symbol_format(fmt);
            }
        }
        if project.map.source {
            let mut map_file = project.rom.output.clone();
            map_file.set_extension("map");
            compiler.set_generate_source_map(map_file);
        }

        compiler.set_generate_rom(project.rom.output.clone());

        // Create output directory
        let mut output_dir = project.rom.output.clone();
        output_dir.set_file_name("");

        let mut builder = DirBuilder::new();
        builder.recursive(true);
        builder
            .create(output_dir)
            .expect("Failed to create output directory");

        match compiler.compile(logger, &mut reader, main_file) {
            Ok(linker) => {
                logger.flush();
                Ok(linker)
            }
            Err(err) => {
                logger.error(err.to_string());
                Err(err)
            }
        }
    }

    pub fn try_load(reader: &ProjectReader) -> Result<ProjectConfig, IOError> {
        let mut dir = reader.base_dir().to_path_buf();
        loop {
            let mut config_file = dir.clone();
            config_file.push("gbc.toml");

            // Check if config file exists at this level
            if let Ok(mut file) = File::open(config_file.clone()) {
                let mut text = String::new();
                file.read_to_string(&mut text)?;
                let mut project = toml::from_str::<ProjectConfig>(&text)
                    .map_err(|err| IOError::new(ErrorKind::Other, err.to_string()))?;

                // Update paths
                let mut input_file = dir.clone();
                input_file.push(project.rom.input);
                project.rom.input = input_file;

                // validate symbol file format
                if let Some(ref fmt) = project.map.symbols {
                    if let Err(err) = SymbolFileFormat::from_str(&fmt) {
                        return Err(IOError::new(ErrorKind::InvalidData, err.to_string()));
                    }
                }

                dir.push(project.rom.output);
                project.rom.output = dir;

                return Ok(project);
            } else if let Some(parent) = dir.parent() {
                dir = parent.into();
            } else {
                return Err(IOError::new(
                    ErrorKind::NotFound,
                    "No project configuration file found.",
                ));
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
pub struct MapConfig {
    #[serde(default)]
    pub symbols: Option<String>,
    #[serde(default)]
    pub source: bool,
}

#[derive(Debug, Default, Deserialize)]
pub struct ReportConfig {
    #[serde(default)]
    pub info: bool,
    #[serde(default)]
    pub segments: bool,
}

#[derive(Debug, Clone, Deserialize)]
pub struct RunnerConfig {
    pub command: String,
    #[serde(default)]
    pub debug: bool,
}
