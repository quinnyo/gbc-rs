// STD Dependencies -----------------------------------------------------------
use std::env;
use std::path::PathBuf;
use std::process::{self, Command};
use std::str::FromStr;

// External Dependencies ------------------------------------------------------
use compiler::compiler::{Compiler, SymbolFileFormat};
use file_io::{FileReader, Logger};
use project::{ProjectConfig, ProjectReader};

// Modules --------------------------------------------------------------------
mod cli;

// CLI Interface --------------------------------------------------------------
fn main() {
    let mut logger = Logger::new();
    let matches = cli::app().get_matches();

    // Support Silent Flag for all Commands
    if matches.occurrences_of("silent") > 0 {
        logger.set_silent();
    }

    // Release Builds
    if matches.subcommand_matches("release").is_some() {
        let config = ProjectConfig::load(
            &mut logger,
            &ProjectReader::from_absolute(env::current_dir().unwrap()),
        );
        if ProjectConfig::build(&config, &mut logger, None, true).is_err() {
            process::exit(1);
        }

    // Debug Builds
    } else if matches.subcommand_matches("debug").is_some() {
        let config = ProjectConfig::load(
            &mut logger,
            &ProjectReader::from_absolute(env::current_dir().unwrap()),
        );
        if ProjectConfig::build(&config, &mut logger, None, false).is_err() {
            process::exit(1);
        }

    // Running
    } else if let Some(matches) = matches.subcommand_matches("run") {
        let args = matches.values_of("RUNNER").unwrap().collect();
        let pass_source_dir = matches.occurrences_of("PASS_SOURCE_DIR") > 0;
        try_runner(&mut logger, false, args, pass_source_dir);

    // De- / Compilation
    } else if let Some(file) = matches.value_of("SOURCE_FILE") {
        // Compile if main is a file and ends with .gbc
        let main = PathBuf::from(file);
        if main.is_file()
            && main
                .extension()
                .map(|e| e.to_str() == Some("gbc"))
                .unwrap_or(false)
        {
            let main_file = PathBuf::from(main.file_name().unwrap());

            // Create a project reader with the directory of the supplied argument file as the project
            // base so includes can be relative to the base directory by prefixing them with "/"
            let mut reader = ProjectReader::from_relative(main.clone());
            let mut compiler = Compiler::new();
            if matches.occurrences_of("segments") > 0 {
                compiler.set_print_segment_map();
            }

            if matches.occurrences_of("debug") == 0 {
                compiler.set_strip_debug_code();
            }

            if matches.occurrences_of("no-optimize") == 0 {
                compiler.set_optimize_instructions();
            }

            if matches.occurrences_of("info") > 0 {
                compiler.set_print_rom_info();
            }

            if let Some(sym_fmt) = matches.value_of("symbol-format") {
                let fmt = SymbolFileFormat::from_str(sym_fmt)
                    .expect("Clap should not allow invalid symbol file formats");
                compiler.set_symbol_format(fmt);
            }

            if let Some(rom) = matches.value_of("ROM_FILE") {
                compiler.set_generate_rom(PathBuf::from(rom));
            }

            if let Some(map) = matches.value_of("SYMBOL_MAP_FILE") {
                compiler.set_generate_symbol_map(PathBuf::from(map));
            }

            if let Some(map) = matches.value_of("SOURCE_MAP_FILE") {
                compiler.set_generate_source_map(PathBuf::from(map));
            }

            match compiler.compile(&mut logger, &mut reader, main_file) {
                Ok(_) => {
                    logger.flush();
                }
                Err(err) => {
                    logger.error(err.to_string());
                    process::exit(1);
                }
            }
        } else {
            logger.error(Logger::format_error(format!(
                "Argument `SOURCE_FILE` (\"{}\") is not a .gbc file",
                file
            )));
            process::exit(2);
        }
    } else {
        cli::app().print_help().ok();
    }
}

fn try_runner(logger: &mut Logger, optional: bool, mut args: Vec<&str>, pass_source_dir: bool) {
    // A project config is required
    let config = if !optional {
        ProjectConfig::load(
            logger,
            &ProjectReader::from_absolute(env::current_dir().unwrap()),
        )
    } else if let Ok(project) =
        ProjectConfig::try_load(&ProjectReader::from_absolute(env::current_dir().unwrap()))
    {
        project
    } else {
        logger.error(Logger::format_error(format!(
            "No runner configuration for \"{}\".",
            args.join(" ")
        )));
        process::exit(3);
    };

    // Get command to run
    let (command, name, mut args, debug) = if let Some(runner) = config.runner.get(args[0]) {
        let mut args = runner.command.as_str().split(' ');
        let name = args.next().expect("Failed to get command name");
        let args: Vec<String> = args.map(|arg| arg.to_string()).collect();
        (runner.command.clone(), name, args, runner.debug)
    } else {
        let command = args.join(" ");
        (
            command,
            args.remove(0),
            args.into_iter().map(|arg| arg.to_string()).collect(),
            true,
        )
    };

    // Build ROM
    if ProjectConfig::build(&config, logger, None, !debug).is_err() {
        process::exit(1);
    }
    logger.status("Running", format!("via \"{}\"...", command));
    logger.flush();

    // Pass source dir to command
    if pass_source_dir {
        let reader = ProjectReader::from_relative(config.rom.input.clone());
        args.push("--source-dir".to_string());
        args.push(reader.base_dir().display().to_string());
    }

    // Hand over ROM path to command
    args.push(config.rom.output.display().to_string());
    if let Err(err) = Command::new(name).args(args).status() {
        logger.error(Logger::format_error(format!(
            "Failed running ROM via \"{}\": {}",
            command, err
        )));
        process::exit(4);
    }
}
