// STD Dependencies -----------------------------------------------------------
use std::env;
use std::path::PathBuf;
use std::process::{self, Command};


// External Dependencies ------------------------------------------------------
use project::{ProjectConfig, ProjectReader};
use file_io::Logger;
use compiler::compiler::Compiler;


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
            &ProjectReader::from_absolute(env::current_dir().unwrap())
        );
        if ProjectConfig::build(&config, &mut logger, None, true).is_err() {
            process::exit(1);
        }

    // Debug Builds
    } else if matches.subcommand_matches("debug").is_some() {
        let config = ProjectConfig::load(
            &mut logger,
            &ProjectReader::from_absolute(env::current_dir().unwrap())
        );
        if ProjectConfig::build(&config, &mut logger, None, false).is_err() {
            process::exit(1);
        }

    // Emulation
    } else if let Some(matches) = matches.subcommand_matches("emu") {
        let name = matches.value_of("EMULATOR").unwrap();
        if !try_emulator(&mut logger, false, name) {
            logger.error(Logger::format_error(
                format!("No emulator configuration for \"{}\".", name)
            ));
            process::exit(1);
        }

    // De- / Compilation
    } else if let Some(file) = matches.value_of("SOURCE_FILE") {

        // Create a project reader with the directory of the supplied argument file as the project
        // base so includes can be relative to the base directory by prefixing them with "/"
        let main = PathBuf::from(file);
        let main_file = PathBuf::from(main.file_name().unwrap());
        let mut reader = ProjectReader::from_relative(main.clone());

        // If the file does not exist, check if it is a configured emulator shortcut
        if !main.is_file() && try_emulator(&mut logger, true, &main.display().to_string()) {
            // Empty

        // Compile
        } else {
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

            if let Some(rom) = matches.value_of("ROM_FILE") {
                compiler.set_generate_rom(PathBuf::from(rom));
            }

            if let Some(map) = matches.value_of("MAP_FILE") {
                compiler.set_generate_symbols(PathBuf::from(map));
            }

            match compiler.compile(&mut logger, &mut reader, main_file) {
                Ok(_) => {
                    logger.flush();
                },
                Err(err) => {
                    logger.error(err.to_string());
                    process::exit(1);
                }
            }
        }

    } else {
        cli::app().print_help().ok();
    }
}

fn try_emulator(logger: &mut Logger, optional: bool, name: &str) -> bool {
    let config = if !optional {
        ProjectConfig::load(logger, &ProjectReader::from_absolute(env::current_dir().unwrap()))

    } else if let Ok(project) = ProjectConfig::try_load(&ProjectReader::from_absolute(env::current_dir().unwrap())) {
        project

    } else {
        return false;
    };
    if let Some(emulator) = config.emulator.get(name) {
        if ProjectConfig::build(&config, logger, None, !emulator.debug).is_err() {
            process::exit(1);
        }
        logger.status("Emulating", format!("Running \"{}\"...", emulator.command));
        logger.flush();

        let mut args = emulator.command.split(' ');
        let name = args.next().expect("Failed to get command name");
        let mut args: Vec<String> = args.map(|arg| arg.to_string()).collect();
        args.push(config.rom.output.display().to_string());
        let status = Command::new(name).args(args).status().expect("Command failed");
        status.success()

    } else {
        false
    }
}

