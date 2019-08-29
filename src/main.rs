// STD Dependencies -----------------------------------------------------------
use std::env;
use std::path::PathBuf;
use std::process::Command;


// External Dependencies ------------------------------------------------------
use decompiler::Decompiler;
use file_io::Logger;
use compiler::compiler::Compiler;


// Modules --------------------------------------------------------------------
mod cli;
mod config;
mod reader;


// Internal Dependencies ------------------------------------------------------
use self::config::ProjectConfig;
use self::reader::ProjectReader;


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
        ProjectConfig::build(&config, &mut logger, true);

    // Debug Builds
    } else if matches.subcommand_matches("debug").is_some() {
        let config = ProjectConfig::load(
            &mut logger,
            &ProjectReader::from_absolute(env::current_dir().unwrap())
        );
        ProjectConfig::build(&config, &mut logger, false);

    // Emulation
    } else if let Some(matches) = matches.subcommand_matches("emu") {
        let name = matches.value_of("EMULATOR").unwrap();
        if try_emulator(&mut logger, false, name) {
            logger.fail(Logger::format_error(
                format!("No emulator configuration for \"{}\".", name)
            ));
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

        // Decompile
        } else if matches.occurrences_of("decompile") > 0 {
            let mut decompiler = Decompiler::new();
            let result = decompiler.decompile_file(&mut logger, &mut reader, main_file);
            logger.finish(result);

        // Compile
        } else {
            let mut compiler = Compiler::new();
            if matches.occurrences_of("segments") > 0 {
                compiler.set_print_segment_map();
            }

            if matches.occurrences_of("lint") > 0 {
                compiler.set_linter_enabled();
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

            let result = compiler.compile(&mut logger, &mut reader, main_file);
            logger.finish(result);
        }

    } else {
        cli::app().print_help().ok();
        println!("");
    }
}

fn try_emulator(logger: &mut Logger, optional: bool, name: &str) -> bool {
    let config = if !optional {
        ProjectConfig::load(logger, &ProjectReader::from_absolute(env::current_dir().unwrap()))

    } else if let Ok(project) = ProjectConfig::try_load(logger, &ProjectReader::from_absolute(env::current_dir().unwrap())) {
        project

    } else {
        return false;
    };
    if let Some(emulator) = config.emulator.get(name) {
        ProjectConfig::build(&config, logger, !emulator.debug);
        logger.status("Emulating", format!("Running \"{}\"...", emulator.command));
        logger.flush();

        let mut args = emulator.command.split(' ');
        let name = args.next().expect("Failed to get command name");
        let mut args: Vec<String> = args.map(|arg| arg.to_string()).collect();
        args.push(config.rom.output.display().to_string());
        let mut child = Command::new(name).args(args).spawn().expect("Command failed");
        child.wait().ok();
        true

    } else {
        false
    }
}

