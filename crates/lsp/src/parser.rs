// STD Dependencies -----------------------------------------------------------
use std::path::PathBuf;
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use file_io::Logger;
use project::{ProjectConfig, ProjectReader};
use compiler::{
    compiler::Compiler,
    generator::Generator,
    lexer::{
        stage::include::{IncludeStage, IncludeToken},
        LexerToken,
        LexerFile
    },
    linker::{
        AnalysisLint, SectionEntry
    }
};


// Internal Dependencies ------------------------------------------------------
use crate::state::State;
use crate::types::{InlayHintsNotification, InlayHintsParams};


// Parser Implementation ------------------------------------------------------
pub struct Parser;
impl Parser {
    pub async fn link(workspace_path: PathBuf, state: State) -> Option<()> {
        // Try and load config for current workspace
        let (config, mut reader) = Self::load_project(&state, workspace_path)?;

        // Tell client about the linking
        let progress_token = state.start_progress("Linker", "Running...").await;

        // Create compiler
        let main_file = PathBuf::from(config.rom.input.file_name().unwrap());
        let mut compiler = Compiler::new();
        compiler.set_optimize_instructions();

        let mut logger = Logger::new();
        logger.set_silent();

        let mut errors: Vec<AnalysisLint> = Vec::new();
        let mut lints: Vec<AnalysisLint> = Vec::new();

        // Run linker
        let start = std::time::Instant::now();
        match compiler.create_linker(&mut logger, &mut reader, main_file) {
            Ok(mut linker) => {
                log::info!("Linked in {}ms", start.elapsed().as_millis());

                // Generate ROM entry diff for running emulator process
                if let Some(process) = state.emulator().as_mut() {
                    lints.append(&mut process.update_entries(linker.rom_entry_map()));
                }

                let start = std::time::Instant::now();
                state.set_address_locations(linker.address_location_map());
                state.set_symbols((linker.analysis.symbols, linker.analysis.macros, linker.analysis.hints));
                state.end_progress(progress_token, "Done").await;
                lints.append(&mut linker.analysis.lints);
                log::info!("Analyzed in {}ms", start.elapsed().as_millis());

            },
            Err(err) => {
                if let Some(lint) = err.into_lint() {
                    errors.push(lint);

                } else {
                    // TODO show at current location
                }
                state.end_progress(progress_token, "Done").await;
                log::error!("{}", format!("Linking failed after {}ms", start.elapsed().as_millis()));
            }
        }

        // Update error
        state.set_error(errors.first().map(|error| {
            (error.uri.clone(), error.detail.range.start.line as usize, error.detail.range.start.character as usize)
        }));

        // Update diagnostics
        {
            let mut diagnostics = state.diagnostics();
            for (_, diagnostics) in diagnostics.iter_mut() {
                diagnostics.clear();
            }
            for error in errors {
                diagnostics.entry(error.uri).or_insert_with(Vec::new).push(error.detail);
            }
            for lint in lints {
                diagnostics.entry(lint.uri).or_insert_with(Vec::new).push(lint.detail);
            }
        }

        // Trigger new inlay hint fetching
        state.client().send_notification::<InlayHintsNotification>(InlayHintsParams {}).await;
        Some(())
    }

    pub async fn build(workspace_path: PathBuf, state: State) -> Option<(HashMap<usize, SectionEntry>, Vec<(u16, u16, String)>, Vec<u8>, PathBuf)> {
        // Try and load config for current workspace
        let (mut config, reader) = Self::load_project(&state, workspace_path)?;
        config.report.segments = false;
        config.report.info = false;

        // Tell client about the build
        let progress_token = state.start_progress("Build", "Running...").await;

        let mut logger = Logger::new();
        logger.set_silent();

        // Run compiler
        match ProjectConfig::build(&config, &mut logger, Some(reader), false) {
            Ok(linker) => {
                // Generate Labels and ROM image (note order is swapped in emulator!)
                let labels = linker.symbol_list().into_iter().map(|(bank, addr, name)| (addr as u16, bank as u16, name)).collect();
                let mut generator = Generator::from_linker(&linker);
                generator.finalize_rom();

                state.end_progress(progress_token, "Complete").await;
                Some((linker.rom_entry_map(), labels, generator.buffer, config.rom.output))
            },
            Err(_) => {
                state.end_progress(progress_token, "Failed").await;
                None
            }
        }
    }

    pub fn get_token(
        state: &State,
        current_file: PathBuf,
        line: usize,
        col: usize

    ) -> Option<(IncludeToken, LexerFile)> {
        let (_, reader) = Self::load_project(state, current_file.clone())?;
        let relative_file = current_file.strip_prefix(reader.base_dir()).unwrap().to_path_buf();

        let mut tokens = state.tokens();
        let results = if let Some(data) = tokens.get(&current_file) {
            Some(data.clone())

        } else {
            let mut files = Vec::new();
            if let Ok(parsed) = IncludeStage::tokenize_single(&reader, &relative_file, &mut files) {
                let result = (parsed, files.remove(0));
                tokens.insert(current_file, result.clone());
                Some(result)

            } else {
                None
            }
        };
        if let Some((tokens, file)) = results {
            let index = file.get_index(line, col);
            for t in tokens {
                if index >= t.inner().start_index && index < t.inner().end_index {
                    return Some((t, file))
                }
            }
            None
        } else {
            None
        }
    }

    fn load_project(state: &State, workspace_path: PathBuf) -> Option<(ProjectConfig, ProjectReader)> {
        if let Ok(config) = ProjectConfig::try_load(&ProjectReader::from_absolute(workspace_path)) {
            let reader = ProjectReader::from_relative(config.rom.input.clone());

            // Overlay LS files on file system reader
            for (path, text) in state.documents().iter() {
                reader.overlay_file(PathBuf::from(path), text.clone());
            }

            Some((config, reader))

        } else {
            None
        }
    }
}

