// STD Dependencies -----------------------------------------------------------
use std::thread;
use std::sync::Arc;
use std::sync::Mutex;
use std::path::PathBuf;
use std::cell::RefCell;
use std::collections::HashMap;
use std::time::{Duration, Instant};
use std::sync::atomic::AtomicUsize;


// External Dependencies ------------------------------------------------------
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{Range, Position};


// Internal Dependencies ------------------------------------------------------
use file_io::Logger;
use project::{ProjectConfig, ProjectReader};
use compiler::{
    compiler::Compiler,
    lexer::{
        stage::include::{IncludeStage, IncludeToken},
        LexerToken,
        LexerFile
    },
    linker::{Completion, Linker, Lookup}
};


pub struct State {
    documents: Arc<Mutex<RefCell<HashMap<String, String>>>>,
    tokens: Mutex<RefCell<HashMap<PathBuf, (Vec<IncludeToken>, LexerFile)>>>,
    linker: Arc<Mutex<Option<Linker>>>,
    link_gen: Arc<AtomicUsize>
}

impl State {
    pub fn new() -> Self {
        Self {
            documents: Arc::new(Mutex::new(RefCell::new(HashMap::new()))),
            tokens: Mutex::new(RefCell::new(HashMap::new())),
            linker: Arc::new(Mutex::new(None)),
            link_gen: Arc::new(AtomicUsize::new(0))
        }
    }

    // TODO handle deletion and renaming of files

    pub fn open_document(&self, path: &str, text: &str) {
        if let Ok(docs) = self.documents.lock() {
            let mut docs = docs.borrow_mut();
            docs.insert(path.to_string(), text.to_string());
        }
        self.link_async(PathBuf::from(path));
    }

    pub fn change_document(&self, path: &str, text: &str) {
        if let Ok(docs) = self.documents.lock() {
            let mut docs = docs.borrow_mut();
            docs.insert(path.to_string(), text.to_string());
        }
        if let Ok(tokens) = self.tokens.lock() {
            let mut tokens = tokens.borrow_mut();
            log::info!(&format!("Cleared cached tokens for {}...", path));
            tokens.remove(&PathBuf::from(path));
        }
        self.link_async(PathBuf::from(path));
    }

    pub fn save_document(&self, path: &str) {
        if let Ok(docs) = self.documents.lock() {
            let mut docs = docs.borrow_mut();
            docs.remove(path);
        }
        if let Ok(tokens) = self.tokens.lock() {
            let mut tokens = tokens.borrow_mut();
            log::info!(&format!("Cleared cached tokens for {}...", path));
            tokens.remove(&PathBuf::from(path));
        }
        self.link_async(PathBuf::from(path));
    }

    pub fn close_document(&self, path: &str) {
        if let Ok(docs) = self.documents.lock() {
            let mut docs = docs.borrow_mut();
            docs.remove(path);
        }
        if let Ok(tokens) = self.tokens.lock() {
            let mut tokens = tokens.borrow_mut();
            log::info!(&format!("Cleared cached tokens for {}...", path));
            tokens.remove(&PathBuf::from(path));
        }
        self.link_async(PathBuf::from(path));
    }

    pub fn lookup_symbol(&self, local_file: &str, line: usize, col: usize) -> Result<Option<(Lookup, Range)>> {
        let local_file = PathBuf::from(local_file);
        if let Some((token, file)) = self.get_token(local_file.clone(), line, col) {
            let token = token.inner();
            if let Some(linker) = self.get_linker(local_file.clone()) {
                if let Some(lookup) = linker.to_lookup_result(token.value.to_string()) {
                    let (sl, sc) = file.get_line_and_col(token.start_index);
                    let (el, ec) = file.get_line_and_col(token.end_index);
                    let range = Range {
                        start: Position {
                            line: sl as u32,
                            character: sc as u32
                        },
                        end: Position {
                            line: el as u32,
                            character: ec as u32
                        }
                    };
                    log::info!("Lookup completed");
                    return Ok(Some((lookup, range)))
                }
            }
        }
        Ok(None)
    }

    pub fn completions(&self, local_file: &str, _line: usize, _col: usize) -> Result<Vec<Completion>> {
        let local_file = PathBuf::from(local_file);
        if let Some(linker) = self.get_linker(local_file.clone()) {
            // TODO sort based on relevance best match, distance to current location etc.
            // language client will filter / sort anyways
            log::info!("Completions generated");
            Ok(linker.to_completion_list(local_file))

        } else {
            Ok(Vec::new())
        }
    }

    pub fn inlay_hints(&self, local_file: &str) -> Result<Vec<((usize, usize), String)>> {
        let local_file = PathBuf::from(local_file);
        if let Some(linker) = self.get_linker(local_file.clone()) {
            log::info!("Hints generated");
            Ok(linker.to_hint_list(local_file))

        } else {
            Ok(Vec::new())
        }
    }

    fn get_token(&self, local_file: PathBuf, line: usize, col: usize) -> Option<(IncludeToken, LexerFile)> {
        log::info!(&format!("Getting token from {:?}:{}:{}...", local_file, line, col));

        let mut logger = Logger::new();
        logger.set_silent();

        let (_, reader) = Self::load_project(&mut logger, local_file.clone(), &self.documents);
        let relative_file = local_file.strip_prefix(reader.base_dir()).unwrap().to_path_buf();

        let data = if let Ok(tokens) = self.tokens.lock() {
            let mut tokens = tokens.borrow_mut();
            if let Some(data) = tokens.get(&local_file) {
                log::info!(&format!("Returned {} cached token(s)...", data.0.len()));
                Some(data.clone())

            } else {
                let mut files = Vec::new();
                if let Ok(parsed) = IncludeStage::tokenize_single(&reader, &relative_file, &mut files) {
                    log::info!(&format!("Parsed {} token(s)...", parsed.len()));
                    let data = (parsed, files.remove(0));
                    tokens.insert(local_file, data.clone());
                    Some(data)

                } else {
                    None
                }
            }

        } else {
            None
        };
        if let Some((tokens, file)) = data {
            let index = file.get_index(line, col);
            for t in tokens {
                if index >= t.inner().start_index && index < t.inner().end_index {
                    return Some((t, file))
                }
            }
        }
        None
    }

    fn get_linker(&self, local_file: PathBuf) -> Option<Linker> {
        let has_linker = if let Ok(linker) = self.linker.lock() {
            linker.is_some()

        } else {
            false
        };
        if has_linker {
            log::info!("Returning cached linker");

        } else {
            log::info!("Force Linking...");
            Self::link(local_file, self.linker.clone(), self.documents.clone());
        }
        if let Ok(linker) = self.linker.lock() {
            linker.clone()

        } else {
            None
        }
    }

    fn link_async(&self, local_file: PathBuf) where Self: 'static {
        let linker = self.linker.clone();
        let link_gen = self.link_gen.clone();
        let documents = self.documents.clone();

        // Increase generation
        link_gen.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Get generation
        let thread_gen = link_gen.load(std::sync::atomic::Ordering::SeqCst);

        // TODO limit concurrent threads?
        thread::spawn(move || {
            // Debounce
            thread::sleep(Duration::from_millis(250));

            // Check if still the latest link request
            let latest_gen = link_gen.load(std::sync::atomic::Ordering::SeqCst);
            if thread_gen == latest_gen {
                log::info!("Linking in background...");
                Self::link(local_file, linker, documents);

            } else {
                log::info!("Link canceled");
            }
        });
    }

    fn load_project(logger: &mut Logger, local_file: PathBuf, documents: &Arc<Mutex<RefCell<HashMap<String, String>>>>) -> (ProjectConfig, ProjectReader) {
        // TODO cache config
        let config = ProjectConfig::load(logger, &ProjectReader::from_absolute(local_file));
        let reader = ProjectReader::from_relative(config.rom.input.clone());

        // Overlay LS files on file system reader
        if let Ok(documents) = documents.lock() {
            let documents = documents.borrow();
            for (path, text) in &*documents {
                reader.overlay_file(PathBuf::from(path), text.clone());
            }
        }
        (config, reader)
    }

    fn link(local_file: PathBuf, outer_linker: Arc<Mutex<Option<Linker>>>, documents: Arc<Mutex<RefCell<HashMap<String, String>>>>) {
        let start = Instant::now();

        let mut logger = Logger::new();
        logger.set_silent();
        let (config, mut reader) = Self::load_project(&mut logger, local_file, &documents);

        // Create compiler
        let main_file = PathBuf::from(config.rom.input.file_name().unwrap());
        let mut compiler = Compiler::new();
        compiler.set_optimize_instructions();
        compiler.set_strip_debug_code();
        compiler.set_linter_enabled();

        // Run and cache linker
        if let Ok(new_linker) = compiler.create_linker(&mut logger, &mut reader, main_file) {
            log::info!(&format!("Linked in {}ms", start.elapsed().as_millis()));
            if let Ok(mut linker) = outer_linker.lock() {
                *linker = Some(new_linker);
            }

        } else {
            log::error!(&format!("Linking failed after {}ms", start.elapsed().as_millis()));
        }
    }
}

