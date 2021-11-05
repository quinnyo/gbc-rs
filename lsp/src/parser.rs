// STD Dependencies -----------------------------------------------------------
use std::sync::Arc;
use std::sync::Mutex;
use std::path::PathBuf;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};


// External Dependencies ------------------------------------------------------
use tower_lsp::Client;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, SymbolKind, Url, Location};
use tower_lsp::lsp_types::{Range, Position, NumberOrString, WorkDoneProgressCreateParams};
use tower_lsp::lsp_types::request::WorkDoneProgressCreate;


// Internal Dependencies ------------------------------------------------------
use file_io::Logger;
use project::{ProjectConfig, ProjectReader};
use compiler::{
    compiler::Compiler,
    lexer::{
        MacroCall,
        stage::include::{IncludeStage, IncludeToken},
        LexerToken,
        LexerFile
    },
    expression::ExpressionResult,
    linker::{
        AccessKind, Linker, SectionEntry, EntryData
    }
};
use crate::types::{InlayHintsNotification, InlayHintsParams, GBCSymbol, MacroExpansion, Optimizations};


// Parser Implementation ------------------------------------------------------
pub struct Parser;
impl Parser {
    pub async fn link(
        client: Arc<Client>,
        link_id: NumberOrString,
        workspace_path: PathBuf,
        outer_symbols: Arc<Mutex<Option<(Vec<GBCSymbol>, Vec<MacroExpansion>, Optimizations)>>>,
        outer_error: Arc<Mutex<Option<(Url, usize, usize)>>>,
        outer_diagnostics: Arc<Mutex<HashMap<Url, Vec<Diagnostic>>>>,
        documents: Arc<Mutex<RefCell<HashMap<String, String>>>>

    ) -> Option<()> {
        // Try and load config for current workspace
        let (config, mut reader) = Self::load_project(workspace_path, &documents)?;

        // Tell client about the linking
        client.send_custom_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
            token: link_id.clone()

        }).await.ok();
        client.show_progress_begin(link_id.clone(), "Linking", "In Progress").await;

        // Create compiler
        let main_file = PathBuf::from(config.rom.input.file_name().unwrap());
        let mut compiler = Compiler::new();
        compiler.set_optimize_instructions();
        compiler.set_strip_debug_code();

        // Run linker
        let mut logger = Logger::new();
        logger.set_silent();

        let start = std::time::Instant::now();
        let mut errors: Vec<(Url, Diagnostic)> = Vec::new();
        let mut lints: Vec<(Url, Diagnostic)> = Vec::new();
        match compiler.create_linker(&mut logger, &mut reader, main_file) {
            Ok(linker) => {
                log::info!(&format!("Linked in {}ms", start.elapsed().as_millis()));
                client.show_progress_end(link_id, "Done").await;

                // Generate and store new symbols
                if let Ok(mut symbols) = outer_symbols.lock() {
                    let (s, m) = Self::parse_symbols(&linker);
                    lints.append(&mut Self::diagnostics(&linker, &s));
                    let optimizations = Self::parse_optimizations(&linker);
                    log::info!(&format!("{} symbol(s)", s.len()));
                    *symbols = Some((s, m, optimizations));
                }
            },
            Err(err) => {
                log::error!(&format!("Linking failed after {}ms", start.elapsed().as_millis()));
                client.show_progress_end(link_id, "Linking failed").await;
                if let Some((path, line, col, message)) = err.into_diagnostic() {
                    let uri = Url::from_file_path(path).unwrap();
                    errors.push((uri, Diagnostic {
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
                        severity: Some(DiagnosticSeverity::Error),
                        message,
                        code: None,
                        code_description: None,
                        source: None,
                        related_information: None,
                        tags: None,
                        data: None
                    }));

                } else {
                    // TODO show at current location
                }
            }
        }

        // Update Error State
        if let Ok(mut error) = outer_error.lock() {
            *error = errors.first().map(|(url, diagnostic)| {
                (url.clone(), diagnostic.range.start.line as usize, diagnostic.range.start.character as usize)
            });
        }

        // Update diagnostics for all files
        if let Ok(mut diagnostics) = outer_diagnostics.lock() {
            let mut workspace_diagnostics = HashMap::new();

            // Make sure to send empty diagnostics for any previousy used files so they get cleared
            for uri in diagnostics.keys() {
                workspace_diagnostics.insert(uri.clone(), Vec::new());
            }

            // Insert new diagnostics
            for (uri, err) in errors {
                workspace_diagnostics.entry(uri).or_insert_with(Vec::new).push(err);
            }
            for (uri, lint) in lints {
                workspace_diagnostics.entry(uri).or_insert_with(Vec::new).push(lint);
            }
            *diagnostics = workspace_diagnostics;
        }

        // Trigger new inlay hint fetching
        client.send_custom_notification::<InlayHintsNotification>(InlayHintsParams {}).await;
        Some(())
    }

    pub fn get_token(
        tokens: &Mutex<RefCell<HashMap<PathBuf, (Vec<IncludeToken>, LexerFile)>>>,
        documents: &Arc<Mutex<RefCell<HashMap<String, String>>>>,
        current_file: PathBuf,
        line: usize,
        col: usize

    ) -> Option<(IncludeToken, LexerFile)> {
        let (_, reader) = Self::load_project(current_file.clone(), documents)?;
        let relative_file = current_file.strip_prefix(reader.base_dir()).unwrap().to_path_buf();

        let data = if let Ok(tokens) = tokens.lock() {
            let mut tokens = tokens.borrow_mut();
            if let Some(data) = tokens.get(&current_file) {
                Some(data.clone())

            } else {
                let mut files = Vec::new();
                if let Ok(parsed) = IncludeStage::tokenize_single(&reader, &relative_file, &mut files) {
                    let data = (parsed, files.remove(0));
                    tokens.insert(current_file, data.clone());
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

    fn resolve_reference(linker: &Linker, file_index: usize, start_index: usize, macro_call_id: Option<usize>, l: usize) -> Location {
        let context = linker.context();

        // Follow macro calls to their original source
        let macro_call = macro_call_id.and_then(|id| context.macro_calls.get(id));
        if let Some(macro_call) = macro_call {
            let token = macro_call.name();
            let file = &context.files[token.file_index];
            let (line, col) = file.get_line_and_col(token.start_index);
            Location {
                uri: Url::from_file_path(&file.path).unwrap(),
                range: Range {
                    start: Position {
                        line: line as u32,
                        character: col as u32
                    },
                    end: Position {
                        line: line as u32,
                        character: (col + l) as u32
                    }
                }
            }

        } else {
            let file = &context.files[file_index];
            let (line, col) = file.get_line_and_col(start_index);
            Location {
                uri: Url::from_file_path(&file.path).unwrap(),
                range: Range {
                    start: Position {
                        line: line as u32,
                        character: col as u32
                    },
                    end: Position {
                        line: line as u32,
                        character: (col + l) as u32
                    }
                }
            }
        }
    }

    fn location_from_file_index(files: &[LexerFile], file_index: usize, start_index: usize, end_index: usize) -> Location {
        let file = &files[file_index];
        let (line, col) = file.get_line_and_col(start_index);
        let (eline, ecol) = file.get_line_and_col(end_index);
        Location {
            uri: Url::from_file_path(&file.path).unwrap(),
            range: Range {
                start: Position {
                    line: line as u32,
                    character: col as u32
                },
                end: Position {
                    line: if eline > line {
                        (eline + 1) as u32

                    } else {
                        eline as u32
                    },
                    character: ecol as u32
                }
            }
        }
    }

    fn load_project(workspace_path: PathBuf, documents: &Arc<Mutex<RefCell<HashMap<String, String>>>>) -> Option<(ProjectConfig, ProjectReader)> {
        if let Ok(config) = ProjectConfig::try_load(&ProjectReader::from_absolute(workspace_path)) {
            let reader = ProjectReader::from_relative(config.rom.input.clone());

            // Overlay LS files on file system reader
            if let Ok(documents) = documents.lock() {
                let documents = documents.borrow();
                for (path, text) in &*documents {
                    reader.overlay_file(PathBuf::from(path), text.clone());
                }
            }

            Some((config, reader))

        } else {
            None
        }
    }

    fn parse_symbols(linker: &Linker) -> (Vec<GBCSymbol>, Vec<MacroExpansion>) {
        let mut symbols: Vec<GBCSymbol> = Vec::new();
        let context = linker.context();

        // Constants
        for (index, constant) in context.constants {
            let token = &constant.inner;
            let name = token.value.to_string();
            if let Some(result) = context.constant_values.get(index) {
                let references = context.constant_usage.get(index).map(|refs| {
                    refs.iter().map(|(file_index, start_index, macro_call_id)| {
                        Self::resolve_reference(&linker, *file_index, *start_index, *macro_call_id, name.len())

                    }).collect()

                }).unwrap_or_else(Vec::new);
                symbols.push(GBCSymbol {
                    kind: SymbolKind::Constant,
                    is_global: index.1.is_none(),
                    in_macro: token.macro_call_id.is_some(),
                    location: Self::location_from_file_index(context.files, constant.inner.file_index, constant.inner.start_index, constant.inner.end_index),
                    address: None,
                    name,
                    width: 0,
                    result: Some(result.clone()),
                    value: result.to_string(),
                    children: Vec::new(),
                    references,
                    calls: Vec::new(),
                    jumps: Vec::new(),
                    reads: Vec::new(),
                    writes: Vec::new()
                });
            }
        }

        // Sections
        for section in &linker.sections {
            symbols.push(GBCSymbol {
                kind: SymbolKind::Namespace,
                is_global: false,
                in_macro: section.inner.macro_call_id.is_some(),
                location: Self::location_from_file_index(context.files, section.inner.file_index, section.inner.start_index, section.inner.end_index),
                name: section.name.to_string(),
                address: None,
                width: 0,
                result: None,
                value: format!("{}[${:0>4X}-${:0>4X}][{}]", section.segment, section.start_address, section.end_address, section.bank),
                children: Vec::new(),
                references: Vec::new(),
                calls: Vec::new(),
                jumps: Vec::new(),
                reads: Vec::new(),
                writes: Vec::new()
            });
        }

        // Macros
        for def in context.macro_defs {
            // ignore callable labels and only include normal macros
            if !def.is_label {
                // TODO also list builtin macros
                symbols.push(GBCSymbol {
                    kind: SymbolKind::Constructor,
                    is_global: def.is_exported,
                    in_macro: false,
                    location: Self::location_from_file_index(context.files, def.name.file_index, def.name.start_index, def.name.end_index),
                    address: None,
                    name:  def.name.value.to_string(),
                    width: 0,
                    result: None,
                    value: format!("({})", def.parameters.iter().map(|(_, name)| {
                        format!("@{}", name.value)

                    }).collect::<Vec<String>>().join(", ")),
                    children: Vec::new(),
                    references: Vec::new(),
                    calls: context.macro_calls.iter().filter(|call| {
                        call.name().value == def.name.value

                    }).map(|call| {
                        let name = call.name();
                        Self::resolve_reference(&linker, name.file_index, name.start_index, name.macro_call_id, name.value.as_str().len())

                    }).collect(),
                    jumps: Vec::new(),
                    reads: Vec::new(),
                    writes: Vec::new()
                });
            }
        }

        let mut macro_call_sizes: HashMap<usize, usize> = HashMap::with_capacity(32);
        let sections = linker.section_entries();
        for entry in sections {

            // Variables / Labels
            let mut entries = entry.iter().peekable();
            while let Some(entry) = entries.next() {

                let file_index = entry.inner.file_index;
                let entry_name = &entry.inner.value;
                let start_index = entry.inner.start_index;
                let mut end_index = entry.inner.end_index;

                if let EntryData::Label { .. } = &entry.data {

                    let mut children = Vec::new();
                    let mut kind = SymbolKind::Function;
                    let mut data_size = 0;
                    while let Some(SectionEntry { size, data, inner, .. }) = entries.peek() {

                        // Encountered next parent label
                        if let EntryData::Label { is_local: false, .. } = data {
                            // Ignore labels generated by macros
                            if inner.macro_call_id.is_none() {
                                break;
                            }
                        }

                        // Child labels
                        if let EntryData::Label { is_local: true, name, .. } = data {
                            let mut label = GBCSymbol {
                                kind: SymbolKind::Method,
                                is_global: false,
                                in_macro: inner.macro_call_id.is_some(),
                                location: Self::location_from_file_index(context.files, inner.file_index, inner.start_index, inner.end_index),
                                address: None,
                                name: format!(".{}", name),
                                width: 0,
                                result: None,
                                value: "".to_string(),
                                children: Vec::new(),
                                references: Vec::new(),
                                jumps: Vec::new(), // TODO figure out actual jumps here?
                                calls: Vec::new(),
                                reads: Vec::new(),
                                writes: Vec::new()
                            };
                            children.push(label.clone());
                            label.name = name.clone();

                            // Copy for goto definition
                            symbols.push(label);
                            entries.next();

                        // Collect Data Storage
                        } else if let EntryData::Data { bytes: Some(b), ..  } = data {
                            if let Some(id) = inner.macro_call_id {
                                let e = macro_call_sizes.entry(id).or_insert(0);
                                *e += b.len();
                            }
                            kind = SymbolKind::Field;
                            data_size += b.len();
                            entries.next();

                        // Variables
                        } else if let EntryData::Data { bytes: None, .. } = data {
                            if let Some(id) = inner.macro_call_id {
                                let e = macro_call_sizes.entry(id).or_insert(0);
                                *e += *size;
                            }
                            data_size = *size;
                            kind = SymbolKind::Variable;
                            entries.next();
                            break;

                        // Any other entry like an instruction
                        } else {
                            if let Some(id) = inner.macro_call_id {
                                let e = macro_call_sizes.entry(id).or_insert(0);
                                *e += *size;
                            }
                            data_size += *size;
                            entries.next();
                        }

                        // Update range of label body
                        if inner.macro_call_id.is_none() {
                            end_index = end_index.max(inner.end_index);
                        }

                    }

                    // Find matching label in context
                    for ((symbol, label_id, is_global), token) in context.labels {
                        if token.file_index == file_index && symbol == entry_name {
                            let name = symbol.to_string();
                            let address = context.label_addresses.get(label_id).unwrap_or(&0);

                            let references = context.label_usage.get(label_id).map(|refs| {
                                refs.iter().filter(|(_, _, _, access)| *access == AccessKind::Reference).map(|(file_index, start_index, macro_call_id, _)| {
                                    Self::resolve_reference(&linker, *file_index, *start_index, *macro_call_id, name.len())

                                }).collect()

                            }).unwrap_or_else(Vec::new);

                            let calls = context.label_usage.get(label_id).map(|refs| {
                                refs.iter().filter(|(_, _, _, access)| *access == AccessKind::Call).map(|(file_index, start_index, macro_call_id, _)| {
                                    Self::resolve_reference(&linker, *file_index, *start_index, *macro_call_id, name.len())

                                }).collect()

                            }).unwrap_or_else(Vec::new);

                            let jumps = context.label_usage.get(label_id).map(|refs| {
                                refs.iter().filter(|(_, _, _, access)| *access == AccessKind::Jump).map(|(file_index, start_index, macro_call_id, _)| {
                                    Self::resolve_reference(&linker, *file_index, *start_index, *macro_call_id, name.len())

                                }).collect()

                            }).unwrap_or_else(Vec::new);

                            let reads = context.label_usage.get(label_id).map(|refs| {
                                refs.iter().filter(|(_, _, _, access)| *access == AccessKind::MemoryRead).map(|(file_index, start_index, macro_call_id, _)| {
                                    Self::resolve_reference(&linker, *file_index, *start_index, *macro_call_id, name.len())

                                }).collect()

                            }).unwrap_or_else(Vec::new);

                            let writes = context.label_usage.get(label_id).map(|refs| {
                                refs.iter().filter(|(_, _, _, access)| *access == AccessKind::MemoryWrite).map(|(file_index, start_index, macro_call_id, _)| {
                                    Self::resolve_reference(&linker, *file_index, *start_index, *macro_call_id, name.len())

                                }).collect()

                            }).unwrap_or_else(Vec::new);

                            // TODO show signature for callable labels, match by finding the macro with the same inner token
                            symbols.push(GBCSymbol {
                                kind,
                                is_global: *is_global,
                                in_macro: token.macro_call_id.is_some(),
                                location: Self::location_from_file_index(context.files, file_index, start_index, end_index),
                                address: Some(*address),
                                name,
                                width: data_size,
                                result: None,
                                value: format!("${:0>4X}", address),
                                children,
                                references,
                                calls,
                                jumps,
                                reads,
                                writes
                            });
                            break;
                        }
                    }
                }
            }
        }

        fn macro_child_size(calls: &[MacroCall], call_sizes: &HashMap<usize, usize>, parent_id: usize, size: &mut usize, children: &mut usize) {
            // Find all calls within the given parent
            for c in calls {
                if c.parent_id() == Some(parent_id) {
                    *children += 1;
                    *size += *call_sizes.get(&c.id()).unwrap_or(&0);
                    // Now recsurse to also find this macro's children
                    macro_child_size(calls, call_sizes, c.id(), size, children);
                }
            }
        }

        // Macro Calls for inlay hints
        let mut macro_expansions = Vec::new();
        for call in context.macro_calls {
            if call.is_expansion() {
                let name = call.name();
                let mut size = *macro_call_sizes.get(&call.id()).unwrap_or(&0);
                let mut children = 0;
                macro_child_size(context.macro_calls, &macro_call_sizes, call.id(), &mut size, &mut children);
                macro_expansions.push(MacroExpansion {
                    location: Self::location_from_file_index(context.files, name.file_index, name.start_index, name.end_index),
                    children,
                    size
                });
            }
        }

        // Remove any duplicate symbols
        let mut symbol_locations = HashSet::new();
        let mut unique_symbols = Vec::new();
        for s in symbols {
            let loc = (
                s.location.uri.path().to_string(),
                s.location.range.start.line,
                s.location.range.start.character,
                s.location.range.end.line,
                s.location.range.end.character
            );
            if !symbol_locations.contains(&loc) {
                symbol_locations.insert(loc);
                unique_symbols.push(s);
            }
        }

        // Sort by Name
        unique_symbols.sort_by(|a, b| {
            a.name.cmp(&b.name)
        });

        // Ignore everything outside of gbc files
        (unique_symbols.into_iter().filter(|s| {
            s.location.uri.path().ends_with(".gbc")

        }).collect(), macro_expansions)
    }

    fn parse_optimizations(linker: &Linker) -> Optimizations {
        let context = linker.context();
        context.optimizations.into_iter().filter(|(inner, _)| {
            inner.macro_call_id.is_none()

        }).map(|(inner, note)| {
            (
                Self::location_from_file_index(context.files, inner.file_index, inner.start_index, inner.end_index),
                note.clone()
            )

        }).collect()
    }

    fn diagnostics(linker: &Linker, symbols: &[GBCSymbol]) -> Vec<(Url, Diagnostic)> {
        let mut lints = Vec::new();
        let mut constants = Vec::new();
        for symbol in symbols {

            // Record constants for later use
            if symbol.kind == SymbolKind::Constant {
                constants.push(symbol);
            }

            // Ignore Sections, child labels, symbols inside macros, symbols prefixed with _ and symbols inside a
            // library folder
            if symbol.kind == SymbolKind::Namespace
                || symbol.kind == SymbolKind::Method
                || symbol.in_macro
                || symbol.name.starts_with('_')
                || symbol.location.uri.path().contains("lib") {
                continue;
            }

            // Unused Symbols
            if symbol.references.is_empty() && symbol.calls.is_empty() && symbol.reads.is_empty() && symbol.writes.is_empty() && symbol.jumps.is_empty() {
                lints.push((symbol.location.uri.clone(), Diagnostic {
                    message: format!("unused {}", symbol.typ()),
                    range: symbol.location.range.clone(),
                    severity: Some(DiagnosticSeverity::Warning),
                    .. Diagnostic::default()
                }));

            // Symbols unused outside their file
            } else if symbol.is_global {
                let outer_refs = symbol.references.iter().filter(|r| r.uri != symbol.location.uri).count();
                let outer_calls = symbol.calls.iter().filter(|r| r.uri != symbol.location.uri).count();
                let outer_jumps = symbol.jumps.iter().filter(|r| r.uri != symbol.location.uri).count();
                let outer_reads = symbol.reads.iter().filter(|r| r.uri != symbol.location.uri).count();
                let outer_writes = symbol.writes.iter().filter(|r| r.uri != symbol.location.uri).count();
                if outer_refs + outer_calls + outer_jumps + outer_reads + outer_writes == 0 {
                    lints.push((symbol.location.uri.clone(), Diagnostic {
                        message: format!("{} never used outside current file", symbol.typ()),
                        range: symbol.location.range.clone(),
                        severity: Some(DiagnosticSeverity::Warning),
                        .. Diagnostic::default()
                    }));
                }
            }
        }

        // Recommend to replace magic number integers with matching constants
        // TODO currently not working due to integer mapping logic in compiler being incompelte
        let context = linker.context();
        for (_, (inner, value)) in context.integers {
            let location = Self::location_from_file_index(context.files, inner.file_index, inner.start_index, inner.end_index);

            // Ignore integers inside macros and inside of a library folder
            if inner.macro_call_id.is_some() || location.uri.path().contains("lib") {
                continue;
            }

            let matching_constants: Vec<&&GBCSymbol> = constants.iter().filter(|symbol|
                if let Some(ExpressionResult::Integer(v)) = symbol.result {
                    let p = v.abs() as usize;
                    if v == *value && !p.is_power_of_two() && (v > 8 || v < 0) && p != 0xFF && p != 0xFFFF && p % 10 != 0 {
                        // Always consider constants in the local file, for word values also consider global symbols
                        symbol.location.uri == location.uri || (p > 255 && symbol.is_global)

                    } else {
                        false
                    }

                } else {
                    false
                }

            ).collect();
            if !matching_constants.is_empty() {
                for symbol in matching_constants {
                    lints.push((location.uri.clone(), Diagnostic {
                        message: format!("`{}` has the same value", symbol.name),
                        range: location.range.clone(),
                        severity: Some(DiagnosticSeverity::Hint),
                        .. Diagnostic::default()
                    }));
                }
            }
        }
        lints
    }
}

