// STD Dependencies -----------------------------------------------------------
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{MacroCall, LexerFile};
use crate::expression::ExpressionResult;
use super::{AccessKind, LinkerContext, OptimizerInfo};
use super::section::entry::{EntryData, SectionEntry};


// External Dependencies ------------------------------------------------------
use lsp_types::{Diagnostic, DiagnosticSeverity, DocumentSymbol, SymbolInformation, SymbolKind, Url, Range, Position, Location};


// Structs --------------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct AnalysisLint {
    pub uri: Url,
    pub context: String,
    pub detail: Diagnostic
}

#[derive(Debug, Clone)]
pub struct AnalysisHint {
    pub location: Location,
    pub detail: String
}

#[derive(Debug, Clone)]
pub struct AnalysisMacroExpansion {
    pub location: Location,
    pub children: usize,
    pub size: usize
}

#[derive(Debug, Clone)]
pub struct AnalysisSymbol {
    pub kind: SymbolKind,
    pub is_global: bool,
    pub in_macro: bool,
    pub location: Location,
    pub address: Option<usize>,
    pub name: String,
    pub value: String,
    pub width: usize,
    pub result: Option<ExpressionResult>,
    pub children: Vec<AnalysisSymbol>,
    pub references: Vec<Location>,
    pub calls: Vec<Location>,
    pub jumps: Vec<Location>,
    pub reads: Vec<Location>,
    pub writes: Vec<Location>
}

impl AnalysisSymbol {
    pub fn info(&self) -> String {
        let mut parts = Vec::with_capacity(3);
        if self.width == 1 {
            parts.push("1 byte".to_string());

        } else if self.width > 1 {
            parts.push(format!("{} bytes", self.width));
        }

        let refs = self.references.len();
        if refs == 1 {
            parts.push("1 ref".to_string());

        } else if refs > 1 {
            parts.push(format!("{} refs", refs));
        }

        let reads = self.reads.len();
        if reads == 1 {
            parts.push("1 read".to_string());

        } else if reads > 1 {
            parts.push(format!("{} reads", reads));
        }

        let writes = self.writes.len();
        if writes == 1 {
            parts.push("1 write".to_string());

        } else if writes > 1 {
            parts.push(format!("{} writes", writes));
        }

        let jumps = self.jumps.len();
        if jumps == 1 {
            parts.push("1 jump".to_string());

        } else if jumps > 1 {
            parts.push(format!("{} jumps", jumps));
        }

        let calls = self.calls.len();
        if calls == 1 {
            parts.push("1 call".to_string());

        } else if calls > 1 {
            parts.push(format!("{} calls", calls));
        }
        parts.join(", ")
    }

    pub fn typ(&self) -> &str {
        match self.kind {
            SymbolKind::CONSTANT => "constant",
            SymbolKind::CONSTRUCTOR => "macro",
            SymbolKind::FUNCTION => "label",
            SymbolKind::VARIABLE => "variable",
            SymbolKind::FIELD => "label",
            _ => "entry"
        }
    }

    pub fn into_symbol_information(self) -> SymbolInformation {
        #[allow(deprecated)]
        SymbolInformation {
            name: self.name,
            kind: self.kind,
            tags: None,
            deprecated: None,
            location: self.location,
            container_name: None
        }
    }

    pub fn into_document_symbol(self) -> DocumentSymbol {
        let info = self.info();
        #[allow(deprecated)]
        DocumentSymbol {
            name: self.name,
            kind: self.kind,
            tags: None,
            deprecated: None,
            range: self.location.range,
            selection_range: self.location.range,
            detail: Some(match self.kind {
                SymbolKind::CONSTANT => format!("= {}", self.value),
                SymbolKind::CONSTRUCTOR => format!("MACRO{}", self.value),
                SymbolKind::FUNCTION => format!("{} ({})", self.value, info),
                SymbolKind::VARIABLE => format!("{} ({})", self.value, info),
                SymbolKind::FIELD => format!("{} ({})", self.value, info),
                _ => self.value
            }),
            children: Some(self.children.into_iter().map(AnalysisSymbol::into_document_symbol).collect())
        }
    }
}


// High Level Program Analyzer ------------------------------------------------
#[derive(Default)]
pub struct Analysis {
    pub symbols: Vec<AnalysisSymbol>,
    pub macros: Vec<AnalysisMacroExpansion>,
    pub lints: Vec<AnalysisLint>,
    pub hints: Vec<AnalysisHint>,
}

impl Analysis {
    pub fn from_context(ctx: LinkerContext, hints: Vec<OptimizerInfo>) -> Self {
        let (symbols, macros) = Self::symbols(ctx);
        Self {
            lints: Self::lints(&symbols),
            hints: Self::hints(ctx, hints),
            symbols,
            macros
        }
    }
}

impl Analysis {
    fn symbols(ctx: LinkerContext) -> (Vec<AnalysisSymbol>, Vec<AnalysisMacroExpansion>) {
        let mut symbols: Vec<AnalysisSymbol> = Vec::with_capacity(1024);

        // Constants
        for (index, constant) in ctx.constants {
            let token = &constant.inner;
            let name = token.value.to_string();
            if let Some(result) = ctx.constant_values.get(index) {
                let references = ctx.constant_usage.get(index).map(|refs| {
                    refs.iter().map(|(file_index, start_index, macro_call_id)| {
                        resolve_reference(ctx, *file_index, *start_index, *macro_call_id, name.len())

                    }).collect()

                }).unwrap_or_else(Vec::new);
                symbols.push(AnalysisSymbol {
                    kind: SymbolKind::CONSTANT,
                    is_global: index.1.is_none(),
                    in_macro: token.macro_call_id.is_some(),
                    location: location_from_file_index(ctx.files, constant.inner.file_index, constant.inner.start_index, constant.inner.end_index),
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
        for section in ctx.sections {
            symbols.push(AnalysisSymbol {
                kind: SymbolKind::NAMESPACE,
                is_global: false,
                in_macro: section.inner.macro_call_id.is_some(),
                location: location_from_file_index(ctx.files, section.inner.file_index, section.inner.start_index, section.inner.end_index),
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
        for def in ctx.macro_defs {
            // ignore callable labels and only include normal macros
            if !def.is_label {
                // TODO also list builtin macros
                symbols.push(AnalysisSymbol {
                    kind: SymbolKind::CONSTRUCTOR,
                    is_global: def.is_exported,
                    in_macro: false,
                    location: location_from_file_index(ctx.files, def.name.file_index, def.name.start_index, def.name.end_index),
                    address: None,
                    name:  def.name.value.to_string(),
                    width: 0,
                    result: None,
                    value: format!("({})", def.parameters.iter().map(|(_, name)| {
                        format!("@{}", name.value)

                    }).collect::<Vec<String>>().join(", ")),
                    children: Vec::new(),
                    references: Vec::new(),
                    calls: ctx.macro_calls.iter().filter(|call| {
                        call.name().value == def.name.value

                    }).map(|call| {
                        let name = call.name();
                        resolve_reference(ctx, name.file_index, name.start_index, name.macro_call_id, name.value.as_str().len())

                    }).collect(),
                    jumps: Vec::new(),
                    reads: Vec::new(),
                    writes: Vec::new()
                });
            }
        }

        let mut macro_call_sizes: HashMap<usize, usize> = HashMap::with_capacity(64);
        for entry in ctx.section_entries() {

            // Variables / Labels
            let mut entries = entry.iter().peekable();
            while let Some(entry) = entries.next() {

                let file_index = entry.inner.file_index;
                let entry_name = &entry.inner.value;
                let start_index = entry.inner.start_index;
                let mut end_index = entry.inner.end_index;

                if let EntryData::Label { .. } = &entry.data {

                    let mut children = Vec::with_capacity(16);
                    let mut kind = SymbolKind::FUNCTION;
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
                            let mut label = AnalysisSymbol {
                                kind: SymbolKind::METHOD,
                                is_global: false,
                                in_macro: inner.macro_call_id.is_some(),
                                location: location_from_file_index(ctx.files, inner.file_index, inner.start_index, inner.end_index),
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
                            kind = SymbolKind::FIELD;
                            data_size += b.len();
                            entries.next();

                        // Variables
                        } else if let EntryData::Data { bytes: None, .. } = data {
                            if let Some(id) = inner.macro_call_id {
                                let e = macro_call_sizes.entry(id).or_insert(0);
                                *e += *size;
                            }
                            data_size = *size;
                            kind = SymbolKind::VARIABLE;
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
                    for ((symbol, label_id, is_global), token) in ctx.labels {
                        if token.file_index == file_index && symbol == entry_name {
                            let name = symbol.to_string();
                            let address = ctx.label_addresses.get(label_id).unwrap_or(&0);

                            let references = ctx.label_usage.get(label_id).map(|refs| {
                                refs.iter().filter(|(_, _, _, access)| *access == AccessKind::Reference).map(|(file_index, start_index, macro_call_id, _)| {
                                    resolve_reference(ctx, *file_index, *start_index, *macro_call_id, name.len())

                                }).collect()

                            }).unwrap_or_else(Vec::new);

                            let calls = ctx.label_usage.get(label_id).map(|refs| {
                                refs.iter().filter(|(_, _, _, access)| *access == AccessKind::Call).map(|(file_index, start_index, macro_call_id, _)| {
                                    resolve_reference(ctx, *file_index, *start_index, *macro_call_id, name.len())

                                }).collect()

                            }).unwrap_or_else(Vec::new);

                            let jumps = ctx.label_usage.get(label_id).map(|refs| {
                                refs.iter().filter(|(_, _, _, access)| *access == AccessKind::Jump).map(|(file_index, start_index, macro_call_id, _)| {
                                    resolve_reference(ctx, *file_index, *start_index, *macro_call_id, name.len())

                                }).collect()

                            }).unwrap_or_else(Vec::new);

                            let reads = ctx.label_usage.get(label_id).map(|refs| {
                                refs.iter().filter(|(_, _, _, access)| *access == AccessKind::MemoryRead).map(|(file_index, start_index, macro_call_id, _)| {
                                    resolve_reference(ctx, *file_index, *start_index, *macro_call_id, name.len())

                                }).collect()

                            }).unwrap_or_else(Vec::new);

                            let writes = ctx.label_usage.get(label_id).map(|refs| {
                                refs.iter().filter(|(_, _, _, access)| *access == AccessKind::MemoryWrite).map(|(file_index, start_index, macro_call_id, _)| {
                                    resolve_reference(ctx, *file_index, *start_index, *macro_call_id, name.len())

                                }).collect()

                            }).unwrap_or_else(Vec::new);

                            // TODO show signature for callable labels, match by finding the macro with the same inner token
                            symbols.push(AnalysisSymbol {
                                kind,
                                is_global: *is_global,
                                in_macro: token.macro_call_id.is_some(),
                                location: location_from_file_index(ctx.files, file_index, start_index, end_index),
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

        fn macro_child_size(
            calls: &[MacroCall],
            call_sizes: &HashMap<usize, usize>,
            parent_id: usize,
            size: &mut usize,
            children: &mut usize
        ) {
            // Find all calls within the given parent
            for c in calls {
                if c.parent_id() == Some(parent_id) {
                    *children += 1;
                    *size += *call_sizes.get(&c.id()).unwrap_or(&0);
                    // Now recurse to also find this macro's children
                    macro_child_size(calls, call_sizes, c.id(), size, children);
                }
            }
        }

        // Macro Calls for inlay hints
        let mut macro_expansions = Vec::with_capacity(1024);
        for call in ctx.macro_calls {
            if call.is_expansion() {
                let name = call.name();
                let mut size = *macro_call_sizes.get(&call.id()).unwrap_or(&0);
                let mut children = 0;
                macro_child_size(ctx.macro_calls, &macro_call_sizes, call.id(), &mut size, &mut children);
                macro_expansions.push(AnalysisMacroExpansion {
                    location: location_from_file_index(ctx.files, name.file_index, name.start_index, name.end_index),
                    children,
                    size
                });
            }
        }

        // Remove any duplicate symbols
        let mut symbol_locations = HashSet::with_capacity(1024);
        let mut unique_symbols = Vec::with_capacity(1024);
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
        unique_symbols.sort_by(|a, b| {
            a.name.cmp(&b.name)
        });

        // Ignore symbols from files generated via using statements
        (unique_symbols.into_iter().filter(|s| {
            s.location.uri.path().ends_with(".gbc")

        }).collect(), macro_expansions)
    }

    fn lints(symbols: &[AnalysisSymbol]) -> Vec<AnalysisLint> {
        let mut lints = Vec::with_capacity(32);
        for symbol in symbols {

            // Ignore Sections, child labels, symbols inside macros, symbols prefixed with _ and symbols inside a
            // library folder
            if symbol.kind == SymbolKind::NAMESPACE
                || symbol.kind == SymbolKind::METHOD
                || symbol.in_macro
                || symbol.name.starts_with('_')
                || symbol.location.uri.path().contains("lib") {
                continue;
            }

            // Unused Symbols
            if symbol.references.is_empty() && symbol.calls.is_empty() && symbol.reads.is_empty() && symbol.writes.is_empty() && symbol.jumps.is_empty() {
                lints.push(AnalysisLint {
                    uri: symbol.location.uri.clone(),
                    context: symbol.name.clone(),
                    detail: Diagnostic {
                        message: format!("unused {}", symbol.typ()),
                        range: symbol.location.range,
                        severity: Some(DiagnosticSeverity::WARNING),
                        .. Diagnostic::default()
                    }
                });

            // Symbols unused outside their file
            } else if symbol.is_global {
                let outer_refs = symbol.references.iter().filter(|r| r.uri != symbol.location.uri).count();
                let outer_calls = symbol.calls.iter().filter(|r| r.uri != symbol.location.uri).count();
                let outer_jumps = symbol.jumps.iter().filter(|r| r.uri != symbol.location.uri).count();
                let outer_reads = symbol.reads.iter().filter(|r| r.uri != symbol.location.uri).count();
                let outer_writes = symbol.writes.iter().filter(|r| r.uri != symbol.location.uri).count();
                if outer_refs + outer_calls + outer_jumps + outer_reads + outer_writes == 0 {
                    lints.push(AnalysisLint {
                        uri: symbol.location.uri.clone(),
                        context: symbol.name.clone(),
                        detail: Diagnostic {
                            message: format!("global {} only ever used locally", symbol.typ()),
                            range: symbol.location.range,
                            severity: Some(DiagnosticSeverity::WARNING),
                            .. Diagnostic::default()
                        }
                    });
                }
            }
        }
        lints
    }

    fn hints(context: LinkerContext, hints: Vec<OptimizerInfo>) -> Vec<AnalysisHint> {
        hints.into_iter().filter(|(inner, _)| {
            inner.macro_call_id.is_none()

        }).map(|(inner, detail)| AnalysisHint {
            location: location_from_file_index(
                context.files,
                inner.file_index,
                inner.start_index,
                inner.end_index
            ),
            detail

        }).collect()
    }
}

fn resolve_reference(
    ctx: LinkerContext,
    file_index: usize,
    start_index: usize,
    macro_call_id: Option<usize>,
    l: usize

) -> Location {

    // Follow macro calls to their original source
    let macro_call = macro_call_id.and_then(|id| ctx.macro_calls.get(id));
    if let Some(macro_call) = macro_call {
        let token = macro_call.name();
        let file = &ctx.files[token.file_index];
        let (line, col) = file.get_line_and_col(token.start_index);
        Location {
            uri: uri_from_file_path(&file.path),
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
        let file = &ctx.files[file_index];
        let (line, col) = file.get_line_and_col(start_index);
        Location {
            uri: uri_from_file_path(&file.path),
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

pub fn location_from_file_index(
    files: &[LexerFile],
    file_index: usize,
    start_index: usize,
    end_index: usize

) -> Location {
    let file = &files[file_index];
    let (line, col) = file.get_line_and_col(start_index);
    let (eline, ecol) = file.get_line_and_col(end_index);
    Location {
        uri: uri_from_file_path(&file.path),
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

fn uri_from_file_path(path: &PathBuf) -> Url {
    Url::from_file_path(path).unwrap_or_else(|_| {
        Url::from_file_path("/").unwrap()
    })
}


// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    // TODO analysis tests
}

