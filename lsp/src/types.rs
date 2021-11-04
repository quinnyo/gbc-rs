// External Dependencies ------------------------------------------------------
use serde::{Serialize, Deserialize};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::{DocumentSymbol, SymbolInformation, SymbolKind, Range, Location};


// Internal Dependencies ------------------------------------------------------
use compiler::expression::ExpressionResult;


// Types ----------------------------------------------------------------------
#[derive(Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum InlayKind {
    TypeHint,
    OptimizerHint,
    ParameterHint,
    ChainingHint,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct InlayHint {
    pub range: Range,
    pub kind: InlayKind,
    pub label: String,
}


pub enum ServerStatusNotification {}
impl Notification for ServerStatusNotification {
    type Params = ServerStatusParams;
    const METHOD: &'static str = "experimental/serverStatus";
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone)]
pub struct ServerStatusParams {
    pub quiescent: bool,
    pub message: Option<String>,
}

pub enum InlayHintsNotification {}
impl Notification for InlayHintsNotification {
    type Params = InlayHintsParams;
    const METHOD: &'static str = "experimental/inlayHints";
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone)]
pub struct InlayHintsParams {}

pub type Optimizations = Vec<(Location, String)>;

#[derive(Debug, Clone)]
pub struct MacroExpansion {
    pub location: Location,
    pub children: usize,
    pub size: usize
}

#[derive(Debug, Clone)]
pub struct GBCSymbol {
    pub kind: SymbolKind,
    pub is_global: bool,
    pub in_macro: bool,
    pub location: Location,
    pub address: Option<usize>,
    pub name: String,
    pub value: String,
    pub width: usize,
    pub result: Option<ExpressionResult>,
    pub children: Vec<GBCSymbol>,
    pub references: Vec<Location>,
    pub calls: Vec<Location>,
    pub jumps: Vec<Location>,
    pub reads: Vec<Location>,
    pub writes: Vec<Location>
}

impl GBCSymbol {
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
            SymbolKind::Constant => "constant",
            SymbolKind::Constructor => "macro",
            SymbolKind::Function => "label",
            SymbolKind::Variable => "variable",
            SymbolKind::Field => "label",
            _ => "entry"
        }
    }

    pub fn into_symbol_information(self) -> SymbolInformation {
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
        DocumentSymbol {
            name: self.name,
            kind: self.kind,
            tags: None,
            deprecated: None,
            range: self.location.range,
            selection_range: self.location.range,
            detail: Some(match self.kind {
                SymbolKind::Constant => format!("= {}", self.value),
                SymbolKind::Constructor => format!("MACRO{}", self.value),
                SymbolKind::Function => format!("{} ({})", self.value, info),
                SymbolKind::Variable => format!("{} ({})", self.value, info),
                SymbolKind::Field => format!("{}", self.value),
                _ => self.value
            }),
            children: Some(self.children.into_iter().map(GBCSymbol::into_document_symbol).collect())
        }
    }
}

