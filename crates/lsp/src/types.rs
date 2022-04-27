// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// External Dependencies ------------------------------------------------------
use serde::{Serialize, Deserialize};
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::Range;


// Types ----------------------------------------------------------------------
#[derive(Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum InlayKind {
    TypeHint,
    OptimizerHint,
    ParameterHint,
    ChainingHint,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Runnable {
    pub label: String,
    pub kind: String,
    pub args: RunnableArgs,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct RunnableArgs {
    #[serde(rename="gbcArgs")]
    pub gbc_args: Vec<String>,
    #[serde(rename="workspaceRoot")]
    pub workspace_root: Option<String>,
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

pub enum DebuggerOutlineNotification {}
impl Notification for DebuggerOutlineNotification {
    type Params = DebuggerOutlineParams;
    const METHOD: &'static str = "experimental/debuggerOutline";
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq, Clone)]
pub struct DebuggerOutlineLocation {
    pub filename: String,
    pub line: usize,
    pub character: usize
}

#[derive(Deserialize, Serialize, PartialEq, Eq, Clone)]
pub struct DebuggerOutlineParams {
    pub lines: Vec<String>,
    pub locations: HashMap<usize, DebuggerOutlineLocation>
}

