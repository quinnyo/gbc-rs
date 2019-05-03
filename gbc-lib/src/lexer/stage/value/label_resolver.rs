// STD Dependencies -----------------------------------------------------------
use std::collections::HashMap;


// Internal Dependencies ------------------------------------------------------
use super::{ValueToken, MacroCallIndex};
use crate::lexer::Symbol;
use crate::error::SourceError;
use crate::lexer::token::{InnerToken, LexerToken};
use crate::lexer::stage::macros::BlockStatement;


// Types ----------------------------------------------------------------------
pub type ParentLabelIndex = (Symbol, Option<usize>, MacroCallIndex);
pub type ChildLabelIndex = (Symbol, MacroCallIndex);

type ParentLabelEntry = Option<(usize, Vec<(ChildLabelIndex, usize)>, Vec<(ChildLabelIndex, usize, ChildCallIndex, Option<usize>)>)>;
type ChildCallIndex = Option<(usize, usize)>;
type ChildLabelError = (usize, usize, ChildCallIndex);

#[derive(Debug, Clone)]
enum ChildLabelRef {
    TopLevel {
        index: usize,
        target: usize
    },
    InsideBranch {
        index: usize,
        arg_index: usize,
        inner_index: usize,
        target: usize
    },
    InsideBody {
        index: usize,
        inner_index: usize,
        target: usize
    }
}


// Label Resolver Implementation ----------------------------------------------
pub struct LabelResolver;
impl LabelResolver {

    pub fn parent_label_id(inner: &InnerToken, parent_only: bool, file_index: Option<usize>) -> ParentLabelIndex {
        if parent_only || inner.macro_call_id.is_none() {
            (inner.value.clone(), file_index, None)

        } else if let Some(call_id) = inner.macro_call_id() {
            (inner.value.clone(), file_index, Some(call_id))

        } else {
            unreachable!()
        }
    }

    pub fn convert_parent_label_refs(
        parent_labels: &HashMap<ParentLabelIndex, (InnerToken, usize)>,
        tokens: Vec<ValueToken>

    ) -> Vec<ValueToken> {
        tokens.into_iter().map(|token| {
            if let ValueToken::Name(inner) = token {

                // Local Lookup
                if let Some((_, id)) = parent_labels.get(&Self::parent_label_id(&inner, true, Some(inner.file_index))) {
                    ValueToken::ParentLabelRef(inner, *id)

                // Global Lookup
                } else if let Some((_, id)) = parent_labels.get(&Self::parent_label_id(&inner, true, None)) {
                    ValueToken::ParentLabelRef(inner, *id)

                } else {
                    ValueToken::Name(inner)
                }

            } else if let ValueToken::BuiltinCall(inner, arguments) = token {
                ValueToken::BuiltinCall(inner, arguments.into_iter().map(|tokens| {
                    Self::convert_parent_label_refs(parent_labels, tokens)

                }).collect())

            } else {
                token
            }

        }).collect()
    }

    pub fn convert_child_labels_refs(mut tokens: Vec<ValueToken>) -> Result<Vec<ValueToken>, SourceError> {

        let mut parent_label_map = HashMap::new();
        let mut child_label_refs = Vec::with_capacity(64);
        let mut error = Self::inner_assign_and_verify_child_label_refs(
            &mut tokens,
            &mut parent_label_map,
            &mut child_label_refs,
            true,
            None,
            None
        );

        // Verify any open parent label scopes
        for (_, mut parent_label) in parent_label_map.drain() {
            if let Some(e) = Self::verify_child_label_refs_under_parent(parent_label.take(), &mut child_label_refs) {
                error = Some(e);
                break;
            }
        }

        // Handle Errors
        if let Some(error) = error {
            let parent = tokens[error.1].inner();
            let label = if let Some((index, arg_index)) = error.2 {
                if let Some(ValueToken::BuiltinCall(_, ref arguments)) = tokens.get(index) {
                    arguments[arg_index][error.0].inner()

                } else {
                    unreachable!();
                }

            } else {
                tokens[error.0].inner()
            };
            return Err(label.error(format!(
                "Reference to unknown child label \"{}\", not defined under the current parent label \"{}\".",
                label.value,
                parent.value

            )).with_reference(parent, "Definition of parent label was"));
        }

        // Set target label ID of all child label references
        for r in child_label_refs.drain(0..) {
            match r {
                ChildLabelRef::TopLevel { index, target } => {
                    if let Some(ValueToken::ChildLabelRef(_, ref mut id, _)) = tokens.get_mut(index) {
                        *id = target;
                        continue;
                    }
                },
                ChildLabelRef::InsideBranch { index, arg_index, inner_index, target} => {
                    if let Some(ValueToken::BuiltinCall(_, ref mut arguments)) = tokens.get_mut(index) {
                        if let Some(arg_tokens) = arguments.get_mut(arg_index) {
                            if let Some(ValueToken::ChildLabelRef(_, ref mut id, _)) = arg_tokens.get_mut(inner_index) {
                                *id = target;
                                continue;
                            }
                        }

                    } else if let Some(ValueToken::IfStatement(_, ref mut if_branches)) = tokens.get_mut(index) {
                        if let Some(branch) = if_branches.get_mut(arg_index) {
                            if let Some(ValueToken::ChildLabelRef(_, ref mut id, _)) = branch.body.get_mut(inner_index) {
                                *id = target;
                                continue;
                            }
                        }
                    }
                },
                ChildLabelRef::InsideBody { index, inner_index, target} => {
                    if let Some(ValueToken::ForStatement(_, ref mut for_statement)) = tokens.get_mut(index) {
                        if let Some(ValueToken::ChildLabelRef(_, ref mut id, _)) = for_statement.body.get_mut(inner_index) {
                            *id = target;
                            continue;
                        }

                    } else if let Some(ValueToken::BlockStatement(_, ref mut block)) = tokens.get_mut(index) {
                        match block {
                            BlockStatement::Using(_, body) | BlockStatement::Volatile(body) => {
                                if let Some(ValueToken::ChildLabelRef(_, ref mut id, _)) = body.get_mut(inner_index) {
                                    *id = target;
                                    continue;
                                }
                            }
                        }
                    }
                }
            }
            unreachable!("Invalid child label ref generated: {:?}", r);
        }

        Ok(tokens)
    }

    fn inner_assign_and_verify_child_label_refs(
        tokens: &mut [ValueToken],
        parent_label_map: &mut HashMap<Option<usize>, ParentLabelEntry>,
        child_label_refs: &mut Vec<ChildLabelRef>,
        parent_def_allowed: bool,
        call_parent: ChildCallIndex,
        stmt_parent: Option<usize>

    ) -> Option<ChildLabelError> {
        for (index, token) in tokens.iter_mut().enumerate() {
            match token {
                ValueToken::ParentLabelDef(inner, _) if parent_def_allowed => {
                    let parent_label = parent_label_map.entry(inner.macro_call_id).or_insert(None);
                    if let Some(error) = Self::verify_child_label_refs_under_parent(parent_label.take(), child_label_refs) {
                        return Some(error);
                    }
                    *parent_label = Some((index, Vec::new(), Vec::new()));
                },
                ValueToken::ChildLabelDef(inner, id, call_id) => {
                    let parent_label = parent_label_map.entry(inner.macro_call_id).or_insert(None);
                    if let Some(parent_label) = parent_label.as_mut() {
                        parent_label.1.push(((inner.value.clone(), call_id.clone()), *id));
                    }
                },
                ValueToken::ChildLabelRef(inner, _, call_id) => {
                    let parent_label = parent_label_map.entry(inner.macro_call_id).or_insert(None);
                    if let Some(parent_label) = parent_label.as_mut() {
                        parent_label.2.push(((inner.value.clone(), call_id.clone()), index, call_parent, stmt_parent));
                    }
                },
                ValueToken::BuiltinCall(_, arguments) => {
                    for (arg_index, arg_tokens) in arguments.iter_mut().enumerate() {
                        if let Some(error) = Self::inner_assign_and_verify_child_label_refs(
                            arg_tokens,
                            parent_label_map,
                            child_label_refs,
                            false,
                            Some((index, arg_index)),
                            None
                        ) {
                            return Some(error);
                        }
                    }
                },
                ValueToken::ForStatement(_, for_statement) => {
                    if let Some(error) = Self::inner_assign_and_verify_child_label_refs(
                        &mut for_statement.body,
                        parent_label_map,
                        child_label_refs,
                        true,
                        None,
                        Some(index)
                    ) {
                        return Some(error);
                    }
                },
                ValueToken::IfStatement(_, if_branches) => {
                    for (branch_index, branch) in if_branches.iter_mut().enumerate() {
                        if let Some(error) = Self::inner_assign_and_verify_child_label_refs(
                            &mut branch.body,
                            parent_label_map,
                            child_label_refs,
                            true,
                            Some((index, branch_index)),
                            None
                        ) {
                            return Some(error);
                        }
                    }
                },
                ValueToken::BlockStatement(_, block) => {
                    match block {
                        BlockStatement::Using(_, body) | BlockStatement::Volatile(body) => {
                            if let Some(error) = Self::inner_assign_and_verify_child_label_refs(
                                body,
                                parent_label_map,
                                child_label_refs,
                                true,
                                None,
                                Some(index)
                            ) {
                                return Some(error);
                            }
                        }
                    }
                },
                _ => {}
            }
        }
        None
    }

    fn verify_child_label_refs_under_parent(
        parent_label: ParentLabelEntry,
        child_label_refs: &mut Vec<ChildLabelRef>

    ) -> Option<ChildLabelError> {
        if let Some((previous_index, child_defs, child_refs)) = parent_label {
            for (ref_name, token_index, call_parent, stmt_parent) in &child_refs {
                let mut label_exists = false;
                for (def_name, label_id) in &child_defs {
                    if def_name == ref_name {
                        label_exists = true;
                        if let Some((index, arg_index)) = call_parent {
                            child_label_refs.push(ChildLabelRef::InsideBranch {
                                index: *index,
                                arg_index: *arg_index,
                                inner_index: *token_index,
                                target: *label_id
                            });

                        } else if let Some(index) = stmt_parent {
                            child_label_refs.push(ChildLabelRef::InsideBody {
                                index: *index,
                                inner_index: *token_index,
                                target: *label_id
                            });

                        } else {
                            child_label_refs.push(ChildLabelRef::TopLevel {
                                index: *token_index,
                                target: *label_id
                            });
                        }
                        break;
                    }
                }
                if !label_exists {
                    return Some((*token_index, previous_index, *call_parent));
                }
            }
        }
        None
    }
}

