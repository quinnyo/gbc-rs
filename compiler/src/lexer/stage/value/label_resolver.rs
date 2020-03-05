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
pub type NamespaceIndex = (Symbol, Option<usize>, MacroCallIndex);

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

    pub fn namespace_id(inner: &InnerToken, parent_only: bool, file_index: Option<usize>) -> NamespaceIndex {
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
        structs: &HashMap<NamespaceIndex, (InnerToken, HashMap<String, (InnerToken, usize)>)>,
        tokens: &mut [ValueToken]

    ) -> Result<(), SourceError> {
        for token in tokens {

            let reference = if let ValueToken::Name(inner) = token {
                // Macro Internal Lookup
                if let Some((_, id)) = parent_labels.get(&Self::parent_label_id(&inner, false, Some(inner.file_index))) {
                    Some(ValueToken::ParentLabelRef(inner.clone(), *id))

                // File Local Lookup
                } else if let Some((_, id)) = parent_labels.get(&Self::parent_label_id(&inner, true, Some(inner.file_index))) {
                    Some(ValueToken::ParentLabelRef(inner.clone(), *id))

                // Global Lookup
                } else if let Some((_, id)) = parent_labels.get(&Self::parent_label_id(&inner, true, None)) {
                    Some(ValueToken::ParentLabelRef(inner.clone(), *id))

                } else {
                    Some(ValueToken::Name(inner.clone()))
                }

            } else if let ValueToken::Lookup(_, members) = token {

                let member_path: Vec<String> = members.iter().map(|f| f.inner().value.to_string()).collect();
                let mut namespace_path = member_path.clone();

                // Find Namespace
                let mut inner = members[0].inner().clone();
                let mut namespace_data = None;
                while !namespace_path.is_empty() {
                    namespace_path.pop();
                    inner.value = namespace_path.join("::").into();

                    // Macro Internal Namespace Lookup
                    if let Some(data) = structs.get(&Self::namespace_id(&inner, false, Some(inner.file_index))) {
                        namespace_data = Some(data);
                        break;

                    // File Local Namespace Lookup
                    } else if let Some(data) = structs.get(&Self::namespace_id(&inner, true, Some(inner.file_index))) {
                        namespace_data = Some(data);
                        break;

                    // Global Namespace Lookup
                    } else if let Some(data) = structs.get(&Self::namespace_id(&inner, true, None)) {
                        namespace_data = Some(data);
                        break;
                    }

                }

                // Found a namespace with the same name
                if let Some((struct_inner, namespace_members)) = namespace_data {

                    // Lookup member in namespace
                    let member_path = member_path.join("::");
                    if let Some((_, id)) = namespace_members.get(&member_path) {
                        let mut inner = members[0].inner().clone();
                        inner.value = member_path.into();
                        inner.end_index = members.last().unwrap().inner().end_index;
                        if let ValueToken::ParentLabelCall(_, _, args) = members.last_mut().unwrap() {
                            Some(ValueToken::ParentLabelCall(inner, *id, args.take()))

                        } else {
                            Some(ValueToken::ParentLabelRef(inner, *id))
                        }

                    } else {
                        // TODO suggest similiar members in other namespaces?
                        return Err(inner.error(
                            format!(
                                "Reference to unknown member \"{}\".",
                                members.last().unwrap().inner().value
                            )

                        ).with_reference(struct_inner, "in namespace defined"));
                    }

                } else {
                    // TODO suggest namespaces with similiar names
                    return Err(inner.error(format!("Reference to unknown namespace \"{}\".", members[0].inner().value)));
                }

            } else {
                None
            };

            // Replace Lookup Token with a simple ParentLabelRef or ParentLabelCall
            if let Some(reference) = reference {
                *token = reference;

            } else if let ValueToken::BuiltinCall(_, arguments) = token {
                for arg_tokens in arguments {
                    Self::convert_parent_label_refs(parent_labels, structs, arg_tokens)?
                }

            } else if let ValueToken::ParentLabelCall(inner, ref mut label_id, Some(arguments)) = token {

                // Macro Internal Lookup
                *label_id = if let Some((_, id)) = parent_labels.get(&Self::parent_label_id(&inner, false, Some(inner.file_index))) {
                    *id

                // File Local Lookup
                } else if let Some((_, id)) = parent_labels.get(&Self::parent_label_id(&inner, true, Some(inner.file_index))) {
                    *id

                // Global Lookup
                } else if let Some((_, id)) = parent_labels.get(&Self::parent_label_id(&inner, true, None)) {
                    *id

                } else {
                    0//unreachable!("Invalid label call ID.");
                };

                for arg_tokens in arguments {
                    Self::convert_parent_label_refs(parent_labels, structs, arg_tokens)?
                }

            } else if let ValueToken::IfStatement(_, branches) = token {
                for branch in branches {
                    if let Some(condition) = branch.condition.as_mut() {
                        Self::convert_parent_label_refs(parent_labels, structs, condition)?;
                    }
                    Self::convert_parent_label_refs(parent_labels, structs, &mut branch.body)?;
                }

            } else if let ValueToken::ForStatement(_, for_statement) = token {
                Self::convert_parent_label_refs(parent_labels, structs, &mut for_statement.body)?;

            } else if let ValueToken::BlockStatement(_, block) = token {
                match block {
                    BlockStatement::Using(_, body) => Self::convert_parent_label_refs(parent_labels, structs, body)?,
                    BlockStatement::Volatile(body) => Self::convert_parent_label_refs(parent_labels, structs, body)?
                };
            }
        }
        Ok(())
    }

    pub fn convert_child_labels_refs(tokens: &mut [ValueToken]) -> Result<(), SourceError> {

        let mut parent_label_map = HashMap::with_capacity(256);
        let mut child_label_refs = Vec::with_capacity(64);
        let mut error = Self::inner_assign_and_verify_child_label_refs(
            tokens,
            &mut parent_label_map,
            &mut child_label_refs,
            true,
            None,
            None
        )?;

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

        Ok(())
    }

    fn inner_assign_and_verify_child_label_refs(
        tokens: &mut [ValueToken],
        parent_label_map: &mut HashMap<Option<usize>, ParentLabelEntry>,
        child_label_refs: &mut Vec<ChildLabelRef>,
        parent_def_allowed: bool,
        call_parent: ChildCallIndex,
        stmt_parent: Option<usize>

    ) -> Result<Option<ChildLabelError>, SourceError> {
        for (index, token) in tokens.iter_mut().enumerate() {
            match token {
                ValueToken::ParentLabelDef(inner, _, _) if parent_def_allowed => {
                    let parent_label = parent_label_map.entry(inner.macro_call_id).or_insert(None);
                    if let Some(error) = Self::verify_child_label_refs_under_parent(parent_label.take(), child_label_refs) {
                        return Ok(Some(error));
                    }
                    *parent_label = Some((index, Vec::with_capacity(2), Vec::with_capacity(2)));
                },
                ValueToken::ChildLabelDef(inner, id, call_id) => {
                    let parent_label = parent_label_map.entry(inner.macro_call_id).or_insert(None);
                    if let Some(parent_label) = parent_label.as_mut() {
                        parent_label.1.push(((inner.value.clone(), *call_id), *id));
                    }
                },
                ValueToken::ChildLabelRef(inner, _, call_id) => {
                    let parent_label = parent_label_map.entry(inner.macro_call_id).or_insert(None);
                    if let Some(parent_label) = parent_label.as_mut() {
                        parent_label.2.push(((inner.value.clone(), *call_id), index, call_parent, stmt_parent));

                    // Child labels in macros must have a local parent
                    } else if inner.macro_call_id.is_some() {
                        return Err(inner.error(
                            "Reference to child label inside of macro without a any parent label inside the macro.".to_string()
                        ));
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
                        )? {
                            return Ok(Some(error));
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
                    )? {
                        return Ok(Some(error));
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
                        )? {
                            return Ok(Some(error));
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
                            )? {
                                return Ok(Some(error));
                            }
                        }
                    }
                },
                _ => {}
            }
        }
        Ok(None)
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

