// STD Dependencies -----------------------------------------------------------
use std::cmp;
use std::collections::{HashMap, HashSet};


// External Dependencies ------------------------------------------------------
use gb_cpu::Register;
use ordered_float::OrderedFloat;


// Internal Dependencies ------------------------------------------------------
use crate::error::SourceError;
use crate::lexer::{InnerToken, Symbol, BUILTIN_MACRO_DEFS, BUILTIN_MACRO_INDEX};
use crate::expression::{DataExpression, Expression, ExpressionValue, ExpressionResult, OptionalDataExpression, Operator};


// Expression Evaluator -------------------------------------------------------
type FileIndex = Option<usize>;
type ConstantIndex = (Symbol, FileIndex);

#[derive(Debug, Eq, PartialEq, Clone)]
struct EvaluatorConstant {
    inner: InnerToken,
    is_default: bool,
    expression: DataExpression,
}

#[derive(Debug, Eq, PartialEq)]
pub struct UsageInformation {
    constants: HashMap<ConstantIndex, HashSet<usize>>,
    labels: HashSet<usize>,
    magic_numbers: Vec<(ConstantIndex, InnerToken, i32)>
}

impl UsageInformation {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
            labels: HashSet::new(),
            magic_numbers: Vec::new()
        }
    }

    fn use_constant(&mut self, index: ConstantIndex, from_file_index: usize) {
        let entry = self.constants.entry(index).or_insert_with(HashSet::new);
        entry.insert(from_file_index);
    }

    fn use_label(&mut self, label: usize) {
        self.labels.insert(label);
    }

    fn merge(&mut self, other: &UsageInformation) {
        for (index, files) in &other.constants {
            let entry = self.constants.entry(index.clone()).or_insert_with(HashSet::new);
            for f in files {
                entry.insert(*f);
            }
        }
        for l in &other.labels {
            self.labels.insert(*l);
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct EvaluatorContext {
    constants: HashMap<ConstantIndex, (ExpressionResult, UsageInformation)>,
    raw_constants: HashMap<ConstantIndex, EvaluatorConstant>,
    label_addresses: HashMap<usize, usize>,
    raw_labels: HashMap<(Symbol, usize), InnerToken>,
    callable_labels: HashMap<usize, (InnerToken, Option<Vec<Register>>)>,
    inactive_labels: HashMap<usize, InnerToken>,
    relative_address_offset: i32,
    linter_enabled: bool
}

impl EvaluatorContext {

    #[allow(clippy::new_without_default)]
    pub fn new(linter_enabled: bool) -> Self {
        Self {
            constants: HashMap::with_capacity(512),
            raw_constants: HashMap::with_capacity(512),
            label_addresses: HashMap::with_capacity(512),
            raw_labels: HashMap::with_capacity(512),
            callable_labels: HashMap::with_capacity(64),
            inactive_labels: HashMap::with_capacity(64),
            relative_address_offset: 0,
            linter_enabled
        }
    }

    pub fn find_unused(&self, usage: &UsageInformation) -> Vec<(usize, Symbol, SourceError)> {
        let mut locations = Vec::new();
        for (index, c) in &self.raw_constants {
            if let Some(files) = usage.constants.get(index) {
                // Warn when non-default, global constants are only used in their file of
                // declaration
                // TODO must check if ANY reference is default instead of only this one
                if !c.is_default && index.1.is_none() && files.len() == 1 {
                    if files.get(&c.inner.file_index).is_some() {
                        locations.push((
                            3,
                            c.inner.value.clone(),
                            c.inner.error(
                                format!("Constant \"{}\" should be made non-global or default, since it is never used outside its declaration", c.inner.value)
                            )
                        ));

                    } else if let Some(file) = files.iter().nth(0) {
                        locations.push((
                            4,
                            c.inner.value.clone(),
                            c.inner.error(
                                format!("Constant \"{}\" should be moved to another file, since it is never used at its declaration", c.inner.value)

                            ).with_file(*file, "Constant is only used")
                        ));
                    }
                }

            } else {
                locations.push((
                    0,
                    c.inner.value.clone(),
                    c.inner.error(
                        format!("Constant \"{}\" is never used, declared", c.inner.value)
                    )
                ));
            }
        }
        for ((_, index), l) in &self.raw_labels {
            // TODO warn about labels that are never referenced from other files
            if !usage.labels.contains(index) {
                locations.push((
                    1,
                    l.value.clone(),
                    l.error(format!("Label \"{}\" is never referenced, declared", l.value))
                ));
            }
        }
        locations
    }

    pub fn find_magic_numbers(&self, usage: &UsageInformation) -> Vec<(usize, Symbol, SourceError)> {
        let mut locations = Vec::new();
        for (constant_index,  integer_token, value) in &usage.magic_numbers {
            locations.push((
                2,
                integer_token.value.clone(),
                integer_token.error(format!(
                    "Fixed integer value (${:0>4x}) could be replaced with the constant \"{}\" that shares the same value",
                    value,
                    constant_index.0
                ))
            ));
        }
        locations
    }

    pub fn declare_constant(&mut self, inner: InnerToken, is_default: bool, is_private: bool, value: DataExpression) {

        let index = if is_private {
            (inner.value.clone(), Some(inner.file_index))

        } else {
            (inner.value.clone(), None)
        };

        let existing_default = self.raw_constants.get(&index).map(|c| c.is_default);
        let set = match (is_default, existing_default) {
            // Constant does not yet exist at all, set either the default or actual value
            (_, None) => true,
            // Constant already exists as a default, override with it with the actual value
            (false, Some(true)) => true,
            // Don't override existing actual value with a later declared default
            (true, Some(false)) => false,
            // Should not happen due to expression and entry stages filtering this case out
            _ => {
                unreachable!("Invalid constant declaration order: {} {:?}", is_default, existing_default)
            }
        };
        if set {
            self.raw_constants.insert(index, EvaluatorConstant {
                inner,
                is_default,
                expression: value
            });
        }
    }

    pub fn declare_label(&mut self, inner: &InnerToken, label_id: usize, args: Option<Vec<Register>>, active: bool) {

        // Record labels which
        if args.is_some() {
            self.callable_labels.insert(label_id, (inner.clone(), args));
        }

        // Record labels which exists but are in branches which are not active
        if !active {
            self.inactive_labels.insert(label_id, inner.clone());
        }

        self.raw_labels.insert((inner.value.clone(), label_id), inner.clone());

    }

    pub fn update_label_address(&mut self, label_id: usize, address: usize) {
        self.label_addresses.insert(label_id, address);
    }

    // Constant Expressions ---------------------------------------------------
    pub fn resolve_all_constants(&mut self) -> Result<(), SourceError> {
        let mut names: Vec<ConstantIndex> = self.raw_constants.keys().cloned().collect();
        names.sort_by(|a, b| {
            a.0.as_str().cmp(&b.0.as_str())
        });

        for name in names {
            // Keep track of other constants used in the current constant's expression
            let mut usage = UsageInformation::new();
            let constant = self.raw_constants[&name].clone();
            let stack = vec![&name.0];
            let c = self.inner_const_resolve(
                &stack[..],
                &constant.expression,
                &mut usage,
                constant.inner.file_index
            )?;
            self.constants.insert(name, (c, usage));
        }
        Ok(())
    }

    pub fn resolve_const_expression(
        &mut self,
        expression: &DataExpression,
        usage: &mut UsageInformation,
        from_file_index: usize

    ) -> Result<ExpressionResult, SourceError> {
        let stack = Vec::new();
        self.inner_const_resolve(&stack, expression, usage, from_file_index)
    }

    pub fn resolve_opt_const_expression(
        &mut self,
        expression: &OptionalDataExpression,
        usage: &mut UsageInformation,
        from_file_index: usize

    ) -> Result<Option<ExpressionResult>, SourceError> {
        if let Some(expr) = expression {
            let stack = Vec::new();
            Ok(Some(self.inner_const_resolve(&stack, expr, usage, from_file_index)?))

        } else {
            Ok(None)
        }
    }

    // Dynamic Expressions ----------------------------------------------------
    pub fn resolve_dyn_expression(
        &self,
        expression: &DataExpression,
        usage: &mut UsageInformation,
        address_offset: Option<i32>,
        is_call_instruction: bool,
        from_file_index: usize

    ) -> Result<ExpressionResult, SourceError> {
        self.inner_dyn_resolve(expression, usage, address_offset, is_call_instruction, from_file_index)
    }

    pub fn resolve_opt_dyn_expression(
        &self,
        expression: &OptionalDataExpression,
        usage: &mut UsageInformation,
        address_offset: Option<i32>,
        from_file_index: usize

    ) -> Result<Option<ExpressionResult>, SourceError> {
        if let Some(expr) = expression {
            Ok(Some(self.inner_dyn_resolve(expr, usage, address_offset, false, from_file_index)?))

        } else {
            Ok(None)
        }
    }

    pub fn resolve_call_signature(
        &self,
        expression: &DataExpression,
        usage: &mut UsageInformation

    ) -> Result<Vec<(Register, Expression)>, SourceError> {
        // Expression must be a valid ParentLabelCall here
        if let Expression::ParentLabelCall { inner, id, args, .. } = expression {
            if let Some((outer, signature)) = self.callable_labels.get(id) {

                if self.linter_enabled {
                    usage.use_label(*id);
                }

                if let Some(signature) = signature {
                    if signature.len() != args.len() {
                        Err(outer.error(
                            "Invalid number of arguments for label call".to_string()

                        ).with_reference(inner, format!("Label takes {} arguments, but {} were supplied", signature.len(), args.len())))

                    } else {
                        Ok(signature.iter().zip(args.iter()).map(|(reg, expr)| {
                            (reg.clone(), expr.clone())

                        }).collect())
                    }

                } else if !args.is_empty() {
                    Err(outer.error(
                        "Invalid number of arguments for label call".to_string()

                    ).with_reference(inner, "Label takes no arguments".to_string()))

                } else {
                    Ok(Vec::new())
                }

            } else {
                panic!("Invalid callable when resolving signature!");
            }

        } else {
            panic!("Invalid callable when resolving signature!");
        }
    }

    fn inner_dyn_resolve(
        &self,
        expression: &Expression,
        usage: &mut UsageInformation,
        address_offset: Option<i32>,
        is_call_instruction: bool,
        from_file_index: usize

    ) -> Result<ExpressionResult, SourceError> {
        match expression {
            Expression::Binary { inner, op, left, right } => {
                let left = self.inner_dyn_resolve(left, usage, address_offset, false, from_file_index)?;
                let right = self.inner_dyn_resolve(right, usage, address_offset, false, from_file_index)?;
                Self::apply_binary_operator(&inner, op, left, right)
            },
            Expression::Unary { inner, op, right } => {
                let right = self.inner_dyn_resolve(right, usage, address_offset, false, from_file_index)?;
                Self::apply_unary_operator(&inner, op, right)
            },
            Expression::Value(value) => {
                Ok(match value {
                    ExpressionValue::ConstantValue(inner, name) => {

                        let global_index = (name.clone(), None);
                        let local_index = (name.clone(), Some(from_file_index));

                        // Local Lookup
                        if let Some(value) = self.constants.get(&local_index) {
                            if self.linter_enabled {
                                usage.merge(&value.1);
                                usage.use_constant(local_index, from_file_index);
                            }
                            value.0.clone()

                        // Global Lookup
                        } else if let Some(value) = self.constants.get(&global_index) {
                            if self.linter_enabled {
                                usage.merge(&value.1);
                                usage.use_constant(global_index, from_file_index);
                            }
                            value.0.clone()

                        } else {
                            return Err(self.undeclared_const_error(inner, name));
                        }
                    },
                    ExpressionValue::Integer(i) => ExpressionResult::Integer(*i),
                    ExpressionValue::LintInteger(inner, i) => {
                        self.lint_integer(usage, inner, *i);
                        ExpressionResult::Integer(*i)
                    },
                    ExpressionValue::Float(f) => ExpressionResult::Float(*f),
                    ExpressionValue::String(s) => ExpressionResult::String(s.to_string()),
                    ExpressionValue::OffsetAddress(_, offset) => {
                        let relative_address_offset = address_offset.expect("Address offset without supplied base address");
                        ExpressionResult::Integer(relative_address_offset + offset)
                    },
                    ExpressionValue::ParentLabelAddress(outer, id) | ExpressionValue::ChildLabelAddress(outer, id) => {
                        if self.linter_enabled {
                            usage.use_label(*id);
                        }

                        if let Some((inner, _)) = self.callable_labels.get(id) {
                            if is_call_instruction {
                                return Err(outer.error(
                                    "Reference to call-only label".to_string()

                                ).with_reference(inner, "Label is declared as callable and must be invoked with arguments".to_string()));
                            }
                        }

                        if let Some(addr) = self.label_addresses.get(id) {
                            ExpressionResult::Integer(*addr as i32)

                        } else if let Some(inner) = self.inactive_labels.get(id) {
                            return Err(outer.error(
                                "Reference to unreachable label".to_string()

                            ).with_reference(inner, "Label is declared inside currently inactive IF branch".to_string()));

                        } else {
                            panic!("Invalid label ID when resolving expression!");
                        }
                    }
                })
            },
            Expression::BuiltinCall { inner, name, args } => {
                let mut arguments = Vec::with_capacity(args.len());
                for arg in args {
                    arguments.push(self.inner_dyn_resolve(
                        arg,
                        usage,
                        address_offset,
                        false,
                        from_file_index
                    )?);
                }
                Self::execute_builtin_call(&inner, &name, arguments)
            },
            Expression::ParentLabelCall { inner, id, .. } => {
                let outer = inner;
                if let Some(addr) = self.label_addresses.get(id) {
                    Ok(ExpressionResult::Integer(*addr as i32))

                } else if let Some(inner) = self.inactive_labels.get(id) {
                    Err(outer.error(
                        "Reference to unreachable label".to_string()

                    ).with_reference(inner, "Label is declared inside currently inactive IF branch".to_string()))

                } else {
                    panic!("Invalid label ID when resolving expression!");
                }
            },
            _ => unreachable!()
        }
    }

    fn inner_const_resolve(
        &mut self,
        constant_stack: &[&Symbol],
        expression: &Expression,
        usage: &mut UsageInformation,
        from_file_index: usize

    ) -> Result<ExpressionResult, SourceError> {
        match expression {
            Expression::Binary { inner, op, left, right } => {
                let left = self.inner_const_resolve(constant_stack, left, usage, from_file_index)?;
                let right = self.inner_const_resolve(constant_stack, right, usage, from_file_index)?;
                Self::apply_binary_operator(&inner, op, left, right)
            },
            Expression::Unary { inner, op, right } => {
                let right = self.inner_const_resolve(constant_stack, right, usage, from_file_index)?;
                Self::apply_unary_operator(&inner, op, right)
            },
            Expression::Value(value) => {
                Ok(match value {
                    ExpressionValue::ConstantValue(parent, name) => {

                        let global_index = (name.clone(), None);
                        let local_index = (name.clone(), Some(from_file_index));

                        // Local Lookup
                        if let Some(result) = self.constants.get(&local_index) {
                            if self.linter_enabled {
                                usage.merge(&result.1);
                                usage.use_constant(local_index, from_file_index);
                            }
                            result.0.clone()

                        // Global Lookup
                        } else if let Some(result) = self.constants.get(&global_index) {
                            if self.linter_enabled {
                                usage.merge(&result.1);
                                usage.use_constant(global_index, from_file_index);
                            }
                            result.0.clone()

                        // Local Declaration
                        } else if let Some(EvaluatorConstant { ref inner, ref expression, .. }) = self.raw_constants.get(&local_index).cloned() {
                            self.declare_constant_inline(
                                constant_stack,
                                parent,
                                inner,
                                name,
                                expression,
                                usage,
                                from_file_index,
                                local_index
                            )?

                        // Global Declaration
                        } else if let Some(EvaluatorConstant { ref inner, ref expression, .. }) = self.raw_constants.get(&global_index).cloned() {
                            self.declare_constant_inline(
                                constant_stack,
                                parent,
                                inner,
                                name,
                                expression,
                                usage,
                                from_file_index,
                                global_index
                            )?

                        } else {
                            return Err(self.undeclared_const_error(parent, name));
                        }
                    },
                    ExpressionValue::Integer(i) => ExpressionResult::Integer(*i),
                    ExpressionValue::LintInteger(inner, i) => {
                        self.lint_integer(usage, inner, *i);
                        ExpressionResult::Integer(*i)
                    },
                    ExpressionValue::Float(f) => ExpressionResult::Float(*f),
                    ExpressionValue::String(s) => ExpressionResult::String(s.to_string()),
                    ExpressionValue::OffsetAddress(_, _) => {
                        unreachable!("Invalid constant expression containing OffsetAddress");
                    },
                    ExpressionValue::ParentLabelAddress(_, _) => {
                        unreachable!("Invalid constant expression containing ParentLabelAddress");
                    },
                    ExpressionValue::ChildLabelAddress(_, _) => {
                        unreachable!("Invalid constant expression containing ChildLabelAddress");
                    }
                })
            },
            Expression::BuiltinCall { inner, name, args } => {
                let mut arguments = Vec::with_capacity(args.len());
                for arg in args {
                    arguments.push(self.inner_const_resolve(
                        constant_stack,
                        arg,
                        usage,
                        from_file_index
                    )?);
                }
                Self::execute_builtin_call(&inner, &name, arguments)
            },
            Expression::ParentLabelCall { .. } => {
                unreachable!("Invalid constant expression containing ParentLabelCall");
            },
            _ => unreachable!()
        }
    }

    fn lint_integer(&self, usage: &mut UsageInformation, inner: &InnerToken, i: i32) {
        for (index, (result, _)) in &self.constants {
            if let ExpressionResult::Integer(v) = result {
                if i == *v && i > 255 {
                    usage.magic_numbers.push((
                        index.clone(),
                        inner.clone(),
                        *v
                    ));
                }
            }
        }
    }

    fn undeclared_const_error(&self, parent: &InnerToken, name: &Symbol) -> SourceError {
        let error = parent.error(format!("Reference to undeclared constant \"{}\".", name));
        for ((symbol, _), constant) in self.raw_constants.iter() {
            // TODO also check lehvenstein distance
            if symbol == name {
                return error.with_reference(&constant.inner, "A non-global constant with the same name is defined");
            }
        }
        for ((symbol, _), inner) in self.raw_labels.iter() {
            if symbol == name {
                return error.with_reference(inner, "A non-global label with the same name is defined");
            }
        }
        error
    }

    fn declare_constant_inline(
        &mut self,
        constant_stack: &[&Symbol],
        parent: &InnerToken,
        inner: &InnerToken,
        name: &Symbol,
        expression: &Expression,
        usage: &mut UsageInformation,
        from_file_index: usize,
        declare_index: (Symbol, Option<usize>)

    ) -> Result<ExpressionResult, SourceError> {
        if constant_stack.contains(&name) {
            Err(parent.error(
                format!("Recursive declaration of constant \"{}\".", name)

            ).with_reference(inner, "Initial declaration was"))

        } else {

            // Keep track of other constants used in the current constant's expression
            let mut constant_usage = UsageInformation::new();

            // Resolve constants that are not yet evaluated
            let mut child_stack = constant_stack.to_vec();
            child_stack.push(name);
            let value = self.inner_const_resolve(
                &child_stack,
                expression,
                &mut constant_usage,
                from_file_index
            )?;
            if self.linter_enabled {
                usage.merge(&constant_usage);
                usage.use_constant(declare_index.clone(), from_file_index);
            }
            self.constants.insert(declare_index, (value.clone(), constant_usage));
            Ok(value)
        }
    }

    fn execute_builtin_call(
        inner: &InnerToken,
        name: &Symbol,
        args: Vec<ExpressionResult>

    ) -> Result<ExpressionResult, SourceError> {
        let def = BUILTIN_MACRO_DEFS.get(*BUILTIN_MACRO_INDEX.get(name).unwrap()).unwrap();

        // Check argument types
        for (index, ((typ, arg_name), arg)) in def.parameters.iter().zip(args.iter()).enumerate() {
            if !arg.is_argument_type(typ) {
                return Err(inner.error(format!(
                    "Parameter #{} (\"{}\") of builtin macro \"{}\" must be of type {}, but is of type {}.",
                    index,
                    arg_name.value,
                    name,
                    typ.as_str(),
                    arg.as_str()
                )));
            }
        }

        fn double_mixed_number<C: Fn(i32, i32) -> i32, D: Fn(f32, f32) -> f32>(a: &ExpressionResult, b: &ExpressionResult, int: C, flt: D) -> ExpressionResult {
            match (a, b) {
                (ExpressionResult::Integer(a), ExpressionResult::Integer(b)) => {
                    ExpressionResult::Integer(int(*a, *b))
                },
                (ExpressionResult::Float(a), ExpressionResult::Float(b)) => {
                    ExpressionResult::Float(OrderedFloat(flt(a.into_inner(), b.into_inner())))
                },
                (ExpressionResult::Float(a), ExpressionResult::Integer(b)) => {
                    ExpressionResult::Float(OrderedFloat(flt(a.into_inner(), *b as f32)))
                },
                (ExpressionResult::Integer(a), ExpressionResult::Float(b)) => {
                    ExpressionResult::Float(OrderedFloat(flt(*a as f32, b.into_inner())))
                },
                _ => unreachable!()
            }
        }

        fn single_float<C: Fn(f32) -> f32>(a: &ExpressionResult, flt: C) -> ExpressionResult {
            match a {
                ExpressionResult::Integer(i) => ExpressionResult::Float(OrderedFloat(flt(*i as f32))),
                ExpressionResult::Float(f) => ExpressionResult::Float(OrderedFloat(flt(f.into_inner()))),
                _ => unreachable!("")
            }
        }

        // Evaluate
        Ok(match name {
            Symbol::DBG => ExpressionResult::Integer(0),
            Symbol::MAX => double_mixed_number(&args[0], &args[1], cmp::max, |a, b| a.max(b)),
            Symbol::MIN => double_mixed_number(&args[0], &args[1], cmp::min, |a, b| a.min(b)),
            Symbol::FLOOR => match args[0] {
                ExpressionResult::Integer(i) => ExpressionResult::Integer(i),
                ExpressionResult::Float(f) => ExpressionResult::Integer(f.into_inner().floor() as i32),
                _ => unreachable!()
            },
            Symbol::CEIL => match args[0] {
                ExpressionResult::Integer(i) => ExpressionResult::Integer(i),
                ExpressionResult::Float(f) => ExpressionResult::Integer(f.into_inner().ceil() as i32),
                _ => unreachable!()
            },
            Symbol::ROUND => match args[0] {
                ExpressionResult::Integer(i) => ExpressionResult::Integer(i),
                ExpressionResult::Float(f) => ExpressionResult::Integer(f.into_inner().round() as i32),
                _ => unreachable!()
            },

            Symbol::LOG => single_float(&args[0], |a| a.log(::std::f32::consts::LOG2_E)),
            Symbol::EXP => single_float(&args[0], |a| a.exp()),
            Symbol::SQRT => single_float(&args[0], |a| a.sqrt()),
            Symbol::ABS => match args[0] {
                ExpressionResult::Integer(i) => ExpressionResult::Integer(i.abs()),
                ExpressionResult::Float(f) => ExpressionResult::Float(OrderedFloat(f.abs())),
                _ => unreachable!()
            },

            // Math
            Symbol::SIN => single_float(&args[0], |a| a.sin()),
            Symbol::COS => single_float(&args[0], |a| a.cos()),
            Symbol::TAN => single_float(&args[0], |a| a.tan()),
            Symbol::ASIN => single_float(&args[0], |a| a.asin()),
            Symbol::ACOS => single_float(&args[0], |a| a.acos()),
            Symbol::ATAN => single_float(&args[0], |a| a.atan()),
            Symbol::ATAN2 => match (&args[0], &args[1]) {
                (ExpressionResult::Integer(a), ExpressionResult::Integer(b)) => {
                    ExpressionResult::Float(OrderedFloat((*a as f32).atan2(*b as f32)))
                },
                (ExpressionResult::Float(a), ExpressionResult::Float(b)) => {
                    ExpressionResult::Float(OrderedFloat(a.into_inner().atan2(b.into_inner())))
                },
                (ExpressionResult::Float(a), ExpressionResult::Integer(b)) => {
                    ExpressionResult::Float(OrderedFloat(a.into_inner().atan2(*b as f32)))
                },
                (ExpressionResult::Integer(a), ExpressionResult::Float(b)) => {
                    ExpressionResult::Float(OrderedFloat((*a as f32).atan2(b.into_inner())))
                },
                _ => unreachable!()
            },

            // String
            Symbol::STRUPR => match &args[0] {
                ExpressionResult::String(s) => ExpressionResult::String(s.to_ascii_uppercase()),
                _ => unreachable!()
            },
            Symbol::STRLWR => match &args[0] {
                ExpressionResult::String(s) => ExpressionResult::String(s.to_ascii_lowercase()),
                _ => unreachable!()
            },
            Symbol::STRLEN => match &args[0] {
                ExpressionResult::String(s) => ExpressionResult::Integer(s.len() as i32),
                _ => unreachable!()
            },
            Symbol::STRSUB => match (&args[0], &args[1], &args[2]) {
                (ExpressionResult::String(s), ExpressionResult::Integer(index), ExpressionResult::Integer(len)) => {
                    if *index < 0  {
                        return Err(inner.error("Parameter #1 (\"index\") of builtin macro \"STRSUB\" must be positive.".to_string()));

                    } else if *len < 0 {
                        return Err(inner.error("Parameter #2 (\"length\") of builtin macro \"STRSUB\" must be positive.".to_string()));

                    } else {
                        let from = cmp::max(*index, 0) as usize;
                        let to = cmp::min(from + *len as usize, s.len());
                        ExpressionResult::String(s[from..to].to_string())
                    }
                },
                _ => unreachable!()
            },
            Symbol::STRIN => match (&args[0], &args[1]) {
                (ExpressionResult::String(s), ExpressionResult::String(i)) => {
                    ExpressionResult::Integer(b2i(s.contains(i)))
                },
                _ => unreachable!()
            },
            Symbol::STRPADR => match (&args[0], &args[1], &args[2]) {
                (ExpressionResult::String(s), ExpressionResult::String(padding), ExpressionResult::Integer(len)) => {
                    if padding.len() != 1  {
                        return Err(inner.error("Parameter #1 (\"padding\") of builtin macro \"STRPADR\" must of length 1.".to_string()));

                    } else if *len < 0 {
                        return Err(inner.error("Parameter #2 (\"length\") of builtin macro \"STRPADR\" must be positive.".to_string()));

                    } else {
                        let extension = cmp::max(cmp::max(*len, 0) as usize, s.len()) - s.len();
                        ExpressionResult::String(format!("{}{}", s, padding.repeat(extension)))
                    }
                },
                _ => unreachable!()
            },
            Symbol::STRPADL => match (&args[0], &args[1], &args[2]) {
                (ExpressionResult::String(s), ExpressionResult::String(padding), ExpressionResult::Integer(len)) => {
                    if padding.len() != 1  {
                        return Err(inner.error("Parameter #1 (\"padding\") of builtin macro \"STRPADL\" must of length 1.".to_string()));

                    } else if *len < 0 {
                        return Err(inner.error("Parameter #2 (\"length\") of builtin macro \"STRPADL\" must be positive.".to_string()));

                    } else {
                        let extension = cmp::max(cmp::max(*len, 0) as usize, s.len()) - s.len();
                        ExpressionResult::String(format!("{}{}", padding.repeat(extension), s))
                    }
                },
                _ => unreachable!()
            },
            _ => unimplemented!("Unimplemented macro call: {}", name)

        })
    }

    fn apply_binary_operator(
        inner: &InnerToken,
        op: &Operator,
        left: ExpressionResult,
        right: ExpressionResult

    ) -> Result<ExpressionResult, SourceError> {
        match (op, &left, &right) {
            // Integer
            (Operator::ShiftRight, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(l >> r))
            },
            (Operator::ShiftLeft, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(l << r))
            },
            (Operator::LogicalAnd, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(i2b(*l) && i2b(*r))))
            },
            (Operator::LogicalOr, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(i2b(*l) || i2b(*r))))
            },
            (Operator::Equals, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(l == r)))
            },
            (Operator::Unequals, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(l != r)))
            },
            (Operator::GreaterThan, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(l > r)))
            },
            (Operator::LessThan, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(l < r)))
            },
            (Operator::Pow, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                if *r > 0  {
                    Ok(ExpressionResult::Integer(l.pow(*r as u32)))

                } else {
                    Err(inner.error(format!("Right-hand side of \"{}\" must be positive and non-zero.", op.as_str())))
                }
            },
            (Operator::DivInt, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(l / r))
            },
            (Operator::LessThanEqual, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(l <= r)))
            },
            (Operator::GreaterThanEqual, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(l >= r)))
            },
            (Operator::Plus, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(l + r))
            },
            (Operator::Minus, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(l - r))
            },
            (Operator::Div, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(l / r))
            },
            (Operator::Mul, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(l * r))
            },
            (Operator::Modulo, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(l % r))
            },
            (Operator::BitAnd, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(l & r))
            },
            (Operator::BitOr, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(l | r))
            },
            (Operator::BitXor, ExpressionResult::Integer(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(l ^ r))
            },

            // Float
            (Operator::Equals, ExpressionResult::Float(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer(b2i(l == r)))
            },
            (Operator::Unequals, ExpressionResult::Float(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer(b2i(l != r)))
            },
            (Operator::GreaterThan, ExpressionResult::Float(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer(b2i(l > r)))
            },
            (Operator::LessThan, ExpressionResult::Float(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer(b2i(l < r)))
            },
            (Operator::Pow, ExpressionResult::Float(l), ExpressionResult::Float(r)) => {
                let r = r.into_inner();
                if r > 0.0  {
                    Ok(ExpressionResult::Float(OrderedFloat(l.powf(r))))

                } else {
                    Err(inner.error(format!("Right-hand side of \"{}\" must be positive and non-zero.", op.as_str())))
                }
            },
            (Operator::DivInt, ExpressionResult::Float(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer((l.into_inner() / r.into_inner()).floor() as i32))
            },
            (Operator::LessThanEqual, ExpressionResult::Float(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer(b2i(l <= r)))
            },
            (Operator::GreaterThanEqual, ExpressionResult::Float(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer(b2i(l >= r)))
            },
            (Operator::Plus, ExpressionResult::Float(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat(l.into_inner() + r.into_inner())))
            },
            (Operator::Minus, ExpressionResult::Float(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat(l.into_inner() - r.into_inner())))
            },
            (Operator::Div, ExpressionResult::Float(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat(l.into_inner() / r.into_inner())))
            },
            (Operator::Mul, ExpressionResult::Float(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat(l.into_inner() * r.into_inner())))
            },

            // Float / Integer
            (Operator::Equals, ExpressionResult::Float(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(l.into_inner() == *r as f32)))
            },
            (Operator::Unequals, ExpressionResult::Float(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(l.into_inner() != *r as f32)))
            },
            (Operator::GreaterThan, ExpressionResult::Float(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(l.into_inner() > *r as f32)))
            },
            (Operator::LessThan, ExpressionResult::Float(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(l.into_inner() < *r as f32)))
            },
            (Operator::GreaterThanEqual, ExpressionResult::Float(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(l.into_inner() >= *r as f32)))
            },
            (Operator::LessThanEqual, ExpressionResult::Float(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Integer(b2i(l.into_inner() <= *r as f32)))
            },
            (Operator::DivInt, ExpressionResult::Float(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat((l.into_inner() / *r as f32).floor())))
            },
            (Operator::Plus, ExpressionResult::Float(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat(l.into_inner() + *r as f32)))
            },
            (Operator::Minus, ExpressionResult::Float(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat(l.into_inner() - *r as f32)))
            },
            (Operator::Mul, ExpressionResult::Float(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat(l.into_inner() * *r as f32)))
            },
            (Operator::Div, ExpressionResult::Float(l), ExpressionResult::Integer(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat(l.into_inner() / *r as f32)))
            },

            // Integer / Float
            (Operator::Equals, ExpressionResult::Integer(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer(b2i(*l as f32 == r.into_inner())))
            },
            (Operator::Unequals, ExpressionResult::Integer(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer(b2i(*l as f32 != r.into_inner())))
            },
            (Operator::GreaterThan, ExpressionResult::Integer(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer(b2i(*l as f32 > r.into_inner())))
            },
            (Operator::LessThan, ExpressionResult::Integer(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer(b2i((*l as f32) < r.into_inner())))
            },
            (Operator::GreaterThanEqual, ExpressionResult::Integer(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer(b2i(*l as f32 >= r.into_inner())))
            },
            (Operator::LessThanEqual, ExpressionResult::Integer(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Integer(b2i((*l as f32) <= r.into_inner())))
            },
            (Operator::DivInt, ExpressionResult::Integer(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat((*l as f32 / r.into_inner()).floor())))
            },
            (Operator::Plus, ExpressionResult::Integer(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat(*l as f32 + r.into_inner())))
            },
            (Operator::Minus, ExpressionResult::Integer(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat(*l as f32 - r.into_inner())))
            },
            (Operator::Mul, ExpressionResult::Integer(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat(*l as f32 * r.into_inner())))
            },
            (Operator::Div, ExpressionResult::Integer(l), ExpressionResult::Float(r)) => {
                Ok(ExpressionResult::Float(OrderedFloat(*l as f32 / r.into_inner())))
            },

            // String
            (Operator::Equals, ExpressionResult::String(l), ExpressionResult::String(r)) => {
                Ok(ExpressionResult::Integer(b2i(l == r)))
            },
            (Operator::Unequals, ExpressionResult::String(l), ExpressionResult::String(r)) => {
                Ok(ExpressionResult::Integer(b2i(l != r)))
            },
            (Operator::Plus, ExpressionResult::String(l), ExpressionResult::String(r)) => {
                Ok(ExpressionResult::String(format!("{}{}", l, r)))
            },

            // String / Integer
            (Operator::Mul, ExpressionResult::String(l), ExpressionResult::Integer(r)) => {
                if *r >= 0  {
                    Ok(ExpressionResult::String(l.repeat(*r as usize)))

                } else {
                    Err(inner.error(format!("Right-hand side of \"{}\" must be positive.", op.as_str())))
                }
            },

            _ => {
                Err(inner.error(format!("Unsupported binary operation: {} {} {}", left.as_str(), op.as_str(), right.as_str())))
            }
        }
    }

    fn apply_unary_operator(
        inner: &InnerToken,
        op: &Operator,
        right: ExpressionResult

    ) -> Result<ExpressionResult, SourceError> {
        match (&op, &right) {
            // Integer
            (Operator::Plus, ExpressionResult::Integer(i)) => {
                Ok(ExpressionResult::Integer(i.abs()))
            },
            (Operator::Minus, ExpressionResult::Integer(i)) => {
                Ok(ExpressionResult::Integer(-i))
            },
            (Operator::BitNegate, ExpressionResult::Integer(i)) => {
                Ok(ExpressionResult::Integer(!i))
            },
            (Operator::LogicalNot, ExpressionResult::Integer(i)) => {
                Ok(ExpressionResult::Integer(if *i == 0 {
                    1

                } else {
                    0
                }))
            },
            // Float
            (Operator::Plus, ExpressionResult::Float(f)) => {
                Ok(ExpressionResult::Float(OrderedFloat(f.abs())))
            },
            (Operator::Minus, ExpressionResult::Float(f)) => {
                Ok(ExpressionResult::Float(OrderedFloat(-f.into_inner())))
            },
            (Operator::LogicalNot, ExpressionResult::Float(f)) => {
                Ok(ExpressionResult::Integer(if *f == OrderedFloat(0.0) {
                    1

                } else {
                    0
                }))
            },
            // String
            (Operator::LogicalNot, ExpressionResult::String(s)) => {
                Ok(ExpressionResult::Integer(if s.is_empty() {
                    1

                } else {
                    0
                }))
            },
            _ => {
                Err(inner.error(format!("Unsupported unary operation: {} {}", op.as_str(), right.as_str())))
            }
        }
    }

}

fn b2i(m: bool) -> i32 {
    if m {
        1
    } else {
        0
    }
}

fn i2b(i: i32) -> bool {
    i != 0
}

// Tests ----------------------------------------------------------------------
#[cfg(test)]
mod test {
    use ordered_float::OrderedFloat;
    use crate::mocks::expr_lex;
    use crate::error::SourceError;
    use crate::lexer::ExpressionToken;
    use super::{EvaluatorContext, ExpressionResult, UsageInformation};

    fn const_expression<S: Into<String>>(s: S) -> ExpressionResult {
        const_expression_result(s).expect("Evaluator failed")
    }

    fn const_expression_error<S: Into<String>>(s: S) -> String {
        const_expression_result(s).err().unwrap().to_string()
    }

    fn const_expression_result<S: Into<String>>(s: S) -> Result<ExpressionResult, SourceError> {
        let token = &expr_lex(s).tokens[0];
        if let ExpressionToken::ConstExpression(_, ref expr) = token {
            let mut context = EvaluatorContext::new(false);
            let mut usage = UsageInformation::new();
            let stack = Vec::new();
            context.inner_const_resolve(&stack, expr, &mut usage, 0)

        } else {
            panic!("Not a constant expression");
        }
    }

    // Tests Expressions ------------------------------------------------------
    #[test]
    fn test_plain_expressions() {
        assert_eq!(const_expression("1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1.1"), ExpressionResult::Float(OrderedFloat(1.1)));
        assert_eq!(const_expression("'Hello World'"), ExpressionResult::String("Hello World".to_string()));
    }

    // Tests Unary Expressions ------------------------------------------------
    #[test]
    fn test_unary_integer_expressions() {
        assert_eq!(const_expression("+1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("- 1"), ExpressionResult::Integer(-1));
        assert_eq!(const_expression("~1"), ExpressionResult::Integer(-2));
        assert_eq!(const_expression("!1"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("!0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("!!0"), ExpressionResult::Integer(0));
    }

    #[test]
    fn test_unary_float_expressions() {
        assert_eq!(const_expression("+1.1"), ExpressionResult::Float(OrderedFloat(1.1)));
        assert_eq!(const_expression("- 1.1"), ExpressionResult::Float(OrderedFloat(-1.1)));
        assert_eq!(const_expression_error("~1.1"), "Unsupported unary operation: ~ Float");
        assert_eq!(const_expression("!0.1"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("!0.0"), ExpressionResult::Integer(1));
    }

    #[test]
    fn test_unary_string_expressions() {
        assert_eq!(const_expression_error("+'Foo'"), "Unsupported unary operation: + String");
        assert_eq!(const_expression_error("-'Foo'"), "Unsupported unary operation: - String");
        assert_eq!(const_expression_error("~'Foo'"), "Unsupported unary operation: ~ String");
        assert_eq!(const_expression("!''"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("!'Foo'"), ExpressionResult::Integer(0));
    }

    // Tests Binary Expressions -----------------------------------------------
    #[test]
    fn test_binary_integer_expressions() {
        assert_eq!(const_expression("2 >> 1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1 << 1"), ExpressionResult::Integer(2));

        assert_eq!(const_expression("1 && 1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1 && 0"), ExpressionResult::Integer(0));

        assert_eq!(const_expression("1 || 0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("0 || 0"), ExpressionResult::Integer(0));

        assert_eq!(const_expression("1 == 1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1 == 0"), ExpressionResult::Integer(0));

        assert_eq!(const_expression("1 != 1"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("1 != 0"), ExpressionResult::Integer(1));

        assert_eq!(const_expression("2 > 1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("2 > 3"), ExpressionResult::Integer(0));

        assert_eq!(const_expression("2 < 1"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("2 < 3"), ExpressionResult::Integer(1));

        assert_eq!(const_expression("2 ** 2"), ExpressionResult::Integer(4));
        assert_eq!(const_expression_error("2 ** 0"), "Right-hand side of \"**\" must be positive and non-zero.");
        assert_eq!(const_expression_error("2 ** -1"), "Right-hand side of \"**\" must be positive and non-zero.");
        assert_eq!(const_expression("2 // 2"), ExpressionResult::Integer(1));

        assert_eq!(const_expression("1 <= 1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1 <= 2"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1 <= 0"), ExpressionResult::Integer(0));

        assert_eq!(const_expression("1 >= 1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1 >= 2"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("1 >= 0"), ExpressionResult::Integer(1));

        assert_eq!(const_expression("1 + 1"), ExpressionResult::Integer(2));
        assert_eq!(const_expression("1 - 1"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("1 * 2"), ExpressionResult::Integer(2));
        assert_eq!(const_expression("2 / 2"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("3 % 2"), ExpressionResult::Integer(1));

        assert_eq!(const_expression("3 & 2"), ExpressionResult::Integer(2));
        assert_eq!(const_expression("1 | 2"), ExpressionResult::Integer(3));
        assert_eq!(const_expression("1 ^ 1"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("0 ^ 1"), ExpressionResult::Integer(1));
    }

    #[test]
    fn test_binary_float_expressions() {
        assert_eq!(const_expression("1.0 == 1.0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1.0 == 0.0"), ExpressionResult::Integer(0));

        assert_eq!(const_expression("1.0 != 1.0"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("1.0 != 0.0"), ExpressionResult::Integer(1));

        assert_eq!(const_expression("2.0 > 1.0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("2.0 > 3.0"), ExpressionResult::Integer(0));

        assert_eq!(const_expression("2.0 < 1.0"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("2.0 < 3.0"), ExpressionResult::Integer(1));

        assert_eq!(const_expression("2.0 ** 2.0"), ExpressionResult::Float(OrderedFloat(4.0)));
        assert_eq!(const_expression_error("2.0 ** 0.0"), "Right-hand side of \"**\" must be positive and non-zero.");
        assert_eq!(const_expression_error("2.0 ** -1.0"), "Right-hand side of \"**\" must be positive and non-zero.");
        assert_eq!(const_expression("2.0 // 2.0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("3.0 // 2.0"), ExpressionResult::Integer(1));

        assert_eq!(const_expression("1.0 <= 1.0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1.0 <= 2.0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1.0 <= 0.0"), ExpressionResult::Integer(0));

        assert_eq!(const_expression("1.0 >= 1.0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1.0 >= 2.0"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("1.0 >= 0.0"), ExpressionResult::Integer(1));

        assert_eq!(const_expression("1.0 + 1.0"), ExpressionResult::Float(OrderedFloat(2.0)));
        assert_eq!(const_expression("1.0 - 1.0"), ExpressionResult::Float(OrderedFloat(0.0)));
        assert_eq!(const_expression("1.0 * 2.0"), ExpressionResult::Float(OrderedFloat(2.0)));
        assert_eq!(const_expression("3.0 / 2.0"), ExpressionResult::Float(OrderedFloat(1.5)));
    }

    #[test]
    fn test_binary_string_expressions() {
        assert_eq!(const_expression("'A' == 'A'"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("'A' == 'B'"), ExpressionResult::Integer(0));

        assert_eq!(const_expression("'A' != 'A'"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("'A' != 'B'"), ExpressionResult::Integer(1));

        assert_eq!(const_expression("'A' + 'B'"), ExpressionResult::String("AB".to_string()));
    }

    #[test]
    fn test_binary_integer_float_expressions() {
        assert_eq!(const_expression("1.0 == 1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1.0 == 0"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("1 == 1.0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1 == 0.0"), ExpressionResult::Integer(0));

        assert_eq!(const_expression("1.0 != 1"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("1.0 != 0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("1 != 1.0"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("1 != 0.0"), ExpressionResult::Integer(1));

        assert_eq!(const_expression("2.0 > 1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("2.0 > 3"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("2 > 1.0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("2 > 3.0"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("2.0 >= 1"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("2.0 >= 3"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("2 >= 1.0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("2 >= 3.0"), ExpressionResult::Integer(0));

        assert_eq!(const_expression("2.0 < 1"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("2.0 < 3"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("2 < 1.0"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("2 < 3.0"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("2.0 <= 1"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("2.0 <= 3"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("2 <= 1.0"), ExpressionResult::Integer(0));
        assert_eq!(const_expression("2 <= 3.0"), ExpressionResult::Integer(1));

        assert_eq!(const_expression("3.0 // 2"), ExpressionResult::Float(OrderedFloat(1.0)));
        assert_eq!(const_expression("3 // 2.0"), ExpressionResult::Float(OrderedFloat(1.0)));

        assert_eq!(const_expression("1.0 + 1"), ExpressionResult::Float(OrderedFloat(2.0)));
        assert_eq!(const_expression("1.0 - 1"), ExpressionResult::Float(OrderedFloat(0.0)));
        assert_eq!(const_expression("1.0 * 2"), ExpressionResult::Float(OrderedFloat(2.0)));
        assert_eq!(const_expression("3.0 / 2"), ExpressionResult::Float(OrderedFloat(1.5)));

        assert_eq!(const_expression("1 + 1.0"), ExpressionResult::Float(OrderedFloat(2.0)));
        assert_eq!(const_expression("1 - 1.0"), ExpressionResult::Float(OrderedFloat(0.0)));
        assert_eq!(const_expression("1 * 2.0"), ExpressionResult::Float(OrderedFloat(2.0)));
        assert_eq!(const_expression("3 / 2.0"), ExpressionResult::Float(OrderedFloat(1.5)));

    }

    #[test]
    fn test_binary_string_integer_expressions() {
        assert_eq!(const_expression("'AB' * 3"), ExpressionResult::String("ABABAB".to_string()));
        assert_eq!(const_expression("'AB' * 0"), ExpressionResult::String("".to_string()));
        assert_eq!(const_expression_error("'AB' * -1"), "Right-hand side of \"*\" must be positive.");
    }

    // Tests Builtin Call Expressions -----------------------------------------
    #[test]
    fn test_error_builtin_macro_calls() {
        assert_eq!(const_expression_error("FLOOR('Foo')"), "Parameter #0 (\"value\") of builtin macro \"FLOOR\" must be of type Number, but is of type String.");
        assert_eq!(const_expression_error("MAX(1, 'Foo')"), "Parameter #1 (\"b\") of builtin macro \"MAX\" must be of type Number, but is of type String.");
    }

    #[test]
    fn test_builtin_macro_calls() {
        assert_eq!(const_expression("DBG()"), ExpressionResult::Integer(0));

        assert_eq!(const_expression("MIN(1, 2)"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("MAX(1, 2)"), ExpressionResult::Integer(2));

        assert_eq!(const_expression("MIN(1.0, 2.0)"), ExpressionResult::Float(OrderedFloat(1.0)));
        assert_eq!(const_expression("MAX(1.0, 2.0)"), ExpressionResult::Float(OrderedFloat(2.0)));

        assert_eq!(const_expression("MIN(1, 2.0)"), ExpressionResult::Float(OrderedFloat(1.0)));
        assert_eq!(const_expression("MAX(1, 2.0)"), ExpressionResult::Float(OrderedFloat(2.0)));

        assert_eq!(const_expression("MIN(1.0, 2)"), ExpressionResult::Float(OrderedFloat(1.0)));
        assert_eq!(const_expression("MAX(1.0, 2)"), ExpressionResult::Float(OrderedFloat(2.0)));

        assert_eq!(const_expression("FLOOR(1)"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("FLOOR(1.6)"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("CEIL(1)"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("CEIL(1.4)"), ExpressionResult::Integer(2));

        assert_eq!(const_expression("ROUND(1)"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("ROUND(1.4)"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("ROUND(1.5)"), ExpressionResult::Integer(2));
        assert_eq!(const_expression("ROUND(1.6)"), ExpressionResult::Integer(2));

        assert_eq!(const_expression("STRUPR('hello')"), ExpressionResult::String("HELLO".to_string()));
        assert_eq!(const_expression("STRLWR('HELLO')"), ExpressionResult::String("hello".to_string()));
        assert_eq!(const_expression("STRLEN('HELLO')"), ExpressionResult::Integer(5));
        assert_eq!(const_expression("STRSUB('ABCDE', 0, 2)"), ExpressionResult::String("AB".to_string()));
        assert_eq!(const_expression("STRSUB('A', 0, 2)"), ExpressionResult::String("A".to_string()));
        assert_eq!(const_expression("STRSUB('A', 0, 0)"), ExpressionResult::String("".to_string()));
        assert_eq!(const_expression("STRSUB('A', 1, 1)"), ExpressionResult::String("".to_string()));
        assert_eq!(const_expression_error("STRSUB('A', -1, 1)"), "Parameter #1 (\"index\") of builtin macro \"STRSUB\" must be positive.");
        assert_eq!(const_expression_error("STRSUB('A', 1, -1)"), "Parameter #2 (\"length\") of builtin macro \"STRSUB\" must be positive.");
        assert_eq!(const_expression("STRIN('ABCDE', 'BCD')"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("STRIN('ABCDE', 'ACD')"), ExpressionResult::Integer(0));
        assert_eq!(const_expression_error("STRPADR('A', '', 4)"), "Parameter #1 (\"padding\") of builtin macro \"STRPADR\" must of length 1.");
        assert_eq!(const_expression_error("STRPADR('A', 'AB', 4)"), "Parameter #1 (\"padding\") of builtin macro \"STRPADR\" must of length 1.");
        assert_eq!(const_expression_error("STRPADR('A', 'A', -1)"), "Parameter #2 (\"length\") of builtin macro \"STRPADR\" must be positive.");
        assert_eq!(const_expression("STRPADR('A', 'B', 4)"), ExpressionResult::String("ABBB".to_string()));
        assert_eq!(const_expression_error("STRPADL('A', '', 4)"), "Parameter #1 (\"padding\") of builtin macro \"STRPADL\" must of length 1.");
        assert_eq!(const_expression_error("STRPADL('A', 'AB', 4)"), "Parameter #1 (\"padding\") of builtin macro \"STRPADL\" must of length 1.");
        assert_eq!(const_expression_error("STRPADL('A', 'A', -1)"), "Parameter #2 (\"length\") of builtin macro \"STRPADL\" must be positive.");
        assert_eq!(const_expression("STRPADL('A', 'B', 4)"), ExpressionResult::String("BBBA".to_string()));

        assert_eq!(const_expression("LOG(2)"), ExpressionResult::Float(OrderedFloat(1.8911946)));
        assert_eq!(const_expression("LOG(2.5)"), ExpressionResult::Float(OrderedFloat(2.5000234)));
        assert_eq!(const_expression("EXP(1)"), ExpressionResult::Float(OrderedFloat(2.7182817)));
        assert_eq!(const_expression("EXP(1.5)"), ExpressionResult::Float(OrderedFloat(4.481689)));
        assert_eq!(const_expression("SQRT(9)"), ExpressionResult::Float(OrderedFloat(3.0)));
        assert_eq!(const_expression("SQRT(9.0)"), ExpressionResult::Float(OrderedFloat(3.0)));
        assert_eq!(const_expression("ABS(1)"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("ABS(1.5)"), ExpressionResult::Float(OrderedFloat(1.5)));
        assert_eq!(const_expression("ABS(-1)"), ExpressionResult::Integer(1));
        assert_eq!(const_expression("ABS(-1.5)"), ExpressionResult::Float(OrderedFloat(1.5)));

        assert_eq!(const_expression("SIN(1)"), ExpressionResult::Float(OrderedFloat(0.84147096)));
        assert_eq!(const_expression("SIN(2.5)"), ExpressionResult::Float(OrderedFloat(0.5984721)));

        assert_eq!(const_expression("COS(1)"), ExpressionResult::Float(OrderedFloat(0.5403023)));
        assert_eq!(const_expression("COS(2.5)"), ExpressionResult::Float(OrderedFloat(-0.8011436)));

        assert_eq!(const_expression("TAN(1)"), ExpressionResult::Float(OrderedFloat(1.5574077)));
        assert_eq!(const_expression("TAN(2.5)"), ExpressionResult::Float(OrderedFloat(-0.74702233)));

        assert_eq!(const_expression("ASIN(1)"), ExpressionResult::Float(OrderedFloat(1.5707964)));
        assert_eq!(const_expression("ASIN(0.5)"), ExpressionResult::Float(OrderedFloat(0.5235988)));

        assert_eq!(const_expression("ACOS(1)"), ExpressionResult::Float(OrderedFloat(0.0)));
        assert_eq!(const_expression("ACOS(0.5)"), ExpressionResult::Float(OrderedFloat(1.0471976)));

        assert_eq!(const_expression("ATAN(1)"), ExpressionResult::Float(OrderedFloat(0.7853982)));
        assert_eq!(const_expression("ATAN(0.5)"), ExpressionResult::Float(OrderedFloat(0.4636476)));

        assert_eq!(const_expression("ATAN2(1, 2)"), ExpressionResult::Float(OrderedFloat(0.4636476)));
        assert_eq!(const_expression("ATAN2(2, 1)"), ExpressionResult::Float(OrderedFloat(1.1071488)));
        assert_eq!(const_expression("ATAN2(2.5, 1)"), ExpressionResult::Float(OrderedFloat(1.19029)));
        assert_eq!(const_expression("ATAN2(2.5, 1.5)"), ExpressionResult::Float(OrderedFloat(1.0303768)));

    }

}
