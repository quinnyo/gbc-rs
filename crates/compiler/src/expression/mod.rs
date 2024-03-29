// STD Dependencies -----------------------------------------------------------
use std::fmt;
use std::collections::HashSet;


// External Dependencies ------------------------------------------------------
use gb_cpu::Register;
use ordered_float::OrderedFloat;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::{InnerToken, Symbol};


// Modules --------------------------------------------------------------------
pub mod data;
pub mod evaluator;


// Types ----------------------------------------------------------------------
pub type DataExpression = Expression;
pub type OptionalDataExpression = Option<DataExpression>;


// Expression Abstraction -----------------------------------------------------
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Binary {
        op: Operator,
        inner: InnerToken,
        left: Box<Expression>,
        right: Box<Expression>
    },
    Unary {
        op: Operator,
        inner: InnerToken,
        right: Box<Expression>
    },
    Value(ExpressionValue),
    BuiltinCall {
        inner: InnerToken,
        name: Symbol,
        args: Vec<Expression>
    },
    ParentLabelCall {
        inner: InnerToken,
        id: usize,
        name: Symbol,
        args: Vec<Expression>
    },
    RegisterArgument {
        inner: InnerToken,
        reg: Register
    },
    MemoryArgument {
        inner: InnerToken,
        value: Box<Expression>
    }
}

impl Expression {
    pub fn is_constant(&self) -> bool {
        match self {
            Expression::Binary { left, right, .. } => right.is_constant() && left.is_constant(),
            Expression::Unary { right, .. } => right.is_constant(),
            Expression::Value(value) => value.is_constant(),
            Expression::BuiltinCall { args, .. } => args.iter().all(Expression::is_constant),
            Expression::ParentLabelCall { .. } => false,
            Expression::RegisterArgument { .. } => true,
            Expression::MemoryArgument { value, .. } => value.is_constant()
        }
    }

    pub fn label_references(&self, references: &mut HashSet<String>) {
        match self {
            Expression::Binary { left, right, .. } => {
                left.label_references(references);
                right.label_references(references);
            },
            Expression::Unary { right, .. } => {
                right.label_references(references);
            },
            Expression::Value(value) => {
                value.label_references(references);
            },
            Expression::BuiltinCall { args, .. } => {
                for a in args {
                    a.label_references(references);
                }
            },
            Expression::ParentLabelCall { args, .. } => {
                for a in args {
                    a.label_references(references);
                }
            },
            Expression::RegisterArgument { .. } => {},
            Expression::MemoryArgument { value, .. } => {
                value.label_references(references);
            }
        }
    }

    pub fn is_register(&self) -> bool {
        matches!(self, Expression::RegisterArgument { .. })
    }

    pub fn is_call(&self) -> bool {
        matches!(self, Expression::ParentLabelCall { .. })
    }

    pub fn replace_constant(&mut self, name: &Symbol, value: &ExpressionValue) {
        match self {
            Expression::Binary { left, right, .. } => {
                left.replace_constant(name, value);
                right.replace_constant(name, value);
            }
            Expression::Unary { right, .. } => {
                right.replace_constant(name, value);
            },
            Expression::Value(ExpressionValue::ConstantValue(_, constant_name)) => {
                if constant_name == name {
                    *self = Expression::Value(value.clone());
                }
            },
            Expression::BuiltinCall { args, .. } => {
                for arg in args {
                    arg.replace_constant(name, value);
                }
            },
            _ => {}
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExpressionValue {
    ConstantValue(InnerToken, Symbol),
    Integer(i32),
    Float(OrderedFloat<f32>),
    String(String),
    OffsetAddress(InnerToken, i32),
    ParentLabelAddress(InnerToken, usize),
    ChildLabelAddress(InnerToken, usize)
}

impl ExpressionValue {
    fn is_constant(&self) -> bool {
        match self {
            ExpressionValue::ConstantValue(_, _) | ExpressionValue::Integer(_) | ExpressionValue::Float(_) | ExpressionValue::String(_) => true,
            ExpressionValue::OffsetAddress(_, _) | ExpressionValue::ParentLabelAddress(_, _) | ExpressionValue::ChildLabelAddress(_, _) => false
        }
    }

    fn label_references(&self, references: &mut HashSet<String>) {
        if let Self::ParentLabelAddress(inner, _) = self {
            references.insert(inner.value.to_string());
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExpressionResult {
    Integer(i32),
    Float(OrderedFloat<f32>),
    String(String)
}

impl fmt::Display for ExpressionResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExpressionResult::Integer(i) => if *i <= 255 {
                write!(f, "{}", i)

            } else {
                write!(f, "${:0>4X}", i)
            },
            ExpressionResult::Float(v) => write!(f, "{:.2}", v),
            ExpressionResult::String(s) => write!(f, "{:?}", s)
        }
    }
}

impl ExpressionResult {

    pub fn is_truthy(&self) -> bool {
        match self {
            ExpressionResult::Integer(i) => *i != 0,
            ExpressionResult::Float(f) => f.into_inner() != 0.0,
            ExpressionResult::String(s) => !s.is_empty()
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            ExpressionResult::Integer(_) => "Integer",
            ExpressionResult::Float(_) => "Float",
            ExpressionResult::String(_) => "String"
        }
    }

    pub fn is_argument_type(&self, typ: &ExpressionArgumenType) -> bool {
        match self {
            ExpressionResult::Integer(_) => match typ {
                ExpressionArgumenType::Any => true,
                ExpressionArgumenType::Tokens => false,
                ExpressionArgumenType::String => false,
                ExpressionArgumenType::Number => true,
                ExpressionArgumenType::Integer => true
            },
            ExpressionResult::Float(_) => match typ {
                ExpressionArgumenType::Any => true,
                ExpressionArgumenType::Tokens => false,
                ExpressionArgumenType::String => false,
                ExpressionArgumenType::Number => true,
                ExpressionArgumenType::Integer => false
            },
            ExpressionResult::String(_) => match typ {
                ExpressionArgumenType::Any => true,
                ExpressionArgumenType::Tokens => false,
                ExpressionArgumenType::String => true,
                ExpressionArgumenType::Number => false,
                ExpressionArgumenType::Integer => false
            }
        }

    }

}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ExpressionArgumenType {
    Any,
    Tokens,
    String,
    Number,
    Integer
}

impl ExpressionArgumenType {
    pub fn as_str(&self) -> &str {
        match self {
            ExpressionArgumenType::Any => "Any",
            ExpressionArgumenType::Tokens => "Tokens",
            ExpressionArgumenType::String => "String",
            ExpressionArgumenType::Number => "Number",
            ExpressionArgumenType::Integer => "Integer"
        }
    }
}


// Operators ------------------------------------------------------------------
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Operator {
    ShiftRight,
    ShiftLeft,
    LogicalAnd,
    LogicalOr,
    Equals,
    Unequals,
    GreaterThanEqual,
    LessThanEqual,
    Pow,
    DivInt,
    LessThan,
    GreaterThan,
    LogicalNot,
    Plus,
    Minus,
    Mul,
    Div,
    Modulo,
    BitAnd,
    BitOr,
    BitNegate,
    BitXor,
}

impl Operator {

    pub fn as_str(&self) -> &str {
        match self {
            Operator::ShiftRight => ">>",
            Operator::ShiftLeft => "<<",
            Operator::LogicalAnd => "&&",
            Operator::LogicalOr => "||",
            Operator::Equals => "==",
            Operator::Unequals => "!=",
            Operator::GreaterThanEqual => ">=",
            Operator::LessThanEqual => "<=",
            Operator::Pow => "**",
            Operator::DivInt => "//",
            Operator::LessThan => "<",
            Operator::GreaterThan => ">",
            Operator::LogicalNot => "!",
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Modulo => "%",
            Operator::BitAnd => "&",
            Operator::BitOr => "|",
            Operator::BitNegate => "~",
            Operator::BitXor => "^"
        }
    }

    pub fn width(&self) -> usize {
        match self {
            Operator::ShiftRight | Operator::ShiftLeft | Operator::LogicalAnd | Operator::LogicalOr |
            Operator::Equals | Operator::Unequals | Operator::GreaterThanEqual | Operator::LessThanEqual |
            Operator::Pow | Operator::DivInt => 2,
            _ => 1
        }
    }

    pub fn associativity(&self) -> usize {
        match self {
            Operator::Pow | Operator::BitXor => 0,
            _ => 1
        }
    }

    pub fn precedence(&self) -> usize {
        match self {
            Operator::LogicalOr => 1,
            Operator::LogicalAnd => 2,
            Operator::Equals | Operator::Unequals | Operator::GreaterThanEqual | Operator::LessThanEqual |
            Operator::LessThan | Operator::GreaterThan => 3,
            Operator::BitOr => 4,
            Operator::BitXor => 5,
            Operator::BitAnd => 6,
            Operator::ShiftRight | Operator::ShiftLeft => 7,
            Operator::Plus | Operator::Minus | Operator::LogicalNot | Operator::BitNegate => 8,
            Operator::Mul | Operator::Div | Operator::Modulo | Operator::DivInt => 10,
            Operator::Pow => 11
        }
    }

    pub fn is_unary(&self) -> bool {
        matches!(self, Operator::Plus | Operator::Minus | Operator::LogicalNot | Operator::BitNegate)
    }

    pub fn is_unary_exclusive(&self) -> bool {
        matches!(self, Operator::LogicalNot | Operator::BitNegate)
    }
}

