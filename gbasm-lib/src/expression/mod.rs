// External Dependencies ------------------------------------------------------
use ordered_float::OrderedFloat;


// Internal Dependencies ------------------------------------------------------
use crate::lexer::InnerToken;


// Modules --------------------------------------------------------------------
pub mod data;


// Constants ------------------------------------------------------------------
pub const TEMPORARY_EXPRESSION_ID: usize = ::std::u32::MAX as usize;


// Types ----------------------------------------------------------------------
pub type DataExpression = (usize, Expression);
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
        name: String,
        args: Vec<Expression>
    }
}

impl Expression {
    pub fn is_constant(&self) -> bool {
        match self {
            Expression::Binary { left, right, .. } => right.is_constant() && left.is_constant(),
            Expression::Unary { right, .. } => right.is_constant(),
            Expression::Value(value) => value.is_constant(),
            Expression::BuiltinCall { args, .. } => args.iter().all(|arg| arg.is_constant())
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExpressionValue {
    ConstantValue(InnerToken, String),
    Integer(i32),
    Float(OrderedFloat<f32>),
    String(String),
    OffsetAddress(InnerToken, i32),
    GlobalLabelAddress(InnerToken, usize),
    LocalLabelAddress(InnerToken, usize)
}

impl ExpressionValue {

    fn is_constant(&self) -> bool {
        match self {
            ExpressionValue::ConstantValue(_, _) | ExpressionValue::Integer(_) | ExpressionValue::Float(_) | ExpressionValue::String(_) => true,
            ExpressionValue::OffsetAddress(_, _) | ExpressionValue::GlobalLabelAddress(_, _) | ExpressionValue::LocalLabelAddress(_, _) => false
        }
    }

}

#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionResult {
    Integer(i32),
    Float(OrderedFloat<f32>),
    String(String)
}

#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionArgumenType {
    Any,
    Tokens,
    String,
    Number,
    Integer
}

#[derive(Debug, Eq, PartialEq)]
pub enum ExpressionReturnType {
    None,
    String,
    Number,
    Float,
    Integer,
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

    pub fn len(&self) -> usize {
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
            Operator::BitOr => 3,
            Operator::BitXor => 4,
            Operator::BitAnd => 5,
            Operator::Equals | Operator::Unequals => 6,
            Operator::GreaterThanEqual | Operator::LessThanEqual | Operator::LessThan | Operator::GreaterThan => 7,
            Operator::ShiftRight | Operator::ShiftLeft => 8,
            Operator::Plus | Operator::Minus | Operator::LogicalNot | Operator::BitNegate => 9,
            Operator::Mul | Operator::Div | Operator::Modulo | Operator::DivInt => 11,
            Operator::Pow => 12,
        }
    }

    pub fn is_unary(&self) -> bool {
        match self {
            Operator::Plus | Operator::Minus | Operator::LogicalNot | Operator::BitNegate => true,
            _ => false
        }
    }

    pub fn is_unary_exclusive(&self) -> bool {
        match self {
            Operator::LogicalNot | Operator::BitNegate => true,
            _ => false
        }
    }

}

