use std::collections::HashMap;
use std::error::Error;
use std::fmt;

pub mod lexer;
pub mod parser;

#[derive(Debug)]
pub enum InterpreterError {
    Lexer(String),
    Parser(String),
}
impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self {
            InterpreterError::Lexer(msg) => write!(f, "Lexer failed because: {}", msg),
            InterpreterError::Parser(msg) => write!(f, "Parser failed because: {}", msg),
        }
    }
}
impl Error for InterpreterError {}

#[macro_use]
pub(crate) extern crate lazy_static;

lazy_static! {
    pub(crate) static ref KEYWORDS: HashMap<&'static str, KeywordKind> =
        HashMap::from([("var", KeywordKind::Var)]);
    // bit 1 set means the op modified left operand
    pub(crate) static ref OPS: HashMap<&'static str, OpKind> = HashMap::from([
        ("+", OpKind::Add),
        ("-", OpKind::Sub),
        ("*", OpKind::Mul),
        ("/", OpKind::Div),
        ("%", OpKind::Mod),
        ("+=", OpKind::AddSelf),
        ("-=", OpKind::SubSelf),
        ("*=", OpKind::MulSelf),
        ("/=", OpKind::DivSelf),
        ("%=", OpKind::ModSelf),
        ("|", OpKind::BitOr),
        ("&", OpKind::BitAnd),
        ("||", OpKind::LogicalOr),
        ("&&", OpKind::LogicalAnd),
        ("^", OpKind::Xor),
        ("<<", OpKind::LShift),
        (">>", OpKind::RShift),
        ("|=", OpKind::BitOrSelf),
        ("&=", OpKind::BitAndSelf),
        ("||=", OpKind::LogicalOrSelf),
        ("&&=", OpKind::LogicalAndSelf),
        ("^=", OpKind::XorSelf),
        ("<<=", OpKind::LShiftSelf),
        (">>=", OpKind::RShiftSelf),
        ("~", OpKind::BitNot),
        ("!", OpKind::LogicalNot),
        ("==", OpKind::Eq),
        ("!=", OpKind::Neq),
        (">", OpKind::Gt),
        ("<", OpKind::Lt),
        (">=", OpKind::Geq),
        ("<=", OpKind::Leq),
        ("=", OpKind::Assign),
        (",", OpKind::Comma),
        (".", OpKind::Dot),
        ("(", OpKind::LBrace),
        ("{", OpKind::LCurly),
        ("[", OpKind::LSquare),
        (")", OpKind::RBrace),
        ("}", OpKind::RCurly),
        ("]", OpKind::RSquare),
        (";", OpKind::StmtEnd),
    ]);
}

#[derive(Debug, Clone)]
pub enum OpKind {
    // self means self-modifying ops like +=
    // standard math ops
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    AddSelf,
    SubSelf,
    MulSelf,
    DivSelf,
    ModSelf,
    // bit ops
    BitOr,
    BitAnd,
    LogicalOr,
    LogicalAnd,
    Xor,
    LShift,
    RShift,
    BitOrSelf,
    BitAndSelf,
    LogicalOrSelf,
    LogicalAndSelf,
    XorSelf,
    LShiftSelf,
    RShiftSelf,
    BitNot,
    LogicalNot,
    // cmp ops
    Eq,
    Neq,
    Gt,
    Lt,
    Geq,
    Leq,
    // miscellaneous
    Assign,
    Comma,
    Dot,
    LBrace,  // (
    LCurly,  // {
    LSquare, // [
    RBrace,  // )
    RCurly,  // }
    RSquare, // ]
    StmtEnd,
}

#[derive(Debug, Clone)]
pub enum KeywordKind {
    Var,
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Str(String),
    NumWhole(i64),
    NumDecimal(f64),
}

#[derive(Debug)]
pub enum Token {
    Id(String),
    Literal(LiteralKind),
    Keyword(KeywordKind),
    Op(OpKind),
}
