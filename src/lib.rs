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
    pub(crate) static ref OPS: HashMap<&'static str, OpKind> = HashMap::from([
        // ops
        ("+", OpKind::Add),
        ("-", OpKind::Sub),
        ("*", OpKind::Mul),
        ("/", OpKind::Div),
        ("%", OpKind::Mod),
        ("=", OpKind::Assign),
        ("!", OpKind::Bang),
        // self-modifying ops
        ("+=", OpKind::SelfAdd),
        ("-=", OpKind::SelfSub),
        ("*=", OpKind::SelfMul),
        ("/=", OpKind::SelfDiv),
        ("%=", OpKind::SelfMod),
        // cmp ops
        ("<", OpKind::CmpGt),
        (">", OpKind::CmpLt),
        ("<=", OpKind::CmpGeq),
        (">=", OpKind::CmpLeq),
        ("==", OpKind::CmpEq),
        ("!=", OpKind::CmpNeq),
        // brackets
        ("(", OpKind::LCurly),
        (")", OpKind::RCurly),
        ("{", OpKind::LBrace),
        ("}", OpKind::RBrace),
        // miscellaneous
        (",", OpKind::Comma),
        (".", OpKind::Dot),
        (";", OpKind::Semicolon),
    ]);
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OpKind {
    // ops
    Add,
    Sub,
    Mul,
    Div,
    Assign,
    Bang,
    Mod,
    // self-modifying ops
    SelfAdd,
    SelfSub,
    SelfMul,
    SelfDiv,
    SelfMod,
    // cmp ops
    CmpGt,
    CmpLt,
    CmpGeq,
    CmpLeq,
    CmpEq,
    CmpNeq,
    // brackets
    LCurly,
    RCurly,
    LBrace,
    RBrace,
    // miscellaneous
    Comma,
    Dot,
    Semicolon,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum KeywordKind {
    Var,
}

#[derive(Debug, PartialEq)]
pub enum LiteralKind {
    Str(String),
    NumWhole(i64),
    NumDecimal(f64),
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Id(String),
    Literal(LiteralKind),
    Keyword(KeywordKind),
    Op(OpKind),
}
