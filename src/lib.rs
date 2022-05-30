use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt;

pub mod lexer;
pub mod parser;

#[derive(Debug)]
pub enum InterpreterError {
    Lexer(&'static str),
    Parser(&'static str),
}
impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
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
    pub(crate) static ref OPS_CHAR: HashSet<char> = HashSet::from(['+', '-', '*', '/', '=']);
    pub(crate) static ref OPS_STR: HashMap<&'static str, OperatorKind> = HashMap::from([
        ("+", OperatorKind::Add),
        ("-", OperatorKind::Sub),
        ("*", OperatorKind::Mul),
        ("/", OperatorKind::Div),
        ("=", OperatorKind::Assign),
        ("+=", OperatorKind::AddOnto),
        ("-=", OperatorKind::SubOnto),
        ("*=", OperatorKind::MulOnto),
        ("/=", OperatorKind::DivOnto),
        ("==", OperatorKind::CmpEq),
    ]);
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OperatorKind {
    Add,
    Sub,
    Mul,
    Div,
    Assign,
    AddOnto,
    SubOnto,
    MulOnto,
    DivOnto,
    CmpEq,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum KeywordKind {
    Var,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LiteralKind {
    String,
    Constant,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenKind {
    Identifier,
    Literal(LiteralKind),
    Keyword(KeywordKind),
    Operator(OperatorKind),
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    val: Option<String>,
}
