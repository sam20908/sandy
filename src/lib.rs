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
        ("+", OpKind::Binary("+")),
        ("-", OpKind::Binary("-")),
        ("*", OpKind::Binary("*")),
        ("/", OpKind::Binary("/")),
        ("%", OpKind::Binary("%")),
        ("|", OpKind::Binary("|")),
        ("&", OpKind::Binary("&")),
        ("^", OpKind::Binary("^")),
        ("||", OpKind::Binary("||")),
        ("&&", OpKind::Binary("&&")),
        ("<<", OpKind::Binary("<<")),
        (">>", OpKind::Binary(">>")),
        ("=", OpKind::BinarySelfMod("=")),
        ("+=", OpKind::BinarySelfMod("+")),
        ("-=", OpKind::BinarySelfMod("-")),
        ("*=", OpKind::BinarySelfMod("*")),
        ("/=", OpKind::BinarySelfMod("/")),
        ("%=", OpKind::BinarySelfMod("%")),
        ("|=", OpKind::BinarySelfMod("|")),
        ("&=", OpKind::BinarySelfMod("&")),
        ("^=", OpKind::BinarySelfMod("^")),
        ("<<=", OpKind::BinarySelfMod("<<")),
        (">>=", OpKind::BinarySelfMod(">>")),
        ("!", OpKind::Unary("!")),
        ("~", OpKind::Unary("~")),
        ("<", OpKind::Cmp("<")),
        (">", OpKind::Cmp(">")),
        ("<=", OpKind::Cmp("<=")),
        (">=", OpKind::Cmp(">=")),
        ("==", OpKind::Cmp("==")),
        ("!=", OpKind::Cmp("!=")),
        ("(", OpKind::LBracket("(")),
        (")", OpKind::RBracket(")")),
        ("{", OpKind::LBracket("{")),
        ("}", OpKind::RBracket("}")),
        (",", OpKind::Comma),
        (".", OpKind::Dot),
        (";", OpKind::ExprEnd),
    ]);
}

#[derive(Debug, Clone)]
pub enum OpKind {
    Binary(&'static str),
    BinarySelfMod(&'static str),
    Unary(&'static str),
    Cmp(&'static str),
    LBracket(&'static str),
    RBracket(&'static str),
    Comma,
    Dot,
    ExprEnd,
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

#[derive(Debug, Clone)]
pub enum Token {
    Id(String),
    Literal(LiteralKind),
    Keyword(KeywordKind),
    Op(OpKind),
}
