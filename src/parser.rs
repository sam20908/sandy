use crate::{InterpreterError, LiteralKind, OpKind, Token};

trait Expr {}

struct LiteralExpr<T> {
    val: T,
}
impl<T> Expr for LiteralExpr<T> {}

struct BinaryOp {
    left: Box<dyn Expr>,
    right: Box<dyn Expr>,
    op: &'static str,
}
impl Expr for BinaryOp {}

struct UnaryOp {
    expr: Box<dyn Expr>,
    op: &'static str,
}
impl Expr for UnaryOp {}

fn expr(tokens: &mut Vec<Token>, pos: &mut usize) -> Box<dyn Expr> {
    eq(tokens, pos)
}

fn eq(tokens: &mut Vec<Token>, pos: &mut usize) -> Box<dyn Expr> {
    let mut left = cmp(tokens, pos);
    while *pos < tokens.len() {
        match tokens[*pos] {
            Token::Op(OpKind::Cmp(op)) if matches!(op, "==" | "!=") => {
                *pos += 1;
                let right = cmp(tokens, pos);
                left = Box::new(BinaryOp { left, right, op });
            }
            _ => break,
        }
    }
    left
}

fn cmp(tokens: &mut Vec<Token>, pos: &mut usize) -> Box<dyn Expr> {
    let mut left = term(tokens, pos);
    while *pos < tokens.len() {
        match tokens[*pos] {
            Token::Op(OpKind::Cmp(op)) if matches!(op, "<" | ">" | "<=" | ">=") => {
                *pos += 1;
                let right = term(tokens, pos);
                left = Box::new(BinaryOp { left, right, op });
            }
            _ => break,
        }
    }
    left
}

fn term(tokens: &mut Vec<Token>, pos: &mut usize) -> Box<dyn Expr> {
    let mut left = factor(tokens, pos);
    while *pos < tokens.len() {
        match tokens[*pos] {
            Token::Op(OpKind::Binary(op)) if matches!(op, "+" | "-") => {
                *pos += 1;
                let right = factor(tokens, pos);
                left = Box::new(BinaryOp { left, right, op });
            }
            _ => break,
        }
    }
    left
}

fn factor(tokens: &mut Vec<Token>, pos: &mut usize) -> Box<dyn Expr> {
    let mut left = unary(tokens, pos);
    while *pos < tokens.len() {
        match tokens[*pos] {
            Token::Op(OpKind::Binary(op)) if matches!(op, "*" | "/") => {
                *pos += 1;
                let right = unary(tokens, pos);
                left = Box::new(BinaryOp { left, right, op });
            }
            _ => break,
        }
    }
    left
}

fn unary(tokens: &mut Vec<Token>, pos: &mut usize) -> Box<dyn Expr> {
    match tokens[*pos] {
        Token::Op(OpKind::Unary(op)) => {
            *pos += 1;
            let expr = unary(tokens, pos);
            Box::new(UnaryOp { expr, op })
        }
        _ => primary(tokens, pos),
    }
}

fn primary(tokens: &mut Vec<Token>, pos: &mut usize) -> Box<dyn Expr> {
    match tokens[*pos].clone() {
        Token::Literal(LiteralKind::Str(str)) => {
            *pos += 1;
            Box::new(LiteralExpr { val: str })
        }
        Token::Literal(LiteralKind::NumWhole(num)) => {
            *pos += 1;
            Box::new(LiteralExpr { val: num })
        }
        Token::Literal(LiteralKind::NumDecimal(num)) => {
            *pos += 1;
            Box::new(LiteralExpr { val: num })
        }
        Token::Op(OpKind::LBracket("(")) => {
            *pos += 1;
            let expr = expr(tokens, pos);
            if *pos == tokens.len() {
                panic!(); // unclosed parenthesis
            } else {
                *pos += 1;
                expr
            }
        }
        _ => panic!(),
    }
}

pub fn parse(tokens: &mut Vec<Token>, parser_errors: &mut Vec<InterpreterError>) {
    let mut pos = 0;
    let expr = expr(tokens, &mut pos);
}
