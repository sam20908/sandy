use crate::{InterpreterError, KeywordKind, LiteralKind, OpKind, Token};
use std::rc::Rc;

enum Expr {
    Literal(LiteralKind),
    Binary(Rc<Expr>, &'static str, Rc<Expr>), // left, op, right
    Unary(&'static str, Rc<Expr>),            // op, right
}

enum Stmt {
    VarDecl(String, Rc<Expr>), // id, expr
    Expr(Rc<Expr>),            // expr stmt
}

macro_rules! check_token {
    ($tokens:ident, $pos:ident, $token:pat) => {
        if *$pos < $tokens.len() && matches!($tokens[*$pos], $token) {
            Some(&$tokens[*$pos])
        } else {
            None
        }
    };
}

fn decl(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Stmt> {
    if let Some(_) = check_token!(tokens, pos, Token::Keyword(KeywordKind::Var)) {
        *pos += 1;
        var_decl(tokens, pos)
    } else {
        stmt(tokens, pos)
    }
}

fn var_decl(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Stmt> {
    // var keyword is already checked
    let var_id = match check_token!(tokens, pos, Token::Id(_)) {
        Some(&Token::Id(ref id)) => id.clone(),
        Some(_) | None => panic!("expected identifier after var keyword"),
    };
    *pos += 1;
    if let None = check_token!(tokens, pos, Token::Op(OpKind::Assign)) {
        panic!("expected assignment operator after variable identifier");
    }
    *pos += 1;
    let expr = expr(tokens, pos);
    if let None = check_token!(tokens, pos, Token::Op(OpKind::StmtEnd)) {
        panic!("expected semicolon at the end of variable declaration");
    }
    Rc::new(Stmt::VarDecl(var_id, expr))
}

fn stmt(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Stmt> {
    // todo handle more statement types
    expr_stmt(tokens, pos)
}

fn expr_stmt(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Stmt> {
    let expr = expr(tokens, pos);
    if let None = check_token!(tokens, pos, Token::Op(OpKind::StmtEnd)) {
        panic!("expected semicolon at the end of expression statement");
    }
    Rc::new(Stmt::Expr(expr))
}

fn expr(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    eq(tokens, pos)
}

fn eq(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    let mut left = cmp(tokens, pos);
    while *pos < tokens.len() {
        match tokens[*pos] {
            Token::Op(OpKind::Cmp(op)) if matches!(op, "==" | "!=") => {
                *pos += 1;
                let right = cmp(tokens, pos);
                left = Rc::new(Expr::Binary(left, op, right));
            }
            _ => break,
        }
    }
    left
}

fn cmp(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    let mut left = term(tokens, pos);
    while *pos < tokens.len() {
        match tokens[*pos] {
            Token::Op(OpKind::Cmp(op)) if matches!(op, "<" | ">" | "<=" | ">=") => {
                *pos += 1;
                let right = term(tokens, pos);
                left = Rc::new(Expr::Binary(left, op, right));
            }
            _ => break,
        }
    }
    left
}

fn term(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    let mut left = factor(tokens, pos);
    while *pos < tokens.len() {
        match tokens[*pos] {
            Token::Op(OpKind::Binary(op)) if matches!(op, "+" | "-") => {
                *pos += 1;
                let right = factor(tokens, pos);
                left = Rc::new(Expr::Binary(left, op, right));
            }
            _ => break,
        }
    }
    left
}

fn factor(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    let mut left = unary(tokens, pos);
    while *pos < tokens.len() {
        match tokens[*pos] {
            Token::Op(OpKind::Binary(op)) if matches!(op, "*" | "/") => {
                *pos += 1;
                let right = unary(tokens, pos);
                left = Rc::new(Expr::Binary(left, op, right));
            }
            _ => break,
        }
    }
    left
}

fn unary(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    match tokens[*pos] {
        Token::Op(OpKind::Unary(op)) => {
            *pos += 1;
            let expr = unary(tokens, pos);
            Rc::new(Expr::Unary(op, expr))
        }
        _ => primary(tokens, pos),
    }
}

fn primary(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    match &tokens[*pos] {
        Token::Literal(literal) => {
            *pos += 1;
            Rc::new(Expr::Literal(literal.clone()))
        }
        Token::Op(OpKind::LBracket("(")) => {
            *pos += 1;
            let expr = expr(tokens, pos);
            if *pos == tokens.len() || !matches!(tokens[*pos], Token::Op(OpKind::RBracket(")"))) {
                panic!(); // unclosed parenthesis
            } else {
                *pos += 1;
                expr
            }
        }
        _ => panic!(), // unrecognized literal
    }
}

fn eval_floating_points(left: f64, right: f64, op: &str) -> Result<LiteralKind, InterpreterError> {
    match op {
        "+" => Ok(LiteralKind::NumDecimal(left + right)),
        "-" => Ok(LiteralKind::NumDecimal(left - right)),
        "*" => Ok(LiteralKind::NumDecimal(left * right)),
        "/" => Ok(LiteralKind::NumDecimal(left / right)),
        _ => Err(InterpreterError::Parser(format!(
            "{op} can't be applied to floating points"
        ))),
    }
}

fn eval(expr: &Expr) -> Result<LiteralKind, InterpreterError> {
    match expr {
        Expr::Literal(literal) => Ok(literal.clone()),
        Expr::Binary(left, op, right) => {
            let left = eval(left)?;
            let right = eval(right)?;
            match (left, right) {
                (LiteralKind::Str(left_str), LiteralKind::Str(right_str)) => {
                    if op == &"+" {
                        Ok(LiteralKind::Str(format!("{left_str}{right_str}")))
                    } else {
                        Err(InterpreterError::Parser(format!(
                            "{op} can't be applied to strings"
                        )))
                    }
                }
                (LiteralKind::NumWhole(left_num), LiteralKind::NumWhole(right_num)) => match op {
                    &"+" => Ok(LiteralKind::NumWhole(left_num + right_num)),
                    &"-" => Ok(LiteralKind::NumWhole(left_num - right_num)),
                    &"*" => Ok(LiteralKind::NumWhole(left_num * right_num)),
                    &"/" => Ok(LiteralKind::NumWhole(left_num / right_num)),
                    &"%" => Ok(LiteralKind::NumWhole(left_num % right_num)),
                    &"|" => Ok(LiteralKind::NumWhole(left_num | right_num)),
                    &"&" => Ok(LiteralKind::NumWhole(left_num & right_num)),
                    &"^" => Ok(LiteralKind::NumWhole(left_num ^ right_num)),
                    _ => Err(InterpreterError::Parser(format!(
                        "{op} can't be applied on whole numbers"
                    ))),
                },
                (LiteralKind::NumWhole(left_num), LiteralKind::NumDecimal(right_num)) => {
                    Ok(eval_floating_points(left_num as f64, right_num, op)?)
                }
                (LiteralKind::NumDecimal(left_num), LiteralKind::NumWhole(right_num)) => {
                    Ok(eval_floating_points(left_num, right_num as f64, op)?)
                }
                (LiteralKind::NumDecimal(left_num), LiteralKind::NumDecimal(right_num)) => {
                    Ok(eval_floating_points(left_num, right_num, op)?)
                }
                _ => Err(InterpreterError::Parser(
                    "Can't have a string and number as operands".to_string(),
                )),
            }
        }
        Expr::Unary(op, expr) => todo!(),
    }
}

pub fn parse(tokens: &Vec<Token>, parser_errors: &mut Vec<InterpreterError>) {
    if tokens.len() > 0 {
        let mut pos = 0;
        let _ = decl(tokens, &mut pos);
    }
}
