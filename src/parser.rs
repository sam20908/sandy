use crate::{InterpreterError, LiteralKind, OpKind, Token};
use std::rc::Rc;

enum Expr {
    Literal(LiteralKind),
    Binary(Rc<Expr>, &'static str, Rc<Expr>), // left, op, right
    Unary(&'static str, Rc<Expr>),            // op, right
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
        _ => Err(InterpreterError::Parser(
            "Unsupported operation on floating points".to_string(),
        )),
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
                        Err(InterpreterError::Parser(
                            "Unsupported operator on strings".to_string(),
                        ))
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
                    _ => Err(InterpreterError::Parser(
                        "Unsupported operator on both whole numbers".to_string(),
                    )),
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
                _ => Err(InterpreterError::Parser("Invalid operands".to_string())),
            }
        }
        Expr::Unary(op, expr) => todo!(),
    }
}

pub fn parse(tokens: &Vec<Token>, parser_errors: &mut Vec<InterpreterError>) {
    if tokens.len() > 0 {
        let mut pos = 0;
        let expr = expr(tokens, &mut pos);
        println!("{:?}", eval(&expr));
    }
}
