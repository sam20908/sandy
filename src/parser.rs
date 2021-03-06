use crate::{InterpreterError, KeywordKind, LiteralKind, OpKind, Token};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
enum Expr {
    Literal(LiteralKind),
    Binary(Rc<Expr>, OpKind, Rc<Expr>), // left, op, right
    Unary(OpKind, Rc<Expr>),            // op, right
    Var(String),                        // var id
}

#[derive(Debug)]
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
    *pos += 1;
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
    *pos += 1;
    Rc::new(Stmt::Expr(expr))
}

fn expr(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    eq(tokens, pos)
}

fn eq(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    let mut left = cmp(tokens, pos);
    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Op(op) if matches!(op, OpKind::Eq | OpKind::Neq) => {
                *pos += 1;
                let right = cmp(tokens, pos);
                left = Rc::new(Expr::Binary(left, op.clone(), right));
            }
            _ => break,
        }
    }
    left
}

fn cmp(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    let mut left = term(tokens, pos);
    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Op(op) if matches!(op, OpKind::Gt | OpKind::Lt | OpKind::Geq | OpKind::Leq) => {
                *pos += 1;
                let right = term(tokens, pos);
                left = Rc::new(Expr::Binary(left, op.clone(), right));
            }
            _ => break,
        }
    }
    left
}

fn term(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    let mut left = factor(tokens, pos);
    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Op(op) if matches!(op, OpKind::Add | OpKind::Sub) => {
                *pos += 1;
                let right = factor(tokens, pos);
                left = Rc::new(Expr::Binary(left, op.clone(), right));
            }
            _ => break,
        }
    }
    left
}

fn factor(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    let mut left = unary(tokens, pos);
    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Op(op) if matches!(op, OpKind::Mul | OpKind::Div) => {
                *pos += 1;
                let right = unary(tokens, pos);
                left = Rc::new(Expr::Binary(left, op.clone(), right));
            }
            _ => break,
        }
    }
    left
}

fn unary(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    match &tokens[*pos] {
        Token::Op(op) if matches!(op, OpKind::LogicalNot | OpKind::Add | OpKind::Sub) => {
            *pos += 1;
            let expr = unary(tokens, pos);
            Rc::new(Expr::Unary(op.clone(), expr))
        }
        _ => primary(tokens, pos),
    }
}

fn primary(tokens: &Vec<Token>, pos: &mut usize) -> Rc<Expr> {
    match &tokens[*pos] {
        Token::Literal(ref literal) => {
            *pos += 1;
            Rc::new(Expr::Literal(literal.clone()))
        }
        Token::Op(OpKind::LBrace) => {
            *pos += 1;
            let expr = expr(tokens, pos);
            if let None = check_token!(tokens, pos, Token::Op(OpKind::RBrace)) {
                panic!("expected closing bracket after expression");
            }
            *pos += 1;
            expr
        }
        Token::Id(ref id) => {
            *pos += 1;
            Rc::new(Expr::Var(id.clone()))
        }
        _ => panic!("unrecognized primary expression"),
    }
}

fn eval_floating_points(
    left: f64,
    right: f64,
    op: OpKind,
) -> Result<LiteralKind, InterpreterError> {
    match op {
        OpKind::Add => Ok(LiteralKind::NumDecimal(left + right)),
        OpKind::Sub => Ok(LiteralKind::NumDecimal(left - right)),
        OpKind::Mul => Ok(LiteralKind::NumDecimal(left * right)),
        OpKind::Div => Ok(LiteralKind::NumDecimal(left / right)),
        _ => Err(InterpreterError::Parser(format!(
            "{op:?} can't be applied to floating points"
        ))),
    }
}

fn eval_expr(
    expr: &Rc<Expr>,
    environment: &HashMap<String, LiteralKind>,
) -> Result<LiteralKind, InterpreterError> {
    match &**expr {
        Expr::Literal(literal) => Ok(literal.clone()),
        Expr::Binary(left, op, right) => {
            let left = eval_expr(&left, environment)?;
            let right = eval_expr(&right, environment)?;
            match (left, right) {
                (LiteralKind::Str(left_str), LiteralKind::Str(right_str)) => {
                    if matches!(op, OpKind::Add) {
                        Ok(LiteralKind::Str(format!("{left_str}{right_str}")))
                    } else {
                        Err(InterpreterError::Parser(format!(
                            "{op:?} can't be applied to strings"
                        )))
                    }
                }
                (LiteralKind::NumWhole(left_num), LiteralKind::NumWhole(right_num)) => match op {
                    OpKind::Add => Ok(LiteralKind::NumWhole(left_num + right_num)),
                    OpKind::Sub => Ok(LiteralKind::NumWhole(left_num - right_num)),
                    OpKind::Mul => Ok(LiteralKind::NumWhole(left_num * right_num)),
                    OpKind::Div => Ok(LiteralKind::NumWhole(left_num / right_num)),
                    OpKind::Mod => Ok(LiteralKind::NumWhole(left_num % right_num)),
                    OpKind::BitOr => Ok(LiteralKind::NumWhole(left_num | right_num)),
                    OpKind::BitAnd => Ok(LiteralKind::NumWhole(left_num & right_num)),
                    OpKind::Xor => Ok(LiteralKind::NumWhole(left_num ^ right_num)),
                    _ => Err(InterpreterError::Parser(
                        "{op:?} can't be applied on whole numbers".to_string(),
                    )),
                },
                (LiteralKind::NumWhole(left_num), LiteralKind::NumDecimal(right_num)) => Ok(
                    eval_floating_points(left_num as f64, right_num, op.clone())?,
                ),
                (LiteralKind::NumDecimal(left_num), LiteralKind::NumWhole(right_num)) => Ok(
                    eval_floating_points(left_num, right_num as f64, op.clone())?,
                ),
                (LiteralKind::NumDecimal(left_num), LiteralKind::NumDecimal(right_num)) => {
                    Ok(eval_floating_points(left_num, right_num, op.clone())?)
                }
                _ => Err(InterpreterError::Parser(
                    "Can't have a string and number as operands".to_string(),
                )),
            }
        }
        Expr::Unary(op, expr) => match op {
            OpKind::LogicalNot => todo!(),
            OpKind::Add => Ok(eval_expr(&expr, environment)?),
            OpKind::Sub => match eval_expr(&expr, environment)? {
                LiteralKind::NumWhole(num) => Ok(LiteralKind::NumWhole(-num)),
                LiteralKind::NumDecimal(num) => Ok(LiteralKind::NumDecimal(-num)),
                _ => Err(InterpreterError::Parser(
                    "{op:?} not supported as unary op for expression".to_string(),
                )),
            },
            _ => Err(InterpreterError::Parser(
                "{op:?} not recognized as unary op".to_string(),
            )),
        },
        Expr::Var(id) => match environment.get(id) {
            Some(val) => Ok(val.clone()),
            None => panic!("undeclared variable referenced"),
        },
    }
}

fn eval_stmt(stmt: &Stmt, environment: &mut HashMap<String, LiteralKind>) {
    match &stmt {
        Stmt::VarDecl(id, expr) => {
            // creating a new variable with an initializer from expr
            if let Some(_) = environment.insert(
                id.clone(),
                eval_expr(expr, environment).expect("failed to evaluate initializer"),
            ) {
                panic!("variable redefinition");
            }
        }
        Stmt::Expr(expr) => todo!(),
    }
}

pub fn parse(tokens: &Vec<Token>, parser_errors: &mut Vec<InterpreterError>) {
    let mut pos = 0;
    let mut environment = HashMap::new();
    while pos < tokens.len() {
        // all declarations are statements, not the other way around
        let stmt = decl(tokens, &mut pos);
        println!("{stmt:?}");
        eval_stmt(&stmt, &mut environment);
    }
}
