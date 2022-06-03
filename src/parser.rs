use crate::{InterpreterError, OpKind};

trait Expr {}

struct BinaryOp {
    left: Box<dyn Expr>,
    right: Box<dyn Expr>,
    op: OpKind,
}
impl Expr for BinaryOp {}
