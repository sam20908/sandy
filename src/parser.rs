use crate::{lexer, InterpreterError, Token, TokenKind};

pub fn eat(
    token: &mut Option<Token>,
    expected_kind: TokenKind,
    pos: &mut usize,
    buf: &Vec<u8>,
) -> Result<(), InterpreterError> {
    if token.kind == expected_kind {
        token = lexer::parse_next_token(pos, buf)?;
    } else {
        Err(InterpreterError::Parser("Unexpected kind of token"))
    }
}
