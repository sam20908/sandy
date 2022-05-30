use std::fs::File;
use std::io;
use std::io::prelude::*;

use interpreter::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut src = File::open("test.txt")?;
    let mut buf = vec![];
    let _ = src.read_to_end(&mut buf)?;
    let mut pos = 0;

    let mut cur_token = lexer::parse_next_token(&mut pos, &buf);
    parser::eat(
        &mut cur_token,
        TokenKind::Literal(LiteralKind::Constant),
        &mut pos,
        &buf,
    )?;

    Ok(())
}
