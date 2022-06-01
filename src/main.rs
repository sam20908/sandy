use std::fs::File;
use std::io;
use std::io::prelude::*;

use interpreter::*;
use parser::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut src = File::open("test.txt")?;
    let mut buf = vec![];
    let _ = src.read_to_end(&mut buf)?;
    let mut pos = 0;

    let mut tokens = vec![];
    while let Some(token) = lexer::parse_next_token(&mut pos, &buf)? {
        tokens.push(token);
    }
    println!("{tokens:?}");

    Ok(())
}
