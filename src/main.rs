use std::fs::File;
use std::io::Read;

use interpreter::lexer;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut src = File::open("test.txt")?;
    let mut buf = vec![];
    let _ = src.read_to_end(&mut buf)?;
    let mut pos = 0;

    let mut tokens = vec![];
    let mut errors = vec![];
    loop {
        match lexer::parse_next_token(&mut pos, &buf) {
            Ok(Some(token)) => tokens.push(token),
            Ok(None) => break,
            Err(err) => errors.push(err),
        }
    }
    println!("{tokens:?}");
    println!("{errors:?}");

    Ok(())
}
