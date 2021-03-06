use crate::{InterpreterError, LiteralKind, OpKind, Token, KEYWORDS, OPS};

#[derive(PartialEq)]
enum NumType {
    Whole,
    Decimal,
}

fn skip_whitespace(pos: &mut usize, buf: &Vec<u8>) {
    while *pos < buf.len() && (buf[*pos] as char).is_whitespace() {
        *pos += 1;
    }
}

fn fwd_until_whitespace(pos: &mut usize, buf: &Vec<u8>) {
    while *pos < buf.len() && !(buf[*pos] as char).is_whitespace() {
        *pos += 1;
    }
}

fn parse_literal_num(
    pos: &mut usize,
    buf: &Vec<u8>,
    token_str: &mut String,
) -> Result<NumType, InterpreterError> {
    let mut seen_dot = false;
    while *pos < buf.len() {
        let c = buf[*pos] as char;
        if c.is_whitespace() {
            break;
        }
        if c == '.' {
            if seen_dot {
                fwd_until_whitespace(pos, buf); // skip over invalid token
                return Err(InterpreterError::Lexer(
                    "Found extraneous dot while parsing decimal".to_string(),
                ));
            } else {
                seen_dot = true;
            }
        } else if c.is_ascii_alphabetic() {
            fwd_until_whitespace(pos, buf); // skip over invalid token
            return Err(InterpreterError::Lexer(
                "Found letter while parsing decimal".to_string(),
            ));
        } else if !c.is_ascii_digit() {
            break;
        }
        token_str.push(c);
        *pos += 1;
    }
    Ok(if seen_dot {
        NumType::Decimal
    } else {
        NumType::Whole
    })
}

fn parse_literal_str(
    pos: &mut usize,
    buf: &Vec<u8>,
    token_str: &mut String,
) -> Result<(), InterpreterError> {
    *pos += 1; // skip leading quote
    while *pos < buf.len() {
        let c = buf[*pos] as char;
        if c == '"' {
            *pos += 1; // skip this quote character for next token
            return Ok(());
        } else {
            // keep consuming characters inside the string literal
            token_str.push(c);
            *pos += 1;
        }
    }
    // if we found a closing quote, we'd never reach here (the end)
    Err(InterpreterError::Lexer("Unclosed quote".to_string()))
}

fn parse_id(pos: &mut usize, buf: &Vec<u8>, token_str: &mut String) {
    // no error handling is needed since we know the first character is already
    // an alphabet, we just stop if we hit a non-alphanumeric character
    while *pos < buf.len() {
        let c = buf[*pos] as char;
        if !c.is_ascii_alphanumeric() {
            break;
        }
        token_str.push(c);
        *pos += 1;
    }
}

fn parse_op(
    pos: &mut usize,
    buf: &Vec<u8>,
    token_str: &mut String,
) -> Result<OpKind, InterpreterError> {
    // try to "eat" as much characters for a valid op as possible
    loop {
        token_str.push(buf[*pos] as char);
        *pos += 1;
        if let None = OPS.get(&token_str.as_str()) {
            break;
        }
    }
    // last character in the token makes the op unrecognized, so don't process it
    token_str.pop();
    *pos -= 1;
    if token_str.len() == 0 {
        // we found an invalid symbol
        fwd_until_whitespace(pos, buf);
        Err(InterpreterError::Lexer("Unrecognized symbol".to_string()))
    } else {
        // we just verified that the op would exist if the last character is gone
        Ok(unsafe { OPS.get(&token_str.as_str()).unwrap_unchecked().clone() })
    }
}

pub fn parse_next_token(pos: &mut usize, buf: &Vec<u8>) -> Result<Option<Token>, InterpreterError> {
    skip_whitespace(pos, buf);
    if *pos == buf.len() {
        return Ok(None);
    }

    let c = buf[*pos] as char;
    let mut token_str = String::new();
    if c == '"' {
        let _ = parse_literal_str(pos, buf, &mut token_str)?;
        Ok(Some(Token::Literal(LiteralKind::Str(token_str))))
    } else if c.is_ascii_digit() {
        let num_type = parse_literal_num(pos, buf, &mut token_str)?;
        if num_type == NumType::Whole {
            Ok(Some(Token::Literal(LiteralKind::NumWhole(
                token_str.parse::<i64>().unwrap(),
            ))))
        } else {
            Ok(Some(Token::Literal(LiteralKind::NumDecimal(
                token_str.parse::<f64>().unwrap(),
            ))))
        }
    } else if c.is_ascii_alphabetic() {
        parse_id(pos, buf, &mut token_str);
        if let Some(keyword_kind) = KEYWORDS.get(&token_str.as_str()) {
            Ok(Some(Token::Keyword(keyword_kind.clone())))
        } else {
            Ok(Some(Token::Id(token_str)))
        }
    } else {
        Ok(Some(Token::Op(parse_op(pos, buf, &mut token_str)?)))
    }
}
