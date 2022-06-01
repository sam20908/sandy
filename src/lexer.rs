use crate::*;

fn skip_whitespace(pos: &mut usize, buf: &Vec<u8>) {
    while *pos < buf.len() && (buf[*pos] as char).is_whitespace() {
        *pos += 1;
    }
}

fn expected_char_from_leading(cur: char, leading: char) -> bool {
    if leading.is_ascii_alphabetic() {
        return cur.is_ascii_alphanumeric();
    }
    if leading.is_ascii_digit() {
        return cur.is_ascii_digit();
    }
    if leading == '+'
        || leading == '-'
        || leading == '*'
        || leading == '/'
        || leading == '='
        || leading == '!'
        || leading == '<'
        || leading == '>'
        || leading == '%'
    {
        return cur == '=';
    }

    leading == '"'
}

pub fn parse_next_token(pos: &mut usize, buf: &Vec<u8>) -> Result<Option<Token>, InterpreterError> {
    skip_whitespace(pos, buf);
    if *pos == buf.len() {
        return Ok(None);
    }

    let first = buf[*pos] as char;
    let is_str = first == '"';
    let mut token_str = if is_str {
        String::new()
    } else {
        first.to_string()
    };
    let is_op = OPS.contains_key(&token_str.as_str());
    let mut str_closed = !is_str;
    let mut all_alpha = is_str || first.is_ascii_alphabetic();
    let mut all_digit = first.is_ascii_digit();
    let mut all_alphaordigit = first.is_ascii_alphanumeric();
    *pos += 1;
    while *pos < buf.len() {
        let c = buf[*pos] as char;
        if c.is_whitespace() {
            break;
        }
        if first.is_ascii_digit() && c.is_ascii_alphabetic() {
            // if we don't catch this now, substrings like 12a will not be detected as
            // invalid because it'll be separated into two tokens
            return Err(InterpreterError::Lexer("Found letter in constant"));
        }
        if !expected_char_from_leading(c, first) {
            break;
        }
        *pos += 1;
        if is_str {
            if c == '"' {
                str_closed = true;
                break;
            } else {
                token_str.push(c);
            }
        } else {
            token_str.push(c);
        }
        all_alpha &= c.is_ascii_alphabetic();
        all_digit &= c.is_ascii_digit();
        all_alphaordigit &= c.is_ascii_alphanumeric();
        if is_op {
            break;
        }
    }
    if let Some(op_kind) = OPS.get(&token_str.as_str()) {
        Ok(Some(Token::Op(*op_kind)))
    } else if let Some(keyword_kind) = KEYWORDS.get(&token_str.as_str()) {
        Ok(Some(Token::Keyword(*keyword_kind)))
    } else {
        match (all_alpha, all_digit) {
            (false, false) => {
                if all_alphaordigit {
                    debug_assert!(first.is_ascii_alphabetic());
                    Ok(Some(Token::Id(token_str)))
                } else {
                    Err(InterpreterError::Lexer("Unrecognized symbol"))
                }
            }
            (true, false) => {
                if is_str {
                    if str_closed {
                        Ok(Some(Token::Literal(LiteralKind::Str(token_str))))
                    } else {
                        Err(InterpreterError::Lexer("Unclosed or mismatched quote"))
                    }
                } else {
                    Ok(Some(Token::Id(token_str)))
                }
            }
            (false, true) => Ok(Some(Token::Literal(LiteralKind::Constant(
                token_str.parse::<i32>().unwrap(),
            )))),
            (true, true) => unreachable!(),
        }
    }
}
