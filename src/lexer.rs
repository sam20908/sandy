use crate::{InterpreterError, LiteralKind, Token, KEYWORDS, OPS};

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
        return cur.is_ascii_digit() || cur == '.';
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
    let mut alpha_count = first.is_ascii_alphabetic() as u32;
    let mut digit_count = first.is_ascii_digit() as u32;
    let mut seen_dot = false;
    *pos += 1;
    while *pos < buf.len() {
        let mut c = buf[*pos] as char;
        if c.is_whitespace() && !is_str {
            break;
        }
        if first.is_ascii_digit() {
            if c.is_ascii_alphabetic() {
                // if we don't catch this now, substrings like 12a will not be detected as
                // invalid because it'll be separated into two tokens
                // keep consuming the token to create meaningful diagnostic
                while *pos < buf.len() && c.is_ascii_alphanumeric() {
                    token_str.push(c);
                    *pos += 1;
                    c = buf[*pos] as char;
                }
                return Err(InterpreterError::Lexer(format!(
                    "Found alphabet while parsing constant literal: {}",
                    token_str
                )));
            }
            if c == '.' {
                // parse the floating point literal, be careful with cases like 12.3.4
                if seen_dot {
                    // there's only one dot max for floating points
                    // keep consuming the token to create meaningful diagnostic
                    while *pos < buf.len() && !c.is_whitespace() {
                        token_str.push(c);
                        *pos += 1;
                        c = buf[*pos] as char;
                    }
                    return Err(InterpreterError::Lexer(format!(
                        "Found extraneous dot while parsing constant literal: {}",
                        token_str
                    )));
                } else {
                    seen_dot = true;
                }
            }
        }
        if !first.is_ascii_alphanumeric() && !is_str && !is_op {
            // some other symbol that's not an op
            // keep consuming the token to create meaningful diagnostic
            while *pos < buf.len() && !c.is_whitespace() {
                token_str.push(c);
                *pos += 1;
                c = buf[*pos] as char;
            }
            return Err(InterpreterError::Lexer(format!(
                "Unrecognized symbol: {}",
                token_str
            )));
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
        alpha_count += c.is_ascii_alphabetic() as u32;
        digit_count += c.is_ascii_digit() as u32;
        if is_op {
            break;
        }
    }
    if let Some(keyword_kind) = KEYWORDS.get(&token_str.as_str()) {
        Ok(Some(Token::Keyword(*keyword_kind)))
    } else {
        if is_str {
            if str_closed {
                return Ok(Some(Token::Literal(LiteralKind::Str(token_str))));
            } else {
                return Err(InterpreterError::Lexer("Unclosed parenthesis".to_string()));
            }
        }
        if alpha_count > 0 && digit_count > 0 {
            // don't have to worry about cases like 12a because they'll get separated as two tokens
            Ok(Some(Token::Id(token_str)))
        } else if alpha_count > 0 && digit_count == 0 {
            if let Some(keyword_kind) = KEYWORDS.get(&token_str.as_str()) {
                Ok(Some(Token::Keyword(*keyword_kind)))
            } else {
                // not a keyword, can only assume it's an variable id
                Ok(Some(Token::Id(token_str)))
            }
        } else if alpha_count == 0 && digit_count > 0 {
            if seen_dot {
                // floating point number
                Ok(Some(Token::Literal(LiteralKind::NumDecimal(
                    token_str.parse::<f64>().unwrap(),
                ))))
            } else {
                Ok(Some(Token::Literal(LiteralKind::NumWhole(
                    token_str.parse::<i64>().unwrap(),
                ))))
            }
        } else {
            if let Some(op_kind) = OPS.get(&token_str.as_str()) {
                Ok(Some(Token::Op(*op_kind)))
            } else {
                // unrecognized ops will be handled earlier, before checking if the
                // current character is expected from the leading character
                unreachable!();
            }
        }
    }
}
