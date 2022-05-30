use crate::{
    InterpreterError, KeywordKind, LiteralKind, OperatorKind, Token, TokenKind, KEYWORDS, OPS_CHAR,
    OPS_STR,
};

fn skip_whitespace(pos: &mut usize, buf: &Vec<u8>) {
    while *pos < buf.len() && (buf[*pos] as char).is_whitespace() {
        *pos += 1;
    }
}

fn expected_char_from_leading(cur: char, leading: char) -> bool {
    if leading.is_ascii_alphabetic() {
        return cur.is_ascii_alphabetic() || cur.is_ascii_digit();
    }
    if leading.is_ascii_digit() {
        return cur.is_ascii_digit();
    }
    if leading == '+' || leading == '-' || leading == '*' || leading == '/' || leading == '=' {
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
    let is_op = OPS_CHAR.contains(&first);
    let mut str_closed = !is_str;
    let mut token_str = if is_str {
        String::from("")
    } else {
        first.to_string()
    };
    *pos += 1;
    while *pos < buf.len() {
        let c = buf[*pos] as char;
        if c.is_whitespace() {
            break;
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
        if is_op {
            break;
        }
    }
    if is_op {
        match OPS_STR.get(&token_str.as_str()) {
            Some(op_kind) => Ok(Some(Token {
                kind: TokenKind::Operator(*op_kind),
                val: None,
            })),
            None => unreachable!(),
        }
    } else {
        match KEYWORDS.get(&token_str.as_str()) {
            Some(keyword_kind) => Ok(Some(Token {
                kind: TokenKind::Keyword(*keyword_kind),
                val: None,
            })),
            None => {
                if is_str {
                    if str_closed {
                        Ok(Some(Token {
                            kind: TokenKind::Literal(LiteralKind::String),
                            val: Some(token_str),
                        }))
                    } else {
                        Err(InterpreterError::Lexer(
                            "Unclosed quotes while parsing string literal",
                        ))
                    }
                } else if first.is_ascii_digit() {
                    Ok(Some(Token {
                        kind: TokenKind::Literal(LiteralKind::Constant),
                        val: Some(token_str),
                    }))
                } else {
                    Ok(Some(Token {
                        kind: TokenKind::Identifier,
                        val: Some(token_str),
                    }))
                }
            }
        }
    }
}
