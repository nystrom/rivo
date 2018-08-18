use num::bigint::BigInt;
use num::rational::BigRational;

use syntax::loc::*;
use parser::tokens::Token;

use unicode_categories::UnicodeCategories;
use std::str::Chars;
use std::iter::Peekable;


// Based on:
// https://raw.githubusercontent.com/chr4/writing_an_interpreter_in_rust/master/src/lexer.rs

// TODO: how do we minimize allocations, both here and in the parser.
// Every clone() should be eliminated somehow.
// First, we need Tokens to implement Copy, but since they contain
// Strings we can't.
// They could contain offsets into the input buffer, however.

pub struct Lexer<'a> {
    // position of the lexer
    offset: usize,
    line: usize,
    column: usize,
    source: &'a str,

    // stack of brackets used for deciding when to insert a virtual ;
    stack: Vec<Token>,
    token_buffer: Vec<Located<Token>>,
    prev_token_can_end_statement: bool,
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, input: &'a str) -> Lexer<'a> {
        Lexer {
            offset: 0,
            line: 1,
            column: 1,
            source: source,
            stack: vec![],
            token_buffer: vec![],
            prev_token_can_end_statement: false,
            input: input.chars().peekable()
        }
    }

    fn newline(&mut self) {
        self.line += 1;
        self.column = 1;
    }

    fn read_char(&mut self) -> Option<char> {
        self.offset += 1;
        self.column += 1;
        self.input.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn peek_char_eq(&mut self, ch: char) -> bool {
        match self.peek_char() {
            Some(&peek_ch) => peek_ch == ch,
            None => false,
        }
    }

    fn locate(&mut self, token: Token, len: usize) -> Located<Token> {
        // println!("locate offset={} len={}", self.offset, len);
        use std::cmp::min;

        Located {
            loc: Loc {
                start: Pos {
                    offset: self.offset - min(len, self.offset),
                    line: self.line,
                    column: self.column - min(len, self.offset),
                },
                end: Pos {
                    offset: self.offset - min(1, self.offset),
                    line: self.line,
                    column: self.column - min(1, self.offset),
                },
                source: Some(String::from(self.source)),
            },
            value: token
        }
    }

    pub fn virtual_semi(&mut self) -> Located<Token> {
        let pos = Pos {
            offset: self.offset,
            line: self.line,
            column: self.column,
        };

        self.newline();

        let prev_can_end = self.prev_token_can_end_statement;
        let next_token = self.next_token();

        // println!("prev_can_end={:?} next={:?}", prev_can_end, next_token);

        // If we're in a block, generate a virtual semicolon.
        let mut insert_semi = self.stack.is_empty();
        if let Some(Token::Lc) = self.stack.last() {
            insert_semi = true;
        }

        if insert_semi && prev_can_end && can_start_statement(&next_token.value) {
            self.token_buffer.push(next_token);
            let semi = Located {
                loc: Loc { start: pos, end: pos, source: Some(String::from(self.source)) },
                value: Token::Semi
            };
            self.prev_token_can_end_statement = false;
            semi
        }
        else {
            next_token
        }
    }

    pub fn peek_token(&mut self) -> Located<Token> {
        if let Some(t) = self.token_buffer.first() {
            return t.clone()
        }

        let t = self.next_token();
        self.token_buffer.insert(0, t.clone());
        t
    }

    pub fn peek_token_value(&mut self) -> Token {
        self.peek_token().value
    }

    pub fn next_token(&mut self) -> Located<Token> {
        if ! self.token_buffer.is_empty() {
            let x = self.token_buffer.remove(0);
            self.prev_token_can_end_statement = can_end_statement(&x.value);
            return x
        }

        let x = match self.peek_char() {
            Some('\r') => {
                self.read_char();
                if self.peek_char_eq('\n') {
                    self.read_char();
                }
                self.virtual_semi()
            },
            Some('\n') => {
                self.read_char();
                self.virtual_semi()
            },
            Some('/') => {
                self.read_char();
                if self.peek_char_eq('/') {
                    self.read_char();
                    self.skip_to_newline();
                    self.next_token()
                }
                else if self.peek_char_eq('*') {
                    self.read_char();
                    self.skip_to_comment_end();
                    self.next_token()
                }
                else {
                    self.read_operator('/')
                }
            },
            Some('\t') => {
                self.read_char();
                self.next_token()
            },
            Some(' ') => {
                self.read_char();
                self.next_token()
            },
            Some(&ch) if ch.is_separator_space() => {
                self.read_char();
                self.next_token()
            },
            Some(&ch) => {
                let t = self.read_token();

                match t.value {
                    Token::Lb | Token::Lp | Token::Lc => {
                        self.stack.push(t.value.clone());
                        t
                    },
                    Token::Rb | Token::Rp | Token::Rc => {
                        // Pop the stack to the next matching delimiter, if any.
                        // We could just blindly pop, but this helps to localize parse errors
                        // when we have unbalanced parens.
                        while let Some(v) = self.stack.pop() {
                            match (v, &t.value) {                  // &t.value to let (,) borrow t
                                (Token::Lb, Token::Rb) => { break; },
                                (Token::Lc, Token::Rc) => { break; },
                                (Token::Lp, Token::Rp) => { break; },
                                _ => { },
                            }
                        }
                        t
                    },
                    _ => t
                }
            },
            None => {
                self.locate(Token::EOF, 0)
            },
        };

        self.prev_token_can_end_statement = can_end_statement(&x.value);
        x
    }

    fn read_token(&mut self) -> Located<Token> {
        match self.peek_char() {
            Some(';') => {
                self.read_char();
                self.locate(Token::Semi, 1)
            },
            Some(',') => {
                self.read_char();
                self.locate(Token::Comma, 1)
            },
            Some('@') => {
                self.read_char();
                self.locate(Token::At, 1)
            },
            Some('.') => {
                self.read_char();
                if self.peek_char_eq('.') {
                    self.read_operator('.')
                }
                else {
                    self.locate(Token::Dot, 1)
                }
            },
            Some('-') => {
                self.read_char();
                if self.peek_char_eq('>') {
                    self.read_char();
                    self.locate(Token::Arrow, 2)
                }
                else {
                    self.read_operator('-')
                }
            },
            Some('<') => {
                self.read_char();
                if self.peek_char_eq('-') {
                    self.read_char();
                    self.locate(Token::BackArrow, 2)
                }
                else {
                    self.read_operator('<')
                }
            },
            Some(':') => {
                self.read_char();
                if self.peek_char_eq('=') {
                    self.read_char();
                    self.locate(Token::Assign, 2)
                }
                else if let Some(&ch) = self.peek_char() {
                    if is_op_char(ch) {
                        self.read_operator(':')
                    }
                    else {
                        self.locate(Token::Colon, 1)
                    }
                }
                else {
                    self.locate(Token::Colon, 1)
                }
            },
            Some('!') => {
                self.read_char();
                if let Some(&ch) = self.peek_char() {
                    if is_op_char(ch) {
                        self.read_operator('!')
                    }
                    else {
                        self.locate(Token::Bang, 1)
                    }
                }
                else {
                    self.locate(Token::Bang, 1)
                }
            },
            Some('?') => {
                self.read_char();
                if let Some(&ch) = self.peek_char() {
                    if is_op_char(ch) {
                        self.read_operator('?')
                    }
                    else {
                        self.locate(Token::Question, 1)
                    }
                }
                else {
                    self.locate(Token::Question, 1)
                }
            },
            Some('=') => {
                self.read_char();
                if let Some(&ch) = self.peek_char() {
                    if is_op_char(ch) {
                        self.read_operator('=')
                    }
                    else {
                        self.locate(Token::Eq, 1)
                    }
                }
                else {
                    self.locate(Token::Eq, 1)
                }
            },
            Some('{') => {
                self.read_char();
                self.locate(Token::Lc, 1)
            },
            Some('}') => {
                self.read_char();
                self.locate(Token::Rc, 1)
            },
            Some('(') => {
                self.read_char();
                self.locate(Token::Lp, 1)
            },
            Some(')') => {
                self.read_char();
                self.locate(Token::Rp, 1)
            },
            Some('[') => {
                self.read_char();
                self.locate(Token::Lb, 1)
            },
            Some(']') => {
                self.read_char();
                self.locate(Token::Rb, 1)
            },
            Some('`') => {
                self.read_char();
                self.locate(Token::Tick, 1)
            },
            Some('\'') => {
                self.read_char_literal()
            },
            Some('"') => {
                self.read_string_literal()
            },
            Some('r') => {
                // r"..."
                self.read_char();
                match self.peek_char() {
                    Some('"') =>
                        self.read_raw_string_literal(),
                    _ =>
                        self.read_identifier('r')
                }
            },
            Some('0') => {
                self.read_char();
                self.read_zero()
            },
            Some(&ch) if ch.is_digit(10) => {
                self.read_dec()
            },
            Some(&ch) if is_id_start(ch) => {
                self.read_char();
                self.read_identifier(ch)
            },
            Some(&ch) if is_op_char(ch) => {
                self.read_char();
                self.read_operator(ch)
            },
            Some(&ch) => {
                self.read_char();
                self.locate(Token::UnexpectedChar(ch), 1)
            },
            None => {
                self.locate(Token::EOF, 0)
            },
        }
    }

    fn read_identifier(&mut self, first: char) -> Located<Token> {
        let mut text = String::new();
        text.push(first);

        while let Some(&ch) = self.peek_char() {
            if is_id_part(ch) {
                text.push(ch);
                self.read_char();
            }
            else {
                break;
            }
        }

        match text.as_str() {
            "_"      => self.locate(Token::Underscore, text.len()),
            "fun"    => self.locate(Token::Fun, text.len()),
            "trait"  => self.locate(Token::Trait, text.len()),
            "val"    => self.locate(Token::Val, text.len()),
            "var"    => self.locate(Token::Var, text.len()),
            "native" => self.locate(Token::Native, text.len()),
            "for"    => self.locate(Token::For, text.len()),
            "with"   => self.locate(Token::With, text.len()),
            "where"  => self.locate(Token::Where, text.len()),
            "import" => self.locate(Token::Import, text.len()),
            text     => self.locate(Token::Id(text.to_string()), text.len())
        }
    }

    fn read_operator(&mut self, first: char) -> Located<Token> {
        let mut text = String::new();
        text.push(first);

        while let Some(&ch) = self.peek_char() {
            if is_op_char(ch) {
                text.push(ch);
                self.read_char();
            }
            else {
                break;
            }
        }

        self.locate(Token::Op(text.to_string()), text.len())
    }

    fn read_zero(&mut self) -> Located<Token> {
        match self.peek_char() {
            Some('x') | Some('X') => {
                self.read_char();
                self.read_number(16)
            },
            Some('b') | Some('B') => {
                self.read_char();
                self.read_number(2)
            },
            Some('o') | Some('O') => {
                self.read_char();
                self.read_number(8)
            },
            _ => {
                self.locate(Token::Int(BigInt::from(0), String::from("0")), 1)
            },
        }
    }

    fn skip_digits(&mut self) {
        while let Some(&ch) = self.peek_char() {
            match from_digit(ch) {
                Some(n) if n < 10 => {
                    self.read_char();
                },
                _ => {
                    break;
                },
            }
        }
    }

    fn read_number(&mut self, radix: u32) -> Located<Token> {
        let mut text = String::new();

        text.push('0');
        match radix {
            2 => text.push('b'),
            8 => text.push('o'),
            16 => text.push('x'),
            r => panic!("unexpected radix {}", r),
        }

        let mut value = BigInt::from(0);

        while let Some(&ch) = self.peek_char() {
            match from_digit(ch) {
                Some(n) if n < radix => {
                    text.push(ch);
                    value *= radix;
                    value += n;
                    self.read_char();
                },
                Some(n) if n < 10 => {
                    self.skip_digits();
                    return self.locate(Token::BadInt, text.len())
                }
                _ => {
                    if text.len() == 2 {
                        // just saw 0b with no more digits
                        return self.locate(Token::BadInt, text.len())
                    }
                    break;
                }
            }
        }

        self.locate(Token::Int(value, text.to_string()), text.len())
    }

    fn read_dec(&mut self) -> Located<Token> {
        let mut text = String::new();

        let mut value = BigInt::from(0);

        while let Some(&ch) = self.peek_char() {
            match from_digit(ch) {
                Some(n) if n < 10 => {
                    text.push(ch);
                    value *= 10;
                    value += n;
                    self.read_char();
                },
                _ => {
                    if text.len() == 0 {
                        return self.locate(Token::BadInt, text.len())
                    }
                    break;
                }
            }
        }

        match self.peek_char() {
            Some('e') | Some('E') => {
                self.read_char();
                text.push('e');
                self.read_exponent(value, BigRational::from_integer(BigInt::from(0)), &mut text)
            },
            Some('.') => {
                self.read_char();
                text.push('.');
                self.read_frac(value, &mut text)
            },
            _ => {
                self.locate(Token::Int(value, text.to_string()), text.len())
            },
        }
    }

    fn read_frac(&mut self, int_part: BigInt, text: &mut String) -> Located<Token> {
        let mut num = BigInt::from(0);
        let mut denom = 1;

        while let Some(&ch) = self.peek_char() {
            match from_digit(ch) {
                Some(n) if n < 10 => {
                    text.push(ch);
                    num *= 10;
                    num += n;
                    denom *= 10;
                    self.read_char();
                },
                _ => break,
            }
        }

        // num / denom
        let frac_part = BigRational::new(num, BigInt::from(denom));

        match self.peek_char() {
            Some('e') | Some('E') => {
                self.read_char();
                text.push('e');
                self.read_exponent(int_part, frac_part, text)
            },
            _ => {
                self.locate(Token::Rat(mk_rat(int_part, frac_part, 0), text.to_string()), text.len())
            },
        }
    }

    fn read_exponent(&mut self, integer: BigInt, frac: BigRational, text: &mut String) -> Located<Token> {
        let mut value = 0;
        let mut sign = 1;

        if let Some(&ch) = self.peek_char() {
            match ch {
                '-' => {
                    self.read_char();
                    text.push('-');
                    sign = -1;
                },
                '+' => {
                    self.read_char();
                    text.push('+');
                },
                _ => {
                }
            }
        }

        while let Some(&ch) = self.peek_char() {
            match from_digit(ch) {
                Some(n) if n < 8 => {
                    text.push(ch);
                    value *= 10;
                    value += n;
                    self.read_char();
                },
                _ => break,
            }
        }

        match self.peek_char() {
            _ => {
                self.locate(Token::Rat(mk_rat(integer, frac, sign * (value as i32)), text.to_string()), text.len())
            },
        }
    }

    fn skip_to_newline(&mut self) {
        loop {
            match self.peek_char() {
                None => break,
                Some('\n') => break,
                Some('\r') => break,
                Some(&ch) => self.read_char(),
            };
        }
    }

    fn skip_to_comment_end(&mut self) {
        while let Some(&ch) = self.peek_char() {
            self.read_char();
            if ch == '*' {
                if let Some(&ch) = self.peek_char() {
                    if ch == '/' {
                        self.read_char();
                        return;
                    }
                }
            }
        }

        self.locate(Token::BadComment, 1);

        return;
    }

    fn read_char_literal(&mut self) -> Located<Token> {
        let start_offset = self.offset;

        // Read the open '
        self.read_char();

        let t = match self.peek_char() {
            Some('\\') => {
                match self.read_char_escape() {
                    Some(ch) => self.read_char_close(ch),
                    None => {
                        self.skip_to_char_close();
                        Token::BadChar
                    },
                }
            },
            Some('\'') => {
                self.read_char();
                Token::BadChar
            }
            Some(&ch) => {
                self.read_char();
                self.read_char_close(ch)
            },
            _ => {
                self.skip_to_char_close();
                Token::BadChar
            }
        };

        let end_offset = self.offset;

        self.locate(t, end_offset - start_offset)
    }

    fn skip_to_char_close(&mut self) {
        loop {
            match self.peek_char() {
                Some('\'') => {
                    self.read_char();
                    break;
                },
                Some('\r') => {
                    break;
                },
                Some('\n') => {
                    break;
                },
                Some(&ch) => {
                    self.read_char();
                },
                None => {
                    break;
                },
            }
        }
    }

    fn skip_to_string_close(&mut self) {
        loop {
            match self.peek_char() {
                Some('"') => {
                    self.read_char();
                    break;
                },
                Some('\r') => {
                    break;
                },
                Some('\n') => {
                    break;
                },
                Some(&ch) => {
                    self.read_char();
                },
                None => {
                    break;
                },
            }
        }
    }

    fn read_char_close(&mut self, ch: char) -> Token {
        match self.peek_char() {
            Some('\'') => {
                self.read_char();
                Token::Char(ch)
            },
            Some(&ch) => {
                self.skip_to_char_close();
                Token::BadChar
            }
            None => {
                Token::BadChar
            }
        }
    }

    fn read_char_escape(&mut self) -> Option<char> {
        self.read_char();
        match self.peek_char() {
            Some('n') => {
                self.read_char();
                Some('\n')
            },
            Some('r') => {
                self.read_char();
                Some('\r')
            },
            Some('t') => {
                self.read_char();
                Some('\t')
            },
            // Some('f') => {
            //     self.read_char();
            //     Some('\f')
            // },
            // Some('b') => {
            //     self.read_char();
            //     Some('\b')
            // },
            Some('\'') => {
                self.read_char();
                Some('\'')
            },
            Some('"') => {
                self.read_char();
                Some('"')
            },
            Some('\\') => {
                self.read_char();
                Some('\\')
            },
            Some('x') => {
                self.read_char();
                self.read_num_escape(16, 2)
            },
            Some('u') => {
                self.read_char();
                self.read_num_escape(16, 4)
            }
            Some('0') | Some('1') | Some('2') | Some('3') => {
                self.read_num_escape(8, 3)
            },
            _ => {
                None
            },
        }
        // FIXME: handle char escapes
        // FIXME: allow HTML escapes too (as {\name})
    }

    fn read_num_escape(&mut self, radix: u32, len: usize) -> Option<char> {
        let mut n = 0;

        for i in 0..len {
            match self.read_char() {
                Some(ch) =>
                    match from_digit(ch) {
                        Some(v) if v < radix => {
                            n *= radix;
                            n += v;
                        },
                        _ =>
                            return None
                    },
                None =>
                    return None
            }
        };

        use std::char;
        char::from_u32(n)
    }

    fn read_string_literal(&mut self) -> Located<Token> {
        let mut text = String::new();
        let mut fail = false;

        let start_offset = self.offset;

        // Read the open "
        self.read_char();

        loop {
            match self.peek_char() {
                Some('\\') => {
                    match self.read_char_escape() {
                        Some(ch) => {
                            text.push(ch);
                        },
                        None => {
                            self.skip_to_string_close();
                            fail = true;
                            break;
                        },
                    }
                },
                Some('"') => {
                    self.read_char();
                    break;
                }
                Some('\r') => {
                    fail = true;
                    break;
                }
                Some('\n') => {
                    fail = true;
                    break;
                }
                Some(&ch) => {
                    self.read_char();
                    text.push(ch);
                },
                None => {
                    fail = true;
                    break;
                }
            };
        }

        let end_offset = self.offset;

        if fail {
            self.locate(Token::BadString, end_offset - start_offset)
        }
        else {
            self.locate(Token::String(text.clone()), end_offset - start_offset)
        }

        // FIXME: handle string escapes
        // FIXME: allow HTML escapes too (as \{name})
        // FIXME: handle string interpolation (as {expression})
        // FIXME: handle raw strings """...""" (the number of " allows one fewer inside the string)
    }

    fn read_raw_string_literal(&mut self) -> Located<Token> {
        let mut text = String::new();
        let mut fail = false;

        // we've already read the r
        let start_offset = self.offset - 1;

        // Read the open "
        self.read_char();

        loop {
            match self.peek_char() {
                Some('"') => {
                    self.read_char();
                    break;
                }
                Some('\\') => {
                    // the only escape is \"
                    self.read_char();
                    match self.peek_char() {
                        Some('"') => {
                            self.read_char();
                            text.push('"');
                        },
                        _ => {
                            text.push('\\');
                        }
                    }
                }
                Some(&ch) => {
                    self.read_char();
                    text.push(ch);
                },
                None => {
                    fail = true;
                    break;
                }
            };
        }

        let end_offset = self.offset;

        if fail {
            self.locate(Token::BadString, end_offset - start_offset)
        }
        else {
            self.locate(Token::String(text.clone()), end_offset - start_offset)
        }

        // FIXME: handle string escapes
        // FIXME: allow HTML escapes too (as \{name})
        // FIXME: handle string interpolation (as {expression})
        // FIXME: handle raw strings """...""" (the number of " allows one fewer inside the string)
    }
}

// Utility functions that don't depend on the Lexer state.

fn from_digit(ch: char) -> Option<u32> {
    if '0' <= ch && ch <= '9' {
        return Some( (ch as u32) - ('0' as u32) )
    }
    else if 'a' <= ch && ch <= 'f' {
        return Some( (ch as u32) - ('a' as u32) + 10 )
    }
    else if 'A' <= ch && ch <= 'F' {
        return Some( (ch as u32) - ('A' as u32) + 10 )
    }
    None
}

fn is_id_start(ch: char) -> bool {
    ch == '⊤' ||
    ch == '⊥' ||
    ch == '_' ||
    ch.is_letter_titlecase() ||
    ch.is_letter_uppercase() ||
    ch.is_letter_lowercase() ||
    ch.is_letter_other()
}

fn is_id_part(ch: char) -> bool {
    ch.is_number() ||
    ch.is_letter_modifier() ||
    is_id_start(ch) ||
    ch == '\''
}

fn is_op_char(ch: char) -> bool {
    (
        ch.is_symbol_currency() ||
        ch.is_symbol_math() ||
        ch.is_symbol_other() ||
        ch.is_punctuation() ||
        ch == '^'
    ) && ! (
        ch == '(' || ch == ')' ||
        ch == '{' || ch == '}' ||
        ch == '[' || ch == ']' ||
        ch == ';' ||
        ch == ',' ||
        ch.is_punctuation_open() ||
        ch.is_punctuation_close() ||
        is_id_part(ch)
    )
}

fn mk_rat(i: BigInt, f: BigRational, e: i32) -> BigRational {
    let ten = BigRational::from_integer(BigInt::from(10));

    if e > 0 {
        let r = mk_rat(i, f, e-1);
        let v = r * ten;
        return v
    }
    else if e < 0 {
        let r = mk_rat(i, f, e+1);
        let v = r / ten;
        return v
    }

    BigRational::from(i) + f
}

fn can_start_statement(t: &Token) -> bool {
    match t {
        Token::EOF => false,
        Token::Rb => false,
        Token::Rc => false,
        Token::Rp => false,
        _ => ! is_infix(t),
    }
}

fn can_end_statement(t: &Token) -> bool {
    match t {
        Token::EOF => false,
        Token::Lc => false,
        Token::Lp => false,
        Token::Lb => false,
        Token::Val => false,
        Token::Var => false,
        Token::Fun => false,
        Token::Trait => false,
        Token::Import => false,
        Token::For => false,
        _ => ! is_infix(t),
    }
}

fn is_infix(t: &Token) -> bool {
    match t {
        Token::Arrow => true,
        Token::Assign => true,
        Token::At => true,
        Token::BackArrow => true,
        Token::Colon => true,
        Token::Comma => true,
        Token::Dot => true,
        Token::Eq => true,
        Token::Semi => true,
        Token::With => true,
        Token::Where => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use num::bigint::BigInt;
    use num::Num;
    use num::rational::BigRational;
    use syntax::loc::*;
    use parser::tokens::Token;
    use parser::lex::Lexer;

    macro_rules! assert_tokens {
        ($s: expr, $($tokens: expr),+) => {
            let mut lex = Lexer::new("foo.ivo", $s);
            assert_tokens_with_lex!(lex, $($tokens),+);
        };
    }

    macro_rules! assert_tokens_with_lex {
        ($lex: expr, $token: expr) => {
            assert_eq!($lex.next_token().value, $token);
        };
        ($lex: expr, $token: expr, $($tokens: expr),+) => {
            assert_eq!($lex.next_token().value, $token);
            assert_tokens_with_lex!($lex, $($tokens),+);
        };
    }

    #[test]
    fn test_builtin_operators() {
        assert_tokens!(
            "-> := @ <- ! : , . = ? ; ` [] {} ()"
            , Token::Arrow
            , Token::Assign
            , Token::At
            , Token::BackArrow
            , Token::Bang
            , Token::Colon
            , Token::Comma
            , Token::Dot
            , Token::Eq
            , Token::Question
            , Token::Semi
            , Token::Tick
            , Token::Lb
            , Token::Rb
            , Token::Lc
            , Token::Rc
            , Token::Lp
            , Token::Rp
            , Token::EOF
        );
    }

    #[test]
    fn test_operators() {
        assert_tokens!(
            "= := += == *= === : := < << <= <<< <-"
            , Token::Eq
            , Token::Assign
            , Token::Op(String::from("+="))
            , Token::Op(String::from("=="))
            , Token::Op(String::from("*="))
            , Token::Op(String::from("==="))
            , Token::Colon
            , Token::Assign
            , Token::Op(String::from("<"))
            , Token::Op(String::from("<<"))
            , Token::Op(String::from("<="))
            , Token::Op(String::from("<<<"))
            , Token::BackArrow
            , Token::EOF
        );
    }

    #[test]
    fn test_keywords() {
        assert_tokens!(
            "_ for fun import native val var trait with where"
            , Token::Underscore
            , Token::For
            , Token::Fun
            , Token::Import
            , Token::Native
            , Token::Val
            , Token::Var
            , Token::Trait
            , Token::With
            , Token::Where
            , Token::EOF
        );
    }

    #[test]
    fn test_identifiers_and_keywords() {
        assert_tokens!(
            "xyzzy foo _1 _ for fun import native val var x x0 ⊥ ⊤ trait with"
            , Token::Id(String::from("xyzzy"))
            , Token::Id(String::from("foo"))
            , Token::Id(String::from("_1"))
            , Token::Underscore
            , Token::For
            , Token::Fun
            , Token::Import
            , Token::Native
            , Token::Val
            , Token::Var
            , Token::Id(String::from("x"))
            , Token::Id(String::from("x0"))
            , Token::Id(String::from("⊥"))
            , Token::Id(String::from("⊤"))
            , Token::Trait
            , Token::With
            , Token::EOF
        );
    }

    #[test]
    fn test_arith_1() {
        assert_tokens!(
            "1+2"
            , Token::Int(BigInt::from(1), String::from("1"))
            , Token::Op(String::from("+"))
            , Token::Int(BigInt::from(2), String::from("2"))
            , Token::EOF
        );
    }

    #[test]
    fn test_lambda_1() {
        assert_tokens!(
            "fun (x) (y) -> 2*x+y"
            , Token::Fun
            , Token::Lp
            , Token::Id(String::from("x"))
            , Token::Rp
            , Token::Lp
            , Token::Id(String::from("y"))
            , Token::Rp
            , Token::Arrow
            , Token::Int(BigInt::from(2), String::from("2"))
            , Token::Op(String::from("*"))
            , Token::Id(String::from("x"))
            , Token::Op(String::from("+"))
            , Token::Id(String::from("y"))
            , Token::EOF
        );
    }

    #[test]
    fn test_dec() {
        assert_tokens!(
            "0 1 2 3 4 5 6 7 8 9 10 100 1001 10191 123456789012345678901234567890"
            , Token::Int(BigInt::from(0), String::from("0"))
            , Token::Int(BigInt::from(1), String::from("1"))
            , Token::Int(BigInt::from(2), String::from("2"))
            , Token::Int(BigInt::from(3), String::from("3"))
            , Token::Int(BigInt::from(4), String::from("4"))
            , Token::Int(BigInt::from(5), String::from("5"))
            , Token::Int(BigInt::from(6), String::from("6"))
            , Token::Int(BigInt::from(7), String::from("7"))
            , Token::Int(BigInt::from(8), String::from("8"))
            , Token::Int(BigInt::from(9), String::from("9"))
            , Token::Int(BigInt::from(10), String::from("10"))
            , Token::Int(BigInt::from(100), String::from("100"))
            , Token::Int(BigInt::from(1001), String::from("1001"))
            , Token::Int(BigInt::from(10191), String::from("10191"))
            , Token::Int(Num::from_str_radix("123456789012345678901234567890", 10).unwrap(), String::from("123456789012345678901234567890"))
            , Token::EOF
        );
    }

    #[test]
    fn test_oct() {
        assert_tokens!(
            "0 0o0 0o1 0o2 0o7 0o8 0o377"
            , Token::Int(BigInt::from(0), String::from("0"))
            , Token::Int(BigInt::from(0), String::from("0o0"))
            , Token::Int(BigInt::from(1), String::from("0o1"))
            , Token::Int(BigInt::from(2), String::from("0o2"))
            , Token::Int(BigInt::from(7), String::from("0o7"))
        // will fail at 0o8
            , Token::BadInt
            , Token::Int(BigInt::from(255), String::from("0o377"))
            , Token::EOF
        );
    }

    #[test]
    fn test_bin() {
        assert_tokens!(
            "0 0b0 0b1 0b1001 0b1010101010101010 0b123"
            , Token::Int(BigInt::from(0), String::from("0"))
            , Token::Int(BigInt::from(0), String::from("0b0"))
            , Token::Int(BigInt::from(1), String::from("0b1"))
            , Token::Int(BigInt::from(9), String::from("0b1001"))
            , Token::Int(BigInt::from(0xaaaau32), String::from("0b1010101010101010"))
        // will fail at 0b123
            , Token::BadInt
            , Token::EOF
        );
    }

    #[test]
    fn test_hex() {
        assert_tokens!(
            "0 0x0 0xf 0xff 0x1 0x 0x123456789abcdef0"
            , Token::Int(BigInt::from(0), String::from("0"))
            , Token::Int(BigInt::from(0), String::from("0x0"))
            , Token::Int(BigInt::from(0xf), String::from("0xf"))
            , Token::Int(BigInt::from(0xff), String::from("0xff"))
            , Token::Int(BigInt::from(1), String::from("0x1"))
            , Token::BadInt
            , Token::Int(BigInt::from(0x123456789abcdef0u64), String::from("0x123456789abcdef0"))
            , Token::EOF
        );
    }

    #[test]
    fn test_rats() {
        assert_tokens!(
            "0 1 2 3.14 1e0 1e1 1e6 1.0e6 1.e6 1.e+6 1e+6 1e-6 12. 12.0"
            , Token::Int(BigInt::from(0), String::from("0"))
            , Token::Int(BigInt::from(1), String::from("1"))
            , Token::Int(BigInt::from(2), String::from("2"))
            , Token::Rat(BigRational::new(BigInt::from(314), BigInt::from(100)), String::from("3.14"))

            , Token::Rat(BigRational::new(BigInt::from(1), BigInt::from(1)), String::from("1e0"))

            , Token::Rat(BigRational::new(BigInt::from(10), BigInt::from(1)), String::from("1e1"))
            , Token::Rat(BigRational::new(BigInt::from(1000000), BigInt::from(1)), String::from("1e6"))
            , Token::Rat(BigRational::new(BigInt::from(1000000), BigInt::from(1)), String::from("1.0e6"))
            , Token::Rat(BigRational::new(BigInt::from(1000000), BigInt::from(1)), String::from("1.e6"))
            , Token::Rat(BigRational::new(BigInt::from(1000000), BigInt::from(1)), String::from("1.e+6"))
            , Token::Rat(BigRational::new(BigInt::from(1000000), BigInt::from(1)), String::from("1e+6"))
            , Token::Rat(BigRational::new(BigInt::from(1), BigInt::from(1000000)), String::from("1e-6"))
            , Token::Rat(BigRational::new(BigInt::from(12), BigInt::from(1)), String::from("12."))
            , Token::Rat(BigRational::new(BigInt::from(12), BigInt::from(1)), String::from("12.0"))
            , Token::EOF
        );
    }

    #[test]
    fn test_chars() {
        assert_tokens!(
            r#"'a' 'b' 'c' '' 'foo' '\n' '\x00' '\u0000' ' '"#
            , Token::Char('a')
            , Token::Char('b')
            , Token::Char('c')
            , Token::BadChar
            , Token::BadChar
            , Token::Char('\n')
            , Token::Char('\u{0000}')
            , Token::Char('\u{0000}')
            , Token::Char(' ')
            , Token::EOF
        );
    }

    #[test]
    fn test_strings() {
        assert_tokens!(
            r#" "hello" "" "\n" "\x00" "\u0000\u00000000" " " r"hello world\n" r"one line \"
            another line
            " "#
            , Token::String(String::from("hello"))
            , Token::String(String::from(""))
            , Token::String(String::from("\n"))
            , Token::String(String::from("\u{0000}"))
            , Token::String(String::from("\u{0000}\u{0000}0000"))
            , Token::String(String::from(" "))
            , Token::String(String::from("hello world\\n"))
            , Token::String(String::from("one line \"\n            another line\n            "))
            , Token::EOF
        );
    }

    #[test]
    fn test_virtual_semis() {
        assert_tokens!(
            r#"
                trait Foo {
                    fun f (x) -> x
                    fun g (x) -> (x)
                    trait Bar {
                        val x
                        val y
                    }
                }
            "#
            , Token::Trait
            , Token::Id(String::from("Foo"))
            , Token::Lc
            , Token::Fun
            , Token::Id(String::from("f"))
            , Token::Lp
            , Token::Id(String::from("x"))
            , Token::Rp
            , Token::Arrow
            , Token::Id(String::from("x"))
            , Token::Semi
            , Token::Fun
            , Token::Id(String::from("g"))
            , Token::Lp
            , Token::Id(String::from("x"))
            , Token::Rp
            , Token::Arrow
            , Token::Lp
            , Token::Id(String::from("x"))
            , Token::Rp
            , Token::Semi
            , Token::Trait
            , Token::Id(String::from("Bar"))
            , Token::Lc
            , Token::Val
            , Token::Id(String::from("x"))
            , Token::Semi
            , Token::Val
            , Token::Id(String::from("y"))
            , Token::Rc
            , Token::Rc
            , Token::EOF
        );
    }

}
