use num::bigint::BigInt;
use num::rational::BigRational;
use std::fmt::Debug;

use syntax::loc::*;
use syntax::trees::*;
use syntax::names::*;

use parser::tokens::*;
use parser::lex::Lexer;
use parser::lex::LexError;

use trace::trace;
trace::init_depth_var!();

// TODO: parse groups of definitions directly rather than building a dummy trait.
// TODO: rename Trait to Record and add a tag, generate by the parser.
// TODO: support pragmas. Add pragmas to the Def ASTs.
// #(prio 0)
// pragmas must be stable paths

// Turns a PResult<T> into a PResult<Located<T>>.
macro_rules! located_ok {
    ($parser: expr, $body: expr) => {
        {
            let first_token = $parser.lookahead()?;
            let v = $body?;
            let last_token = &$parser.last_token;
            // println!("first token {:?} {:?}", first_token.loc, first_token.value);
            // println!("last token {:?} {:?}", last_token.loc, last_token.value);

            Ok(Located::new(
                Loc::span_from(&first_token, &last_token),
                v
            ))
        }
    };
}

// Turns a T into a Located<T>.
macro_rules! located {
    ($parser: expr, $body: expr) => {
        {
            let first_token = $parser.lookahead()?;
            let v = $body;
            let last_token = &$parser.last_token;
            Located::new(
                Loc::span_from(&first_token, &last_token),
                v
            )
        }
    };
}

macro_rules! consume_or_else {
    ($parser: expr, $expected: pat, $body: expr, $otherwise: expr) => {
        match *$parser.lookahead()? {
            $expected => {
                $parser.eat();
                $body
            },
            _ => {
                $otherwise
            }
        }
    }
}

macro_rules! consume {
    ($parser: expr, $expected: expr) => {
        {
            let t = $parser.lookahead()?;
            // $expected is a expr, not a pattern so we have to compare using ==
            if *t == $expected {
                $parser.eat();
                Ok({})
            }
            else {
                $parser.error_unexpected(vec![$expected], &t)
            }
        }
    }
}

macro_rules! consume_without_check {
    ($parser: expr, $expected: expr) => {
        assert_eq!(*$parser.lookahead()?, $expected);
        $parser.eat();
    }
}

macro_rules! filter_collect_loc {
    ($vec: expr, $pat: pat, $exp: expr) => {
        $vec.into_iter()
            .flat_map(|l| {
                match l {
                    Located { loc, value: $pat } =>
                        vec!(Located { loc: loc.clone(), value: $exp }),
                    _ =>
                        vec![],
                }
            })
            .collect();
    };
}

#[derive(Debug)]
enum MixfixParam {
    Name(Part),
    Param(Located<Param>),
}

type PResult<A> = Result<A, Located<String>>;

#[derive(Debug)]
pub struct Parser<'a> {
    pub lex: Lexer<'a>,
    pub last_token: Located<Token>,
    pub errors: Vec<Located<String>>,
    pub node_id_generator: &'a mut NodeIdGenerator,
}

// #[cfg_attr(debug_assertions, cfg_attr(not(test), trace))]
impl<'a> Parser<'a> {
    pub fn new(source: &'a Source, input: &'a str, node_id_generator: &'a mut NodeIdGenerator) -> Parser<'a> {
        Parser::new_from_lexer(Lexer::new(source, input), node_id_generator)
    }

    pub fn new_from_lexer(lex: Lexer<'a>, node_id_generator: &'a mut NodeIdGenerator) -> Parser<'a> {
        Parser {
            lex,
            last_token: Located {
                loc: NO_LOC,
                value: Token::EOF
            },
            errors: vec![],
            node_id_generator,
        }
    }

    fn push_back(&mut self, token: Located<Token>) {
        self.lex.push_back(token);
    }

    fn lookahead(&mut self) -> PResult<Located<Token>> {
        match self.lex.peek_token() {
            Err(Located { loc, value: LexError::BadInt }) => {
                self.error(loc, "Invalid integer literal.")
            },
            Err(Located { loc, value: LexError::BadChar }) => {
                self.error(loc, "Invalid character literal.")
            },
            Err(Located { loc, value: LexError::BadString }) => {
                self.error(loc, "Invalid string literal.")
            },
            Err(Located { loc, value: LexError::BadRat }) => {
                self.error(loc, "Invalid rational literal.")
            },
            Err(Located { loc, value: LexError::BadComment }) => {
                self.error(loc, "Invalid comment.")
            },
            Err(Located { loc, value: LexError::UnexpectedChar(ch) }) => {
                self.error(loc, &format!("Unexpected character '{}'.", ch))
            },
            Ok(t) => {
                Ok(t)
            },
        }
    }

    fn error_unexpected<T: Debug>(&mut self, expected: Vec<Token>, got: &Located<Token>) -> PResult<T> {
        if expected.is_empty() {
            let msg = format!("Unexpected token {}.", got.value);
            self.error(got.loc, &msg)
        }
        else if expected.len() == 1 {
            let msg = format!("Unexpected token {}, expected {}.", got.value, expected.first().unwrap());
            self.error(got.loc, &msg)
        }
        else {
            let msg = format!("Unexpected token {}, expected one of {}.", got.value, TokenVec(expected));
            self.error(got.loc, &msg)
        }
    }

    fn error_here<T: Debug>(&mut self, msg: &str) -> PResult<T> {
        let t = self.lookahead()?;
        self.error(t.loc, msg)
    }

    fn error<T: Debug>(&mut self, loc: Loc, msg: &str) -> PResult<T> {
        let lmsg = Located { loc, value: String::from(msg) };
        self.errors.push(lmsg.clone());
        // panic!("{:?}", lmsg);
        Err(lmsg)
    }

    fn error_void(&mut self, loc: Loc, msg: &str) {
        let lmsg = Located { loc, value: String::from(msg) };
        self.errors.push(lmsg.clone());
        // panic!("{:?}", lmsg);
    }

    fn error_string<T: Debug>(&mut self, lmsg: Located<String>) -> PResult<T> {
        self.errors.push(lmsg.clone());
        Err(lmsg)
    }

    fn eat(&mut self) {
        match self.lex.next_token() {
            Err(Located { loc, value: LexError::BadInt }) => {
                self.error_void(loc, "Invalid integer literal.");
            },
            Err(Located { loc, value: LexError::BadChar }) => {
                self.error_void(loc, "Invalid character literal.");
            },
            Err(Located { loc, value: LexError::BadString }) => {
                self.error_void(loc, "Invalid string literal.");
            },
            Err(Located { loc, value: LexError::BadRat }) => {
                self.error_void(loc, "Invalid rational literal.");
            },
            Err(Located { loc, value: LexError::BadComment }) => {
                self.error_void(loc, "Invalid comment.");
            },
            Err(Located { loc, value: LexError::UnexpectedChar(ch) }) => {
                self.error_void(loc, format!("Unexpected character '{}'.", ch).as_str());
            },
            Ok(t) => {
                self.last_token = t;
            },
        }
    }

    pub fn parse_bundle(&mut self) -> PResult<Located<Root>> {
        use crate::parser::grammar::*;
        let p = RootParser::new();
        let mut err: Vec<Located<String>> = vec![];
        let lex = self.lex.clone();
        let mut lex2 = lex.clone();
        while let Some(t) = lex2.next() {
            println!("{:?}", t);
        }
        let t = p.parse(
            &mut err, 
            lex.map(|Located { loc, value }| (loc.start_loc(), value, loc.end_loc()))
        );
        match t {
            Ok(t) => {
                if err.is_empty() {
                    println!("{:?}", t);
                }
                else {
                    println!("parse failed nicely {:?}", err);
                }
            },
            e => {
                println!("parse failed {:?} {:?}", e, err);
            }
        }

        located_ok!(self, {
            let id = self.alloc_node_id();
            let cmds = self.parse_cmds()?;
            Ok(Root::Bundle { id, cmds })
        })
    }

    fn parse_block(&mut self) -> PResult<Vec<Located<Cmd>>> {
        consume!(self, Token::Lc)?;
        let cmds = self.parse_cmds()?;
        consume!(self, Token::Rc)?;
        Ok(cmds)
    }

    fn parse_cmds(&mut self) -> PResult<Vec<Located<Cmd>>> {
        let mut cmds = Vec::new();

        loop {
            match *self.lookahead()? {
                Token::EOF => {
                    break;
                },
                Token::Rc => {
                    break;
                },
                Token::Semi => {
                    // Eat any semicolons.
                    self.eat();
                },
                _ => {
                    match self.parse_cmd() {
                        Ok(cs) => {
                            for c in cs {
                                cmds.push(c);
                            }

                            // A command should be followed by a ; or } or EOF.
                            let t = self.lookahead()?;
                            match *t {
                                Token::Semi => {},
                                Token::Rc => {},
                                Token::EOF => {},
                                _ => {
                                    // FIXME: this error is wrong since if we forget an `=` before block in a fun or trait def,
                                    // we'll pop out to here. The error should really be about the missing `=`.
                                    // For now, just add the `=` as an expected token.
                                    return self.error_unexpected(vec![Token::Eq, Token::Semi, Token::Rc, Token::EOF], &t);
                                },
                            }
                        },
                        Err(msg) => {
                            // Skip to the next ; or } or EOF
                            loop {
                                match *self.lookahead()? {
                                    Token::Semi => break,
                                    Token::Rc => break,
                                    Token::EOF => break,
                                    _ => {
                                        self.eat();
                                    },
                                }
                            }

                            return self.error_string::<Vec<Located<Cmd>>>(msg);
                        }
                    }
                },
            }
        }

        Ok(cmds)
    }

    fn parse_attr(&mut self) -> PResult<Located<Attr>> {
        located_ok!(self, {
            match *self.lookahead()? {
                Token::Arrow => { self.eat(); Ok(Attr::Arrow) },
                Token::Assign => { self.eat(); Ok(Attr::Assign) },
                Token::At => { self.eat(); Ok(Attr::At) },
                Token::Bang => { self.eat(); Ok(Attr::Bang) },
                Token::Backarrow => { self.eat(); Ok(Attr::Backarrow) },
                Token::Colon => { self.eat(); Ok(Attr::Colon) },
                Token::Comma => { self.eat(); Ok(Attr::Comma) },
                Token::Dot => { self.eat(); Ok(Attr::Dot) },
                Token::Eq => { self.eat(); Ok(Attr::Eq) },
                Token::Hash => { self.eat(); Ok(Attr::Hash) },
                Token::Question => { self.eat(); Ok(Attr::Question) },
                Token::Semi => { self.eat(); Ok(Attr::Semi) },

                Token::For => { self.eat(); Ok(Attr::For) },
                Token::Fun => { self.eat(); Ok(Attr::Fun) },
                Token::Import => { self.eat(); Ok(Attr::Import) },
                Token::Let => { self.eat(); Ok(Attr::Let) },
                Token::Val => { self.eat(); Ok(Attr::Val) },
                Token::Var => { self.eat(); Ok(Attr::Var) },
                Token::Trait => { self.eat(); Ok(Attr::Trait) },
                Token::With => { self.eat(); Ok(Attr::With) },
                Token::Where => { self.eat(); Ok(Attr::Where) },

                Token::Tick => { let x = self.parse_mixfix_name()?; Ok(Attr::Name { name: x }) },
                Token::Id(ref s) => { self.eat(); Ok(Attr::Name { name: Name::Id(Interned::new(&s)) }) },
                Token::Op(ref s) => { self.eat(); Ok(Attr::Name { name: Name::Op(Interned::new(&s)) }) },

                Token::Char(ref v) => { self.eat(); Ok(Attr::Lit { lit: Lit::Char { value: *v } }) },
                Token::Rat(ref v, _) => { self.eat(); Ok(Attr::Lit { lit: Lit::Rat { value: v.clone() } }) },
                Token::Int(ref v, _) => { self.eat(); Ok(Attr::Lit { lit: Lit::Int { value: v.clone() } }) },
                Token::String(ref v) => { self.eat(); Ok(Attr::Lit { lit: Lit::String { value: v.clone() } }) },
                Token::Underscore => { self.eat(); Ok(Attr::Lit { lit: Lit::Wildcard }) },

                Token::Lb => {
                    consume!(self, Token::Lb)?;
                    let first_loc = self.lookahead()?.loc;
                    let es = self.parse_attr_tuple(Token::Rb)?;
                    consume!(self, Token::Rb)?;
                    let last_loc = self.last_token.loc;
                    let loc = Loc::span(first_loc, last_loc);
                    match es.as_slice() {
                        [] => Ok(Attr::Brackets(Box::new(Located::new(loc, Attr::CommaSeq(vec![]))))),
                        [e] => Ok(Attr::Brackets(Box::new(e.clone()))),
                        es => Ok(Attr::Brackets(Box::new(Located::new(loc, Attr::CommaSeq(es.to_vec()))))),
                    }
                },
                Token::Lc => {
                    consume!(self, Token::Lc)?;
                    let first_loc = self.lookahead()?.loc;
                    let es = self.parse_attr_tuple(Token::Rc)?;
                    consume!(self, Token::Rc)?;
                    let last_loc = self.last_token.loc;
                    let loc = Loc::span(first_loc, last_loc);
                    match es.as_slice() {
                        [] => Ok(Attr::Braces(Box::new(Located::new(loc, Attr::CommaSeq(vec![]))))),
                        [e] => Ok(Attr::Braces(Box::new(e.clone()))),
                        es => Ok(Attr::Braces(Box::new(Located::new(loc, Attr::CommaSeq(es.to_vec()))))),
                    }
                },
                Token::Lp => {
                    consume!(self, Token::Lp)?;
                    if *self.lookahead()? == Token::Rp {
                        self.eat();
                        Ok(Attr::Lit { lit: Lit::Nothing })
                    }
                    else {
                        let first_loc = self.lookahead()?.loc;
                        let es = self.parse_attr_tuple(Token::Rp)?;
                        consume!(self, Token::Rp)?;
                        let last_loc = self.last_token.loc;
                        let loc = Loc::span(first_loc, last_loc);
                        match es.as_slice() {
                            [] => Ok(Attr::Parens(Box::new(Located::new(loc, Attr::CommaSeq(vec![]))))),
                            [e] => Ok(Attr::Parens(Box::new(e.clone()))),
                            es => Ok(Attr::Parens(Box::new(Located::new(loc, Attr::CommaSeq(es.to_vec()))))),
                        }
                    }
                },

                Token::EOF => { self.error_here("Unexpected end of file.") }
                ref t => { self.error_here(&format!("Unexpected token {}.", t)) }
            }
        })
    }

    fn parse_attrs(&mut self) -> PResult<Located<Attr>> {
        let e = located_ok!(self, {
            let mut attrs = Vec::new();

            loop {
                match *self.lookahead()? {
                    Token::Rb | Token::Rc | Token::Rp | Token::Comma => { break; },
                    _ => {},
                }
                let attr = self.parse_attr()?;
                attrs.push(attr);
            }

            if attrs.len() == 1 {
                Ok(attrs.first().unwrap().value.clone())
            }
            else {
                Ok(Attr::Seq(attrs))
            }
        })?;

        Ok(e)
    }

    fn parse_cmd(&mut self) -> PResult<Vec<Located<Cmd>>> {
        let mut attrs = Vec::new();

        while let Token::Hash = *self.lookahead()? {
            self.eat();
            let t = self.lookahead()?;
            match *t {
                Token::Lb | Token::Lc | Token::Lp => {
                    let attr = self.parse_attr()?;
                    attrs.push(attr);
                }
                _ => {
                    self.error_unexpected(vec![Token::Lb, Token::Lc, Token::Lp], &t)?;
                }
            }

            // Eat optional semicolons between attributes and before the command.
            while let Token::Semi = *self.lookahead()? {
                self.eat();
            }
        }

        match *self.lookahead()? {
            Token::Fun => {
                let cmd = self.parse_fun_cmd(attrs)?;
                Ok(vec![cmd])
            },
            Token::Trait => {
                let r = self.parse_trait_def(attrs)?;
                Ok(vec![r.map(|d| Cmd::Def(d))])
            },
            Token::Val => {
                let r = self.parse_val_def(attrs)?;
                Ok(vec![r.map(|d| Cmd::Def(d))])
            },
            Token::Var => {
                let r = self.parse_var_def(attrs)?;
                Ok(vec![r.map(|d| Cmd::Def(d))])
            },
            Token::Import => {
                if ! attrs.is_empty() {
                    self.error_here("Unexpected token `import`: import statements cannot have attributes.")?;
                }
                let r = self.parse_import_def()?;
                Ok(r.iter().map(|Located { loc, value }| Located { loc: *loc, value: Cmd::Def(value.clone()) }).collect())
            },
            _ => {
                if ! attrs.is_empty() {
                    self.error_here("Unexpected token. Expressions cannot have attributes.")?;
                }
                let r = self.parse_exp()?;
                Ok(vec![r.map(|e| Cmd::Exp(e))])
            },
        }
    }

    fn parse_mixfix_return(&mut self, mode_given: bool) -> PResult<(bool, Located<Param>)> {
        let first_token = self.lookahead()?;

        let (mode_given2, param) = match self.lookahead()? {
            Located { loc, value: Token::Lp } => {
                self.eat();
                match *self.lookahead()? {
                    Token::Question => {
                        self.eat();
                        let e = self.parse_tuple()?;
                        consume!(self, Token::Rp)?;
                        Ok(
                            (
                                true,
                                Param {
                                    attr: ParamAttr {
                                        assoc: Assoc::NonAssoc,
                                        by_name: CallingConv::ByValue,
                                        mode: CallingMode::Input,
                                    },
                                    pat: Box::new(e),
                                }
                            )
                        )
                    },
                    Token::Bang => {
                        self.eat();
                        let e = self.parse_tuple()?;
                        consume!(self, Token::Rp)?;
                        Ok(
                            (
                                true,
                                Param {
                                    attr: ParamAttr {
                                        assoc: Assoc::NonAssoc,
                                        by_name: CallingConv::ByValue,
                                        mode: CallingMode::Output,
                                    },
                                    pat: Box::new(e),
                                }
                            )
                        )
                    },
                    _ => {
                        if mode_given {
                            return self.error_here("Invalid return attribute. Expected an attribute mode, either `!` or `?`.")
                        }

                        // We have the beginning of an expression.
                        // We should parse as an expression, but we've already eaten the (.
                        // This may consume past the end of the tuple we're starting.
                        self.push_back(Located { loc, value: Token::Lp });

                        let e = self.parse_exp()?;

                        Ok(
                            (
                                false,
                                Param {
                                    attr: ParamAttr {
                                        assoc: Assoc::NonAssoc,
                                        by_name: CallingConv::ByValue,
                                        mode: CallingMode::Output,
                                    },
                                    pat: Box::new(e),
                                }
                            )
                        )
                    }
                }
            },
            Located { loc, value: Token::Lc } => {
                self.eat();
                match *self.lookahead()? {
                    Token::Question => {
                        self.eat();
                        let e = self.parse_tuple()?;
                        consume!(self, Token::Rc)?;
                        Ok(
                            (
                                true,
                                Param {
                                    attr: ParamAttr {
                                        assoc: Assoc::NonAssoc,
                                        by_name: CallingConv::ByName,
                                        mode: CallingMode::Input,
                                    },
                                    pat: Box::new(e),
                                }
                            )
                        )
                    },
                    _ => {
                        if mode_given {
                            return self.error_here("Invalid return attribute. Call-by-name attributes must have an output mode. Expected `?`.")
                        }

                        // We have the beginning of a layout block or struct block.
                        // We should parse as an expression, but we've already eaten the {.
                        self.push_back(Located { loc, value: Token::Lc });

                        let e = self.parse_exp()?;

                        Ok(
                            (
                                false,
                                Param {
                                    attr: ParamAttr {
                                        assoc: Assoc::NonAssoc,
                                        by_name: CallingConv::ByValue,
                                        mode: CallingMode::Output,
                                    },
                                    pat: Box::new(e),
                                }
                            )
                        )
                    }
                }
            },
            _ => {
                if mode_given {
                    return self.error_here("Invalid return attribute. Expected `(` or `{` and an attribute mode.")
                }

                let e = self.parse_exp()?;

                Ok(
                    (
                        false,
                        Param {
                            attr: ParamAttr {
                                assoc: Assoc::NonAssoc,
                                by_name: CallingConv::ByValue,
                                mode: CallingMode::Output,
                            },
                            pat: Box::new(e),
                        }
                    )
                )
            }
        }?;

        Ok( (mode_given2, Located::new(Loc::span_from(&first_token, &self.last_token), param) ) )
    }

    // where Exp
    fn parse_opt_guard(&mut self) -> PResult<Option<Located<Exp>>> {
        consume_or_else!(self,
            Token::Where,
            {
                let e = self.parse_exp0()?;
                Ok(Some(e))
            },
            {
                Ok(None)
            }
        )
    }

    // returns the vec of mixfix elements plus a bool indicating if any element specified a mode
    // (in which case the return must also specify a mode)
    fn parse_mixfix_elements(&mut self) -> PResult<(bool, Vec<MixfixParam>)> {
        let mut mode_given = false;
        let mut elements = Vec::new();

        loop {
            match *self.lookahead()? {
                Token::Id(ref s) => {
                    self.eat();
                    let e = MixfixParam::Name(Part::Id(Interned::new(s)));
                    elements.push(e);
                },
                Token::Op(ref s) => {
                    self.eat();
                    let e = MixfixParam::Name(Part::Op(Interned::new(s)));
                    elements.push(e);
                },
                Token::Bang => {
                    self.eat();
                    let e = MixfixParam::Name(Part::Op(Interned::new("!")));
                    elements.push(e);
                },
                Token::Question => {
                    self.eat();
                    let e = MixfixParam::Name(Part::Op(Interned::new("?")));
                    elements.push(e);
                },
                Token::Lp => {
                    let p = located!(self, {
                        self.eat();
                        match *self.lookahead()? {
                            Token::Lp => {
                                self.eat();
                                match *self.lookahead()? {
                                    Token::Bang => {
                                        mode_given = true;
                                        self.eat();
                                        let e = self.parse_tuple()?;
                                        consume!(self, Token::Rp)?;
                                        consume!(self, Token::Rp)?;
                                        Param {
                                            attr: ParamAttr {
                                                assoc: Assoc::Assoc,
                                                by_name: CallingConv::ByValue,
                                                mode: CallingMode::Output,
                                            },
                                            pat: Box::new(e),
                                        }
                                    },
                                    Token::Question => {
                                        mode_given = true;
                                        self.eat();
                                        let e = self.parse_tuple()?;
                                        consume!(self, Token::Rp)?;
                                        consume!(self, Token::Rp)?;
                                        Param {
                                            attr: ParamAttr {
                                                assoc: Assoc::Assoc,
                                                by_name: CallingConv::ByValue,
                                                mode: CallingMode::Input,
                                            },
                                            pat: Box::new(e),
                                        }
                                    },
                                    _ => {
                                        if mode_given {
                                            return self.error_here("Invalid function header. Either all attributes, or none, must have a mode.")
                                        }
                                        let e = self.parse_tuple()?;
                                        consume!(self, Token::Rp)?;
                                        consume!(self, Token::Rp)?;
                                        Param {
                                            attr: ParamAttr {
                                                assoc: Assoc::Assoc,
                                                by_name: CallingConv::ByValue,
                                                mode: CallingMode::Input,
                                            },
                                            pat: Box::new(e),
                                        }
                                    },
                                }
                            },
                            Token::Bang => {
                                mode_given = true;
                                self.eat();
                                let e = self.parse_tuple()?;
                                consume!(self, Token::Rp)?;
                                Param {
                                    attr: ParamAttr {
                                        assoc: Assoc::NonAssoc,
                                        by_name: CallingConv::ByValue,
                                        mode: CallingMode::Output,
                                    },
                                    pat: Box::new(e),
                                }
                            },
                            Token::Question => {
                                mode_given = true;
                                self.eat();
                                let e = self.parse_tuple()?;
                                consume!(self, Token::Rp)?;
                                Param {
                                    attr: ParamAttr {
                                        assoc: Assoc::NonAssoc,
                                        by_name: CallingConv::ByValue,
                                        mode: CallingMode::Input,
                                    },
                                    pat: Box::new(e),
                                }
                            },
                            _ => {
                                if mode_given {
                                    return self.error_here("Invalid function header. Either all attributes, or none, must have a mode.")
                                }
                                let e = self.parse_tuple()?;
                                consume!(self, Token::Rp)?;
                                Param {
                                    attr: ParamAttr {
                                        assoc: Assoc::NonAssoc,
                                        by_name: CallingConv::ByValue,
                                        mode: CallingMode::Input,
                                    },
                                    pat: Box::new(e),
                                }
                            },
                        }
                    });
                    elements.push(MixfixParam::Param(p));
                },
                Token::Lc => {
                    let p = located!(self, {
                        self.eat();
                        match *self.lookahead()? {
                            Token::Lc => {
                                self.eat();
                                match *self.lookahead()? {
                                    Token::Bang => {
                                        mode_given = true;
                                        self.eat();
                                        let e = self.parse_tuple()?;
                                        consume!(self, Token::Rc)?;
                                        consume!(self, Token::Rc)?;
                                        Param {
                                            attr: ParamAttr {
                                                assoc: Assoc::Assoc,
                                                by_name: CallingConv::ByName,
                                                mode: CallingMode::Output,
                                            },
                                            pat: Box::new(e),
                                        }
                                    },
                                    Token::Question => {
                                        mode_given = true;
                                        self.eat();
                                        let e = self.parse_tuple()?;
                                        consume!(self, Token::Rc)?;
                                        consume!(self, Token::Rc)?;
                                        Param {
                                            attr: ParamAttr {
                                                assoc: Assoc::Assoc,
                                                by_name: CallingConv::ByName,
                                                mode: CallingMode::Input,
                                            },
                                            pat: Box::new(e),
                                        }
                                    },
                                    _ => {
                                        if mode_given {
                                            return self.error_here("Invalid function header. Either all attributes, or none, must have a mode.")
                                        }
                                        let e = self.parse_tuple()?;
                                        consume!(self, Token::Rc)?;
                                        consume!(self, Token::Rc)?;
                                        Param {
                                            attr: ParamAttr {
                                                assoc: Assoc::Assoc,
                                                by_name: CallingConv::ByName,
                                                mode: CallingMode::Input,
                                            },
                                            pat: Box::new(e),
                                        }
                                    },
                                }
                            },
                            Token::Bang => {
                                mode_given = true;
                                self.eat();
                                let e = self.parse_tuple()?;
                                consume!(self, Token::Rc)?;
                                Param {
                                    attr: ParamAttr {
                                        assoc: Assoc::NonAssoc,
                                        by_name: CallingConv::ByName,
                                        mode: CallingMode::Output,
                                    },
                                    pat: Box::new(e),
                                }
                            },
                            Token::Question => {
                                mode_given = true;
                                self.eat();
                                let e = self.parse_tuple()?;
                                consume!(self, Token::Rc)?;
                                Param {
                                    attr: ParamAttr {
                                        assoc: Assoc::NonAssoc,
                                        by_name: CallingConv::ByName,
                                        mode: CallingMode::Input,
                                    },
                                    pat: Box::new(e),
                                }
                            },
                            _ => {
                                if mode_given {
                                    return self.error_here("Invalid function header. Either all attributes, or none, must have a mode.")
                                }
                                let e = self.parse_tuple()?;
                                consume!(self, Token::Rc)?;
                                Param {
                                    attr: ParamAttr {
                                        assoc: Assoc::NonAssoc,
                                        by_name: CallingConv::ByName,
                                        mode: CallingMode::Input,
                                    },
                                    pat: Box::new(e),
                                }
                            },
                        }
                    });
                    elements.push(MixfixParam::Param(p));
                },
                _ => {
                    break;
                },
            }
        }

        Ok((mode_given, elements))
    }

    fn parse_fun_cmd(&mut self, attrs: Vec<Located<Attr>>) -> PResult<Located<Cmd>> {
        located_ok!(self, {
            consume_without_check!(self, Token::Fun);

            let (mode_given, elements) = self.parse_mixfix_elements()?;
            let opt_guard = self.parse_opt_guard()?;

            let mut has_name = false;

            for element in &elements {
                match element {
                    MixfixParam::Name(_) => { has_name = true; },
                    _ => {},
                }
            }

            let params = Parser::make_params(&elements);

            if ! has_name && ! mode_given {
                // We have a lambda.
                let mut lambda_params = Vec::new();

                for param in params {
                    match *param {
                        Param { attr: ParamAttr { assoc: Assoc::Assoc, .. }, .. } => {
                            return self.error(param.loc, "Anonymous function parameter cannot be associative.")
                        },
                        Param { attr: ParamAttr { by_name: CallingConv::ByName, .. }, .. } => {
                            return self.error(param.loc, "Anonymous function parameter cannot be call-by-name.")
                        },
                        Param { attr: ParamAttr { mode: CallingMode::Output, .. }, .. } => {
                            return self.error(param.loc, "Anonymous function parameter cannot be an output parameter.")
                        },
                        Param { pat: ref exp, .. } => {
                            // copy the location from the Param.
                            let unboxed: Located<Exp> = *exp.clone();
                            lambda_params.push(unboxed.with_loc(param.loc));
                        },
                    }
                }

                consume!(self, Token::Arrow)?;

                let body = self.parse_exp()?;

                let id = self.alloc_node_id();

                Ok(Cmd::Exp(
                    Exp::Lambda {
                        id,
                        opt_guard: opt_guard.map(|e| Box::new(e)),
                        params: lambda_params,
                        ret: Box::new(body),
                    }
                ))
            }
            else {
                // We have a function definition.
                let name = Parser::make_mixfix_name(&elements);

                let id = self.alloc_node_id();

                match *self.lookahead()? {
                    Token::Eq => {
                        self.eat();

                        let (mode_given_in_ret, e) = self.parse_mixfix_return(mode_given)?;

                        // If a mode was given or there are no parameters, we require a mode.
                        let opt_body = if mode_given || mode_given_in_ret {
                            self.parse_opt_guard()?
                        }
                        else {
                            None
                        };

                        Ok(Cmd::Def(Def::FunDef {
                            id,
                            attrs,
                            name,
                            opt_guard: opt_guard.map(|e| Box::new(e)),
                            params,
                            ret: e,
                            opt_body: opt_body.map(|e| Box::new(e)),
                        }))
                    },
                    _ => {
                        // Just the function signature (without return).
                        // We add a () guard to make sure the function cannot be
                        // invoked.

                        let nothing = Located {
                            loc: Loc::from(&self.last_token),
                            value: Exp::Lit { lit: Lit::Nothing },
                        };

                        let opt_guard2 = match opt_guard {
                            Some(g) => Some(g),
                            None => Some(nothing.clone())
                        };

                        Ok(Cmd::Def(Def::FunDef {
                            id,
                            attrs,
                            name,
                            opt_guard: opt_guard2.map(|e| Box::new(e)),
                            params,
                            ret: Parser::make_param_from_exp(nothing, CallingMode::Output),
                            opt_body: None,
                        }))
                    },
                }
            }
        })
    }

    fn parse_val_def(&mut self, attrs: Vec<Located<Attr>>) -> PResult<Located<Def>> {
        located_ok!(self, {
            assert_eq!(*self.lookahead()?, Token::Val);
            self.eat();
            let e = self.parse_exp()?;
            Ok(Def::FormulaDef {
                attrs,
                flag: FormulaFlag::Val,
                formula: Box::new(e)
            })
        })
    }

    fn parse_var_def(&mut self, attrs: Vec<Located<Attr>>) -> PResult<Located<Def>> {
        located_ok!(self, {
            assert_eq!(*self.lookahead()?, Token::Var);
            self.eat();
            let e = self.parse_exp()?;
            Ok(Def::FormulaDef {
                attrs,
                flag: FormulaFlag::Var,
                formula: Box::new(e)
            })
        })
    }

    fn parse_import_def(&mut self) -> PResult<Vec<Located<Def>>> {
        assert_eq!(*self.lookahead()?, Token::Import);
        self.eat();
        let e = self.parse_exp()?;
        self.convert_imports(e)
    }

    /// Flatten an expression used as a namespace into a vector of expressions.
    /// The main transformation is to treat . as a cross product:
    /// (a, b).(x, y) --> a.x, b.x, a.y, b.y

    fn flatten_exp(&mut self, e: Located<Exp>) -> Vec<Located<Exp>> {
        match e.value {
            Exp::Tuple { es } => es.iter().flat_map(|e| self.flatten_exp(e.clone())).collect(),
            Exp::Union { box e1, box e2 } => [e1, e2].iter().flat_map(|e| self.flatten_exp(e.clone())).collect(),
            Exp::Select { exp, name } => {
                self.flatten_exp(*exp).iter().map(
                    |e| Located::new(e.loc, Exp::Select { exp: box e.clone(), name })
                ).collect()
            },
            Exp::Within { id, e1, e2 } => {
                let v1 = self.flatten_exp(*e1);
                let v2 = self.flatten_exp(*e2);
                let id = self.alloc_node_id();
                let loc = e.loc;
                v1.iter().flat_map(
                    |e1| v2.iter().map(
                        move |e2| Located::new(loc, Exp::Within { id, e1: box e1.clone(), e2: box e2.clone() })
                    )
                ).collect()
            },
            _ => vec![e]
        }
    }

    fn convert_imports(&mut self, e: Located<Exp>) -> PResult<Vec<Located<Def>>> {
        let mut defs = vec![];

        for e1 in self.flatten_exp(e) {
            let def = self.convert_import(None, e1)?;
            defs.push(def);
        }

        Ok(defs)
    }

    fn convert_import(&mut self, opt_path: Option<Located<Exp>>, e: Located<Exp>) -> PResult<Located<Def>> {
        let r = match e.value {
            Exp::Lit { lit: Lit::Nothing } => {
                Located::new(e.loc,
                    Def::ImportDef { opt_path: opt_path.map(|p| box p), selector: Selector::Nothing })
            },
            Exp::Lit { lit: Lit::Wildcard } => {
                Located::new(e.loc,
                    Def::ImportDef { opt_path: opt_path.map(|p| box p), selector: Selector::All })
            },
            Exp::Name { name, .. } => {
                Located::new(e.loc,
                    Def::ImportDef { opt_path: opt_path.map(|p| box p), selector: Selector::Including { name } })
            }
            Exp::Arrow { id, arg, ret } => {
                match self.convert_import(opt_path, *arg)? {
                    Located { loc, value: Def::ImportDef { opt_path, selector: Selector::Including { name: x } } } => {
                        match *ret {
                            Located { value: Exp::Name { name: y, .. }, .. } =>
                                Located::new(e.loc,
                                    Def::ImportDef { opt_path, selector: Selector::Renaming { name: x, rename: y }}),
                            Located { value: Exp::Lit { lit: Lit::Nothing }, .. } =>
                                Located::new(e.loc,
                                    Def::ImportDef { opt_path, selector: Selector::Excluding { name: x }}),
                            _ => {
                                return Err(Located::new(e.loc, "Invalid renaming import. Missing new name.".to_owned()));
                            }
                        }
                    }
                    _ => {
                        return Err(Located::new(e.loc, "Invalid renaming import. Missing old name.".to_owned()));
                    }
                }
            },
            Exp::Select { exp, name } => {
                match opt_path {
                    None => {
                        let id = self.alloc_node_id();
                        self.convert_import(Some(*exp), Located::new(e.loc, Exp::Name { id, name }))?
                    },
                    Some(e0) => {
                        let id = self.alloc_node_id();
                        self.convert_import(Some(Located::new(e.loc, Exp::Within { id, e1: box e0, e2: exp })), Located::new(e.loc, Exp::Name { id, name }))?
                    },
                }
            },
            Exp::Within { id, e1, e2 } => {
                match opt_path {
                    None => {
                        self.convert_import(Some(*e1), *e2)?
                    },
                    Some(e0) => {
                        let id = self.alloc_node_id();
                        self.convert_import(Some(Located::new(e.loc, Exp::Within { id, e1: box e0, e2: e1 })), *e2)?
                    },
                }
            },
            _ => {
                return Err(Located::new(e.loc, "Invalid import namespace. Expected one of `()`, `_`, or a stable path.".to_owned()));
            },
        };

        Ok(r)
    }

    // A trait has a block for a body.
    fn parse_trait_def(&mut self, attrs: Vec<Located<Attr>>) -> PResult<Located<Def>> {
        located_ok!(self, {
            consume_without_check!(self, Token::Trait);

            let (mode_given, elements) = self.parse_mixfix_elements()?;
            let opt_guard = self.parse_opt_guard()?;

            let mut has_name = false;

            for element in &elements {
                match element {
                    MixfixParam::Name(_) => { has_name = true; },
                    _ => {},
                }
            }

            let params = Parser::make_params(&elements);

            if ! has_name {
                return self.error_here("Missing struct name.")
            }

            if mode_given {
                return self.error_here("A struct cannot have attribute modes.")
            }

            for p in &params {
                if p.attr.mode == CallingMode::Output {
                    return self.error_here("A struct cannot have output attributes.")
                }
            }

            // We have a valid struct definition.
            let name = Parser::make_mixfix_name(&elements);

            let id = self.alloc_node_id();

            let mut supers = vec![];

            while let Token::With = *self.lookahead()? {
                self.eat();
                let s = self.parse_primary()?;
                match s {
                    Located { loc, value: Exp::Tuple { es } } => {
                        for e in es {
                            supers.push(e);
                        }
                    },
                    Located { loc, value: Exp::Union { box e1, box e2 } } => {
                        supers.push(e1);
                        supers.push(e2);
                    },
                    e => {
                        supers.push(e);
                    }
                }
            }

            let defs = match *self.lookahead()? {
                Token::Eq => {
                    self.eat();

                    let loc = self.lookahead()?.loc;

                    let cmds = self.parse_block()?;

                    for cmd in &cmds {
                        match **cmd {
                            Cmd::Exp(_) => {
                                return self.error_here("A struct block cannot contain expressions.")
                            },
                            _ => {},
                        }
                    }

                    filter_collect_loc!(cmds, Cmd::Def(d), d)
                },
                _ => {
                    vec![]
                }
            };

            Ok(Def::TraitDef {
                id,
                attrs,
                name,
                opt_guard: opt_guard.map(|e| box e),
                params,
                defs: defs,
                supers: supers.clone()
            })
        })
    }

    fn parse_exp(&mut self) -> PResult<Located<Exp>> {
        located_ok!(self, {
            let left = self.parse_exp0()?;

            match *self.lookahead()? {
                Token::Assign => {
                    self.eat();
                    let right = self.parse_exp()?;
                    Ok(Exp::Assign { lhs: box left, rhs: box right })
                },
                Token::Eq => {
                    self.eat();
                    let right = self.parse_exp()?;
                    Ok(Exp::Bind { lhs: box left, rhs: box right })
                },
                Token::Backarrow => {
                    self.eat();
                    let right = self.parse_exp()?;
                    Ok(Exp::Generator { lhs: box left, rhs: box right })
                },
                Token::Arrow => {
                    self.eat();
                    let right = self.parse_exp()?;
                    Ok(Exp::Arrow { id: self.alloc_node_id(), arg: box left, ret: box right })
                },
                Token::With => {
                    self.eat();
                    let right = self.parse_exp()?;
                    Ok(Exp::Union { e1: box left, e2: box right })
                },
                Token::Where => {
                    self.eat();
                    let right = self.parse_exp()?;
                    Ok(Exp::Where { pat: box left, guard: box right })
                },
                _ => {
                    Ok(left.value)
                },
            }
        })
    }

    fn parse_exp0(&mut self) -> PResult<Located<Exp>> {
        located_ok!(self, {
            let left = self.parse_mixfix_exp()?;

            match *self.lookahead()? {
                Token::At => {
                    self.eat();
                    let right = self.parse_exp()?;
                    Ok(Exp::Intersect { e1: box left, e2: box right })
                },
                Token::Arrow => {
                    self.eat();
                    let right = self.parse_exp()?;
                    Ok(Exp::Arrow {
                        id : self.alloc_node_id(),
                        arg: box left,
                        ret: box right
                    })
                },
                _ => {
                    Ok(left.value)
                },
            }
        })
    }

    fn parse_mixfix_exp(&mut self) -> PResult<Located<Exp>> {
        match *self.lookahead()? {
            Token::For => {
                located_ok!(self, {
                    self.eat();

                    let id = self.alloc_node_id();
                    let formula = self.parse_select()?;
                    let body = self.parse_exp()?;

                    Ok(Exp::For {
                        id,
                        formula: box formula,
                        body: box body
                    })
                })
            },
            Token::Let => {
                located_ok!(self, {
                    self.eat();

                    let id = self.alloc_node_id();
                    let formula = self.parse_select()?;
                    let body = self.parse_exp()?;

                    Ok(Exp::Let {
                        id,
                        formula: box formula,
                        body: box body
                    })
                })
            },
            Token::Var => {
                located_ok!(self, {
                    self.eat();

                    let id = self.alloc_node_id();
                    let formula = self.parse_select()?;
                    let body = self.parse_exp()?;

                    Ok(Exp::LetVar {
                        id,
                        formula: box formula,
                        body: box body
                    })
                })
            },
            Token::Fun => {
                located_ok!(self, {
                    self.eat();

                    let id = self.alloc_node_id();
                    let mut params = Vec::new();

                    while *self.lookahead()? == Token::Lp {
                        let arg = located_ok!(self, self.parse_tuple_exp())?;
                        params.push(arg);
                    }

                    let opt_guard = self.parse_opt_guard()?;
                    consume!(self, Token::Arrow)?;
                    let body = self.parse_exp()?;

                    Ok(Exp::Lambda {
                        id,
                        opt_guard: opt_guard.map(|e| box e),
                        params,
                        ret: box body,
                    })
                })
            },
            _ => {
                located_ok!(self, {
                    let mut es = vec![];

                    loop {
                        let first = self.parse_select()?;
                        es.push(first);

                        // If the lookahead can start a primary expression,
                        // parse recursively.
                        match *self.lookahead()? {
                            Token::Lc | Token::Lp | Token::Lb | Token::Id(_) | Token::Op(_) | Token::Tick | Token::Int(_, _) | Token::Rat(_, _) | Token::String(_) | Token::Char(_) | Token::Bang | Token::Question | Token::Underscore => {
                                continue;
                            },
                            _ => {
                                break;
                            }
                        }
                    }

                    match es.first() {
                        Some(first) => {
                            if es.len() == 1 {
                                Ok(first.value.clone())
                            }
                            else {
                                let id = self.alloc_node_id();
                                Ok(Exp::MixfixApply { id, es })
                            }
                        },
                        None => {
                            unreachable!()
                        },
                    }
                })
            },
        }
    }

    fn parse_select(&mut self) -> PResult<Located<Exp>> {
        let exp = self.parse_primary()?;

        match *self.lookahead()? {
            Token::Dot =>
                self.parse_selectors(exp),
            _ =>
                Ok(exp),
        }
    }

    fn parse_selectors(&mut self, left: Located<Exp>) -> PResult<Located<Exp>> {
        consume_without_check!(self, Token::Dot);

        match *self.lookahead()? {
            Token::Id(_) | Token::Op(_) | Token::Tick | Token::Bang | Token::Question => {
                // In this case we have e.x, e.+, e.`foo _`.
                // Here we should just do a normal select.
                let right = self.parse_primary()?;

                // clone the locs now so we can move left and right.
                let loc = Loc::span_from(&left, &right);

                let sel = match *right {
                    Exp::Name { ref name, .. } =>
                        Ok(Located::new(
                            loc,
                            Exp::Select {
                                exp: box left,
                                name: *name
                            })),
                    ref t =>
                        self.error(loc, "Invalid selection expression. Expected a name.")
                }?;

                match *self.lookahead()? {
                    Token::Dot => self.parse_selectors(sel),
                    _ => Ok(sel)
                }
            },
            Token::Underscore | Token::Lp => {
                let id = self.alloc_node_id();

                let right = self.parse_primary()?;
                let loc = Loc::span_from(&left, &right);

                let lsel = Located::new(
                    loc,
                    Exp::Within {
                        id,
                        e1: box left,
                        e2: box right,
                    });

                match *self.lookahead()? {
                    Token::Dot => self.parse_selectors(lsel),
                    _ => Ok(lsel)
                }
            },
            _ =>
                self.error_here("Invalid select expression. Expected an identifier, operator, `_`, or `(`"),
        }
    }

    fn parse_mixfix_name(&mut self) -> PResult<Name> {
        consume_without_check!(self, Token::Tick);

        let mut parts = Vec::new();

        loop {
            match *self.lookahead()? {
                Token::Id(ref s) => {
                    self.eat();
                    parts.push(Part::Id(Interned::new(s)))
                },
                Token::Op(ref s) => {
                    self.eat();
                    parts.push(Part::Op(Interned::new(s)))
                },
                Token::Underscore => {
                    self.eat();
                    parts.push(Part::Placeholder)
                },
                Token::Tick => {
                    break;
                },
                _ => {
                    return self.error_here("Invalid mixfix name. Expected identifier, operator or `_`.")
                }
            }
        }

        consume!(self, Token::Tick)?;

        match parts[..] {
            [] =>
                self.error_here("Mixfix names cannot be empty."),
            [Part::Id(ref s)] =>
                Ok(Name::Id(*s)),
            [Part::Op(ref s)] =>
                Ok(Name::Op(*s)),
            _ =>
                Ok(Name::Mixfix(Name::encode_parts(&parts)))
        }

    }

    // () is nothing
    // (e) is just e
    // (e1, e2) is a tuple
    fn parse_tuple_exp(&mut self) -> PResult<Exp> {
        consume_without_check!(self, Token::Lp);

        match *self.lookahead()? {
            Token::Rp => {
                // () is nothing
                self.eat();
                Ok(Exp::Lit { lit: Lit::Nothing })
            },
            _ => {
                let first = self.parse_exp()?;

                let t = self.lookahead()?;

                match *t {
                    Token::Rp => {
                        // (e) is just e
                        self.eat();
                        Ok(first.value)
                    },
                    Token::Comma => {
                        // (e1, e2)
                        self.eat();

                        let mut es = Vec::new();
                        es.push(first);

                        loop {
                            if *self.lookahead()? == Token::Rp {
                                break;
                            }

                            let e = self.parse_exp()?;
                            es.push(e);

                            if *self.lookahead()? == Token::Comma {
                                self.eat();
                            }
                            else {
                                break;
                            }
                        }

                        consume!(self, Token::Rp)?;

                        Ok(self.make_tuple(es))
                    },
                    _ => {
                        self.error_unexpected(vec![Token::Comma, Token::Rp], &t)
                    },
                }
            },
        }
    }

    // () is nothing
    // (e) is just e
    // (e1, e2) is a tuple
    fn parse_tuple(&mut self) -> PResult<Located<Exp>> {
        located_ok!(self, {
            match *self.lookahead()? {
                Token::Rp | Token::Rb | Token::Rc => {
                    Ok(Exp::Lit { lit: Lit::Nothing })
                },
                _ => {
                    let first = self.parse_exp()?;

                    match *self.lookahead()? {
                        Token::Comma => {
                            // (e1, e2)
                            self.eat();

                            let mut es = Vec::new();
                            es.push(first);

                            loop {
                                let e = self.parse_exp()?;
                                es.push(e);

                                if *self.lookahead()? == Token::Comma {
                                    self.eat();
                                }
                                else {
                                    break;
                                }
                            }

                            Ok(self.make_tuple(es))
                        },
                        ref t => {
                            Ok(first.value)
                        },
                    }
                },
            }
        })
    }

    fn make_tuple(&self, es: Vec<Located<Exp>>) -> Exp {
        Exp::Tuple { es }
        /*
        let n = es.len();
        assert!(n >= 2);

        // Make the name Tuple _ _ _ (for n)
        let parts = vec![];
        parts.push(Part::Id(Interned::new("Tuple")));
        for _ in &es {
            parts.push(Part::Placeholder);
        }
        let name = Name::Mixfix(Name::encode_parts(parts));

        let root = Located::new(Loc::no_loc(), Exp::Global);
        let prelude = Located::new(Loc::no_loc(), Exp::Select { exp: global, name: Name::Id(Interned::new("Prelude")) });
        let tag = Located::new(Loc::no_loc(), Exp::Select { exp: prelude, name: name });

        let fields = vec![];
        for (i, e) in es.iter().enumerate() {
            fields.push(Field { name: Name::Id(Interned::new(format!("_{}", i+1))), value: e });
        }
        Exp::RecordLit { tag, fields }
        */

        // FIXME: add record literals.
        // Tags are just names, not expressions. trait arguments don't change the tag.
        // If we overload a trait it might have different fields! This is bad, potentially.
        // Trait declared #unique don't have this problem.
        //
        // trait Cons (head) (tail)
        // is sugar for:
        // trait Cons (x) (xs) = `Cons _ _` { head: x, tail: xs }
        //
        // trait Nil
        // is sugar for:
        // trait Nil = Nil { }
        //
        // trait Foo = { }
        // is sugar for:
        // trait Foo = Foo { }
    }

    // () is nothing
    // (e) is just e
    // (e1, e2) is a tuple
    fn parse_attr_tuple(&mut self, end_token: Token) -> PResult<Vec<Located<Attr>>> {
        if *self.lookahead()? == end_token {
            return Ok(vec![]);
        }

        let first = self.parse_attrs()?;

        match *self.lookahead()? {
            Token::Comma => {
                // (e1, e2)
                self.eat();

                let mut es = Vec::new();
                es.push(first);

                loop {
                    let e = self.parse_attrs()?;
                    es.push(e);

                    if *self.lookahead()? == Token::Comma {
                        self.eat();
                    }
                    else {
                        break;
                    }
                }

                Ok(es)
            },
            ref t => {
                Ok(vec![first])
            },
        }
    }

    fn parse_list_exp(&mut self) -> PResult<Exp> {
        consume_without_check!(self, Token::Lb);

        let mut es = Vec::new();

        match *self.lookahead()? {
            Token::Rb => {
            },
            _ => {
                let first = self.parse_exp()?;

                match *self.lookahead()? {
                    Token::Comma => {
                        self.eat();
                        es.push(first);

                        while let Token::Comma = *self.lookahead()? {
                            self.eat();
                            if *self.lookahead()? != Token::Rb {
                                let e = self.parse_exp()?;
                                es.push(e);
                            }
                        }
                    },
                    _ => {
                    },
                }
            },
        }

        consume!(self, Token::Rb)?;

        Ok(Exp::List { es })
    }

    fn parse_primary(&mut self) -> PResult<Located<Exp>> {
        located_ok!(self, {
            match *self.lookahead()? {
                Token::Dot => {
                    let loc = self.last_token.loc;
                    Ok(self.parse_selectors(Located::new(loc, Exp::Root))?.value)
                },
                Token::Underscore => {
                    self.eat();
                    Ok(Exp::Lit { lit: Lit::Wildcard })
                },
                Token::Int(ref n, _) => {
                    self.eat();
                    Ok(Exp::Lit { lit: Lit::Int { value: n.clone() } })
                },
                Token::Rat(ref n, _) => {
                    self.eat();
                    Ok(Exp::Lit { lit: Lit::Rat { value: n.clone() } })
                },
                Token::String(ref n) => {
                    self.eat();
                    Ok(Exp::Lit { lit: Lit::String { value: n.clone() } })
                },
                Token::Char(ref n) => {
                    self.eat();
                    Ok(Exp::Lit { lit: Lit::Char { value: *n } })
                },
                Token::Lp => {
                    self.parse_tuple_exp()
                },
                Token::Lb => {
                    self.parse_list_exp()
                }
                Token::Lc => {
                    let loc = self.lookahead()?.loc;

                    let cmds = self.parse_block()?;

                    let all_arrows = cmds.iter().all(|cmd|
                        match &cmd.value {
                            Cmd::Exp(Exp::Arrow { .. }) => true,
                            _ => false,
                        }
                    );

                    // TODO:
                    // Get rid of formula defs.
                    // Flatten into a let expression.
                    // What about var and sequences? We need blocks.

                    if all_arrows {
                        // All the commands are arrows.
                        // Create a union of functions.
                        let arrows: Vec<Located<Exp>> = filter_collect_loc!(cmds, Cmd::Exp(e),
                            match e {
                                Exp::Arrow { id, box arg, box ret } =>
                                    Exp::Lambda {
                                        id,
                                        opt_guard: None,
                                        params: vec![arg],
                                        ret: box ret
                                    },
                                _ => panic!("not all arrows after all")
                            }
                        );

                        if let Some((hd, tl)) = arrows.split_first() {
                            let Located { value: e, .. } = tl.iter().fold(hd.clone(),
                                |l, r|
                                Located::new(Loc::span_from(&l, &r),
                                             Exp::Union { e1: box l, e2: box r.clone() }));
                            Ok(e)
                        }
                        else {
                            Ok(Exp::Lit { lit: Lit::Nothing })
                        }
                    }
                    else {
                        if ! cmds.is_empty() {
                            // At least some of the cmds are not arrows.
                            // Create a layout.
                            Ok(Exp::Layout { id: self.alloc_node_id(), cmds: cmds })
                        }
                        else {
                            // Empty layout.
                            Ok(Exp::Lit { lit: Lit::Nothing })
                        }
                    }
                },
                Token::Bang => {
                    self.eat();
                    match *self.lookahead()? {
                        // !x is a var, not just a name
                        Token::Id(_) => {
                            let id = self.alloc_node_id();
                            let name = self.parse_name()?;
                            Ok(Exp::Var { name, id })
                        },
                        _ => {
                            let id = self.alloc_node_id();
                            let name = Name::Op(Interned::new("!"));
                            Ok(Exp::Name { name, id })
                        },
                    }
                },
                Token::Question => {
                    self.eat();
                    match *self.lookahead()? {
                        // ?x is an unknown, not just a name
                        Token::Id(_) => {
                            let id = self.alloc_node_id();
                            let name = self.parse_name()?;
                            Ok(Exp::Unknown { name, id })
                        },
                        _ => {
                            let id = self.alloc_node_id();
                            let name = Name::Op(Interned::new("?"));
                            Ok(Exp::Name { name, id })
                        },
                    }
                },
                Token::Id(_) | Token::Op(_) | Token::Tick => {
                    let id = self.alloc_node_id();
                    let name = self.parse_name()?;
                    Ok(Exp::Name { name, id })
                },
                ref t => {
                    // panic!("unexpected token {}", t.clone());
                    self.error_here(&format!("Unexpected token {}. Expected identifier, operator, bracket, or literal.", t))
                },
            }
        })
    }

    fn parse_name(&mut self) -> PResult<Name> {
        match *self.lookahead()? {
            Token::Id(ref s) => {
                self.eat();
                Ok(Name::Id(Interned::new(s)))
            },
            Token::Op(ref s) => {
                self.eat();
                Ok(Name::Op(Interned::new(s)))
            },
            Token::Bang => {
                self.eat();
                Ok(Name::Op(Interned::new("!")))
            },
            Token::Question => {
                self.eat();
                Ok(Name::Op(Interned::new("?")))
            },
            Token::Tick => {
                self.parse_mixfix_name()
            },
            ref t => {
                self.error_here("Unexpected token. Expected a name.")
            },
        }
    }

    fn alloc_node_id(&mut self) -> NodeId {
        self.node_id_generator.new_id()
    }

    fn make_mixfix_name(elements: &[MixfixParam]) -> Name {
        let mut parts = Vec::new();

        for element in elements {
            match element {
                MixfixParam::Name(p) => {
                    parts.push(p.clone());
                },
                _ => {
                    parts.push(Part::Placeholder);
                },
            }
        }

        if parts.len() == 1 {
            if let Some(Part::Id(s)) = parts.first() {
                return Name::Id(*s);
            }
            if let Some(Part::Op(s)) = parts.first() {
                return Name::Op(*s);
            }
        }

        Name::Mixfix(Name::encode_parts(&parts))
    }

    fn make_params(elements: &[MixfixParam]) -> Vec<Located<Param>> {
        let mut params = Vec::new();
        for element in elements {
            match element {
                MixfixParam::Param(p) => {
                    params.push(p.clone());
                },
                _ => {},
            }
        }
        params
    }

    fn make_param_from_exp(e: Located<Exp>, mode: CallingMode) -> Located<Param> {
        e.map_with_loc(
            |loc, e| Param {
                attr: ParamAttr {
                    assoc: Assoc::NonAssoc,
                    by_name: CallingConv::ByValue,
                    mode,
                },
                pat: Box::new(Located{ loc, value: e })
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use parser::parse::Parser;
    use syntax::trees::*;
    use syntax::names::*;
    use syntax::loc::*;
    use std::path::PathBuf;

    macro_rules! test_parse_ok {
        ($input: expr, $ast: expr) => {
            let source = Source::FileSource(PathBuf::from("foo.ivo"));
            let mut n = NodeIdGenerator::new();
            let mut p = Parser::new(&source, $input, &mut n);
            match p.parse_bundle() {
                Ok(t) => {
                    assert_eq!(*t, $ast);
                },
                Err(msg) => {
                    assert_eq!(Err(msg), Ok($ast));
                }
            }
        };
    }

    #[test]
    fn test_empty_bundle_1() {
        test_parse_ok!("", Root::Bundle { id: NodeId(0), cmds: vec!() });
    }

    #[test]
    fn test_empty_bundle_2() {
        test_parse_ok!(";;;;", Root::Bundle { id: NodeId(0), cmds: vec!() });
    }

    #[test]
    fn test_lambda() {
        test_parse_ok!("fun (x) -> x",
            Root::Bundle {
                id: NodeId(0),
                cmds: vec![
                    Located {
                        loc: Loc::new(0, 11),
                        value: Cmd::Exp(Exp::Lambda {
                            id: NodeId(3),
                            opt_guard: None,
                            params: vec![
                                Located {
                                    loc: Loc::new(4, 6),
                                    value: Exp::Name { name: Name::Id(Interned::new("x")), id: NodeId(1) }
                                }],
                            ret: Box::new(Located {
                                loc: Loc::new(11, 11),
                                value: Exp::Name { name: Name::Id(Interned::new("x")), id: NodeId(2) }
                            })
                        })
                    }
                ]
            }
        );
    }

    #[test]
    fn test_trait_without_body() {
        test_parse_ok!("trait T",
            Root::Bundle {
                id: NodeId(0),
                cmds: vec![
                    Located {
                        loc: Loc::new(0, 6),
                        value: Cmd::Def(
                            Def::MixfixDef {
                                id: NodeId(1),
                                attrs: vec![],
                                flag: MixfixFlag::Trait,
                                name: Name::Id(Interned::new("T")),
                                opt_guard: None,
                                opt_body: None,
                                supers: vec![],
                                params: vec![],
                                ret: Located {
                                    loc: Loc::new(6, 6),
                                    value: Param {
                                        attr: ParamAttr {
                                            assoc: Assoc::NonAssoc,
                                            by_name: CallingConv::ByValue,
                                            mode: CallingMode::Output,
                                        },
                                        pat: Box::new(Located {
                                            loc: Loc::new(6, 6),
                                            value: Exp::Record {
                                                id: NodeId(2),
                                                defs: vec![]
                                            }
                                        })
                                    }
                                }
                            }
                        )
                    }
                ]
            }
        );
    }

    #[test]
    fn test_trait_with_empty_body() {
        test_parse_ok!("trait T = {}",
            Root::Bundle {
                id: NodeId(0),
                cmds: vec![
                    Located {
                        loc: Loc::new(0, 11),
                        value: Cmd::Def(
                            Def::MixfixDef {
                                id: NodeId(1),
                                attrs: vec![],
                                flag: MixfixFlag::Trait,
                                name: Name::Id(Interned::new("T")),
                                opt_guard: None,
                                opt_body: None,
                                supers: vec![],
                                params: vec![],
                                ret: Located {
                                    loc: Loc::new(10, 11),
                                    value: Param {
                                        attr: ParamAttr {
                                            assoc: Assoc::NonAssoc,
                                            by_name: CallingConv::ByValue,
                                            mode: CallingMode::Output,
                                        },
                                        pat: Box::new(Located {
                                            loc: Loc::new(10, 11),
                                            value: Exp::Lit { lit: Lit::Nothing }
                                        })
                                    }
                                }
                            }
                        )
                    }
                ]
            }
        );
    }

    #[test]
    fn test_trait_with_union() {
        test_parse_ok!("trait T = A with B",
            Root::Bundle {
                id: NodeId(0),
                cmds: vec![
                    Located {
                        loc: Loc::new(0, 17),
                        value: Cmd::Def(
                            Def::MixfixDef {
                                id: NodeId(1),
                                attrs: vec![],
                                flag: MixfixFlag::Trait,
                                name: Name::Id(Interned::new("T")),
                                opt_guard: None,
                                opt_body: None,
                                supers: vec![],
                                params: vec![],
                                ret: Located {
                                    loc: Loc::new(10, 17),
                                    value: Param {
                                        attr: ParamAttr {
                                            assoc: Assoc::NonAssoc,
                                            by_name: CallingConv::ByValue,
                                            mode: CallingMode::Output,
                                        },
                                        pat: Box::new(Located {
                                            loc: Loc::new(10, 17),
                                            value: Exp::Union {
                                                e1: box
                                                    Located {
                                                        loc: Loc::new(10, 10),
                                                        value: Exp::Name {
                                                            name: Name::Id(Interned::new("A")),
                                                            id: NodeId(2),
                                                        }
                                                    },
                                                e2: box
                                                    Located {
                                                        loc: Loc::new(17, 17),
                                                        value: Exp::Name {
                                                            name: Name::Id(Interned::new("B")),
                                                            id: NodeId(3),
                                                        }
                                                    }
                                            }
                                        })
                                    }
                                }
                            }
                        )
                    }
                ]
            }
        );
    }

    #[test]
    fn test_trait_with_param() {
        test_parse_ok!("trait T (x)",
            Root::Bundle {
                id: NodeId(0),
                cmds: vec!(
                    Located::new(
                        Loc::new(0, 10),
                        Cmd::Def(
                            Def::MixfixDef {
                                id: NodeId(2),
                                attrs: vec![],
                                flag: MixfixFlag::Trait,
                                name: Name::Mixfix(Name::encode_parts(&vec![Part::Id(Interned::new("T")), Part::Placeholder])),
                                opt_guard: None,
                                opt_body: None,
                                supers: vec![],
                                params: vec!(
                                    Located::new(
                                        Loc::new(8, 10),
                                        Param {
                                            attr: ParamAttr {
                                                assoc: Assoc::NonAssoc,
                                                by_name: CallingConv::ByValue,
                                                mode: CallingMode::Input,
                                            },
                                            pat: Box::new(
                                                Located::new(
                                                    Loc::new(9, 9),
                                                    Exp::Name {
                                                        name: Name::Id(Interned::new("x")),
                                                        id: NodeId(1),
                                                    }
                                                )
                                            )
                                        }
                                    )
                                ),
                                ret: Located::new(
                                    Loc::new(10,10),
                                    Param {
                                        attr: ParamAttr {
                                            assoc: Assoc::NonAssoc,
                                            by_name: CallingConv::ByValue,
                                            mode: CallingMode::Output,
                                        },
                                        pat: Box::new(
                                            Located::new(
                                                Loc::new(10,10),
                                                Exp::Record {
                                                    id: NodeId(3),
                                                    defs: vec![]
                                                }
                                            )
                                        )
                                    }
                                ),
                            }
                        )
                    )
                )
            }
        );
    }

    #[test]
    fn test_trait_with_union_with_empty() {
        test_parse_ok!("trait T with A = {}",
            Root::Bundle {
                id: NodeId(0),
                cmds: vec![
                    Located {
                        loc: Loc::new(0, 18),
                        value: Cmd::Def(
                            Def::MixfixDef {
                                id: NodeId(1),
                                attrs: vec![],
                                flag: MixfixFlag::Trait,
                                name: Name::Id(Interned::new("T")),
                                opt_guard: None,
                                opt_body: None,
                                params: vec![],
                                supers: vec![
                                    Located {
                                        loc: Loc::new(10, 10),
                                        value: Exp::Name {
                                            name: Name::Id(Interned::new("A")),
                                            id: NodeId(2),
                                        }
                                    }
                                ],
                                ret: Located {
                                    loc: Loc::new(10, 18),
                                    value: Param {
                                        attr: ParamAttr {
                                            assoc: Assoc::NonAssoc,
                                            by_name: CallingConv::ByValue,
                                            mode: CallingMode::Output,
                                        },
                                        pat: box Located {
                                            loc: Loc::new(17, 18),
                                            value: Exp::Lit { lit: Lit::Nothing }
                                        }
                                    }
                                }
                            }
                        )
                    }
                ]
            }
        );
    }

    #[test]
    fn test_trait_with_guard() {
        test_parse_ok!("trait T (x) where x = {}",
            Root::Bundle {
                id: NodeId(0),
                cmds: vec!(
                    Located::new(
                        Loc::new(0, 23),
                        Cmd::Def(
                            Def::MixfixDef {
                                id: NodeId(3),
                                attrs: vec![],
                                flag: MixfixFlag::Trait,
                                name: Name::Mixfix(Name::encode_parts(&vec![Part::Id(Interned::new("T")), Part::Placeholder])),
                                opt_guard: Some(
                                    Box::new(
                                        Located::new(
                                            Loc::new(18,18),
                                            Exp::Name {
                                                name: Name::Id(Interned::new("x")),
                                                id: NodeId(2),
                                            }
                                        )
                                    )
                                ),
                                opt_body: None,
                                supers: vec![],

                                params: vec!(
                                    Located::new(
                                        Loc::new(8, 10),
                                        Param {
                                            attr: ParamAttr {
                                                assoc: Assoc::NonAssoc,
                                                by_name: CallingConv::ByValue,
                                                mode: CallingMode::Input,
                                            },
                                            pat: Box::new(
                                                Located::new(
                                                    Loc::new(9, 9),
                                                    Exp::Name {
                                                        name: Name::Id(Interned::new("x")),
                                                        id: NodeId(1),
                                                    }
                                                )
                                            )
                                        }
                                    )
                                ),
                                ret: Located::new(
                                    Loc::new(22,23),
                                    Param {
                                        attr: ParamAttr {
                                            assoc: Assoc::NonAssoc,
                                            by_name: CallingConv::ByValue,
                                            mode: CallingMode::Output,
                                        },
                                        pat: Box::new(
                                            Located::new(
                                                Loc::new(22,23),
                                                Exp::Lit { lit: Lit::Nothing }
                                            )
                                        )
                                    }
                                ),
                            }
                        )
                    )
                )
            }
        );
    }

    fn test_names() {
        test_parse_ok!(r#"
            x y z `foo` `_ + _`
        "#,
        Root::Bundle {
                    id: NodeId(0),
                    cmds: vec!()
        });
    }
}
