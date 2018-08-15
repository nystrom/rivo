use num::bigint::BigInt;
use num::rational::BigRational;

use syntax::loc::*;
use syntax::trees::*;
use syntax::names::*;

use parser::tokens::Token;
use parser::lex::Lexer;

// Turns a PResult<T> into a PResult<Located<T>>.
#[macro_export]
macro_rules! located_ok {
    ($parser: expr, $body: expr) => {
        {
            let first_token = $parser.lookahead();
            let v = $body?;
            let last_token = &$parser.last_token;
            Ok(Located {
                loc: Loc {
                    start: first_token.loc.start.clone(),
                    end: last_token.loc.end.clone(),
                    source: first_token.loc.source.clone(),
                },
                value: v
            })
        }
    };
}

// Turns a T into a Located<T>.
#[macro_export]
macro_rules! located {
    ($parser: expr, $body: expr) => {
        {
            let first_token = $parser.lookahead();
            println!("first = {:?}", first_token);
            let v = $body;
            let last_token = &$parser.last_token;
            println!("last = {:?}", last_token);
            Located {
                loc: Loc {
                    start: first_token.loc.start.clone(),
                    end: last_token.loc.end.clone(),
                    source: first_token.loc.source.clone(),
                },
                value: v
            }
        }
    };
}

#[macro_export]
macro_rules! consume_or_else {
    ($parser: expr, $expected: pat, $body: expr, $otherwise: expr) => {
        match *$parser.lookahead() {
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

#[macro_export]
macro_rules! consume {
    ($parser: expr, $expected: expr) => {
        match *$parser.lookahead() {
            // $expected is a expr, not a pattern so we have to compare using ==
            ref t if *t == $expected => {
                $parser.eat();
                Ok({})
            },
            ref t => {
                $parser.error_unexpected(vec![$expected], t.clone())
            }
        }
    }
}

#[macro_export]
macro_rules! consume_without_check {
    ($parser: expr, $expected: expr) => {
        assert_eq!(*$parser.lookahead(), $expected);
        $parser.eat();
    }
}

enum MixfixParam {
    Name(Part),
    Param(Located<Param>),
}

type PResult<A> = Result<A, Located<String>>;

pub struct Parser<'a> {
    pub lex: Lexer<'a>,
    pub last_token: Located<Token>,
    pub errors: Vec<Located<String>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, input: &'a str) -> Parser<'a> {
        Parser {
            lex: Lexer::new(source, input),
            last_token: Located {
                loc: NO_LOC,
                value: Token::EOF
            },
            errors: vec![],
        }
    }

    pub fn new_from_lexer(lex: Lexer<'a>) -> Parser<'a> {
        Parser {
            lex,
            last_token: Located {
                loc: NO_LOC,
                value: Token::EOF
            },
            errors: vec![],
        }
    }

    fn lookahead(&mut self) -> Located<Token> {
        self.lex.peek_token()
    }

    fn error_unexpected<T>(&mut self, expected: Vec<Token>, got: Token) -> PResult<T> {
        if expected.len() == 0 {
            let msg = format!("unexpected token {:?}", got);
            self.error_here(&msg)
        }
        else if expected.len() == 1 {
            let msg = format!("unexpected token {:?}, expected {:?}", got, expected.first().unwrap());
            self.error_here(&msg)
        }
        else {
            let msg = format!("unexpected token {:?}, expected one of {:?}", got, expected);
            self.error_here(&msg)
        }
    }

    fn error_here<T>(&mut self, msg: &str) -> PResult<T> {
        let t = self.lookahead();
        let loc = t.loc.clone();
        self.error(loc, msg)
    }

    fn error<T>(&mut self, loc: Loc, msg: &str) -> PResult<T> {
        println!("{}", msg);
        let lmsg = Located { loc: loc, value: String::from(msg) };
        self.errors.push(lmsg.clone());
        Err(lmsg)
    }

    fn error_string<T>(&mut self, lmsg: Located<String>) -> PResult<T> {
        println!("{}", lmsg.value);
        self.errors.push(lmsg.clone());
        Err(lmsg)
    }

    fn eat(&mut self) {
        let t = self.lookahead();
        let loc = t.loc.clone();
        match *t {
            Token::BadInt => {
                self.error::<()>(loc, "bad integer literal");
            },
            Token::BadChar => {
                self.error::<()>(loc, "bad character literal");
            },
            Token::BadString => {
                self.error::<()>(loc, "bad string literal");
            },
            Token::BadRat => {
                self.error::<()>(loc, "bad rational literal");
            },
            Token::BadComment => {
                self.error::<()>(loc, "bad comment");
            },
            Token::UnexpectedChar(ch) => {
                self.error::<()>(loc, "unexpected character");
            },
            _ => {
                self.last_token = self.lex.next_token();
            },
        }
    }

    pub fn parse_bundle(&mut self) -> PResult<Located<Root>> {
        located_ok!(self, {
            let cmds = self.parse_cmds()?;
            Ok(Root::Parsed { cmds })
        })
    }

    fn parse_block(&mut self) -> PResult<Located<Exp>> {
        located_ok!(self, {
            consume!(self, Token::Lc)?;
            let cmds = self.parse_cmds()?;
            Ok(Exp::AmbLayout { cmds })
        })
    }

    fn parse_cmds(&mut self) -> PResult<Vec<Located<Cmd>>> {
        let mut cmds = Vec::new();

        loop {
            match *self.lookahead() {
                Token::EOF => {
                    break;
                },
                Token::Rc => {
                    break;
                },
                Token::Semi => {
                    // separates commands
                    self.eat();
                },
                _ => {
                    match self.parse_cmd() {
                        Ok(c) => {
                            cmds.push(c);
                        },
                        Err(msg) => {
                            self.error_string::<()>(msg);

                            // Skip to the next ;
                            loop {
                                match *self.lookahead() {
                                    Token::Semi => break,
                                    _ => {},
                                }
                            }
                        }
                    }
                },
            }
        }

        println!("commands {:?}", cmds);
        Ok(cmds)
    }

    fn parse_cmd(&mut self) -> PResult<Located<Cmd>> {
        match *self.lookahead() {
            Token::Fun => {
                self.parse_fun_cmd()
            },
            Token::Val => {
                let r = self.parse_val_def()?;
                Ok(r.map(|d| Cmd::Def(d)))
            },
            Token::Var => {
                let r = self.parse_var_def()?;
                Ok(r.map(|d| Cmd::Def(d)))
            },
            Token::Import => {
                let r = self.parse_import_def()?;
                Ok(r.map(|d| Cmd::Def(d)))
            },
            Token::Trait => {
                self.parse_trait_cmd()
            },
            _ => {
                let r = self.parse_exp()?;
                Ok(r.map(|e| Cmd::Exp(e)))
            },
        }
    }

    fn parse_mixfix_return(&mut self) -> PResult<Located<Exp>> {
        unimplemented!()
    }

    // for Exp
    fn parse_opt_guard(&mut self) -> PResult<Option<Located<Exp>>> {
        consume_or_else!(self,
            Token::For,
            {
                let e = self.parse_exp()?;
                Ok(Some(e))
            },
            {
                Ok(None)
            }
        )
    }

    fn parse_mixfix_elements(&mut self) -> PResult<Vec<MixfixParam>> {
        let mut elements = Vec::new();

        loop {
            match *self.lookahead() {
                Token::Id(ref s) => {
                    self.eat();
                    let e = MixfixParam::Name(Part::Id(s.clone()));
                    elements.push(e);
                },
                Token::Op(ref s) => {
                    self.eat();
                    let e = MixfixParam::Name(Part::Op(s.clone()));
                    elements.push(e);
                },
                Token::Lp => {
                    let p = located!(self, {
                        self.eat();
                        match *self.lookahead() {
                            Token::Lp => {
                                self.eat();
                                match *self.lookahead() {
                                    Token::Bang => {
                                        self.eat();
                                        let e = self.parse_exp()?;
                                        consume!(self, Token::Rp)?;
                                        consume!(self, Token::Rp)?;
                                        Param {
                                            assoc: true,
                                            by_name: false,
                                            mode: CallingMode::Output,
                                            pat: Box::new(e),
                                        }
                                    },
                                    Token::Question => {
                                        self.eat();
                                        let e = self.parse_exp()?;
                                        consume!(self, Token::Rp)?;
                                        consume!(self, Token::Rp)?;
                                        Param {
                                            assoc: true,
                                            by_name: false,
                                            mode: CallingMode::Input,
                                            pat: Box::new(e),
                                        }
                                    },
                                    _ => {
                                        let e = self.parse_exp()?;
                                        consume!(self, Token::Rp)?;
                                        consume!(self, Token::Rp)?;
                                        Param {
                                            assoc: true,
                                            by_name: false,
                                            mode: CallingMode::Input,
                                            pat: Box::new(e),
                                        }
                                    },
                                }
                            },
                            Token::Bang => {
                                self.eat();
                                let e = self.parse_exp()?;
                                consume!(self, Token::Rp)?;
                                Param {
                                    assoc: false,
                                    by_name: false,
                                    mode: CallingMode::Output,
                                    pat: Box::new(e),
                                }
                            },
                            Token::Question => {
                                self.eat();
                                let e = self.parse_exp()?;
                                consume!(self, Token::Rp)?;
                                Param {
                                    assoc: false,
                                    by_name: false,
                                    mode: CallingMode::Input,
                                    pat: Box::new(e),
                                }
                            },
                            _ => {
                                let e = self.parse_exp()?;
                                consume!(self, Token::Rp)?;
                                Param {
                                    assoc: false,
                                    by_name: false,
                                    mode: CallingMode::Input,
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
                        match *self.lookahead() {
                            Token::Lc => {
                                self.eat();
                                match *self.lookahead() {
                                    Token::Bang => {
                                        self.eat();
                                        let e = self.parse_exp()?;
                                        consume!(self, Token::Rc)?;
                                        consume!(self, Token::Rc)?;
                                        Param {
                                            assoc: true,
                                            by_name: true,
                                            mode: CallingMode::Output,
                                            pat: Box::new(e),
                                        }
                                    },
                                    Token::Question => {
                                        self.eat();
                                        let e = self.parse_exp()?;
                                        consume!(self, Token::Rc)?;
                                        consume!(self, Token::Rc)?;
                                        Param {
                                            assoc: true,
                                            by_name: true,
                                            mode: CallingMode::Input,
                                            pat: Box::new(e),
                                        }
                                    },
                                    _ => {
                                        let e = self.parse_exp()?;
                                        consume!(self, Token::Rc)?;
                                        consume!(self, Token::Rc)?;
                                        Param {
                                            assoc: true,
                                            by_name: true,
                                            mode: CallingMode::Input,
                                            pat: Box::new(e),
                                        }
                                    },
                                }
                            },
                            Token::Bang => {
                                self.eat();
                                let e = self.parse_exp()?;
                                consume!(self, Token::Rc)?;
                                Param {
                                    assoc: false,
                                    by_name: true,
                                    mode: CallingMode::Output,
                                    pat: Box::new(e),
                                }
                            },
                            Token::Question => {
                                self.eat();
                                let e = self.parse_exp()?;
                                consume!(self, Token::Rc)?;
                                Param {
                                    assoc: false,
                                    by_name: true,
                                    mode: CallingMode::Input,
                                    pat: Box::new(e),
                                }
                            },
                            _ => {
                                let e = self.parse_exp()?;
                                consume!(self, Token::Rc)?;
                                Param {
                                    assoc: false,
                                    by_name: true,
                                    mode: CallingMode::Input,
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

        return Ok(elements)
    }

    fn parse_fun_cmd(&mut self) -> PResult<Located<Cmd>> {
        located_ok!(self, {
            consume_without_check!(self, Token::Fun);

            let elements = self.parse_mixfix_elements()?;
            let opt_guard = self.parse_opt_guard()?;

            let mut has_name = false;

            for element in &elements {
                match element {
                    MixfixParam::Name(_) => { has_name = true; },
                    _ => {},
                }
            }

            let params = make_params(&elements);

            if ! has_name {
                // We have a lambda not a function def.
                let mut lambda_params = Vec::new();

                for param in params {
                    match *param {
                        Param { assoc: true, .. } => {
                            return self.error(param.loc.clone(), "anonymous function parameter cannot be associative.")
                        },
                        Param { by_name: true, .. } => {
                            return self.error(param.loc.clone(), "anonymous function parameter cannot be call-by-name.")
                        },
                        Param { mode: CallingMode::Output, .. } => {
                            return self.error(param.loc.clone(), "anonymous function parameter cannot be an output parameter.")
                        },
                        Param { pat: ref exp, .. } => {
                            // copy the location from the Param.
                            let unboxed: Located<Exp> = *exp.clone();
                            lambda_params.push(unboxed.with_loc(&param.loc));
                        },
                    }
                }

                consume!(self, Token::Arrow)?;

                let body = self.parse_exp()?;

                Ok(Cmd::Exp(
                    Exp::AmbLambda {
                        opt_guard: opt_guard.map(|e| Box::new(e)),
                        params: lambda_params,
                        ret: Box::new(body),
                    }
                ))
            }
            else {
                let name = make_mixfix_name(&elements);

                match *self.lookahead() {
                    Token::Eq => {
                        self.eat();
                        let e = self.parse_exp()?;
                        Ok(Cmd::Def(Def::AmbMixfixDef {
                            flag: MixfixFlag::Fun,
                            name,
                            opt_guard: opt_guard.map(|e| Box::new(e)),
                            params,
                            ret: make_param_from_exp(e, CallingMode::Output)
                        }))
                    },
                    Token::Arrow => {
                        self.eat();
                        let e = self.parse_exp()?;
                        Ok(Cmd::Def(Def::AmbMixfixDef {
                            flag: MixfixFlag::Fun,
                            name,
                            opt_guard: opt_guard.map(|e| Box::new(e)),
                            params,
                            ret: make_param_from_exp(e, CallingMode::Output)
                        }))
                    },
                    Token::BackArrow => {
                        self.eat();
                        let e = self.parse_mixfix_return()?;
                        let opt_guard2 = match opt_guard {
                            Some(g) => Some(g),
                            None => self.parse_opt_guard()?
                        };
                        Ok(Cmd::Def(Def::AmbMixfixDef {
                            flag: MixfixFlag::Fun,
                            name,
                            opt_guard: opt_guard2.map(|e| Box::new(e)),
                            params,
                            ret: make_param_from_exp(e, CallingMode::Input)
                        }))
                    },
                    ref t => {
                        self.error_unexpected(vec![Token::Eq, Token::Arrow, Token::BackArrow], t.clone())
                    }
                }
            }
        })
    }

    fn parse_val_def(&mut self) -> PResult<Located<Def>> {
        located_ok!(self, {
            assert_eq!(*self.lookahead(), Token::Val);
            self.eat();
            self.parse_formula_def(FormulaFlag::Val)
        })
    }

    fn parse_var_def(&mut self) -> PResult<Located<Def>> {
        located_ok!(self, {
            assert_eq!(*self.lookahead(), Token::Var);
            self.eat();
            self.parse_formula_def(FormulaFlag::Var)
        })
    }

    fn parse_formula_def(&mut self, flag: FormulaFlag) -> PResult<Def> {
        Ok(unimplemented!())
    }

    fn parse_import_def(&mut self) -> PResult<Located<Def>> {
        located_ok!(self, {
            assert_eq!(*self.lookahead(), Token::Import);
            self.eat();
            Ok(unimplemented!())
        })
    }

    fn parse_trait_cmd(&mut self) -> PResult<Located<Cmd>> {
        located_ok!(self, {
            assert_eq!(*self.lookahead(), Token::Trait);
            self.eat();
            Ok(unimplemented!())
        })
    }

    fn parse_exp(&mut self) -> PResult<Located<Exp>> {
        located_ok!(self, {
            match *self.lookahead() {
                Token::Id(ref s) => {
                    self.eat();
                    Ok(Exp::AmbName { name: Name::Id(s.clone()) })
                },
                Token::Op(ref s) => {
                    self.eat();
                    Ok(Exp::AmbName { name: Name::Op(s.clone()) })
                },
                _ =>
                    Ok(unimplemented!()),
            }
        })
    }
}

fn make_mixfix_name(elements: &Vec<MixfixParam>) -> Name {
    let mut parts = Vec::new();
    for element in elements {
        match element {
            MixfixParam::Name(p) => {
                parts.push(p.clone());
            },
            _ => {},
        }
    }

    if parts.len() == 1 {
        if let Some(Part::Id(s)) = parts.first() {
            return Name::Id(s.clone());
        }
        if let Some(Part::Op(s)) = parts.first() {
            return Name::Op(s.clone());
        }
    }

    Name::Mixfix(parts)
}

fn make_params(elements: &Vec<MixfixParam>) -> Vec<Located<Param>> {
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
            assoc: false,
            by_name: false,
            mode: mode,
            pat: Box::new(Located{ loc: loc, value: e })
        }
    )
}

mod tests {
    use parser::parse::Parser;
    use syntax::trees::*;
    use syntax::names::*;
    use syntax::loc::*;

    #[test]
    fn test_empty_bundle_1() {
        let mut p = Parser::new("foo.ivo", "");
        match p.parse_bundle() {
            Ok(t) =>
                assert_eq!(*t, Root::Parsed { cmds: vec![] }),
            Err(msg) => {
                println!("{:?}", msg);
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_bundle_2() {
        let mut p = Parser::new("foo.ivo", ";;;;");
        match p.parse_bundle() {
            Ok(t) =>
                assert_eq!(*t, Root::Parsed { cmds: vec![] }),
            Err(msg) => {
                println!("{:?}", msg);
                assert!(false)
            }
        }
    }

    #[test]
    fn test_empty_bundle_3() {
        let mut p = Parser::new("foo.ivo", "fun (x) -> x");
        match p.parse_bundle() {
            Ok(t) =>
                assert_eq!(*t, Root::Parsed {
                    cmds: vec![
                        Located {
                            loc: Loc { start: Pos { offset: 0, line: 1, column: 1 }, end: Pos { offset: 11, line: 1, column: 12 }, source: Some(String::from("foo.ivo")) },
                            value: Cmd::Exp(Exp::AmbLambda {
                                opt_guard: None,
                                params: vec![
                                    Located {
                                        loc: Loc { start: Pos { offset: 4, line: 1, column: 5 }, end: Pos { offset: 6, line: 1, column: 7 }, source: Some(String::from("foo.ivo")) },
                                        value: Exp::AmbName { name: Name::Id(String::from("x")) }
                                    }],
                                ret: Box::new(Located {
                                    loc: Loc { start: Pos { offset: 11, line: 1, column: 12 }, end: Pos { offset: 11, line: 1, column: 12 }, source: Some(String::from("foo.ivo")) },
                                    value: Exp::AmbName { name: Name::Id(String::from("x")) }
                                })
                            })
                        }
                    ]
                }),
            Err(msg) => {
                println!("{:?}", msg);
                assert!(false)
            }
        }
    }
}
