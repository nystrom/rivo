use num::bigint::BigInt;
use num::rational::BigRational;

use syntax::loc::*;
use syntax::trees::*;
use syntax::names::*;

use parser::tokens::*;
use parser::lex::Lexer;

// TODO: parse groups of definitions directly rather than building a dummy trait.
// TODO: rename Trait to Record and add a tag, generate by the parser.
// TODO: support pragmas. Add pragmas to the Def ASTs.
// #(prio 0)
// pragmas must be stable paths

// Turns a PResult<T> into a PResult<Located<T>>.
macro_rules! located_ok {
    ($parser: expr, $body: expr) => {
        {
            let first_token = $parser.lookahead();
            let v = $body?;
            let last_token = &$parser.last_token;
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
            let first_token = $parser.lookahead();
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

macro_rules! consume_without_check {
    ($parser: expr, $expected: expr) => {
        assert_eq!(*$parser.lookahead(), $expected);
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
                        vec!(),
                }
            })
            .collect();
    };
}

macro_rules! with_scope {
    ($self: expr, $scope: expr, $s: expr) => {
        {
            $self.scope_stack.push($scope);
            let x = $s;
            $self.scope_stack.pop();
            x
        }
    };
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
    pub next_name_id: usize,
    pub next_mixfix_id: usize,
    pub next_scope_id: usize,
    pub scope_stack: Vec<ScopeId>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, input: &'a str) -> Parser<'a> {
        Parser {
            lex: Lexer::new(source, input),
            last_token: Located {
                loc: NO_LOC,
                value: Token::EOF
            },
            errors: vec!(),
            next_name_id: 0,
            next_mixfix_id: 0,
            next_scope_id: 0,
            scope_stack: Vec::new(),
        }
    }

    pub fn new_from_lexer(lex: Lexer<'a>) -> Parser<'a> {
        Parser {
            lex,
            last_token: Located {
                loc: NO_LOC,
                value: Token::EOF
            },
            errors: vec!(),
            next_name_id: 0,
            next_mixfix_id: 0,
            next_scope_id: 0,
            scope_stack: Vec::new(),
        }
    }

    fn current_scope(&self) -> ScopeId {
        *self.scope_stack.last().unwrap_or(&ScopeId::Global)
    }

    fn parent_scope(&self) -> ScopeId {
        // return the scope just before the last scope.
        let i = self.scope_stack.len() - 2;
        *self.scope_stack.get(i).unwrap_or(&ScopeId::Global)
    }

    fn lookahead(&mut self) -> Located<Token> {
        self.lex.peek_token()
    }

    fn error_unexpected<T>(&mut self, expected: Vec<Token>, got: Token) -> PResult<T> {
        if expected.len() == 0 {
            let msg = format!("unexpected token '{}'", got);
            self.error_here(&msg)
        }
        else if expected.len() == 1 {
            let msg = format!("unexpected token '{}', expected '{}'", got, expected.first().unwrap());
            self.error_here(&msg)
        }
        else {
            let msg = format!("unexpected token '{}', expected one of {}", got, TokenVec(expected));
            self.error_here(&msg)
        }
    }

    fn error_here<T>(&mut self, msg: &str) -> PResult<T> {
        let t = self.lookahead();
        let loc = t.loc.clone();
        self.error(loc, msg)
    }

    fn error<T>(&mut self, loc: Loc, msg: &str) -> PResult<T> {
        let lmsg = Located { loc: loc, value: String::from(msg) };
        self.errors.push(lmsg.clone());
        panic!("{:?}", lmsg);
        Err(lmsg)
    }

    fn error_void(&mut self, loc: Loc, msg: &str) {
        let lmsg = Located { loc: loc, value: String::from(msg) };
        self.errors.push(lmsg.clone());
        panic!("{:?}", lmsg);
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
                self.error_void(loc, "bad integer literal");
            },
            Token::BadChar => {
                self.error_void(loc, "bad character literal");
            },
            Token::BadString => {
                self.error_void(loc, "bad string literal");
            },
            Token::BadRat => {
                self.error_void(loc, "bad rational literal");
            },
            Token::BadComment => {
                self.error_void(loc, "bad comment");
            },
            Token::UnexpectedChar(ch) => {
                self.error_void(loc, "unexpected character");
            },
            _ => {
                self.last_token = self.lex.next_token();
                // println!("token {} {:?}", self.last_token.loc, self.last_token.value);
            },
        }
    }

    pub fn parse_bundle(&mut self) -> PResult<Located<Root>> {
        located_ok!(self, {
            let id = self.alloc_scope();
            with_scope!(self, id, {
                let cmds = self.parse_cmds()?;
                Ok(Root::Bundle { scope_id: id, cmds })
            })
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
            match *self.lookahead() {
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
                        Ok(c) => {
                            cmds.push(c);

                            // A command should be followed by a ; or } or EOF.
                            match *self.lookahead() {
                                Token::Semi => {},
                                Token::Rc => {},
                                Token::EOF => {},
                                ref t => {
                                    return self.error_here(format!("unexpected token '{}'", *t).as_str());
                                },
                            }
                        },
                        Err(msg) => {
                            // Skip to the next ; or } or EOF
                            loop {
                                match *self.lookahead() {
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
                let r = self.parse_trait_def()?;
                Ok(r.map(|d| Cmd::Def(d)))
            },
            _ => {
                let r = self.parse_exp()?;
                Ok(r.map(|e| Cmd::Exp(e)))
            },
        }
    }

    fn parse_mixfix_return(&mut self) -> PResult<Located<Param>> {
        located_ok!(self, {
            match *self.lookahead() {
                Token::Lp => {
                    self.eat();
                    match *self.lookahead() {
                        Token::Question => {
                            self.eat();
                            let e = self.parse_tuple()?;
                            consume!(self, Token::Rp)?;
                            Ok(Param {
                                assoc: Assoc::NonAssoc,
                                by_name: CallingConv::ByValue,
                                mode: CallingMode::Input,
                                pat: Box::new(e),
                            })
                        },
                        _ => {
                            let e = self.parse_tuple()?;
                            consume!(self, Token::Rp)?;
                            Ok(Param {
                                assoc: Assoc::NonAssoc,
                                by_name: CallingConv::ByValue,
                                mode: CallingMode::Input,
                                pat: Box::new(e),
                            })
                        },
                    }
                },
                Token::Lc => {
                    self.eat();
                    match *self.lookahead() {
                        Token::Question => {
                            self.eat();
                            let e = self.parse_tuple()?;
                            consume!(self, Token::Rc)?;
                            Ok(Param {
                                assoc: Assoc::NonAssoc,
                                by_name: CallingConv::ByName,
                                mode: CallingMode::Input,
                                pat: Box::new(e),
                            })
                        },
                        _ => {
                            let e = self.parse_tuple()?;
                            consume!(self, Token::Rc)?;
                            Ok(Param {
                                assoc: Assoc::NonAssoc,
                                by_name: CallingConv::ByName,
                                mode: CallingMode::Input,
                                pat: Box::new(e),
                            })
                        },
                    }
                },
                _ => {
                    let e = self.parse_exp()?;
                    Ok(Param {
                        assoc: Assoc::NonAssoc,
                        by_name: CallingConv::ByValue,
                        mode: CallingMode::Input,
                        pat: Box::new(e),
                    })
                },
            }
        })
    }

    // where Exp
    fn parse_opt_guard(&mut self) -> PResult<Option<Located<Exp>>> {
        consume_or_else!(self,
            Token::Where,
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
                Token::Bang => {
                    self.eat();
                    let e = MixfixParam::Name(Part::Op(String::from("!")));
                    elements.push(e);
                },
                Token::Question => {
                    self.eat();
                    let e = MixfixParam::Name(Part::Op(String::from("?")));
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
                                        let e = self.parse_tuple()?;
                                        consume!(self, Token::Rp)?;
                                        consume!(self, Token::Rp)?;
                                        Param {
                                            assoc: Assoc::Assoc,
                                            by_name: CallingConv::ByValue,
                                            mode: CallingMode::Output,
                                            pat: Box::new(e),
                                        }
                                    },
                                    Token::Question => {
                                        self.eat();
                                        let e = self.parse_tuple()?;
                                        consume!(self, Token::Rp)?;
                                        consume!(self, Token::Rp)?;
                                        Param {
                                            assoc: Assoc::Assoc,
                                            by_name: CallingConv::ByValue,
                                            mode: CallingMode::Input,
                                            pat: Box::new(e),
                                        }
                                    },
                                    _ => {
                                        let e = self.parse_tuple()?;
                                        consume!(self, Token::Rp)?;
                                        consume!(self, Token::Rp)?;
                                        Param {
                                            assoc: Assoc::Assoc,
                                            by_name: CallingConv::ByValue,
                                            mode: CallingMode::Input,
                                            pat: Box::new(e),
                                        }
                                    },
                                }
                            },
                            Token::Bang => {
                                self.eat();
                                let e = self.parse_tuple()?;
                                consume!(self, Token::Rp)?;
                                Param {
                                    assoc: Assoc::NonAssoc,
                                    by_name: CallingConv::ByValue,
                                    mode: CallingMode::Output,
                                    pat: Box::new(e),
                                }
                            },
                            Token::Question => {
                                self.eat();
                                let e = self.parse_tuple()?;
                                consume!(self, Token::Rp)?;
                                Param {
                                    assoc: Assoc::NonAssoc,
                                    by_name: CallingConv::ByValue,
                                    mode: CallingMode::Input,
                                    pat: Box::new(e),
                                }
                            },
                            _ => {
                                let e = self.parse_tuple()?;
                                consume!(self, Token::Rp)?;
                                Param {
                                    assoc: Assoc::NonAssoc,
                                    by_name: CallingConv::ByValue,
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
                                        let e = self.parse_tuple()?;
                                        consume!(self, Token::Rc)?;
                                        consume!(self, Token::Rc)?;
                                        Param {
                                            assoc: Assoc::Assoc,
                                            by_name: CallingConv::ByName,
                                            mode: CallingMode::Output,
                                            pat: Box::new(e),
                                        }
                                    },
                                    Token::Question => {
                                        self.eat();
                                        let e = self.parse_tuple()?;
                                        consume!(self, Token::Rc)?;
                                        consume!(self, Token::Rc)?;
                                        Param {
                                            assoc: Assoc::Assoc,
                                            by_name: CallingConv::ByName,
                                            mode: CallingMode::Input,
                                            pat: Box::new(e),
                                        }
                                    },
                                    _ => {
                                        let e = self.parse_tuple()?;
                                        consume!(self, Token::Rc)?;
                                        consume!(self, Token::Rc)?;
                                        Param {
                                            assoc: Assoc::Assoc,
                                            by_name: CallingConv::ByName,
                                            mode: CallingMode::Input,
                                            pat: Box::new(e),
                                        }
                                    },
                                }
                            },
                            Token::Bang => {
                                self.eat();
                                let e = self.parse_tuple()?;
                                consume!(self, Token::Rc)?;
                                Param {
                                    assoc: Assoc::NonAssoc,
                                    by_name: CallingConv::ByName,
                                    mode: CallingMode::Output,
                                    pat: Box::new(e),
                                }
                            },
                            Token::Question => {
                                self.eat();
                                let e = self.parse_tuple()?;
                                consume!(self, Token::Rc)?;
                                Param {
                                    assoc: Assoc::NonAssoc,
                                    by_name: CallingConv::ByName,
                                    mode: CallingMode::Input,
                                    pat: Box::new(e),
                                }
                            },
                            _ => {
                                let e = self.parse_tuple()?;
                                consume!(self, Token::Rc)?;
                                Param {
                                    assoc: Assoc::NonAssoc,
                                    by_name: CallingConv::ByName,
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
                // We have a lambda.
                let mut lambda_params = Vec::new();

                for param in params {
                    match *param {
                        Param { assoc: Assoc::Assoc, .. } => {
                            return self.error(param.loc.clone(), "anonymous function parameter cannot be associative.")
                        },
                        Param { by_name: CallingConv::ByName, .. } => {
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

                let id = self.alloc_scope();

                Ok(Cmd::Exp(
                    Exp::Lambda {
                        scope_id: id,
                        opt_guard: opt_guard.map(|e| Box::new(e)),
                        params: lambda_params,
                        ret: Box::new(body),
                    }
                ))
            }
            else {
                // We have a function definition.
                let name = make_mixfix_name(&elements);

                let id = self.alloc_scope();

                match *self.lookahead() {
                    Token::Eq => {
                        self.eat();
                        let e = self.parse_exp()?;
                        Ok(Cmd::Def(Def::MixfixDef {
                            scope_id: id,
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
                        Ok(Cmd::Def(Def::MixfixDef {
                            scope_id: id,
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
                        Ok(Cmd::Def(Def::MixfixDef {
                            scope_id: id,
                            flag: MixfixFlag::Fun,
                            name,
                            opt_guard: opt_guard2.map(|e| Box::new(e)),
                            params,
                            ret: e,
                        }))
                    },
                    _ => {
                        // Just the function signature (without return).
                        // We add a () guard to make sure the function cannot be
                        // invoked.
                        let nothing = located!(self, {
                            Exp::Lit {
                                lit: Lit::Nothing
                            }
                        });

                        let opt_guard2 = match opt_guard {
                            Some(g) => Some(g),
                            None => Some(nothing.clone())
                        };

                        Ok(Cmd::Def(Def::MixfixDef {
                            scope_id: id,
                            flag: MixfixFlag::Fun,
                            name,
                            opt_guard: opt_guard2.map(|e| Box::new(e)),
                            params,
                            ret: make_param_from_exp(nothing, CallingMode::Output)
                        }))
                    },
                }
            }
        })
    }

    fn parse_val_def(&mut self) -> PResult<Located<Def>> {
        located_ok!(self, {
            assert_eq!(*self.lookahead(), Token::Val);
            self.eat();
            let e = self.parse_exp()?;
            Ok(Def::FormulaDef {
                flag: FormulaFlag::Val,
                formula: Box::new(e)
            })
        })
    }

    fn parse_var_def(&mut self) -> PResult<Located<Def>> {
        located_ok!(self, {
            assert_eq!(*self.lookahead(), Token::Var);
            self.eat();
            let e = self.parse_exp()?;
            Ok(Def::FormulaDef {
                flag: FormulaFlag::Var,
                formula: Box::new(e)
            })
        })
    }

    fn parse_import_def(&mut self) -> PResult<Located<Def>> {
        located_ok!(self, {
            assert_eq!(*self.lookahead(), Token::Import);
            self.eat();

            let namespace = self.parse_exp()?;

            Ok(Def::ImportDef {
                import: Box::new(namespace)
            })
        })
    }

    fn parse_trait_def(&mut self) -> PResult<Located<Def>> {
        located_ok!(self, {
            consume_without_check!(self, Token::Trait);

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
                self.error_here("trait missing name")
            }
            else {
                // We have a trait definition.
                let name = make_mixfix_name(&elements);

                let id = self.alloc_scope();

                match *self.lookahead() {
                    Token::Eq => {
                        self.eat();
                        let e = self.parse_exp()?;
                        Ok(Def::MixfixDef {
                            scope_id: id,
                            flag: MixfixFlag::Trait,
                            name,
                            opt_guard: opt_guard.map(|e| Box::new(e)),
                            params,
                            ret: make_param_from_exp(e, CallingMode::Output)
                        })
                    },
                    Token::Arrow => {
                        self.eat();
                        let e = self.parse_exp()?;
                        Ok(Def::MixfixDef {
                            scope_id: id,
                            flag: MixfixFlag::Trait,
                            name,
                            opt_guard: opt_guard.map(|e| Box::new(e)),
                            params,
                            ret: make_param_from_exp(e, CallingMode::Output)
                        })
                    },
                    Token::BackArrow => {
                        self.eat();
                        let e = self.parse_mixfix_return()?;
                        let opt_guard2 = match opt_guard {
                            Some(g) => Some(g),
                            None => self.parse_opt_guard()?
                        };
                        Ok(Def::MixfixDef {
                            scope_id: id,
                            flag: MixfixFlag::Trait,
                            name,
                            opt_guard: opt_guard2.map(|e| Box::new(e)),
                            params,
                            ret: e,
                        })
                    },
                    _ => {
                        // trait T (a)
                        // trait T (a) = { }
                        let tr = located!(self, {
                            Exp::Trait {
                                scope_id: self.alloc_scope(),
                                defs: vec!(),
                            }
                        });

                        Ok(Def::MixfixDef {
                            scope_id: id,
                            flag: MixfixFlag::Trait,
                            name,
                            opt_guard: opt_guard.map(|e| Box::new(e)),
                            params,
                            ret: make_param_from_exp(tr, CallingMode::Output)
                        })
                    },
                }
            }
        })
    }

    fn parse_exp(&mut self) -> PResult<Located<Exp>> {
        match *self.lookahead() {
            Token::Native =>
                located_ok!(self, {
                    self.eat();
                    Ok(Exp::Native)
                }),
            _ =>
                located_ok!(self, {
                    let left = self.parse_exp0()?;

                    match *self.lookahead() {
                        Token::Assign => {
                            self.eat();
                            let right = self.parse_exp()?;
                            Ok(Exp::Assign { lhs: Box::new(left), rhs: Box::new(right) })
                        },
                        Token::Eq => {
                            self.eat();
                            let right = self.parse_exp()?;
                            Ok(Exp::Bind { lhs: Box::new(left), rhs: Box::new(right) })
                        },
                        Token::BackArrow => {
                            self.eat();
                            let right = self.parse_exp()?;
                            Ok(Exp::Generator { lhs: Box::new(left), rhs: Box::new(right) })
                        },
                        Token::Arrow => {
                            self.eat();
                            let right = self.parse_exp()?;
                            Ok(Exp::Arrow { arg: Box::new(left), ret: Box::new(right) })
                        },
                        Token::With => {
                            self.eat();
                            let right = self.parse_exp()?;
                            Ok(Exp::Union { es: vec!(left, right) })
                        },
                        _ => {
                            Ok(left.value)
                        },
                    }
                }),
        }
    }

    fn parse_exp0(&mut self) -> PResult<Located<Exp>> {
        located_ok!(self, {
            let left = self.parse_mixfix_exp()?;

            match *self.lookahead() {
                Token::At => {
                    self.eat();
                    let right = self.parse_exp()?;
                    Ok(Exp::Intersect { es: vec!(left, right) })
                },
                Token::Colon => {
                    self.eat();
                    let right = self.parse_exp()?;
                    Ok(Exp::Ascribe { exp: Box::new(left), pat: Box::new(right) })
                },
                Token::Arrow => {
                    self.eat();
                    let right = self.parse_exp()?;
                    Ok(Exp::Arrow {
                        arg: Box::new(left),
                        ret: Box::new(right)
                    })
                },
                _ => {
                    Ok(left.value)
                },
            }
        })
    }

    fn parse_mixfix_exp(&mut self) -> PResult<Located<Exp>> {
        match *self.lookahead() {
            Token::For => {
                located_ok!(self, {
                    self.eat();

                    let id = self.alloc_scope();
                    let generator = self.parse_select()?;
                    let body = self.parse_exp()?;

                    Ok(Exp::For {
                        scope_id: id,
                        generator: Box::new(generator),
                        body: Box::new(body)
                    })
                })
            },
            Token::Fun => {
                located_ok!(self, {
                    self.eat();

                    let id = self.alloc_scope();
                    let mut params = Vec::new();

                    while *self.lookahead() == Token::Lp {
                        let arg = located_ok!(self, self.parse_tuple_exp())?;
                        params.push(arg);
                    }

                    let opt_guard = self.parse_opt_guard()?;
                    consume!(self, Token::Arrow)?;
                    let body = self.parse_exp()?;

                    Ok(Exp::Lambda {
                        scope_id: id,
                        opt_guard: opt_guard.map(|e| Box::new(e)),
                        params,
                        ret: Box::new(body),
                    })
                })
            },
            _ => {
                located_ok!(self, {
                    let first = self.parse_select()?;

                    match *self.lookahead() {
                        // If the lookahead can start a primary expression,
                        // parse recursively.
                        Token::Lc | Token::Lp | Token::Lb | Token::Id(_) | Token::Op(_) | Token::Tick | Token::Int(_, _) | Token::Rat(_, _) | Token::String(_) | Token::Char(_) | Token::Bang | Token::Question | Token::Underscore => {
                            let rest = self.parse_mixfix_exp()?;

                            match *rest.clone() {
                                Exp::MixfixApply { id, ref es } => {
                                    let mut more = Vec::new();
                                    more.push(first);
                                    more.append(&mut es.clone());
                                    Ok(Exp::MixfixApply { id, es: more })
                                },
                                _ => {
                                    let id = self.alloc_mixfix();
                                    let more = vec!(first, rest);
                                    Ok(Exp::MixfixApply { id, es: more })
                                },
                            }
                        },
                        _ => {
                            // Otherwise, we're done.
                            Ok(first.value)
                        },
                    }
                })
            },
        }
    }

    fn parse_select(&mut self) -> PResult<Located<Exp>> {
        let exp = self.parse_primary()?;

        match *self.lookahead() {
            Token::Dot =>
                self.parse_selectors(exp),
            _ =>
                Ok(exp),
        }
    }

    fn parse_selectors(&mut self, left: Located<Exp>) -> PResult<Located<Exp>> {
        consume_without_check!(self, Token::Dot);

        match *self.lookahead() {
            Token::Id(_) | Token::Op(_) | Token::Tick | Token::Bang | Token::Question => {
                // In this case we have e.x, e.+, e.`foo _`.
                // Here we should just do a normal select.
                let right = self.parse_primary()?;

                // clone the locs now so we can move left and right.
                let loc = Loc::span_from(&left, &right);

                let sel = match *right {
                    Exp::Name { ref name, id } =>
                        Ok(Located::new(
                            loc,
                            Exp::Select {
                                exp: Box::new(left),
                                name: name.clone()
                            })),
                    _ =>
                        self.error(loc, "invalid selection")
                }?;

                match *self.lookahead() {
                    Token::Dot => self.parse_selectors(sel),
                    _ => Ok(sel)
                }
            },
            Token::Underscore | Token::Lp => {
                let id = self.alloc_scope();

                let lsel = with_scope!(self, id, {
                    let right = self.parse_primary()?;
                    let loc = Loc::span_from(&left, &right);

                    Ok(Located::new(
                        loc,
                        Exp::Within {
                            scope_id: id,
                            e1: Box::new(left),
                            e2: Box::new(right),
                        }))
                })?;

                match *self.lookahead() {
                    Token::Dot => self.parse_selectors(lsel),
                    _ => Ok(lsel)
                }
            },
            _ =>
                self.error_here("invalid selector"),
        }
    }

    fn parse_mixfix_name(&mut self) -> PResult<Name> {
        consume_without_check!(self, Token::Tick);

        let mut parts = Vec::new();

        loop {
            match *self.lookahead() {
                Token::Id(ref s) => {
                    self.eat();
                    parts.push(Part::Id(s.clone()))
                },
                Token::Op(ref s) => {
                    self.eat();
                    parts.push(Part::Op(s.clone()))
                },
                Token::Underscore => {
                    self.eat();
                    parts.push(Part::Placeholder)
                },
                Token::Tick => {
                    break;
                },
                _ => {
                    return self.error_here("bad mixfix name part")
                }
            }
        }

        consume!(self, Token::Tick)?;

        match parts[..] {
            [] =>
                self.error_here("invalid mixfix name"),
            [Part::Id(ref s)] =>
                Ok(Name::Id(s.clone())),
            [Part::Op(ref s)] =>
                Ok(Name::Op(s.clone())),
            _ =>
                Ok(Name::Mixfix(parts.clone()))
        }

    }

    // () is nothing
    // (e) is just e
    // (e1, e2) is a tuple
    fn parse_tuple_exp(&mut self) -> PResult<Exp> {
        consume_without_check!(self, Token::Lp);

        match *self.lookahead() {
            Token::Rp => {
                // () is nothing
                self.eat();
                Ok(Exp::Lit { lit: Lit::Nothing })
            },
            _ => {
                let first = self.parse_exp()?;

                match *self.lookahead() {
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
                            if *self.lookahead() == Token::Rp {
                                break;
                            }

                            let e = self.parse_exp()?;
                            es.push(e);

                            if *self.lookahead() == Token::Comma {
                                self.eat();
                            }
                            else {
                                break;
                            }
                        }

                        consume!(self, Token::Rp)?;

                        Ok(Exp::Tuple { es })
                    },
                    ref t => {
                        self.error_unexpected(vec![Token::Rp, Token::Comma], t.clone())
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
            match *self.lookahead() {
                Token::Rp | Token::Rb | Token::Rc => {
                    Ok(Exp::Lit { lit: Lit::Nothing })
                },
                _ => {
                    let first = self.parse_exp()?;

                    match *self.lookahead() {
                        Token::Comma => {
                            // (e1, e2)
                            self.eat();

                            let mut es = Vec::new();
                            es.push(first);

                            loop {
                                let e = self.parse_exp()?;
                                es.push(e);

                                if *self.lookahead() == Token::Comma {
                                    self.eat();
                                }
                                else {
                                    break;
                                }
                            }

                            Ok(Exp::Tuple { es })
                        },
                        ref t => {
                            Ok(first.value)
                        },
                    }
                },
            }
        })
    }

    fn parse_list_exp(&mut self) -> PResult<Exp> {
        consume_without_check!(self, Token::Lb);

        let mut es = Vec::new();

        match *self.lookahead() {
            Token::Rb => {
            },
            _ => {
                let first = self.parse_exp()?;

                match *self.lookahead() {
                    Token::Comma => {
                        self.eat();
                        es.push(first);

                        loop {
                            match *self.lookahead() {
                                Token::Comma => {
                                    self.eat();
                                    if *self.lookahead() != Token::Rb {
                                        let e = self.parse_exp()?;
                                        es.push(e);
                                    }
                                },
                                _ => {
                                    break;
                                }
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
            match *self.lookahead() {
                Token::Underscore => {
                    // _ is the any value
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
                    Ok(Exp::Lit { lit: Lit::Char { value: n.clone() } })
                },
                Token::Lp => {
                    self.parse_tuple_exp()
                },
                Token::Lb => {
                    self.parse_list_exp()
                }
                Token::Lc => {
                    let cmds = self.parse_block()?;

                    let mut all_arrows = true;
                    let mut all_defs = true;

                    for cmd in &cmds {
                        match **cmd {
                            Cmd::Def(ref d) => {
                                all_arrows = false;
                            },
                            Cmd::Exp(ref e @ Exp::Lambda { .. }) => {
                                all_defs = false;
                            },
                            Cmd::Exp(ref e @ Exp::Arrow { .. }) => {
                                all_defs = false;
                            },
                            _ => {
                                all_arrows = false;
                                all_defs = false;
                            },
                        }
                    }

                    if cmds.len() == 0 {
                        // The block is empty.
                        // Create a nothing.
                        Ok(Exp::Lit { lit: Lit::Nothing })
                    }
                    else if all_arrows {
                        // All the commands are arrows.
                        // Create a union of functions.
                        let arrows: Vec<Located<Exp>> = filter_collect_loc!(cmds, Cmd::Exp(e), e);
                        Ok(Exp::Union { es: arrows })
                    }
                    else if all_defs {
                        // All the cmds are defs.
                        // Create a trait.
                        let defs: Vec<Located<Def>> = filter_collect_loc!(cmds, Cmd::Def(d), d);
                        Ok(Exp::Trait { scope_id: self.alloc_scope(), defs: defs })
                    }
                    else {
                        // At least some of the cmds are expressions.
                        // Create a layout.
                        Ok(Exp::Layout { scope_id: self.alloc_scope(), cmds: cmds })
                    }
                },
                Token::Id(_) | Token::Op(_) | Token::Bang | Token::Question | Token::Tick => {
                    let id = self.alloc_name();
                    let name = self.parse_name()?;
                    Ok(Exp::Name { name, id })
                },
                ref t => {
                    panic!("unexpected token {}", t.clone());
                    self.error_here("unexpected token")
                },
            }
        })
    }

    fn parse_name(&mut self) -> PResult<Name> {
        match *self.lookahead() {
            Token::Id(ref s) => {
                self.eat();
                Ok(Name::Id(s.clone()))
            },
            Token::Op(ref s) => {
                self.eat();
                Ok(Name::Op(s.clone()))
            },
            Token::Bang => {
                self.eat();
                Ok(Name::Op(String::from("!")))
            },
            Token::Question => {
                self.eat();
                Ok(Name::Op(String::from("?")))
            },
            Token::Tick => {
                self.parse_mixfix_name()
            },
            _ => {
                self.error_here("expected name")
            },
        }
    }

    fn alloc_scope(&mut self) -> ScopeId {
        let id = self.next_scope_id;
        self.next_scope_id += 1;
        ScopeId::Scope(id)
    }

    fn alloc_name(&mut self) -> NameId {
        let id = self.next_name_id;
        self.next_name_id += 1;
        id
    }

    fn alloc_mixfix(&mut self) -> MixfixId {
        let id = self.next_mixfix_id;
        self.next_mixfix_id += 1;
        id
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
            assoc: Assoc::NonAssoc,
            by_name: CallingConv::ByValue,
            mode: mode,
            pat: Box::new(Located{ loc: loc, value: e })
        }
    )
}

#[cfg(test)]
mod tests {
    use parser::parse::Parser;
    use syntax::trees::*;
    use syntax::names::*;
    use syntax::loc::*;

    macro_rules! test_parse_ok {
        ($input: expr, $ast: expr) => {
            let mut p = Parser::new("foo.ivo", $input);
            match p.parse_bundle() {
                Ok(t) =>
                    assert_eq!(*t, $ast),
                Err(msg) =>
                    assert_eq!(*msg, String::from("no error")),
            }
        };
    }

    #[test]
    fn test_empty_bundle_1() {
        let mut p = Parser::new("foo.ivo", "");
        match p.parse_bundle() {
            Ok(t) =>
                assert_eq!(*t, Root::Bundle { scope_id: ScopeId::Scope(0), cmds: vec!() }),
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
                assert_eq!(*t, Root::Bundle { scope_id: ScopeId::Scope(0), cmds: vec!() }),
            Err(msg) => {
                println!("{:?}", msg);
                assert!(false)
            }
        }
    }

    #[test]
    fn test_lambda() {
        let mut p = Parser::new("foo.ivo", "fun (x) -> x");
        match p.parse_bundle() {
            Ok(t) =>
                assert_eq!(*t, Root::Bundle {
                    scope_id: ScopeId::Scope(0),
                    cmds: vec![
                        Located {
                            loc: Loc { start: Pos { offset: 0, line: 1, column: 1 }, end: Pos { offset: 11, line: 1, column: 12 }, source: Some(String::from("foo.ivo")) },
                            value: Cmd::Exp(Exp::Lambda {
                                scope_id: ScopeId::Scope(1),
                                opt_guard: None,
                                params: vec![
                                    Located {
                                        loc: Loc { start: Pos { offset: 4, line: 1, column: 5 }, end: Pos { offset: 6, line: 1, column: 7 }, source: Some(String::from("foo.ivo")) },
                                        value: Exp::Name { name: Name::Id(String::from("x")), id: 0 }
                                    }],
                                ret: Box::new(Located {
                                    loc: Loc { start: Pos { offset: 11, line: 1, column: 12 }, end: Pos { offset: 11, line: 1, column: 12 }, source: Some(String::from("foo.ivo")) },
                                    value: Exp::Name { name: Name::Id(String::from("x")), id: 1 }
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

    #[test]
    fn test_trait_without_body() {
        let mut p = Parser::new("foo.ivo", r#"
            trait T
        "#);
        match p.parse_bundle() {
            Ok(t) =>
                assert_eq!(*t, Root::Bundle {
                    scope_id: ScopeId::Scope(0),
                    cmds: vec!()
                }),
            Err(msg) => {
                println!("{:?}", msg);
                assert!(false)
            }
        }
    }

    #[test]
    fn test_trait_with_empty_body() {
        let mut p = Parser::new("foo.ivo", r#"
            trait T = { }
        "#);
        match p.parse_bundle() {
            Ok(t) =>
                assert_eq!(*t, Root::Bundle {
                    scope_id: ScopeId::Scope(0),
                    cmds: vec!()
                }),
            Err(msg) => {
                println!("{:?}", msg);
                assert!(false)
            }
        }
    }

    #[test]
    fn test_trait_with_union() {
        test_parse_ok!("trait T = A with B",
            Root::Bundle {
                scope_id: ScopeId::Scope(0),
                cmds: vec!()
            });
    }

    #[test]
    fn test_trait_with_param() {
        test_parse_ok!("trait T (x)",
            Root::Bundle {
                scope_id: ScopeId::Scope(0),
                cmds: vec!()
            });
    }

    #[test]
    fn test_trait_with_union_with_empty() {
        test_parse_ok!("trait T = A with {}",
            Root::Bundle {
                scope_id: ScopeId::Scope(0),
                cmds: vec!()
            });
    }

    #[test]
    fn test_trait_with_guard() {
        test_parse_ok!("trait T (x) for x = {}",
            Root::Bundle {
                scope_id: ScopeId::Scope(0),
                cmds: vec!(
                    Located::new(
                        NO_LOC,
                        Cmd::Def(
                            Def::MixfixDef {
                                scope_id: ScopeId::Scope(1),
                                flag: MixfixFlag::Trait,
                                name: Name::Id(String::from("T")),
                                opt_guard: Some(
                                    Box::new(
                                        Located::new(
                                            NO_LOC,
                                            Exp::Name {
                                                name: Name::Id(String::from("x")),
                                                id: 0
                                            }
                                        )
                                    )
                                ),
                                params: vec!(
                                    Located::new(
                                        NO_LOC,
                                        Param {
                                            assoc: Assoc::NonAssoc,
                                            by_name: CallingConv::ByValue,
                                            mode: CallingMode::Input,
                                            pat: Box::new(
                                                Located::new(
                                                    NO_LOC,
                                                    Exp::Name {
                                                        name: Name::Id(String::from("x")),
                                                        id: 0
                                                    }
                                                )
                                            )
                                        }
                                    )
                                ),
                                ret: Located::new(
                                    NO_LOC,
                                    Param {
                                        assoc: Assoc::NonAssoc,
                                        by_name: CallingConv::ByValue,
                                        mode: CallingMode::Input,
                                        pat: Box::new(
                                            Located::new(
                                                NO_LOC,
                                                Exp::Trait {
                                                    scope_id: ScopeId::Scope(2),
                                                    defs: vec!()
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

    fn test_names() {
        let mut p = Parser::new("foo.ivo", r#"
            x y z `foo` `_ + _`
        "#);
        match p.parse_bundle() {
            Ok(t) =>
                assert_eq!(*t, Root::Bundle {
                    scope_id: ScopeId::Scope(0),
                    cmds: vec!()
                }),
            Err(msg) => {
                println!("{:?}", msg);
                assert!(false)
            }
        }
    }
}
