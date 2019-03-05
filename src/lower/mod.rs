// Lower from Ivo to forward Ivo
use syntax::trees::*;
use syntax::loc::*;
use syntax::names::*;

// Core expressions.
#[derive(Debug, Clone, PartialEq)]
struct Var {
    name: Name,
    // ty: CoreType,
}

#[derive(Debug, Clone, PartialEq)]
struct FunMode {
    param_modes: Vec<CallingMode>,
    ret_mode: CallingMode
}

#[derive(Debug, Clone, PartialEq)]
enum Core {
    // e1: T1
    // e2: T2
    // -------
    // e1; e2 : T2
    Seq { e1: Box<Core>, e2: Box<Core> },
    Let { var: Var, init: Box<Core>, body: Box<Core> },
    Assign { lhs: Var, rhs: Box<Core> },

    Record { fields: Vec<(Var, Core)> },
    RecordGet { tuple: Box<Core>, field: Var },
    RecordSet { tuple: Box<Core>, field: Var, rhs: Box<Core> },

    Lit { lit: Lit },
    Var { var: Var },
    Lambda { mode: FunMode, param: Var, body: Box<Core> },
    Apply { fun: Box<Core>, arg: Box<Core> },

    // e: T
    // ------
    // thunk e : thunk T
    Thunk { flag: ThunkFlag, exp: Box<Core> },
    // e: M[thunk T]
    // -----------
    // select_thunk e : M[thunk T]
    SelectThunk { exp: Box<Core> },
    // e: thunk T
    // ------------
    // force e : T
    Force { exp: Box<Core> },
    // e: M[T]
    // --------------
    // unwrap e : T  (or error)
    Unwrap { exp: Box<Core> },
    // e: T -> T'
    // -----------
    // mode_select e : M[T -> T']
    ModeSelect { mode: FunMode, exp: Box<Core> },

    // m : M[S]
    // f : S => M[T]
    // -------------
    // m >>= f : M[T]
    MBind { m: Box<Core>, f: Box<Core> },
    // m1: M[T]
    // m2: M[T]
    // ------------
    // m1 ++ m2 : M[T]
    MPlus { m1: Box<Core>, m2: Box<Core> },
    // e : T
    // ----------
    // return e : M[T]
    MReturn { exp: Box<Core> },
    // mzero: M[T]
    MZero /* { ty: CoreType } */,

    // array
    NewArray { /* ty: CoreType, */ len: Box<Core> },
    ArrayLit { es: Vec<Core> },
    ArrayGet { a: Box<Core>, i: Box<Core> },
    ArraySet { a: Box<Core>, i: Box<Core>, v: Box<Core> },

    If { cond: Box<Core>, if_true: Box<Core>, if_false: Box<Core> },
    // e1: T
    // e2: T
    // -------
    // e1 = e2 : Bool
    Eq { e1: Box<Core>, e2: Box<Core> },
}

#[derive(Debug, Clone, PartialEq)]
enum CoreType {
    Void, Boolean, I8, I16, I32, I64,
    M(Box<CoreType>),
    Fun(Box<CoreType>, Box<CoreType>),
    Record(Vec<(Name, CoreType)>),
    Thunk(Box<CoreType>),
    Array(Box<CoreType>),
    Dyn
}

#[derive(Debug, Clone, PartialEq)]
enum ThunkFlag {
    Default,
    Unique,
    Normal,
}

struct Lower;

impl Lower {
    #[allow(non_snake_case)]
    fn TRUE() -> Core {
        // FIXME
        Core::Lit { lit: Lit::Wildcard }
    }

    #[allow(non_snake_case)]
    fn VOID() -> Core {
        Core::Lit { lit: Lit::Nothing }
    }

    fn contains_attribute(name: Name, attrs: &Vec<Located<Attr>>) -> bool {
        for attr in attrs {
            if Lower::is_attribute(name, attr) {
                return true;
            }
        }
        false
    }

    fn is_attribute(name: Name, attr: &Located<Attr>) -> bool {
        match &attr.value {
            Attr::Parens(attr) => Lower::is_attribute(name, attr),
            Attr::Braces(attr) => Lower::is_attribute(name, attr),
            Attr::Brackets(attr) => Lower::is_attribute(name, attr),
            Attr::Name { name: x } => *x == name,
            _ => false,
        }
    }

    // TODO:
    // traits memoize

    // trait f (p) = e
    // -->
    // var z = HashMap::new();
    // val f = fun (x) -> {
    //     if z.contains(x) { return Thunk z.get(x) }
    //     else {
    //         let p = x in { let r = Thunk e in z.put(x, r); r }
    //     }
    // }

    // TODO:
    // call by name parameters (leave for now and just bake in `if`).

    // Lower a mixfix definition.
    fn lower_lambda(attrs: &Vec<Located<Attr>>,
                            flag: &MixfixFlag,
                            name: &Name,
                            opt_guard: &Option<Box<Located<Exp>>>,
                            opt_body: &Option<Box<Located<Exp>>>,
                            params: &Vec<Located<Param>>,
                            ret: &Located<Param>) -> Core {

        if ret.attr.mode == CallingMode::Output {
            assert!(params.iter().all(|Located { value: Param { attr, .. }, .. }| attr.mode == CallingMode::Input));
            Lower::lower_forward_lambda(attrs, flag, name, opt_guard, opt_body, params, ret)
        }
        else {
            Lower::lower_backward_lambda(attrs, flag, name, opt_guard, opt_body, params, ret)
        }
    }

    // Forward lambda.
    fn lower_forward_lambda(attrs: &Vec<Located<Attr>>,
                            flag: &MixfixFlag,
                            name: &Name,
                            opt_guard: &Option<Box<Located<Exp>>>,
                            opt_body: &Option<Box<Located<Exp>>>,
                            params: &Vec<Located<Param>>,
                            ret: &Located<Param>) -> Core {

        // If there are input parameters, make a lambda that matches the first parameter,
        // then returns a lambda with one fewer input.
        if let Some((Located { value: Param { attr, pat }, .. }, rest)) = params.split_first() {
            /*
            (~~> (translate-λ-fwd ω [(μ_1 e_11) (μ_2 e_12) (μ_3 e_13) ...] ret (! e_2) where [e_3 ...])
                 (λ [μ_1 μ_2 μ_3 ... -> !]
                   x ->
                   (translate-let
                    (e_11 = x) in
                    (translate-λ-fwd ω [(? e_12) (? e_13) ...] ret (! e_2) where [e_3 ...])))
                 (where (? ...) (μ_1 μ_2 μ_3 ...))
                 (fresh x)
                 "λ-fwd-inputs")
            */

            let x = Name::fresh("input");

            Core::Lambda {
                mode: FunMode {
                    param_modes: params.iter().map(|_| CallingMode::Input).collect(),
                    ret_mode: CallingMode::Output,
                },
                param: Var { name: x },
                body: box
                    Lower::lower_let(pat, Core::Var { var: Var { name: x } },
                        Lower::lower_forward_lambda(attrs, flag, name, opt_guard, opt_body, &rest.to_vec(), ret))
            }
        }

        // If there are no input parameters, match the guard, if any.
        // If not, return a thunk that evaluates the function body.
        else {
            if let Some(guard) = opt_guard {
                /*
                        ; forward mode lambda without inputs
                        ; check the guard
                        (~~> (translate-λ-fwd ω [] ret (! e_2) where [e_31 e_32 ...])
                             (translate-let (e_31 = tt) in (translate-λ-fwd ω [] ret (! e_2) where [e_32 ...]))
                             "λ-fwd-guard")
                */

                // Match the guard.
                Lower::lower_let(guard, Lower::TRUE(),
                    Lower::lower_forward_lambda(attrs, flag, name, &None, opt_body, params, ret))
            }
            else {
                let flag = Lower::get_thunk_flag(attrs);

                /*
                ; return the body as a thunk
                (~~> (translate-λ-fwd ω [] ret (! e_2) where [])
                (amb (θ ω (translate e_2)))
                "λ-fwd-output")
                */

                // If there is a body formula, solve it and eval the return expression.
                if let Some(body) = opt_body {
                    Core::MReturn {
                        exp: box
                        Core::Thunk {
                            flag: flag,
                            exp: box
                                Core::Unwrap {
                                    exp: box Lower::lower_let(body, Lower::TRUE(), Lower::lower_exp(&*ret.pat))
                                }
                        }
                    }
                }
                else {
                    // If there is no body formula, just eval the return expression.
                    Core::MReturn {
                        exp: box
                            Core::Thunk {
                                flag: flag,
                                exp: box Lower::lower_exp(&*ret.pat)
                            }
                    }
                }
            }
        }
    }

    fn lower_backward_lambda(attrs: &Vec<Located<Attr>>,
                            flag: &MixfixFlag,
                            name: &Name,
                            opt_guard: &Option<Box<Located<Exp>>>,
                            opt_body: &Option<Box<Located<Exp>>>,
                            params: &Vec<Located<Param>>,
                            ret: &Located<Param>) -> Core {


        /*
        ; match the return input
        (~~> (translate-λ-bwd ω [(μ_1 e_1) ...] ret (? e_2) where [e_3 ...])
             (λ [μ_1 ... -> ?]
               b ->
               (translate-let
                (e_2 = b) in
                (translate-λ-bwd ω [(μ_1 e_1) ...] where [e_3 ...])))
             (fresh b)
             "λ-bwd-return")
        */

        let b = Name::fresh("return");

        Core::Lambda {
            mode: FunMode {
                param_modes: params.iter().map(|p| p.attr.mode).collect(),
                ret_mode: CallingMode::Input,
            },
            param: Var { name: b },
            body: box
                Lower::lower_let(&*ret.pat, Core::Var { var: Var { name: b } },
                    Lower::lower_backward_lambda_after_ret(attrs, flag, name, opt_guard, opt_body, params))
            }
    }

    fn lower_backward_lambda_after_ret(attrs: &Vec<Located<Attr>>,
                        flag: &MixfixFlag,
                        name: &Name,
                        opt_guard: &Option<Box<Located<Exp>>>,
                        opt_body: &Option<Box<Located<Exp>>>,
                        params: &Vec<Located<Param>>) -> Core {

        let mut first_input = None;
        let mut remaining = vec![];

        for p in params {
            match first_input {
                None => {
                    if p.value.attr.mode == CallingMode::Input {
                        first_input = Some(p)
                    }
                    else {
                        remaining.push(p.clone())
                    }
                }
                _ => {
                    remaining.push(p.clone())
                }
            }
        }

        // If there is an input parameter, match it.
        if let Some(p) = first_input {
            /*
                ; match the first inputs
                (~~> (translate-λ-bwd ω [(! e_11) ... (? e_12) (μ_13 e_13) ...] where [e_3 ...])
                     (amb (λ [? -> !]
                       a ->
                       (translate-let
                        (e_12 = a) in
                        (translate-λ-bwd ω [(! e_11) ... (μ_13 e_13) ...] where [e_3 ...]))))
                     (fresh a)
                     "λ-bwd-inputs")
            */

            let a = Name::fresh("input");

            Core::MReturn {
                exp: box
                    Core::Lambda {
                        mode: FunMode {
                            param_modes: vec![CallingMode::Input],
                            ret_mode: CallingMode::Output,
                        },
                        param: Var { name: a },
                        body: box
                            Lower::lower_let(&*p.value.pat, Core::Var { var: Var { name: a } },
                                Lower::lower_backward_lambda_after_ret(attrs, flag, name, opt_guard, opt_body, &remaining))
                    }
            }
        }
        else if let Some(guard) = opt_guard {
            /*
            ; only outputs
            ; match the guard
            (~~> (translate-λ-bwd ω [(! e_11) ...] where [e_31 e_32 ...])
            (translate-let (e_31 = tt) in (translate-λ-bwd ω [(! e_11) ...] where [e_32 ...]))
            "λ-bwd-guard")
            */

            Lower::lower_let(guard,
                Lower::TRUE(),
                Lower::lower_backward_lambda_after_ret(attrs, flag, name, &None, opt_body, params))
        }
        else {
            /*
            ; only outputs. return a tuple (regardless of arity)
            (~~> (translate-λ-bwd ω [(! e_11) ...] where [])
            (amb (θ ω (tuple (translate e_11) ...)))
            "λ-bwd-thunk")
            */

            let mut result;

            match params.as_slice() {
                [] => {
                    result = Lower::VOID();
                },
                [p] => {
                    result = Lower::lower_exp(&*p.pat);
                },
                _ => {
                    result = Core::Record {
                        fields: params.iter().enumerate().map(|(i, p)|
                        (Var { name: Name::new(&format!("_{}", i)) }, Lower::lower_exp(&*p.pat))
                    ).collect()
                }
                }
            }

            if let Some(body) = opt_body {
                /*
                ; only outputs
                ; match the guard
                (~~> (translate-λ-bwd ω [(! e_11) ...] where [e_31 e_32 ...])
                (translate-let (e_31 = tt) in (translate-λ-bwd ω [(! e_11) ...] where [e_32 ...]))
                "λ-bwd-guard")
                */

                result =
                    Lower::lower_let(body,
                        Lower::TRUE(),
                        result);
            }

            let flag = Lower::get_thunk_flag(attrs);

            Core::MReturn {
                exp: box
                    Core::Thunk {
                        flag: flag,
                        exp: box result
                    }
            }

        }
    }

    fn get_thunk_flag(attrs: &Vec<Located<Attr>>) -> ThunkFlag {
        let is_default = Lower::contains_attribute(Name::new("default"), attrs);
        let is_unique = Lower::contains_attribute(Name::new("unique"), attrs);

        let flag = if is_unique { ThunkFlag::Unique }
                   else if is_default { ThunkFlag::Default }
                   else { ThunkFlag::Normal };

        flag
    }

    fn record_to_struct(tag: &Located<Exp>, defs: &Vec<Located<Def>>) -> Core {
        let mixfixes: Vec<Located<Def>> = defs.iter().filter_map(|c| match &c.value {
            d @ Def::TraitDef { .. } => Some(c.with_value(d.clone())),
            d @ Def::FunDef { .. } => Some(c.with_value(d.clone())),
            _ => None,
        }).collect();

        let formulas: Vec<Located<Exp>> = defs.iter().filter_map(|c| match &c.value {
            Def::FormulaDef { attrs, flag, box formula } => Some(formula.clone()),
            _ => None,
        }).collect();

        let unknowns: Vec<Name> = Lower::get_unknowns_from_formulas(&formulas);
        let mixfix_names: Vec<Name> = defs.iter().filter_map(|c| match &c.value {
            Def::TraitDef { name, .. } => Some(*name),
            Def::FunDef { name, .. } => Some(*name),
            _ => None,
        }).collect();

        let mut fields = vec![];

        fields.push((Var { name: Name::new("#tag") }, Lower::lower_exp(tag)));

        for x in mixfix_names {
            fields.push((Var { name: x }, Core::Var { var: Var { name: x }}));
        }

        for x in unknowns {
            fields.push((Var { name: x }, Core::Var { var: Var { name: x }}));
        }

        let cmds = defs.iter().map(|d| d.with_value(Cmd::Def(d.value.clone()))).collect();

        Lower::layout_to_letrec(&cmds, Core::Record { fields })
    }

    fn get_unknowns_from_formulas(fs: &Vec<Located<Exp>>) -> Vec<Name> {
        let mut names = vec![];
        for f in fs {
            Lower::add_unknowns_from_exp(f, &mut names)
        }
        names
    }

    fn add_unknowns_from_exp(f: &Located<Exp>, names: &mut Vec<Name>) {
        match &f.value {
            Exp::Unknown { name, id } => { names.push(*name) },
            Exp::Layout { id, cmds } => {},
            // Exp::RecordLit { id, tag, fields } => if fields.iter().all(|(x,e)| Lower::mode(e) == CallingMode::Output) { CallingMode::Output } else { CallingMode::Input },
            Exp::Union { box e1, box e2 } => {
                Lower::add_unknowns_from_exp(&e1, names);
                Lower::add_unknowns_from_exp(&e2, names);
            },
            Exp::Intersect { box e1, box e2 } => {
                Lower::add_unknowns_from_exp(&e1, names);
                Lower::add_unknowns_from_exp(&e2, names);
            },
            Exp::Tuple { es } => for e in es { Lower::add_unknowns_from_exp(e, names) },
            Exp::List { es } => for e in es { Lower::add_unknowns_from_exp(e, names) },
            Exp::Lambda { id, opt_guard, params, box ret } => {},
            Exp::For { id, box formula, box body } => {},
            Exp::Let { id, box formula, box body } => {},
            Exp::LetVar { id, box formula, box body } => {},
            Exp::Where { box pat, box guard } => {
                Lower::add_unknowns_from_exp(&pat, names);
                Lower::add_unknowns_from_exp(&guard, names);
            },
            Exp::Arrow { id, box arg, box ret } => {},
            Exp::Assign { box lhs, box rhs } => {},
            Exp::Bind { box lhs, box rhs } => Lower::add_unknowns_from_exp(&lhs, names),
            Exp::Generator { box lhs, box rhs } => Lower::add_unknowns_from_exp(&lhs, names),
            Exp::Select { box exp, name } => {},
            Exp::Within { id, box e1, box e2 } => {},
            Exp::Apply { box fun, box arg } => {
                Lower::add_unknowns_from_exp(&fun, names);
                Lower::add_unknowns_from_exp(&arg, names);
            },
            Exp::Lit { lit } => {},
            Exp::Name { name, id } => {},
            Exp::MixfixPart { name, id } => {},
            Exp::Var { name, id } => {},
            Exp::MixfixApply { es, id } => {},
            _ => unimplemented!()
        }
    }

    // Desugar a layout into a letrec with the body v.
    fn layout_to_letrec(cmds: &Vec<Located<Cmd>>, v: Core) -> Core {
        let mixfixes: Vec<Located<Def>> = cmds.iter().filter_map(|c| match &c.value {
            Cmd::Def(d @ Def::TraitDef { .. }) => Some(c.with_value(d.clone())),
            Cmd::Def(d @ Def::FunDef { .. }) => Some(c.with_value(d.clone())),
            Cmd::Def(_) => None,
            Cmd::Exp(e) => None,
        }).collect();

        let formulas: Vec<Located<Exp>> = cmds.iter().filter_map(|c| match &c.value {
            Cmd::Def(Def::FormulaDef { attrs, flag, box formula }) => Some(formula.clone()),
            Cmd::Def(_) => None,
            Cmd::Exp(_) => None,
        }).collect();

        // Collect all unknowns from the formulas.
        let unknowns: Vec<Name> = Lower::get_unknowns_from_formulas(&formulas);

        // Go from the back to the front.
        let (body, _) = cmds.iter().rev().fold((v, true), |(t, is_first), c| match &c.value {
            Cmd::Exp(e) => {
                let t1 = Lower::lower_exp(&c.with_value(e.clone()));
                if is_first {
                    (t1, false)
                }
                else {
                    (Core::Seq {
                        e1: box t1,
                        e2: box t
                    }, false)
                }
            },
            Cmd::Def(Def::FormulaDef { attrs, flag, formula }) => {
                let mut subst = vec![];
                let formula2 = Lower::freshen(formula, &mut subst);
                // TODO: collect identical unknowns and overload.
                let t2 = subst.iter().fold(t, |t, (x, y)| {
                    Core::Seq {
                        e1: box Core::Assign { lhs: Var { name: *x }, rhs: box Core::Var { var: Var { name: *y } }, },
                        e2: box t
                    }
                });
                (Core::Unwrap {
                    exp: box Lower::lower_let(&formula2, Lower::TRUE(), t2)
                }, is_first)
            },
            Cmd::Def(Def::TraitDef { .. }) => (t, is_first),
            Cmd::Def(Def::FunDef { .. }) => (t, is_first),
            Cmd::Def(Def::ImportDef { .. }) => (t, is_first),
        });

        let mixfix_bind = cmds.iter().rev().fold(body, |t, c| match &c.value {
            Cmd::Exp(e) => t,
            Cmd::Def(Def::FormulaDef { attrs, flag, formula }) => t,
            Cmd::Def(Def::TraitDef { id, attrs, name, opt_guard, params, supers, defs }) => {
                let ret = Located::new(
                    Loc::no_loc(),
                    Param {
                        attr: ParamAttr {
                            mode: CallingMode::Output,
                            assoc: Assoc::NonAssoc,
                            by_name: CallingConv::ByValue
                        },
                        pat: box Located::new(
                            Loc::no_loc(),
                            unimplemented!()
                            // Exp::Record { id: NodeId(0), defs: defs.clone() }
                        )
                    }
                );
                Core::Seq {
                    e1: box Core::Assign {
                        lhs: Var { name: *name },
                        rhs: box Lower::lower_lambda(attrs, &MixfixFlag::Trait, name, opt_guard, &None, params, &ret),
                    },
                    e2: box t
                }
            },
            Cmd::Def(Def::FunDef { id, attrs, name, opt_guard, opt_body, params, ret }) => {
                // TODO: collect identical names and overload
                Core::Seq {
                    e1: box Core::Assign {
                        lhs: Var { name: *name },
                        rhs: box Lower::lower_lambda(attrs, &MixfixFlag::Fun, name, opt_guard, opt_body, params, ret),
                    },
                    e2: box t
                }
            },
            Cmd::Def(Def::ImportDef { .. }) => t,
        });

        let mixfix_init = cmds.iter().rev().fold(mixfix_bind, |t, c| match &c.value {
            Cmd::Def(Def::TraitDef { name, .. }) => {
                Core::Let {
                    var: Var { name: *name },
                    init: box Lower::VOID(),
                    body: box t
                }
            },
            Cmd::Def(Def::FunDef { name, .. }) => {
                Core::Let {
                    var: Var { name: *name },
                    init: box Lower::VOID(),
                    body: box t
                }
            },
            _ => t,
        });

        let unknown_init = unknowns.iter().rev().fold(mixfix_init, |t, x|
            Core::Let {
                var: Var { name: *x },
                init: box Lower::VOID(),
                body: box t
            }
        );

        unknown_init
    }

    fn freshen(e: &Located<Exp>, subst: &mut Vec<(Name, Name)>) -> Located<Exp> {
        let e1 = match &e.value {
            Exp::Unknown { name: x, id } => {
                let y = Name::fresh(&x.to_string());
                subst.push((*x, y));
                Exp::Unknown { name: y, id: *id }
            },
            Exp::Bind { box lhs, box rhs } => {
                Exp::Bind { lhs: box Lower::freshen(&lhs, subst), rhs: box rhs.clone() }
            },
            Exp::Generator { box lhs, box rhs } => {
                Exp::Generator { lhs: box Lower::freshen(lhs, subst), rhs: box rhs.clone() }
            },
            Exp::Where { box pat, box guard } => {
                Exp::Where { pat: box Lower::freshen(pat, subst), guard: box Lower::freshen(guard, subst) }
            },
            Exp::Apply { box fun, box arg } => {
                Exp::Apply { fun: box Lower::freshen(&fun, subst), arg: box Lower::freshen(&arg, subst) }
            }
            e => unimplemented!(),
        };

        e.with_value(e1)
    }

    fn lower_let(lhs: &Located<Exp>, rhs: Core, body: Core) -> Core {
        if Lower::mode(lhs) == CallingMode::Output {
            Core::If {
                cond: box Core::Eq {
                    e1: box Lower::lower_exp(lhs),
                    e2: box rhs
                },
                if_true: box body,
                if_false: box Core::MZero
            }
        }
        else {
            match &lhs.value {
                Exp::Unknown { id, name } => {
                    Core::Let { var: Var { name: *name }, init: box rhs, body: box body }
                },
                Exp::Bind { lhs: box lhs1, rhs: box rhs1 } => {
                    // let (e1 = e2) = t1 in t2
                    // ->
                    // if t1 then let e1 = [e2] in t2
                    Core::If {
                        cond: box rhs,
                        if_true: box Lower::lower_let(&lhs1, Lower::lower_exp(&rhs1), body),
                        if_false: box Core::MZero,
                    }
                },
                Exp::Generator { lhs: box lhs1, rhs: box rhs1 } => {
                    let x = Name::fresh("element");
                    Core::If {
                        cond: box rhs,
                        if_true:
                            box Core::MBind {
                                m: box Lower::lower_exp(&*rhs1),
                                f: box
                                    Core::Lambda {
                                        mode: FunMode {
                                            param_modes: vec![CallingMode::Input],
                                            ret_mode: CallingMode::Output,
                                        },
                                        param: Var { name: x },
                                        body: box Lower::lower_let(&lhs1, Core::Var { var: Var { name: x } }, body)
                                    }
                            },
                        if_false: box Core::MZero,
                    }
                },
                Exp::Where { box pat, box guard } => {
                    Lower::lower_let(&pat, rhs, Lower::lower_let(&guard, Lower::TRUE(), body))
                },
                Exp::Apply { box fun, box arg } => {
                    let (fun, args) = Lower::match_call(lhs);

                    let inputs: Vec<Located<Exp>> = args.iter().filter(|a| Lower::invert_mode(a) == CallingMode::Input).cloned().collect();
                    let outputs: Vec<Located<Exp>> = args.iter().filter(|a| Lower::invert_mode(a) == CallingMode::Output).cloned().collect();
                    let modes: Vec<CallingMode> = args.iter().map(|a| Lower::invert_mode(a)).collect();

                    let f = Name::fresh("f");
                    let xs: Vec<Name> = inputs.iter().map(|_| Name::fresh("a")).collect();
                    let x = Name::fresh("x");
                    let g = Name::fresh("g");
                    let th = Name::fresh("th");
                    let r = Name::fresh("r");

                    let mode_select = Core::MBind {
                        m: box Core::Var { var: Var { name: f } },
                        f: box Core::Lambda {
                            mode: FunMode {
                                param_modes: vec![CallingMode::Input],
                                ret_mode: CallingMode::Output,
                            },
                            param: Var { name: g },
                            body: box Core::ModeSelect {
                                mode: FunMode {
                                    param_modes: modes,
                                    ret_mode: CallingMode::Input,
                                },
                                exp: box Core::Var { var: Var { name: g } },
                            }
                        }
                    };

                    let pass_return = Core::MBind {
                        m: box mode_select,
                        f: box Core::Lambda {
                            mode: FunMode {
                                param_modes: vec![CallingMode::Input],
                                ret_mode: CallingMode::Output,
                            },
                            param: Var { name: g },
                            body: box Core::Apply {
                                fun: box Core::Var { var: Var { name: g } },
                                arg: box rhs,
                            }
                        }
                    };

                    let pass_inputs = inputs.iter().rev().fold(pass_return,
                        |m, a|
                        Core::MBind {
                            m: box m,
                            f: box Core::Lambda {
                                mode: FunMode {
                                    param_modes: vec![CallingMode::Input],
                                    ret_mode: CallingMode::Output,
                                },
                                param: Var { name: g },
                                body: box Core::Apply {
                                    fun: box Core::Var { var: Var { name: g } },
                                    arg: box Lower::lower_exp(a),
                                }
                            }
                        }
                    );

                    let select_thunk = Core::SelectThunk { exp: box pass_inputs };

                    let run_thunk = Core::MBind {
                        m: box select_thunk,
                        f: box Core::Lambda {
                            mode: FunMode {
                                param_modes: vec![CallingMode::Input],
                                ret_mode: CallingMode::Output,
                            },
                            param: Var { name: th },
                            body: box Core::MReturn {
                                exp: box Core::Force {
                                    exp: box Core::Var { var: Var { name: th } },
                                }
                            }
                        }
                    };

                    // match the result against the output patterns
                    let match_outputs = {
                        match outputs.as_slice() {
                            [] => {
                                Core::MBind {
                                    m: box run_thunk,
                                    f: box Core::Lambda {
                                        mode: FunMode {
                                            param_modes: vec![CallingMode::Input],
                                            ret_mode: CallingMode::Output,
                                        },
                                        param: Var { name: r },
                                        body: box body, // r should be 'void'. if not, run_thunk should fail and we won't be called. No need to check.
                                    }
                                }
                            },
                            [output] => {
                                Core::MBind {
                                    m: box run_thunk,
                                    f: box Core::Lambda {
                                        mode: FunMode {
                                            param_modes: vec![CallingMode::Input],
                                            ret_mode: CallingMode::Output,
                                        },
                                        param: Var { name: r },
                                        body: box Lower::lower_let(output, Core::Var { var: Var { name: r } }, body)
                                    }
                                }
                            },
                            outputs => {
                                Core::MBind {
                                    m: box run_thunk,
                                    f: box Core::Lambda {
                                        mode: FunMode {
                                            param_modes: vec![CallingMode::Input],
                                            ret_mode: CallingMode::Output,
                                        },
                                        param: Var { name: r },
                                        body: box Lower::lower_let(
                                            &Located::new(Loc::no_loc(),
                                                          Exp::Tuple { es: outputs.to_vec() }),
                                            Core::Var { var: Var { name: r } },
                                            body)
                                    }
                                }
                            },
                        }
                    };

                    let let_inputs = inputs.iter().zip(xs.iter()).rev().fold(match_outputs, |t, (input, a)|
                        Core::Let {
                            var: Var { name: *a },
                            init: box Lower::lower_exp(input),
                            body: box t
                        }
                    );

                    Core::Let {
                        var: Var { name: f },
                        init: box Lower::lower_amb(&fun),
                        body: box let_inputs,
                    }
                },
                _ => unimplemented!()
            }
        }
    }

    fn lower_amb(e: &Located<Exp>) -> Core {
        unimplemented!()
    }

    fn match_call(e: &Located<Exp>) -> (Located<Exp>, Vec<Located<Exp>>) {
        match &e.value {
            Exp::Apply { box fun, box arg } => {
                let (fun, fargs) = Lower::match_call(fun);
                let mut args = vec![];
                args.extend(fargs.iter().cloned());
                args.push(arg.clone());
                (fun, args)
            },
            _ => (e.clone(), vec![]),
        }
    }

    fn make_call(fun: Core, args: &[Core]) -> Core {
        if let Some((last, init)) = args.split_last() {
            Core::Apply {
                fun: box Lower::make_call(fun, &init),
                arg: box last.clone(),
            }
        }
        else {
            fun
        }
    }

    // Lower a forward mode call.
    fn lower_exp(e: &Located<Exp>) -> Core {
        match &e.value {
            Exp::Layout { id, cmds } => {
                Core::Force {
                    exp: box
                        Core::Unwrap {
                            exp: box
                                Lower::layout_to_letrec(cmds, Lower::VOID())
                        }
                }
            },
            Exp::Union { box e1, box e2 } => { unimplemented!() },
            Exp::Intersect { box e1, box e2 } => { unimplemented!() },
            Exp::Tuple { es } => { unimplemented!() },
            Exp::List { es } => { unimplemented!() },
            Exp::Lambda { id, opt_guard, params, box ret } => { unimplemented!() },
            Exp::Let { id, box formula, box body } => {
                Core::Unwrap {
                    exp: box Lower::lower_let(formula, Lower::TRUE(), Lower::lower_amb(body))
                }
            },
            Exp::LetVar { id, box formula, box body } => {
                Core::Unwrap {
                    exp: box Lower::lower_let(formula, Lower::TRUE(), Lower::lower_amb(body))
                }
            },
            Exp::For { id, box formula, box body } => {
                Lower::lower_let(formula, Lower::TRUE(), Lower::lower_amb(body))
            },
            Exp::Where { box pat, box guard } => {
                // let x where x > 0 = y
                // x where x > 0  -- evaluates to x and asserts x > 0
                // for (x > 0) x  -- evaluates to all positive x
                let e1 = Lower::lower_exp(pat);
                let e2 = Lower::lower_exp(guard);
                let x = Name::fresh("x");
                Core::Let {
                    var: Var { name: x },
                    init: box e1,
                    body: box Core::If {
                        cond: box e2,
                        if_true: box Core::Var { var: Var { name: x } },
                        if_false: box Core::Lit { lit: Lit::Nothing },
                    }
                }
            },
            Exp::Arrow { id, box arg, box ret } => { unimplemented!() },
            Exp::Assign { box lhs, box rhs } => {
                unimplemented!()
            },
            Exp::Bind { box lhs, box rhs } => { unimplemented!() },
            Exp::Generator { box lhs, box rhs } => { unimplemented!() },
            Exp::Select { box exp, name } => { unimplemented!() },
            Exp::Within { id, box e1, box e2 } => { unimplemented!() },
            Exp::Apply { box fun, box arg } => {
                let (fun, args) = Lower::match_call(&e);

                // (apply-θ
                //   (unwrap
                //     (translate-amb-delay (e_1 e_2 e_3 ...))))

                Core::Force {
                    exp: box
                        Core::Unwrap {
                            exp: box
                                Lower::lower_amb_delay(&fun, &args)
                        }
                }
            },
            Exp::Lit { lit } => {
                Core::Lit { lit: lit.clone() }
            },
            Exp::Name { name, id } => { unimplemented!() },
            Exp::Unknown { name, id } => { unimplemented!() },
            Exp::MixfixPart { name, id } => { unimplemented!() },
            Exp::Var { name, id } => {
                Core::Var { var: Var { name: name.clone() } }
            },
            Exp::MixfixApply { es, id } => { unimplemented!() },

            // Special cases:
            // Prelude.True --> tt
            // Prelude.False --> ff
            // Prim.foo -> native core code

            // Call by name functions (not yet implemented in lowering, so special case them)
            // Prelude.if -> native core code
            // Prelude.whlie -> native core code
            // Prelude.match -> native core code
            // Prelude.&&
            // Prelude.||

            e => unimplemented!(),
        }
    }

    fn lower_amb_delay(fun: &Located<Exp>, args: &Vec<Located<Exp>>) -> Core {
        // (select-θ
        //   (let [(x_11 = ((translate-amb e_1)
        //      >>= (λ [? -> !] x_12 -> (mode-select [μ ... -> !] x_12))))] in
        //     (translate-let (((unk x_13) <- x_11) = tt)
        //        (((unk x_2) = e_2) = tt)
        //        (((unk x_3) = e_3) = tt) ... in
        //        (x_13 x_2 x_3 ...))))
        // (fresh x_11 x_12 x_13 x_2 ((x_3 ...) (e_3 ...)))
        // (where (μ ...) ((invert-mode e_2) (invert-mode e_3) ...))
        // (where (? ...) (μ ...))

        let x11 = Name::fresh("x11");
        let x12 = Name::fresh("x12");
        let x13 = Name::fresh("x13");

        let x2s: Vec<Name> = args.iter().map(|_| Name::fresh("x2")).collect();
        let x2vars: Vec<Core> = x2s.iter().map(|x2| Core::Var { var: Var { name: *x2 } }).collect();
        let apply = Lower::make_call(Core::Var { var: Var { name: x13 } }, &x2vars);

        let body = args.iter().zip(x2s.iter()).fold(apply, |t, (a, x2)| {
            Core::Let {
                var: Var { name: *x2 },
                init: box Lower::lower_exp(a),
                body: box t
            }
        });

        let param_modes = args.iter().map(|_| CallingMode::Input).collect();
        let ret_mode = CallingMode::Output;

        Core::SelectThunk {
            exp: box Core::MBind {
                m: box Core::MBind {
                    m: box Lower::lower_amb(fun),
                    f: box Core::Lambda {
                        mode: FunMode {
                            param_modes: vec![CallingMode::Input],
                            ret_mode: CallingMode::Output,
                        },
                        param: Var { name: x12 },
                        body: box Core::ModeSelect {
                            mode: FunMode {
                                param_modes, ret_mode
                            },
                            exp: box Core::Var { var: Var { name: x12 } }
                        }
                    }
                },
                f: box Core::Lambda {
                    mode: FunMode {
                        param_modes: vec![CallingMode::Input],
                        ret_mode: CallingMode::Output,
                    },
                    param: Var { name: x13 },
                    body: box body
                },
            }
        }
    }

    fn invert_mode(e: &Located<Exp>) -> CallingMode {
        match Lower::mode(e) {
            CallingMode::Input => CallingMode::Output,
            CallingMode::Output => CallingMode::Input,
        }
    }

    // Return In if any subexpression is an unknown.
    fn mode(e: &Located<Exp>) -> CallingMode {
        match &e.value {
            Exp::Layout { id, cmds } => CallingMode::Output,
            Exp::Union { box e1, box e2 } => if [e1, e2].iter().all(|e| Lower::mode(e) == CallingMode::Output) { CallingMode::Output } else { CallingMode::Input },
            Exp::Intersect { box e1, box e2 } => if [e1, e2].iter().all(|e| Lower::mode(e) == CallingMode::Output) { CallingMode::Output } else { CallingMode::Input },
            Exp::Tuple { es } => if es.iter().all(|e| Lower::mode(e) == CallingMode::Output) { CallingMode::Output } else { CallingMode::Input },
            Exp::List { es } => if es.iter().all(|e| Lower::mode(e) == CallingMode::Output) { CallingMode::Output } else { CallingMode::Input },
            Exp::Lambda { id, opt_guard, params, box ret } => CallingMode::Output,
            Exp::Let { id, box formula, box body } => CallingMode::Output,
            Exp::LetVar { id, box formula, box body } => CallingMode::Output,
            Exp::For { id, box formula, box body } => CallingMode::Output,
            Exp::Where { box pat, box guard } => if [pat, guard].iter().all(|e| Lower::mode(e) == CallingMode::Output) { CallingMode::Output } else { CallingMode::Input },
            Exp::Arrow { id, box arg, box ret } => CallingMode::Output,
            Exp::Assign { box lhs, box rhs } => CallingMode::Output,
            Exp::Bind { box lhs, box rhs } => Lower::mode(&lhs),
            Exp::Generator { box lhs, box rhs } => Lower::mode(&lhs),
            Exp::Select { box exp, name } => CallingMode::Output,
            Exp::Within { id, box e1, box e2 } => CallingMode::Output,
            Exp::Apply { box fun, box arg } => if [fun, arg].iter().all(|e| Lower::mode(e) == CallingMode::Output) { CallingMode::Output } else { CallingMode::Input },
            Exp::Lit { lit } => CallingMode::Output,
            Exp::Name { name, id } => CallingMode::Output,
            Exp::Unknown { name, id } => CallingMode::Input,
            Exp::MixfixPart { name, id } => CallingMode::Output,
            Exp::Var { name, id } => CallingMode::Output,
            Exp::MixfixApply { es, id } => CallingMode::Output,
            _ => unimplemented!()
        }
    }
}
