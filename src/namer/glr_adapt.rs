use crate::namer::glr::*;

use syntax::loc::*;
use syntax::names::*;
use syntax::trees;

use namer::symbols::*;
use namer::glr;

use std::collections::HashMap;
use std::collections::HashSet;

pub(super) struct GLRAdapter;

impl GLRAdapter {
    fn make_rhs_from_name(x: &Name, assoc: Option<usize>, prio: Prio, scope: Scope) -> Vec<Symbol> {
        let lowest = Symbol::Nonterm(Nonterm::E);
        let current = Symbol::Nonterm(Nonterm::M(scope, prio.0));
        let next = Symbol::Nonterm(Nonterm::M(scope, prio.0 + 1));
        let highest = Symbol::Nonterm(Nonterm::Br);

        match x {
            Name::Op(x) => {
                vec![Symbol::Term(Term::Name(Part::Op(x.clone())))]
            },
            Name::Id(x) => {
                vec![Symbol::Term(Term::Name(Part::Id(x.clone())))]
            },
            Name::Mixfix(parts) => {
                match parts.as_slice() {
                    [] => vec![],
                    [Part::Op(x), Part::Placeholder] => {
                        // Unary prefix operators are right associative.
                        vec![Symbol::Term(Term::Name(Part::Op(x.clone()))), current]
                    },
                    [Part::Placeholder, Part::Op(x)] => {
                        // Unary postfix operators are left associative.
                        vec![current, Symbol::Term(Term::Name(Part::Op(x.clone())))]
                    },
                    parts => {
                        println!("parts {:?}", parts);

                        // map all expressions to the highest precedence nonterminal.
                        let mut rhs: Vec<Symbol> = parts.iter().map(|part|
                            match part {
                                Part::Placeholder => highest.clone(),
                                x => Symbol::Term(Term::Name(x.clone())),
                            }).collect();

                        let n = rhs.len();

                        // Handle the associativity annotations on the first and last tokens.
                        if n > 2 {
                            match (&rhs[0], &rhs[1]) {
                                // _ + ...
                                (Symbol::Nonterm(_), Symbol::Term(_)) => {
                                    if assoc == Some(0) || assoc == None {
                                        rhs[0] = current.clone();
                                    }
                                    else {
                                        rhs[0] = next.clone();
                                    }
                                }
                                _ => {},
                            }

                            match (&rhs[n-2], &rhs[n-1]) {
                                // ... + _
                                (Symbol::Term(_), Symbol::Nonterm(_)) => {
                                    if assoc == Some(n-1) {
                                        rhs[n-1] = current.clone();
                                    }
                                    else {
                                        rhs[n-1] = next.clone();
                                    }
                                }
                                _ => {},
                            }
                        }

                        for i in 0..n {
                            if 1 <= i && i+1 < n {
                                match (&rhs[i-1], &rhs[i], &rhs[i+1]) {
                                    // Hole: a nonterminal surrounded by terminals has lowest priority.
                                    // if _ then _ else
                                    // | _ |
                                    (Symbol::Term(_), Symbol::Nonterm(_), Symbol::Term(_)) => {
                                        rhs[i] = lowest.clone();
                                    },
                                    _ => {},
                                }

                                match (&rhs[i-1], &rhs[i], &rhs[i+1]) {
                                    // _ _ x --> use lowest priority before x
                                    (Symbol::Nonterm(Nonterm::Br), Symbol::Nonterm(_), Symbol::Term(_)) => {
                                        rhs[i] = lowest.clone();
                                    },
                                    _ => {},
                                }
                            }
                        }

                        if n > 2 {
                            match (&rhs[n-2], &rhs[n-1]) {
                                (Symbol::Nonterm(Nonterm::Br), Symbol::Nonterm(_)) => {
                                    rhs[n-1] = lowest.clone();
                                }
                                _ => {},
                            }
                        }

                        println!("rhs {:?}", rhs);
                        rhs
                    },
                }
            },
        }
    }

    fn make_rhs_from_decl(decl: &Decl) -> Rule {
        let name = decl.name();
        let assoc = decl.assoc();
        let id = decl.scope();
        let prio = decl.prio();
        let rhs = GLRAdapter::make_rhs_from_name(&name, assoc, prio, id);
        Rule { lhs: Nonterm::M(id, prio.0), rhs: rhs }
    }

    pub fn glr_from_decls(decls: &Vec<Located<Decl>>) -> glr::GLR {
        // S -> E $
        let start_rule = Rule { lhs: Nonterm::S, rhs: vec![Symbol::Nonterm(Nonterm::E), Symbol::Term(Term::End)] };
        // Pr -> Br
        let pr_br = Rule { lhs: Nonterm::Pr, rhs: vec![Symbol::Nonterm(Nonterm::Br)] };
        // Br -> p
        let br_p = Rule { lhs: Nonterm::Br, rhs: vec![Symbol::Term(Term::Primary)] };
        // E -> Pr
        let e_pr = Rule { lhs: Nonterm::E, rhs: vec![Symbol::Nonterm(Nonterm::Pr)] };
        // Pr -> Pr Br
        let pr_pr_br = Rule { lhs: Nonterm::Pr, rhs: vec![Symbol::Nonterm(Nonterm::Pr), Symbol::Nonterm(Nonterm::Br)] };

        let mut rules = vec![
            start_rule.clone(),
            pr_br,
            br_p,
            pr_pr_br,
        ];

        // Take the first decl of each (scope, name) pair -- that is, the one with lowest prio.
        let mut first_decls: HashMap<(Name, Scope), &Decl> = HashMap::new();

        for decl in decls {
            let key = (decl.value.name(), decl.value.scope());
            match first_decls.get(&key) {
                Some(existing) => {
                    if existing.prio().0 < decl.prio().0 {
                        first_decls.insert(key, &decl.value);
                    }
                },
                None => {
                    first_decls.insert(key, &decl.value);
                }
            }
        }

        let decl_rules = first_decls.values().map(|&decl| GLRAdapter::make_rhs_from_decl(decl));
        rules.extend(decl_rules);

        let mut min_prio = HashMap::new();
        let mut max_prio = HashMap::new();
        let mut keys = HashSet::new();

        for rule in &rules {
            match rule.lhs {
                Nonterm::M(id, prio) => {
                    keys.insert(id);
                    match min_prio.get(&id) {
                        Some(i) if prio < *i => { min_prio.insert(id, prio); },
                        None => { min_prio.insert(id, prio); },
                        _ => {},
                    }
                    match max_prio.get(&id) {
                        Some(i) if prio > *i => { max_prio.insert(id, prio+1); },
                        None => { max_prio.insert(id, prio+1); },
                        _ => {},
                    }
                }
                _ => {},
            }
        }

        // Add E -> Pr if there are no other rules.
        if keys.is_empty() {
            rules.push(e_pr);
        }

        for id in keys {
            match (min_prio.get(&id), max_prio.get(&id)) {
                (Some(min), Some(max)) => {
                    // M{min} -> M{n} -> M{n+1} -> ... -> M{max}
                    for k in *min .. *max {
                        rules.push(
                            Rule {
                                lhs: Nonterm::M(id, k),
                                rhs: vec![Symbol::Nonterm(Nonterm::M(id, k+1))],
                            }
                        );
                    }

                    // E -> M{min}
                    rules.push(
                        Rule {
                            lhs: Nonterm::E,
                            rhs: vec![Symbol::Nonterm(Nonterm::M(id, *min))],
                        }
                    );

                    // M{max} -> Br
                    rules.push(
                        Rule {
                            lhs: Nonterm::M(id, *max),
                            rhs: vec![Symbol::Nonterm(Nonterm::Pr)],
                        }
                    );
                },
                _ => {},
            }
        }

        println!("rules {:#?}", &rules);

        let mut lr = LR::new(rules);
        let state = lr.add_state(lr.closure(vec![Item { dot: 0, rule: start_rule }])).unwrap();
        GLR::new(lr, state)
    }
}
