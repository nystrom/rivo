// To implement mixfix parsing, we use the gearley parser library.
// The library is rather low-level, but fast and works for our purposes.

// This module:
// - creates a grammar from the declarations.
// - maps tokens to u32 to be able to use the library.
// - runs the parser.
// - maps the results back into a vec of MixfixTree.

use cfg::Symbol;
use gearley::forest::{Bocage, Traversal};
use gearley::forest::depth_first::{NullOrder, ArrayEvaluator, ValueArray, ActionClosureEvaluator};
use gearley::grammar::Grammar;
use gearley::grammar::InternalGrammar;
use gearley::recognizer::Recognizer;
use gearley::util::slice_builder::SliceBuilder;

use syntax::loc::*;
use syntax::names::*;
use namer::symbols::*;
use namer::graph::EnvIndex;

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Exp(MixfixTree),
    Name(Part, Vec<Located<Decl>>),
}

#[derive(Debug)]
pub enum Term {
    Name(Part),
    Primary,
    End
}

pub(super) struct Earley {
    grammar: InternalGrammar,
    nonterminals: HashMap<(Scope, usize), Symbol>,
    terminals: HashMap<Part, Symbol>,
    start: Symbol,
    exp: Symbol,
    prefix: Symbol,
    brackets: Symbol,
    primary: Symbol,
}

pub(super) struct EarleyBuilder<'a> {
    grammar: Grammar,
    nonterminals: &'a mut HashMap<(Scope, usize), Symbol>,
    terminals: &'a mut HashMap<Part, Symbol>,
    start: Symbol,
    exp: Symbol,
    prefix: Symbol,
    brackets: Symbol,
    primary: Symbol,
}

#[allow(non_upper_case_globals)]
static mut depth: usize = 0;

impl Earley {
    pub fn new(decls: &Vec<Located<Decl>>) -> Earley {
        EarleyBuilder::new(&mut HashMap::new(), &mut HashMap::new()).from_decls(decls)
    }

    #[trace]
    fn make_tree(&self, children: &[&Option<MixfixTree>]) -> Option<MixfixTree> {
        // Accumulate the name parts and the declarations of those names.
        let mut parts: Vec<Part> = Vec::new();
        let mut decls: Vec<Located<Decl>> = Vec::new();

        // Accumulate the call arguments.
        let mut args: Vec<MixfixTree> = Vec::new();
        let mut has_name = false;

        for t in children {
            match t {
                Some(MixfixTree::Name(x, xdecls)) => {
                    match x {
                        Name::Id(x) => parts.push(Part::Id(x.clone())),
                        Name::Op(x) => parts.push(Part::Op(x.clone())),
                        _ => {
                            // This should not happen.
// panic!("unexpected {:?} in MixfixTree", x);
                            return None;
                        },
                    }

                    for xd in xdecls {
                        match &xd.value {
                            Decl::MixfixPart { orig, .. } => {
                                decls.push(Located::new(xd.loc, *orig.clone()));
                            },
                            _ => {
                                // This should not happen.
// panic!("unexpected {:?} in MixfixTree decls", xd);
                                return None;
                            },
                        }
                    }

                    has_name = true;
                }
                Some(t) => {
                    parts.push(Part::Placeholder);
                    args.push(t.clone());
                }
                None => {
                    // Parse failed below, so we should also fail.
                    // This shouldn't happen, but we check this for robustness.
// panic!("unexpected mixfix parse failure: child failed");
                    return None
                },
            }
        }

        if ! has_name {
            // If there were no name parts, just left associate all the expression into a call.
            return MixfixTree::make_nameless_call(&args)
        }

        let new_name = Name::Mixfix(Name::encode_parts(&parts));

        let call = MixfixTree::make_call(
            MixfixTree::Name(new_name.clone(), decls.iter().filter(|decl| decl.name() == new_name).cloned().collect()),
            args.as_slice()
        );

        Some(call)
    }


    #[trace]
    fn make_symbol_tree(&self, sym: &Symbol, decls_map: &HashMap<Part, &Vec<Located<Decl>>>) -> Option<MixfixTree> {
        if sym == &self.start {
            return Some(MixfixTree::Exp);
        }
        if sym == &self.exp {
            return Some(MixfixTree::Exp);
        }
        if sym == &self.prefix {
            return Some(MixfixTree::Exp);
        }
        if sym == &self.brackets {
            return Some(MixfixTree::Exp);
        }
        if sym == &self.primary {
            return Some(MixfixTree::Exp);
        }

        // Look for the matching symbol and create a tree.
        // If a the symbol is a terminal, create a Name tree.
        // Otherwise, create an Exp tree.
        for (_, t) in &self.nonterminals {
            println!("nonterm {:?} == {:?} ?", t, sym);
            if t == sym {
                return Some(MixfixTree::Exp);
            }
        }

        for (x, t) in &self.terminals {
            println!("term {:?} == {:?} ?", t, sym);
            if t == sym {
                let y = match x {
                    Part::Id(x) => Name::Id(x.clone()),
                    Part::Op(x) => Name::Op(x.clone()),
                    _ => { return None; },
                };
                println!(" x = {}", x);
                println!(" y = {}", y);
                return decls_map.get(x).map(|&xdecls| MixfixTree::Name(y, xdecls.clone()));
            }
        }

        println!("symbol {:?} not found. this should not happen.", sym);

        None
    }

    pub fn parse(&self, tokens: &Vec<Token>) -> Vec<MixfixTree> {
        let mut decls_map = HashMap::new();

        // We assume that each occurrence of the same name part resolves to the same declarations.
        // This SHOULD be true since the tokens are all created by looking up the names in the
        // same environment.
        for token in tokens {
            if let Token::Name(part, decls) = token {
                decls_map.insert(part.clone(), decls);
            }
        }

        // Create the recognizer from the grammar.

        // Bocage is the gearley version of an SPPF.
        // "Bocage" is woodland interspersed with pasture typical of southern England and Normandy.
        // A sparse forest, in other words. Ha!
        let bocage: Bocage<(), Option<MixfixTree>> = Bocage::new(&self.grammar);
        let mut recognizer = Recognizer::new(&self.grammar, &bocage);

        // Parse!
        for token in tokens {
            let internal_token = match token {
                Token::Exp(_) => self.primary,
                Token::Name(part, decls) => self.term_name(part.clone())
            };

            recognizer.scan(internal_token, ());

            if ! recognizer.advance() {
                return vec![];
            }
        }

        if ! recognizer.is_finished() {
            return vec![];
        }

        // Run the evaluator on the parse tree to create the MixfixTrees.
        // Actions return an Option<MixfixTree>.

        let values = ValueArray::new();
        let mut evaluator = ArrayEvaluator::new(
            &values,
            ActionClosureEvaluator::new(
                // Action to perform at the leaves.
                |sym: Symbol, _: Option<&()>| self.make_symbol_tree(&sym, &decls_map),

                // Action to perform at the internal nodes of the parse tree.
                |rule: u32, children: &[&Option<MixfixTree>]| self.make_tree(children),

                // Action to perform at null. Should not happen with the generated grammar (no rules are nullable).
                |_, _: &mut SliceBuilder<Option<MixfixTree>>| unreachable!()
            )
        );

        let mut traversal = Traversal::new(&bocage, NullOrder::new());

        let mut result: Vec<MixfixTree>;
        result = evaluator.traverse(&mut traversal, recognizer.finished_node())
                          .iter()
                          .filter_map(|o| o.clone())
                          .collect();
        result.sort();
        result.dedup();
        result
    }

    /// Convert a scope and priority into a nonterminal symbol.
    fn nonterm_name(&self, scope: Scope, prio: usize) -> Symbol {
        self.nonterminals.get(&(scope, prio)).unwrap().clone()
    }

    /// Convert a name part into a terminal symbol.
    fn term_name(&self, part: Part) -> Symbol {
        self.terminals.get(&part).unwrap().clone()
    }
}

impl<'a> EarleyBuilder<'a> {
    fn new(nonterminals: &'a mut HashMap<(Scope, usize), Symbol>, terminals: &'a mut HashMap<Part, Symbol>) -> EarleyBuilder<'a> {
        let mut grammar = Grammar::new();

        // nonterminals
        let start = grammar.sym();
        let exp = grammar.sym();
        let prefix = grammar.sym();
        let brackets = grammar.sym();

        // terminals
        let primary = grammar.sym();

        EarleyBuilder {
            grammar,
            nonterminals,
            terminals,
            start,
            exp,
            prefix,
            brackets,
            primary,
        }
    }

    fn from_decls(&mut self, decls: &Vec<Located<Decl>>) -> Earley {
        // terminals
        // S -> E
        self.grammar.rule(self.start).rhs([self.exp]);
        // Pr -> Pr Br
        self.grammar.rule(self.prefix).rhs([self.prefix, self.brackets]);
        // Pr -> Br
        self.grammar.rule(self.prefix).rhs([self.brackets]);
        // Br -> p
        self.grammar.rule(self.brackets).rhs([self.primary]);

        self.grammar.set_start(self.start);

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

        // For each decl, find the minimum and maximum prios so we can fill in the gaps between rules.
        let mut min_prio = HashMap::new();
        let mut max_prio = HashMap::new();

        for decl in first_decls.values() {
            let scope = decl.scope();
            let prio = decl.prio().0;

            // Skip prefix and bracket names.
            if decl.name().is_prefix_name() || decl.name().is_brackets_name() {
                continue;
            }

            match min_prio.get(&scope) {
                Some(i) if prio < *i => { min_prio.insert(scope, prio); },
                None => { min_prio.insert(scope, prio); },
                _ => {},
            }
            match max_prio.get(&scope) {
                Some(i) if prio > *i => { max_prio.insert(scope, prio+1); },
                None => { max_prio.insert(scope, prio+1); },
                _ => {},
            }
        }

        for decl in first_decls.values() {
            let scope = decl.scope();
            let prio = decl.prio().0;

            let rhs = self.make_rule_from_decl(decl);

            if decl.name().is_prefix_name() {
                self.grammar.rule(self.prefix).rhs(rhs);
            }
            else if decl.name().is_brackets_name() {
                self.grammar.rule(self.brackets).rhs(rhs);
            }
            else {
                let lhs = self.nonterm_name(scope, prio);
                self.grammar.rule(lhs).rhs(rhs);
            }
        }

        // Add E -> Pr if there are no other rules.
        if min_prio.is_empty() || max_prio.is_empty() {
            self.grammar.rule(self.exp).rhs([self.prefix]);
        }

        for scope in min_prio.keys() {
            if let (Some(min), Some(max)) = (min_prio.get(scope), max_prio.get(scope)) {
                // M{min} -> M{n} -> M{n+1} -> ... -> M{max}
                for k in *min .. *max {
                    let lhs = self.nonterm_name(*scope, k);
                    let rhs = self.nonterm_name(*scope, k+1);
                    self.grammar.rule(lhs).rhs([rhs]);
                }

                let min = self.nonterm_name(*scope, *min);
                let max = self.nonterm_name(*scope, *max);

                // E -> M{min}
                self.grammar.rule(self.exp).rhs([min]);

                // M{max} -> Br
                self.grammar.rule(max).rhs([self.prefix]);
            }
        }

        Earley {
            grammar: self.grammar.into_internal_grammar(),
            nonterminals: self.nonterminals.clone(),
            terminals: self.terminals.clone(),
            start: self.start,
            exp: self.exp,
            prefix: self.prefix,
            brackets: self.brackets,
            primary: self.primary,
        }
    }

    /// Convert a scope and priority into a nonterminal symbol.
    fn nonterm_name(&mut self, scope: Scope, prio: usize) -> Symbol {
        match self.nonterminals.get(&(scope, prio)) {
            Some(sym) => sym.clone(),
            None => {
                let sym = self.grammar.sym();
                self.nonterminals.insert((scope, prio), sym);
                sym
            },
        }
    }

    /// Convert a name part into a terminal symbol.
    fn term_name(&mut self, part: Part) -> Symbol {
        match self.terminals.get(&part) {
            Some(sym) => sym.clone(),
            None => {
                let sym = self.grammar.sym();
                self.terminals.insert(part, sym);
                sym
            },
        }
    }

    fn make_rule_from_decl(&mut self, decl: &Decl) -> Vec<Symbol> {
        let name = decl.name();
        let assoc = decl.assoc();
        let scope = decl.scope();
        let prio = decl.prio();
        self.make_rhs_from_name(&name, assoc, prio, scope)
    }

    fn make_rhs_from_name(&mut self, x: &Name, assoc: Option<usize>, prio: Prio, scope: Scope) -> Vec<Symbol> {
        let lowest = self.exp;
        let highest = self.brackets;
        let current = self.nonterm_name(scope, prio.0);
        let next = self.nonterm_name(scope, prio.0 + 1);


        match x {
            Name::Op(x) => {
                vec![self.term_name(Part::Op(x.clone()))]
            },
            Name::Id(x) => {
                vec![self.term_name(Part::Id(x.clone()))]
            },
            Name::Mixfix(s) => {
                let parts = Name::decode_parts(*s);

                match parts.as_slice() {
                    [] => vec![],

                    [Part::Op(x), Part::Placeholder] => {
                        // Unary prefix operators are right associative.
                        vec![self.term_name(Part::Op(x.clone())), current]
                    },
                    [Part::Placeholder, Part::Op(x)] => {
                        // Unary postfix operators are left associative.
                        vec![current, self.term_name(Part::Op(x.clone()))]
                    },
                    // FIXME: prefix rules too...
                    parts => {
                        println!("parts {:?}", parts);


                        #[derive(Debug)]
                        enum Sym {
                            Term(Part),
                            Nonterm(Symbol),
                        }

                        // map all expressions to the highest precedence nonterminal.
                        let mut rhs: Vec<Sym> = parts.iter().map(|part|
                            match part {
                                Part::Placeholder => Sym::Nonterm(highest.clone()),
                                x => Sym::Term(x.clone()),
                            }).collect();

                        // For prefix names, all symbosl on the RHS should be brackets.
                        if x.is_prefix_name() {
                            return rhs.iter().map(|sym| match sym {
                                Sym::Nonterm(x) => x.clone(),
                                Sym::Term(x) => self.term_name(x.clone()),
                            }).collect()
                        }

                        // Handle the associativity annotations on the first and last tokens.
                        // Change from brackets to current next.
                        let n = rhs.len();

                        if n > 2 {
                            match (&rhs[0], &rhs[1]) {
                                // _ + ...
                                (Sym::Nonterm(_), Sym::Term(_)) => {
                                    if assoc == Some(0) || assoc == None {
                                        rhs[0] = Sym::Nonterm(current.clone());
                                    }
                                    else {
                                        rhs[0] = Sym::Nonterm(next.clone());
                                    }
                                }
                                _ => {},
                            }

                            match (&rhs[n-2], &rhs[n-1]) {
                                // ... + _
                                (Sym::Term(_), Sym::Nonterm(_)) => {
                                    if assoc == Some(n-1) {
                                        rhs[n-1] = Sym::Nonterm(current.clone());
                                    }
                                    else {
                                        rhs[n-1] = Sym::Nonterm(next.clone());
                                    }
                                }
                                _ => {},
                            }
                        }

                        // Find any holes or quasi-holes and replace brackets with lowest priority.
                        for i in 1..(n-1) {
                            match (&rhs[i-1], &rhs[i], &rhs[i+1]) {
                                // Hole: a nonterminal surrounded by terminals has lowest priority.
                                // if _ then _ else
                                // | _ |
                                (Sym::Term(_), Sym::Nonterm(_), Sym::Term(_)) => {
                                    rhs[i] = Sym::Nonterm(lowest.clone());
                                },
                                _ => {},
                            }

                            match (&rhs[i-1], &rhs[i], &rhs[i+1]) {
                                // _ _ x --> use lowest.clone() priority before x
                                (Sym::Nonterm(x), Sym::Nonterm(_), Sym::Term(_)) if *x == highest => {
                                    rhs[i] = Sym::Nonterm(lowest.clone());
                                },
                                _ => {},
                            }
                        }

                        if n > 2 {
                            match (&rhs[n-2], &rhs[n-1]) {
                                (Sym::Nonterm(x), Sym::Nonterm(_)) if *x == highest => {
                                    rhs[n-1] = Sym::Nonterm(lowest.clone());
                                }
                                _ => {},
                            }
                        }

                        println!("rhs {:?}", rhs);

                        rhs.iter().map(|sym| match sym {
                            Sym::Nonterm(x) => x.clone(),
                            Sym::Term(x) => self.term_name(x.clone()),
                        }).collect()
                    },
                }
            },
        }
    }
}
