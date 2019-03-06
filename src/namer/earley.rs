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

use std::iter::Step;
use std::collections::HashMap;

use trace::trace;
trace::init_depth_var!();

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Exp(MixfixTree),
    Name(Part, Vec<LocalRef>),
}

#[derive(Debug)]
pub enum Term {
    Name(Part),
    Primary,
    End
}

pub(super) struct Earley {
    grammar: InternalGrammar,
    nonterminals: HashMap<(Ref, Prio), Symbol>,
    terminals: HashMap<Part, Symbol>,
    start: Symbol,
    exp: Symbol,
    prefix: Symbol,
    brackets: Symbol,
    primary: Symbol,
    decls: HashMap<LocalRef, Located<Decl>>,
    part_decls: HashMap<LocalRef, Located<Decl>>,
}

pub(super) struct EarleyBuilder<'a> {
    grammar: Grammar,
    nonterminals: &'a mut HashMap<(Ref, Prio), Symbol>,
    terminals: &'a mut HashMap<Part, Symbol>,
    start: Symbol,
    exp: Symbol,
    prefix: Symbol,
    brackets: Symbol,
    primary: Symbol,
}

impl Earley {
    pub fn new(decls: &Vec<(LocalRef, Located<Decl>)>, part_decls: &Vec<(LocalRef, Located<Decl>)>) -> Earley {
        EarleyBuilder::new(&mut HashMap::new(), &mut HashMap::new()).from_decls(decls, part_decls)
    }

    #[trace]
    fn make_tree(&self, children: &[&Option<MixfixTree>]) -> Option<MixfixTree> {
        // Accumulate the name parts and the declarations of those names.
        let mut parts: Vec<Part> = Vec::new();
        let mut grefs_with_names: Vec<(Name, LocalRef)> = Vec::new();

        // Accumulate the call arguments.
        let mut args: Vec<MixfixTree> = Vec::new();
        let mut has_name = false;

        for t in children {
            match t {
                Some(MixfixTree::Name(x, grefs)) => {
                    match x {
                        Name::Id(x) => parts.push(Part::Id(x.clone())),
                        Name::Op(x) => parts.push(Part::Op(x.clone())),
                        _ => {
                            // This should not happen.
                            panic!("unexpected name {:?} in MixfixTree", x);
                            return None;
                        },
                    }

                    for gref in grefs {
                        match self.part_decls.get(gref) {
                            Some(Located { loc, value: Decl::MixfixPart { full, orig, .. } }) => {
                                grefs_with_names.push((*full, *orig));
                            },
                            Some(xd) => {
                                // This should not happen.
                                panic!("unexpected {:?} --> {:?} in MixfixTree decls", gref, xd);
                                return None;
                            },
                            None => {
                                // This should not happen.
                                panic!("unexpected {:?} --> None in MixfixTree decls {:?}", gref, self.part_decls);
                                return None;
                            }
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
                    panic!("unexpected mixfix parse failure: child failed");
                    return None
                },
            }
        }

        if ! has_name {
            // If there were no name parts, just left associate all the expression into a call.
            return MixfixTree::make_nameless_call(&args)
        }

        let new_name = Name::Mixfix(Name::encode_parts(&parts));
        let grefs = grefs_with_names.iter().filter_map(|(name, gref)|
            if name == &new_name {
                Some(*gref)
            }
            else {
                None
            }
        ).collect();

        let call = MixfixTree::make_call(
            MixfixTree::Name(new_name, grefs),
            args.as_slice()
        );

        Some(call)
    }


    #[trace]
    fn make_symbol_tree(&self, sym: &Symbol, decls_map: &HashMap<Part, &Vec<LocalRef>>) -> Option<MixfixTree> {
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
                return decls_map.get(x).map(|&decls| MixfixTree::Name(y, decls.clone()));
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
        Earley::longest_match(&mut result);
        result
    }

    // Remove trees which are otherwise identical, but where a prefix name in one tree
    // is the same but lower arity than in another tree. That is, longest arity wins.
    // TODO TEST
    fn longest_match(result: &mut Vec<MixfixTree>) {
        if result.len() <= 1 {
            return;
        }

        fn left_is_same_or_longer(l: &MixfixTree, r: &MixfixTree) -> bool {
            match (l, r) {
                (MixfixTree::Apply(box l1, box l2),
                 MixfixTree::Apply(box r1, box r2)) => {
                    left_is_same_or_longer(l1, r1) && left_is_same_or_longer(l2, r2)
                },
                (MixfixTree::Exp, MixfixTree::Exp) => {
                    true
                },
                (MixfixTree::Name(xl, _), MixfixTree::Name(xr, _)) => {
                    if xl == xr {
                        true
                    }
                    else {
                        xl.is_prefix_name() && xr.is_prefix_name() &&
                        xl.parts().first() == xr.parts().first() &&
                        xl.parts().len() >= xr.parts().len()
                    }
                },
                _ => {
                    false
                },
            }
        }

        let mut n = result.len();
        let mut i = 0;

        while i < n {
            let mut j = 0;

            while j < n {
                // println!("i={} j={} n={}", i, j, n);

                if i == j {
                    j += 1;
                    continue;
                }

                let ri = result.get(i).unwrap();
                let rj = result.get(j).unwrap();

                if left_is_same_or_longer(ri, rj) {
                    result.remove(j);
                    n -= 1;
                    if i > j {
                        i -= 1;
                    }
                }
                else {
                    j += 1;
                }
            }

            i += 1;
        }
    }

    /// Convert a scope and priority into a nonterminal symbol.
    fn nonterm_name(&self, scope: Ref, prio: Prio) -> Symbol {
        self.nonterminals.get(&(scope, prio)).unwrap().clone()
    }

    /// Convert a name part into a terminal symbol.
    fn term_name(&self, part: Part) -> Symbol {
        self.terminals.get(&part).unwrap().clone()
    }
}

impl<'a> EarleyBuilder<'a> {
    fn new(nonterminals: &'a mut HashMap<(Ref, Prio), Symbol>, terminals: &'a mut HashMap<Part, Symbol>) -> EarleyBuilder<'a> {
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

    fn from_decls(&mut self, decls: &Vec<(LocalRef, Located<Decl>)>, part_decls: &Vec<(LocalRef, Located<Decl>)>) -> Earley {
        println!("from_decls {:?}", decls);

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
        let mut first_decls: HashMap<(Name, Ref), (Name, Ref, Prio, Option<usize>)> = HashMap::new();

        for (gref, Located { loc, value: decl }) in decls {
            let scope = decl.parent().unwrap().to_ref();
            let prio = decl.prio();
            let name = decl.name();
            let assoc = decl.assoc();

            let key = (name, scope);

            match first_decls.get(&key) {
                Some((_, _, existing_prio, _)) => {
                    if *existing_prio < prio {
                        first_decls.insert(key, (name, scope, prio, assoc));
                    }
                },
                None => {
                    first_decls.insert(key, (name, scope, prio, assoc));
                }
            }
        }

        // For each decl, find the minimum and maximum prios so we can fill in the gaps between rules.
        let mut min_prio = HashMap::new();
        let mut max_prio = HashMap::new();

        for (name, scope, prio, assoc) in first_decls.values() {
            // Skip prefix and bracket names.
            if name.is_prefix_name() || name.is_brackets_name() {
                continue;
            }

            match min_prio.get(&*scope) {
                Some(i) if *prio < *i => { min_prio.insert(*scope, *prio); },
                None => { min_prio.insert(*scope, *prio); },
                _ => {},
            }

            match max_prio.get(&*scope) {
                Some(i) if *prio > *i => { max_prio.insert(*scope, prio.add_one()); },
                None => { max_prio.insert(*scope, prio.add_one()); },
                _ => {},
            }
        }

        for (name, scope, prio, assoc) in first_decls.values() {
            let rhs = self.make_rhs_from_name(*name, *assoc, *prio, *scope);

            if name.is_prefix_name() {
                self.grammar.rule(self.prefix).rhs(rhs);
            }
            else if name.is_brackets_name() {
                self.grammar.rule(self.brackets).rhs(rhs);
            }
            else {
                let lhs = self.nonterm_name(*scope, *prio);
                self.grammar.rule(lhs).rhs(rhs);
            }
        }

        // Add E -> Pr if there are no other rules.
        if min_prio.is_empty() || max_prio.is_empty() {
            self.grammar.rule(self.exp).rhs([self.prefix]);
        }

        for scope in min_prio.keys() {
            if let (Some(&min), Some(&max)) = (min_prio.get(scope), max_prio.get(scope)) {
                // M{min} -> M{n} -> M{n+1} -> ... -> M{max}
                for prio in min .. max {
                    let lhs = self.nonterm_name(*scope, prio);
                    let rhs = self.nonterm_name(*scope, prio.add_one());
                    self.grammar.rule(lhs).rhs([rhs]);
                }

                let min_sym = self.nonterm_name(*scope, min);
                let max_sym = self.nonterm_name(*scope, max);

                // E -> M{min}
                self.grammar.rule(self.exp).rhs([min_sym]);

                // M{max} -> Br
                self.grammar.rule(max_sym).rhs([self.prefix]);
            }
        }

        // FIXME: this maps to the full decl and we should map to the mixfix part.
        let decls_map = decls.iter().cloned().collect();
        let part_decls_map = part_decls.iter().cloned().collect();

        Earley {
            grammar: self.grammar.into_internal_grammar(),
            nonterminals: self.nonterminals.clone(),
            terminals: self.terminals.clone(),
            start: self.start,
            exp: self.exp,
            prefix: self.prefix,
            brackets: self.brackets,
            primary: self.primary,
            decls: decls_map,
            part_decls: part_decls_map,
        }
    }

    /// Convert a scope and priority into a nonterminal symbol.
    fn nonterm_name(&mut self, scope: Ref, prio: Prio) -> Symbol {
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

    fn make_rhs_from_name(&mut self, name: Name, assoc: Option<usize>, prio: Prio, scope: Ref) -> Vec<Symbol> {
        let lowest = self.exp;
        let highest = self.brackets;
        let current = self.nonterm_name(scope, prio);
        let next = self.nonterm_name(scope, prio.add_one());

        match name {
            Name::Op(x) => {
                vec![self.term_name(Part::Op(x))]
            },
            Name::Id(x) => {
                vec![self.term_name(Part::Id(x))]
            },
            Name::Mixfix(s) => {
                let parts = Name::decode_parts(s);

                match parts.as_slice() {
                    [] => vec![],

                    [Part::Op(x), Part::Placeholder] => {
                        // Unary prefix operators are right associative.
                        vec![self.term_name(Part::Op(*x)), current]
                    },
                    [Part::Placeholder, Part::Op(x)] => {
                        // Unary postfix operators are left associative.
                        vec![current, self.term_name(Part::Op(*x))]
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
                        if name.is_prefix_name() {
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
