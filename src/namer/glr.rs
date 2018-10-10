// The mixfix parser is based on a GLR parser.
// As we parse, we simulate an LR(0) state machine, using a
// graph-structured stack (GSS) to handle divergent parses.
//
// Parsing also constructs the resulting unambiguous expression during reduces.
// If there is a unique parse at the end, we're done.
// Otherwise, we report an ambiguity.
//
// This class just returns all possible parses and lets the client deal with the
// ambiguity.
//
// The grammar we parse with is derived from the function definitions
// in scope as well as their precedence and associativity.
//
// The parser is implemented very imperatively, so don't look here for functional
// programming goodness. The implementation is based on the Ibex parser generator
// implemented in Java in 2005-6 by the author. This, in turn, was based on the Elkhound
// parser generator by Scott McPeak and George Necula (CC'2004).

use typed_arena::Arena;

use std::collections::VecDeque;

use syntax::loc::*;
use syntax::names::*;
use syntax::trees::NodeId;
use namer::symbols::MixfixTree;
use namer::symbols::Decl;

type Prio = u32;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Exp(MixfixTree),
    Name(Part, Vec<Decl>),
    End,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Symbol {
    Nonterm(Nonterm),
    Term(Term)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Nonterm {
    S, E, M(NodeId, Prio), Pr, Br,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Name(Part),
    Primary,
    End,
}

impl Term {
    fn from(t: &Token) -> Term {
        match t {
            Token::Exp(_) => Term::Primary,
            Token::Name(x, _) => Term::Name(x.clone()),
            Token::End => Term::End,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Rule {
    lhs: Nonterm,
    rhs: Vec<Symbol>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    dot: usize,
    rule: Rule,
}

#[derive(Debug, PartialEq)]
pub struct LR {
    rules: Vec<Rule>,
    states: Vec<Vec<Item>>,
}

#[derive(Clone, Debug, PartialEq)]
enum Action {
    Shift(State),
    Reduce(Rule),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct State(usize);

impl LR {
    pub fn new(rules: Vec<Rule>) -> LR {
        LR {
            rules,
            states: vec![]
        }
    }

    // pub fn parse(&mut self, input: &Vec<Token>) -> Vec<MixfixTree> {
    //     if input.is_empty() {
    //         return vec!()
    //     }
    //
    //     if let Some(Token::Exp(ref e)) = input.first() {
    //         if input.len() == 1 {
    //             return vec!(e.clone())
    //         }
    //     }
    //
    //     GLR::new(self)
    //
    //     // self.glr_parse(input)
    //     vec![]
    // }

    fn get_items(&self, state: &State) -> Vec<Item> {
        match self.states.get(state.0) {
            Some(v) => v.clone(),
            None => Vec::new()
        }
    }

    fn same_items(&self, items1: &Vec<Item>, items2: &Vec<Item>) -> bool {
        for item1 in items1 {
            if ! items2.contains(item1) {
                return false;
            }
        }
        return items1.len() == items2.len()
    }

    fn add_state(&mut self, items: Vec<Item>) -> Option<State> {
        if items.is_empty() {
            return None
        }

        for (i, other_items) in self.states.iter().enumerate() {
            if self.same_items(&items, other_items) {
                return Some(State(i))
            }
        }

        println!("add_state {:#?} --> {:#?}", State(self.states.len()), &items);
        self.states.push(items);
        Some(State(self.states.len()-1))
    }

    fn reductions(&self, state: &State) -> Vec<Rule> {
        println!("reductions: items of {:?} --> {:#?}", state, self.get_items(state));

        let rules = self.get_items(state).iter().flat_map(
            |Item { ref dot, ref rule }| {
                if *dot == rule.rhs.len() {
                    vec!(rule.clone())
                }
                else {
                    vec!()
                }
            }
        ).collect();

        println!("reductions: reductions of {:?} --> {:#?}", state, rules);

        rules
    }

    fn shift_symbol(&mut self, state: &State, t: &Symbol) -> Option<State> {
        let items = self.get_items(state).iter().flat_map(
            |Item { ref dot, ref rule }| {
                match rule.rhs.get(*dot) {
                    Some(s) if *s == *t => {
                        vec!(Item { dot: dot+1, rule: rule.clone() })
                    },
                    _ => {
                        vec!()
                    },
                }
            }
        ).collect();

        self.add_state(self.closure(items))
    }

    fn shift(&mut self, state: &State, t: &Term) -> Option<State> {
        self.shift_symbol(state, &Symbol::Term(t.clone()))
    }

    fn goto(&mut self, state: &State, t: &Nonterm) -> Option<State> {
        self.shift_symbol(state, &Symbol::Nonterm(t.clone()))
    }

    fn closure(&self, items: Vec<Item>) -> Vec<Item> {
        let mut worklist = items;
        let mut new_items = Vec::new();

        while let Some(item) = worklist.pop() {
            if ! new_items.contains(&item) {
                if let Some(Symbol::Nonterm(ref a)) = item.rule.rhs.get(item.dot) {
                    for rule in &self.rules {
                        if rule.lhs == *a {
                            worklist.push(Item { dot: 0, rule: rule.clone() })
                        }
                    }
                }

                new_items.push(item);
            }
        }

        new_items
    }
}

#[derive(Debug)]
struct GLR {
    parser: LR,
    stacks: Vec<Stack>
}

#[derive(Clone, Debug, PartialEq)]
struct Node {
    token: Token,
    state: State,
}

#[derive(Clone, Debug, PartialEq)]
struct Stack {
    top: State,
    stack: Vec<Node>,
}

// We don't implement GLR using a GSS as in Tomita and other work.
// Instead we just clone the stack. Why? Because it's much much simpler.
// It wastes some memory, but since the grammar is small (typically 3-4 rules)
// and since the inputs are small (typically 3-4 tokens), the lack of sharing
// should not hurt performance.

impl GLR {
    fn new(parser: LR, state: State) -> GLR {
        GLR {
            parser,
            stacks: vec![Stack { top: state, stack: vec![] }]
        }
    }

    fn parse(&mut self, input: Vec<Token>) -> Vec<MixfixTree> {
        for t in input {
            println!("PARSER LOOP");
            println!("shifting t = {:?}", &t);
            println!("self = {:#?}", &self);
            self.do_reductions(&t);
            self.do_shifts(&t);
        }

        println!("LAST REDUCTIONS");
        self.do_reductions(&Token::End);
        println!("SHIFTING $");
        self.do_shifts(&Token::End);
        println!("PARSING DONE");

        self.get_accepts()
    }


    // push the token t on all the stacks where we can shift.
    // if we cannot shift, remove the stack.
    fn do_shifts(&mut self, t: &Token) {
        let mut i = 0;

        println!("shift ${:?}", t);

        while i < self.stacks.len() {
            if let Some(Stack { ref mut top, ref mut stack }) = self.stacks.get_mut(i) {
                if let Some(dest) = self.parser.shift(&top, &Term::from(&t)) {
                    let old_top = *top;
                    *top = dest;
                    stack.push(Node { token: t.clone(), state: old_top });
                    println!("shift {:?} from {:?} to state {:?}", t, old_top, dest);
                    i += 1;
                }
                else {
                    println!("shift {:?} from {:?} failed", t, *top);
                    // remove i and replace with last element of the vector, which we'll visit on the
                    // next iteration.
                    // swap_remove() reorders the list, but is O(1), while remove() is O(n)
                    self.stacks.swap_remove(i);
                }
            }
        }
    }

    // inspect all the stacks and perform any reductions.
    // this will just make the list of stacks grow; we do not remove stacks
    // that cannot reduce (they would be removed by do_shifts).
    fn do_reductions(&mut self, t: &Token) {
        let mut new_stacks = Vec::new();

        // We need to borrow self.stacks to iterate over it.
        // And we need to call reduce_with_rule which mutates parser.
        // We have to borrow the stacks and parser separately since
        // we can't mutably borrow self at the same time we're borrowing
        // it to iterate over the stacks.

        let parser = &mut self.parser;

        for Stack { ref top, ref stack } in &self.stacks {
            for rule in parser.reductions(&top) {
                if let Some(new_stack) = GLR::reduce_with_rule(parser, top, stack, &rule) {
                    new_stacks.push(new_stack);
                }
            }
        }

        let mut changed = false;

        {
            let stacks = &mut self.stacks;

            println!("do_reductions: stacks before = {:#?}", stacks);
            println!("do_reductions: new stacks    = {:#?}", new_stacks);

            for new_stack in &new_stacks {
                let mut j = 0;
                while j < stacks.len() {
                    if let Some(s) = stacks.get(j) {
                        if let Some(s1) = GLR::merge_stacks(s, new_stack) {
                            // if we can merge the stacks, replace the old stack
                            // with the merged stack
                            stacks[j] = s1;
                            break;
                        }
                    }
                    j += 1;
                }

                // the new stack was not merged into an old stack.
                if j == stacks.len() {
                    stacks.push(new_stack.clone());
                    changed = true;
                }
            }
        }

        // Repeat until we hit a fixpoint.
        // Infinite loop! FIXME
        if changed {
            self.do_reductions(t);
        }
    }

    fn merge_stacks(stack1: &Stack, stack2: &Stack) -> Option<Stack> {
        if stack1.top != stack2.top {
            return None;
        }

        if stack1.stack.len() != stack2.stack.len() {
            return None;
        }

        let mut s = Vec::new();

        for (n1, n2) in stack1.stack.iter().zip(stack2.stack.iter()) {
            if n1.state != n2.state {
                return None;
            }

            if let Some(merged_value) = GLR::merge_values(&n1.token, &n2.token) {
                s.push(Node { state: n1.state, token: merged_value });
            }
            else {
                return None;
            }
        }

        Some(Stack { top: stack1.top, stack: s })
    }

    // Reduce using the given rule.
    fn reduce_with_rule(parser: &mut LR, top: &State, stack: &Vec<Node>, rule: &Rule) -> Option<Stack> {
        // Perform a reduction, creating a new stack.
        let mut new_stack = stack.clone();
        let entries = new_stack.split_off(stack.len() - rule.rhs.len());
        assert_eq!(entries.len(), rule.rhs.len());

        let Node { token: _, state: old_top } = entries.first().unwrap();

        // goto might modify parser (because it creates a new state and updates the states vec)
        // so we need self to be mutable.
        if let Some(new_top) = parser.goto(old_top, &rule.lhs) {
            let tokens = entries.iter().map(
                |Node { ref token, ref state }| token.clone()
            ).collect();

            if let Some(value) = GLR::make_semantic_value(&tokens, &rule.rhs) {
                new_stack.push(Node { token: value, state: old_top.clone() });
                return Some(Stack { top: new_top, stack: new_stack.clone() })
            }
        }

        None
    }


    fn make_semantic_value(values: &Vec<Token>, rhs: &Vec<Symbol>) -> Option<Token> {
        let mut args = Vec::new();
        let mut parts = Vec::new();
        let mut decls = Vec::new();

        for (token, symbol) in values.iter().zip(rhs.iter()) {
            match (token, symbol) {
                (Token::Exp(e), Symbol::Nonterm(_)) => {
                    args.push(e.clone());
                    parts.push(Part::Placeholder);
                },
                (Token::Exp(e), Symbol::Term(Term::Primary)) => {
                    args.push(e.clone());
                    parts.push(Part::Placeholder);
                },
                (Token::Name(x, xdecls), Symbol::Term(Term::Name(y))) if x == y => {
                    parts.push(x.clone());
                    for xd in xdecls {
                        decls.push(xd);
                    }
                },
                _ => {
                    return None;
                },
            }
        }

        if args.len() == values.len() {
            if let Some((e, es)) = args.split_first() {
                return Some(Token::Exp(GLR::make_call(e.clone(), es)));
            }
        }
        else {
            return Some(
                Token::Exp(
                    GLR::make_call(
                        GLR::make_var(Name::Mixfix(parts)),
                        args.as_slice()
                    )
                ));
        }

        None
    }

    fn make_var(name: Name) -> MixfixTree {
        MixfixTree::Name(name, vec![])
    }

    fn make_call(e: MixfixTree, es: &[MixfixTree]) -> MixfixTree {
        if let Some((arg, args)) = es.split_first() {
            GLR::make_call(
                MixfixTree::Apply(Box::new(e), Box::new(arg.clone())),
                args)
        }
        else {
            e
        }
    }

    fn merge_values(t1: &Token, t2: &Token) -> Option<Token> {
        match (t1, t2) {
            (Token::End, Token::End) => Some(Token::End),
            (Token::Name(ref s1, ref decls1), Token::Name(ref s2, ref decls2)) if s1 == s2 => {
                let mut decls = Vec::new();
                for d in decls1 { decls.push(d.clone()); }
                for d in decls2 { decls.push(d.clone()); }
                Some(Token::Name(s1.clone(), decls))
            },
            (Token::Exp(ref e1), Token::Exp(ref e2)) if e1 == e2 => Some(t1.clone()),
            _ => unimplemented!()
        }
    }

    fn get_accepts(&self) -> Vec<MixfixTree> {
        let mut values = Vec::new();

        // All the stacks should be in the configuration:
        // S -> E $ .
        // So the token at the top of the stack should be an Exp token.
        for Stack { top, stack } in &self.stacks {
            // Should only have E $ on the stack.
            if stack.len() != 2 {
                continue;
            }

            if stack.last().unwrap().token != Token::End {
                continue;
            }

            // the start state should be at the bottom of the stack.
            if stack.first().unwrap().state != State(0) {
                continue;
            }

            if let Some(Node { token, state }) = stack.first() {
                match token {
                    Token::Exp(e) => {
                        values.push(e.clone());
                    },
                    _ => {},
                }
            }
        }

        values
    }
}

#[cfg(test)]
mod tests {
    use namer::glr::*;
    use syntax::trees::NodeId;
    use syntax::names::Part;
    use syntax::names::Name;
    use namer::symbols::*;

    struct OneRule;
    impl OneRule {
        fn glr() -> GLR {
            let start_rule = Rule { lhs: Nonterm::S, rhs: vec![Symbol::Term(Term::Primary), Symbol::Term(Term::End)] };
            let rules = vec![start_rule.clone()];
            let mut lr = LR::new(rules);
            let state = lr.add_state(vec![Item { dot: 0, rule: start_rule }]).unwrap();
            let glr = GLR::new(lr, state);
            glr
        }
    }

    // grammar:
    // S -> p $
    // input: e
    #[test]
    pub fn test_primary() {
        let mut glr = OneRule::glr();
        let input = vec![Token::Exp(MixfixTree::Exp)];
        let output = glr.parse(input);
        assert_eq!(output, vec![MixfixTree::Exp]);
    }

    // grammar:
    // S -> E $
    // E -> M0
    // M0 -> M0 + M1
    // M0 -> M1
    // M1 -> p
    struct BinaryPlusLeft;
    impl BinaryPlusLeft {
        fn glr() -> GLR {
            let start_rule = Rule { lhs: Nonterm::S, rhs: vec![Symbol::Nonterm(Nonterm::E), Symbol::Term(Term::End)] };
            let rules = vec![
                start_rule.clone(),
                Rule { lhs: Nonterm::E, rhs: vec![
                    Symbol::Nonterm(Nonterm::M(NodeId(0), 0))] },
                Rule { lhs: Nonterm::M(NodeId(0), 0), rhs: vec![
                    Symbol::Nonterm(Nonterm::M(NodeId(0), 0)),
                    Symbol::Term(Term::Name(Part::Op(String::from("+")))),
                    Symbol::Nonterm(Nonterm::M(NodeId(0), 1))] },
                Rule { lhs: Nonterm::M(NodeId(0), 0), rhs: vec![
                    Symbol::Nonterm(Nonterm::M(NodeId(0), 1))] },
                Rule { lhs: Nonterm::M(NodeId(0), 1), rhs: vec![
                    Symbol::Term(Term::Primary)] },
            ];
            let mut lr = LR::new(rules);
            let state = lr.add_state(lr.closure(vec![Item { dot: 0, rule: start_rule }])).unwrap();
            GLR::new(lr, state)
        }
    }

    // grammar:
    // S -> E $
    // E -> M0
    // M0 -> M1 + M0
    // M0 -> M1
    // M1 -> p
    struct BinaryPlusRight;
    impl BinaryPlusRight {
        fn glr() -> GLR {
            let start_rule = Rule { lhs: Nonterm::S, rhs: vec![Symbol::Nonterm(Nonterm::E), Symbol::Term(Term::End)] };
            let rules = vec![
                start_rule.clone(),
                Rule { lhs: Nonterm::E, rhs: vec![
                    Symbol::Nonterm(Nonterm::M(NodeId(0), 0))] },
                Rule { lhs: Nonterm::M(NodeId(0), 0), rhs: vec![
                    Symbol::Nonterm(Nonterm::M(NodeId(0), 1)),
                    Symbol::Term(Term::Name(Part::Op(String::from("+")))),
                    Symbol::Nonterm(Nonterm::M(NodeId(0), 0))] },
                Rule { lhs: Nonterm::M(NodeId(0), 0), rhs: vec![
                    Symbol::Nonterm(Nonterm::M(NodeId(0), 1))] },
                Rule { lhs: Nonterm::M(NodeId(0), 1), rhs: vec![
                    Symbol::Term(Term::Primary)] },
            ];
            let mut lr = LR::new(rules);
            let state = lr.add_state(lr.closure(vec![Item { dot: 0, rule: start_rule }])).unwrap();
            GLR::new(lr, state)
        }
    }

    // grammar:
    // S -> E $
    // E -> M0
    // M0 -> M1 + M1
    // M0 -> M1
    // M1 -> p
    struct BinaryPlusNone;
    impl BinaryPlusNone {
        fn glr() -> GLR {
            let start_rule = Rule { lhs: Nonterm::S, rhs: vec![Symbol::Nonterm(Nonterm::E), Symbol::Term(Term::End)] };
            let rules = vec![
                start_rule.clone(),
                Rule { lhs: Nonterm::E, rhs: vec![
                    Symbol::Nonterm(Nonterm::M(NodeId(0), 0))] },
                Rule { lhs: Nonterm::M(NodeId(0), 0), rhs: vec![
                    Symbol::Nonterm(Nonterm::M(NodeId(0), 1)),
                    Symbol::Term(Term::Name(Part::Op(String::from("+")))),
                    Symbol::Nonterm(Nonterm::M(NodeId(0), 1))] },
                Rule { lhs: Nonterm::M(NodeId(0), 0), rhs: vec![
                    Symbol::Nonterm(Nonterm::M(NodeId(0), 1))] },
                Rule { lhs: Nonterm::M(NodeId(0), 1), rhs: vec![
                    Symbol::Term(Term::Primary)] },
            ];
            let mut lr = LR::new(rules);
            let state = lr.add_state(lr.closure(vec![Item { dot: 0, rule: start_rule }])).unwrap();
            GLR::new(lr, state)
        }
    }

    // input: e + e
    #[test]
    pub fn test_e_plus_e_left() {
        let mut glr = BinaryPlusLeft::glr();
        let input = vec![
            Token::Exp(MixfixTree::Exp),
            Token::Name(Part::Op(String::from("+")), vec![]),
            Token::Exp(MixfixTree::Exp),
        ];
        let output = glr.parse(input);
        assert_eq!(output, vec![
            MixfixTree::Apply(
                Box::new(
                    MixfixTree::Apply(
                        Box::new(MixfixTree::Name(
                            Name::Mixfix(vec![
                                Part::Placeholder,
                                Part::Op(String::from("+")),
                                Part::Placeholder,
                            ]),
                            vec![])),
                        Box::new(MixfixTree::Exp)
                    ),
                ),
                Box::new(MixfixTree::Exp)
            )
        ]);
    }

    // input: e + e
    #[test]
    pub fn test_e_plus_e_right() {
        let mut glr = BinaryPlusRight::glr();
        let input = vec![
            Token::Exp(MixfixTree::Exp),
            Token::Name(Part::Op(String::from("+")), vec![]),
            Token::Exp(MixfixTree::Exp),
        ];
        let output = glr.parse(input);
        assert_eq!(output, vec![
            MixfixTree::Apply(
                Box::new(
                    MixfixTree::Apply(
                        Box::new(MixfixTree::Name(
                            Name::Mixfix(vec![
                                Part::Placeholder,
                                Part::Op(String::from("+")),
                                Part::Placeholder,
                            ]),
                            vec![])),
                        Box::new(MixfixTree::Exp)
                    ),
                ),
                Box::new(MixfixTree::Exp)
            )
        ]);
    }

    // input: e + e
    #[test]
    pub fn test_e_plus_e_none() {
        let mut glr = BinaryPlusNone::glr();
        let input = vec![
            Token::Exp(MixfixTree::Exp),
            Token::Name(Part::Op(String::from("+")), vec![]),
            Token::Exp(MixfixTree::Exp),
        ];
        let output = glr.parse(input);
        assert_eq!(output, vec![
            MixfixTree::Apply(
                Box::new(
                    MixfixTree::Apply(
                        Box::new(MixfixTree::Name(
                            Name::Mixfix(vec![
                                Part::Placeholder,
                                Part::Op(String::from("+")),
                                Part::Placeholder,
                            ]),
                            vec![])),
                        Box::new(MixfixTree::Exp)
                    ),
                ),
                Box::new(MixfixTree::Exp)
            )
        ]);
    }

    // input: e + e + e
    #[test]
    pub fn test_e_plus_e_plus_e_left() {
        let mut glr = BinaryPlusLeft::glr();
        let input = vec![
            Token::Exp(MixfixTree::Exp),
            Token::Name(Part::Op(String::from("+")), vec![]),
            Token::Exp(MixfixTree::Exp),
            Token::Name(Part::Op(String::from("+")), vec![]),
            Token::Exp(MixfixTree::Exp),
        ];
        let output = glr.parse(input);
        assert_eq!(output, vec![
            MixfixTree::Apply(
                Box::new(
                    MixfixTree::Apply(
                        Box::new(MixfixTree::Name(
                            Name::Mixfix(vec![
                                Part::Placeholder,
                                Part::Op(String::from("+")),
                                Part::Placeholder,
                            ]),
                            vec![])),
                        Box::new(
                            MixfixTree::Apply(
                                Box::new(
                                    MixfixTree::Apply(
                                        Box::new(MixfixTree::Name(
                                            Name::Mixfix(vec![
                                                Part::Placeholder,
                                                Part::Op(String::from("+")),
                                                Part::Placeholder,
                                            ]),
                                            vec![])),
                                        Box::new(MixfixTree::Exp)
                                    ),
                                ),
                                Box::new(MixfixTree::Exp)
                            )
                        )
                    ),
                ),
                Box::new(MixfixTree::Exp)
            )
        ]);
    }

    // input: e + e + e
    #[test]
    pub fn test_e_plus_e_plus_e_right() {
        let mut glr = BinaryPlusRight::glr();
        let input = vec![
            Token::Exp(MixfixTree::Exp),
            Token::Name(Part::Op(String::from("+")), vec![]),
            Token::Exp(MixfixTree::Exp),
            Token::Name(Part::Op(String::from("+")), vec![]),
            Token::Exp(MixfixTree::Exp),
        ];
        let output = glr.parse(input);
        assert_eq!(output, vec![
            MixfixTree::Apply(
                Box::new(
                    MixfixTree::Apply(
                        Box::new(MixfixTree::Name(
                            Name::Mixfix(vec![
                                Part::Placeholder,
                                Part::Op(String::from("+")),
                                Part::Placeholder,
                            ]),
                            vec![])),
                        Box::new(MixfixTree::Exp)
                    ),
                ),
                Box::new(
                    MixfixTree::Apply(
                        Box::new(
                            MixfixTree::Apply(
                                Box::new(MixfixTree::Name(
                                    Name::Mixfix(vec![
                                        Part::Placeholder,
                                        Part::Op(String::from("+")),
                                        Part::Placeholder,
                                    ]),
                                    vec![])),
                                Box::new(MixfixTree::Exp)
                            ),
                        ),
                        Box::new(MixfixTree::Exp)
                    )
                )
            )
        ]);
    }
}
