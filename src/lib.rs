#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unreachable_code)]
// #![allow(unused_must_use)]

#![feature(nll)]

extern crate ansi_term;
extern crate term;
extern crate typed_arena;
#[cfg(test)] #[macro_use] extern crate pretty_assertions;
extern crate num;
extern crate unicode_categories;

pub mod parser;
pub mod syntax;
pub mod namer;
pub mod visit;
