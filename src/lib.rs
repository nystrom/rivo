#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unreachable_code)]
// #![allow(unused_must_use)]

#![feature(vec_remove_item)]
#![feature(nll)]

extern crate ansi_term;
extern crate term;
extern crate typed_arena;
extern crate num;
extern crate unicode_categories;
extern crate pretty;
extern crate rpds;

#[macro_use] extern crate lazy_static;
extern crate lazycell;

#[cfg(test)] #[macro_use] extern crate pretty_assertions;

pub mod parser;
pub mod syntax;
pub mod namer;
pub mod visit;
