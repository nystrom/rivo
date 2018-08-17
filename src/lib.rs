#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unreachable_code)]
#![allow(unused_must_use)]

extern crate ansi_term;
extern crate term;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
#[cfg(test)] #[macro_use] extern crate pretty_assertions;
extern crate rustc_serialize;
extern crate num;
extern crate unicode_categories;

pub mod parser;
pub mod syntax;
