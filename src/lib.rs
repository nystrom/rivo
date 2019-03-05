#![feature(custom_attribute, plugin)]
// #![cfg_attr(test, plugin(quickcheck_macros))]

#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unreachable_code)]

#![feature(vec_remove_item)]
#![feature(nll)]
#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(step_trait)]
#![feature(drain_filter)]

extern crate term;
extern crate num;
extern crate unicode_categories;
extern crate pretty;
extern crate lazycell;
extern crate rpds;
// #[macro_use]
// extern crate failure;

// extern crate rayon;
// extern crate chalk;
//extern crate datafrog;

extern crate cfg;
extern crate gearley;

extern crate string_interner;
#[macro_use]
extern crate lazy_static;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

#[macro_use]
extern crate trace;

pub mod parser;
pub mod syntax;
pub mod namer;
pub mod visit;
pub mod driver;

pub mod lower;
