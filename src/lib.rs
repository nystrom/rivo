#![feature(custom_attribute, plugin)]
#![plugin(trace)]

#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unreachable_code)]
// #![allow(unused_must_use)]

#![feature(vec_resize_default)] 
#![feature(vec_remove_item)]
#![feature(nll)]

extern crate term;
extern crate num;
extern crate unicode_categories;
extern crate pretty;
extern crate lazycell;
extern crate rpds;
#[macro_use] extern crate failure;

#[cfg(test)] #[macro_use] extern crate pretty_assertions;

pub mod parser;
pub mod syntax;
pub mod namer;
pub mod visit;
pub mod driver;
