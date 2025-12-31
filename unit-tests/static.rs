#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

pub fn _main() -> i32 { return x }

pub const x: i32 = 0i32;
fn main() { assert_eq!(0, _main()) }
