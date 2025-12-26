#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

#[derive(PartialEq, Clone, Copy)]
pub enum E
{
  A,
  B
}

pub fn _main() -> i32 { return (E::A == E::B) as i32 }
fn main() { assert_eq!(0, _main()) }
