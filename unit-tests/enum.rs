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

#[derive(PartialEq, Clone, Copy)]
pub enum F
{
  C,
  D
}

pub fn _main() -> i32
{
  let c: F = F::C;
  return (E::A == E::B || c == F::D) as i32
}
fn main() { assert_eq!(0, _main()) }
