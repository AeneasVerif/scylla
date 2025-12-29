#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

pub fn _main() -> i32
{
  let mut r: i32 = 0i32;
  match e::A
  {
    e::B => r = 1i32,
    e::C => r = 1i32,
    e::A => r = 0i32,
    _ => panic!("Incomplete pattern matching")
  };
  return r
}

#[derive(PartialEq, Clone, Copy)]
pub enum e
{
  A,
  B,
  C
}
fn main() { assert_eq!(0, _main()) }
