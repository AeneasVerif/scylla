#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

pub fn _main() -> i32
{
  let x: u16 = 0u16;
  let y: u16 = 1u16;
  return
  (f(std::slice::from_ref::<u16>(&x), std::slice::from_ref::<u16>(&y)) as u32 != 4294967295u32)
  as
  i32
}

pub fn f(x: &[u16], y: &[u16]) -> i32
{
  let z: i32 = (x[0usize] as i32).wrapping_sub(y[0usize] as i32);
  return z
}
fn main() { assert_eq!(0, _main()) }
