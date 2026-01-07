#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

pub fn _main() -> i32
{
  let mut i: i32 = 0i32;
  while
  {
    i = i.wrapping_add(1i32);
    (
      {
        i = i.wrapping_add(1i32);
        i
      }
    )
    <
    0i32
  }
  { () };
  return
  ({
    i = i.wrapping_add(1i32);
    i
  }
  ==
  0i32)
  as
  i32
}
fn main() { assert_eq!(0, _main()) }
