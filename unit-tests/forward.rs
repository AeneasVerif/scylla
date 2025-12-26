#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

pub fn _main() -> i32
{
  let p: point = point { x: 0i32, y: 0i32 };
  return (p.x).wrapping_add(p.y)
}

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct point
{ pub x: i32, pub y: i32 }
fn main() { assert_eq!(0, _main()) }
