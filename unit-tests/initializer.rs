#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

pub fn _main() -> i32
{
  let v: vector = vector { base: point { x: 0i32, y: 0i32 }, magnitude: [0i32, 0i32] };
  let vs: [vector; 1] =
      [vector { base: point { x: 0i32, y: 0i32 }, magnitude: [0i32, 0i32] }; 1usize];
  return (v.base.x != (vs[0usize]).magnitude[0usize]) as i32
}

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct point
{ pub x: i32, pub y: i32 }

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct vector
{ pub base: point, pub magnitude: [i32; 2] }
fn main() { assert_eq!(0, _main()) }
