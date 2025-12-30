#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

#[derive(PartialEq, Clone, Copy)] pub enum CPUFeature { kNeon }

pub type VP8CPUInfo <'a> = &'a [fn (CPUFeature) -> i32];

pub const VP8GetCPUInfo: &[fn (CPUFeature) -> i32] =
    std::slice::from_ref::<fn (CPUFeature) -> i32>(&(armCPUInfo as fn (CPUFeature) -> i32));

pub fn _main() -> i32
{
  init();
  init2();
  return 0i32
}

pub fn armCPUInfo(feature: CPUFeature) -> i32 { return 0i32 }

pub fn init() { () }

pub fn init2() { () }
fn main() { assert_eq!(0, _main()) }
