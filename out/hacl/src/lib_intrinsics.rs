#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]

pub fn Lib_IntTypes_Intrinsics_add_carry_u32(cin: u32, x: u32, y: u32, r: &mut [u32]) -> u32
{
    let res: u64 = (x as u64).wrapping_add(cin as u64).wrapping_add(y as u64);
    let c: u32 = res.wrapping_shr(32u32) as u32;
    r[0usize] = res as u32;
    c
}

pub fn Lib_IntTypes_Intrinsics_sub_borrow_u32(cin: u32, x: u32, y: u32, r: &mut [u32]) -> u32
{
    let res: u64 = (x as u64).wrapping_sub(y as u64).wrapping_sub(cin as u64);
    let c: u32 = res.wrapping_shr(32u32) as u32 & 1u32;
    r[0usize] = res as u32;
    c
}

pub fn Lib_IntTypes_Intrinsics_add_carry_u64(cin: u64, x: u64, y: u64, r: &mut [u64]) -> u64
{
    let res: u128 = (x as u128).wrapping_add(cin as u128).wrapping_add(y as u128);
    let c: u64 = res.wrapping_shr(64u32) as u64;
    r[0usize] = res as u64;
    c
}

pub fn Lib_IntTypes_Intrinsics_sub_borrow_u64(cin: u64, x: u64, y: u64, r: &mut [u64]) -> u64
{
    let res: u128 = (x as u128).wrapping_sub(y as u128).wrapping_sub(cin as u128);
    let c: u64 = res.wrapping_shr(64u32) as u64 & 1u64;
    r[0usize] = res as u64;
    c
}
