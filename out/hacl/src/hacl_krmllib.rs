#![allow(non_snake_case)]

pub fn FStar_UInt64_eq_mask(a: u64, b: u64) -> u64
{
  let x = a ^ b;
  let minus_x = (!x).wrapping_add(1u64);
  let x_or_minus_x = x | minus_x;
  let xnx = x_or_minus_x.wrapping_shr(63);
  xnx.wrapping_sub(1u64)
}

pub fn FStar_UInt64_gte_mask(a: u64, b: u64) -> u64
{
  let x = a;
  let y = b;
  let x_xor_y = x ^ y;
  let x_sub_y = x.wrapping_sub(y);
  let x_sub_y_xor_y = x_sub_y ^ y;
  let q = x_xor_y | x_sub_y_xor_y;
  let x_xor_q = x ^ q;
  let x_xor_q_ = x_xor_q.wrapping_shr(63);
  x_xor_q_.wrapping_sub(1u64)
}

pub fn FStar_UInt32_eq_mask(a: u32, b: u32) -> u32
{
  let x = a ^ b;
  let minus_x = (!x).wrapping_add(1u32);
  let x_or_minus_x = x | minus_x;
  let xnx = x_or_minus_x.wrapping_shr(31);
  xnx.wrapping_sub(1u32)
}

pub fn FStar_UInt32_gte_mask(a: u32, b: u32) -> u32
{
  let x = a;
  let y = b;
  let x_xor_y = x ^ y;
  let x_sub_y = x.wrapping_sub(y);
  let x_sub_y_xor_y = x_sub_y ^ y;
  let q = x_xor_y | x_sub_y_xor_y;
  let x_xor_q = x ^ q;
  let x_xor_q_ = x_xor_q.wrapping_shr(31);
  x_xor_q_.wrapping_sub(1u32)
}

pub fn FStar_UInt16_eq_mask(a: u16, b: u16) -> u16
{
  let x = a ^ b;
  let minus_x = (!x).wrapping_add(1u16);
  let x_or_minus_x = x | minus_x;
  let xnx = x_or_minus_x.wrapping_shr(15);
  xnx.wrapping_sub(1u16)
}

pub fn FStar_UInt16_gte_mask(a: u16, b: u16) -> u16
{
  let x = a;
  let y = b;
  let x_xor_y = x ^ y;
  let x_sub_y = x.wrapping_sub(y);
  let x_sub_y_xor_y = x_sub_y ^ y;
  let q = x_xor_y | x_sub_y_xor_y;
  let x_xor_q = x ^ q;
  let x_xor_q_ = x_xor_q.wrapping_shr(15);
  x_xor_q_.wrapping_sub(1u16)
}

pub fn FStar_UInt8_eq_mask(a: u8, b: u8) -> u8
{
  let x = a ^ b;
  let minus_x = (!x).wrapping_add(1u8);
  let x_or_minus_x = x | minus_x;
  let xnx = x_or_minus_x.wrapping_shr(7);
  xnx.wrapping_sub(1u8)
}

pub fn FStar_UInt8_gte_mask(a: u8, b: u8) -> u8
{
  let x = a;
  let y = b;
  let x_xor_y = x ^ y;
  let x_sub_y = x.wrapping_sub(y);
  let x_sub_y_xor_y = x_sub_y ^ y;
  let q = x_xor_y | x_sub_y_xor_y;
  let x_xor_q = x ^ q;
  let x_xor_q_ = x_xor_q.wrapping_shr(7);
  x_xor_q_.wrapping_sub(1u8)
}

#[inline(always)]
pub fn FStar_UInt128_add(a: crate::fstar_uint128::uint128, b: crate::fstar_uint128::uint128) -> crate::fstar_uint128::uint128 {
    crate::fstar_uint128::add(a, b)
}

#[inline(always)]
pub fn FStar_UInt128_shift_left(a: crate::fstar_uint128::uint128, b: u32) -> crate::fstar_uint128::uint128 {
    crate::fstar_uint128::shift_left(a, b)
}

#[inline(always)]
pub fn FStar_UInt128_shift_right(a: crate::fstar_uint128::uint128, b: u32) -> crate::fstar_uint128::uint128 {
    crate::fstar_uint128::shift_right(a, b)
}

#[inline(always)]
pub fn FStar_UInt128_mul_wide(a: u64, b: u64) -> crate::fstar_uint128::uint128 {
    crate::fstar_uint128::mul_wide(a, b)
}

#[inline(always)]
pub fn FStar_UInt128_uint64_to_uint128(a: u64) -> crate::fstar_uint128::uint128 {
    crate::fstar_uint128::uint64_to_uint128(a)
}

#[inline(always)]
pub fn FStar_UInt128_uint128_to_uint64(a: crate::fstar_uint128::uint128) -> u64 {
    crate::fstar_uint128::uint128_to_uint64(a)
}

pub fn load128_be(bytes: &[u8]) -> u128 {
    u128::from_be_bytes(bytes[0..16].try_into().unwrap())
}

pub fn store128_be(bytes: &mut[u8], x: u128) {
    bytes[0..16].copy_from_slice(&u128::to_be_bytes(x))
}
