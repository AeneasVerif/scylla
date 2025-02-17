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

