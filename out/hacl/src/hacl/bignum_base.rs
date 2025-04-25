#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

#[inline] pub fn Hacl_Bignum_Addition_bn_add_eq_len_u32(
  aLen: u32,
  a: &[u32],
  b: &[u32],
  res: &mut [u32]
) ->
    u32
{
  let mut c: u32 = 0u32;
  for i in 0u32..aLen.wrapping_div(4u32)
  {
    let t1: u32 = a[4u32.wrapping_mul(i) as usize];
    let t20: u32 = b[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u32], &mut [u32]) = res.split_at_mut(4u32.wrapping_mul(i) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t1, t20, res_i0.1);
    let t10: u32 = a[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u32 = b[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u32], &mut [u32]) = res_i0.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t10, t21, res_i1.1);
    let t11: u32 = a[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u32 = b[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u32], &mut [u32]) = res_i1.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t11, t22, res_i2.1);
    let t12: u32 = a[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u32 = b[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u32], &mut [u32]) = res_i2.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t12, t2, res_i.1)
  };
  for i in aLen.wrapping_div(4u32).wrapping_mul(4u32)..aLen
  {
    let t1: u32 = a[i as usize];
    let t2: u32 = b[i as usize];
    let res_i: (&mut [u32], &mut [u32]) = res.split_at_mut(i as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t1, t2, res_i.1)
  };
  return c
}

#[inline] pub fn Hacl_Bignum_Addition_bn_add_eq_len_u64(
  aLen: u32,
  a: &[u64],
  b: &[u64],
  res: &mut [u64]
) ->
    u64
{
  let mut c: u64 = 0u64;
  for i in 0u32..aLen.wrapping_div(4u32)
  {
    let t1: u64 = a[4u32.wrapping_mul(i) as usize];
    let t20: u64 = b[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = res.split_at_mut(4u32.wrapping_mul(i) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t1, t20, res_i0.1);
    let t10: u64 = a[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u64 = b[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t10, t21, res_i1.1);
    let t11: u64 = a[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u64 = b[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t11, t22, res_i2.1);
    let t12: u64 = a[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u64 = b[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t12, t2, res_i.1)
  };
  for i in aLen.wrapping_div(4u32).wrapping_mul(4u32)..aLen
  {
    let t1: u64 = a[i as usize];
    let t2: u64 = b[i as usize];
    let res_i: (&mut [u64], &mut [u64]) = res.split_at_mut(i as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t1, t2, res_i.1)
  };
  return c
}

#[inline] pub fn Hacl_Bignum_Addition_bn_sub_eq_len_u32(
  aLen: u32,
  a: &[u32],
  b: &[u32],
  res: &mut [u32]
) ->
    u32
{
  let mut c: u32 = 0u32;
  for i in 0u32..aLen.wrapping_div(4u32)
  {
    let t1: u32 = a[4u32.wrapping_mul(i) as usize];
    let t20: u32 = b[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u32], &mut [u32]) = res.split_at_mut(4u32.wrapping_mul(i) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c, t1, t20, res_i0.1);
    let t10: u32 = a[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u32 = b[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u32], &mut [u32]) = res_i0.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c, t10, t21, res_i1.1);
    let t11: u32 = a[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u32 = b[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u32], &mut [u32]) = res_i1.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c, t11, t22, res_i2.1);
    let t12: u32 = a[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u32 = b[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u32], &mut [u32]) = res_i2.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c, t12, t2, res_i.1)
  };
  for i in aLen.wrapping_div(4u32).wrapping_mul(4u32)..aLen
  {
    let t1: u32 = a[i as usize];
    let t2: u32 = b[i as usize];
    let res_i: (&mut [u32], &mut [u32]) = res.split_at_mut(i as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c, t1, t2, res_i.1)
  };
  return c
}

#[inline] pub fn Hacl_Bignum_Addition_bn_sub_eq_len_u64(
  aLen: u32,
  a: &[u64],
  b: &[u64],
  res: &mut [u64]
) ->
    u64
{
  let mut c: u64 = 0u64;
  for i in 0u32..aLen.wrapping_div(4u32)
  {
    let t1: u64 = a[4u32.wrapping_mul(i) as usize];
    let t20: u64 = b[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = res.split_at_mut(4u32.wrapping_mul(i) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, t20, res_i0.1);
    let t10: u64 = a[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u64 = b[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t10, t21, res_i1.1);
    let t11: u64 = a[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u64 = b[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t11, t22, res_i2.1);
    let t12: u64 = a[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u64 = b[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t12, t2, res_i.1)
  };
  for i in aLen.wrapping_div(4u32).wrapping_mul(4u32)..aLen
  {
    let t1: u64 = a[i as usize];
    let t2: u64 = b[i as usize];
    let res_i: (&mut [u64], &mut [u64]) = res.split_at_mut(i as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, t2, res_i.1)
  };
  return c
}

#[inline] pub fn Hacl_Bignum_Base_mul_wide_add2_u32(a: u32, b: u32, c_in: u32, out: &mut [u32]) ->
    u32
{
  let out0: u32 = out[0usize];
  let res: u64 =
      (a as u64).wrapping_mul(b as u64).wrapping_add(c_in as u64).wrapping_add(out0 as u64);
  out[0usize] = res as u32;
  return res.wrapping_shr(32u32) as u32
}

#[inline] pub fn Hacl_Bignum_Base_mul_wide_add2_u64(a: u64, b: u64, c_in: u64, out: &mut [u64]) ->
    u64
{
  let out0: u64 = out[0usize];
  let res: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(a, b),
          crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c_in)
        ),
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(out0)
      );
  out[0usize] = crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(res);
  return
  crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
    crate::hacl_krmllib::FStar_UInt128_shift_right(res, 64u32)
  )
}

#[inline] pub fn Hacl_Bignum_Convert_bn_from_bytes_be_uint64(
  len: u32,
  b: &[u8],
  res: &mut [u64]
)
{
  let bnLen: u32 = len.wrapping_sub(1u32).wrapping_div(8u32).wrapping_add(1u32);
  let tmpLen: u32 = 8u32.wrapping_mul(bnLen);
  let mut tmp: Box<[u8]> = vec![0u8; tmpLen as usize].into_boxed_slice();
  ((&mut (&mut tmp)[(tmpLen as usize).wrapping_sub(len as usize)..])[0usize..len as usize]).copy_from_slice(
    &b[0usize..len as usize]
  );
  for i in 0u32..bnLen
  {
    let u: u64 =
        crate::lowstar_endianness::load64_be(
          &(&tmp)[bnLen.wrapping_sub(i).wrapping_sub(1u32).wrapping_mul(8u32) as usize..]
        );
    let x: u64 = u;
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

#[inline] pub fn Hacl_Bignum_Convert_bn_to_bytes_be_uint64(len: u32, b: &[u64], res: &mut [u8])
{
  let bnLen: u32 = len.wrapping_sub(1u32).wrapping_div(8u32).wrapping_add(1u32);
  let tmpLen: u32 = 8u32.wrapping_mul(bnLen);
  let mut tmp: Box<[u8]> = vec![0u8; tmpLen as usize].into_boxed_slice();
  for i in 0u32..bnLen
  {
    crate::lowstar_endianness::store64_be(
      &mut (&mut tmp)[i.wrapping_mul(8u32) as usize..],
      b[bnLen.wrapping_sub(i).wrapping_sub(1u32) as usize]
    )
  };
  (res[0usize..len as usize]).copy_from_slice(
    &(&(&tmp)[(tmpLen as usize).wrapping_sub(len as usize)..])[0usize..len as usize]
  )
}

#[inline] pub fn Hacl_Bignum_Lib_bn_get_bits_u32(len: u32, b: &[u32], i: u32, l: u32) -> u32
{
  let i1: u32 = i.wrapping_div(32u32);
  let j: u32 = i.wrapping_rem(32u32);
  let p1: u32 = (b[i1 as usize]).wrapping_shr(j);
  let mut ite: u32;
  if i1.wrapping_add(1u32) < len && 0u32 < j
  { ite = p1 | (b[i1.wrapping_add(1u32) as usize]).wrapping_shl(32u32.wrapping_sub(j)) }
  else
  { ite = p1 };
  return ite & 1u32.wrapping_shl(l).wrapping_sub(1u32)
}

#[inline] pub fn Hacl_Bignum_Lib_bn_get_bits_u64(len: u32, b: &[u64], i: u32, l: u32) -> u64
{
  let i1: u32 = i.wrapping_div(64u32);
  let j: u32 = i.wrapping_rem(64u32);
  let p1: u64 = (b[i1 as usize]).wrapping_shr(j);
  let mut ite: u64;
  if i1.wrapping_add(1u32) < len && 0u32 < j
  { ite = p1 | (b[i1.wrapping_add(1u32) as usize]).wrapping_shl(64u32.wrapping_sub(j)) }
  else
  { ite = p1 };
  return ite & 1u64.wrapping_shl(l).wrapping_sub(1u64)
}

#[inline] pub fn Hacl_Bignum_Lib_bn_get_top_index_u32(len: u32, b: &[u32]) -> u32
{
  let mut r#priv: u32 = 0u32;
  for i in 0u32..len
  {
    let mask: u32 = crate::hacl_krmllib::FStar_UInt32_eq_mask(b[i as usize], 0u32);
    r#priv = mask & r#priv | ! mask & i
  };
  return r#priv
}

#[inline] pub fn Hacl_Bignum_Lib_bn_get_top_index_u64(len: u32, b: &[u64]) -> u64
{
  let mut r#priv: u64 = 0u64;
  for i in 0u32..len
  {
    let mask: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(b[i as usize], 0u64);
    r#priv = mask & r#priv | ! mask & i as u64
  };
  return r#priv
}

#[inline] pub fn Hacl_Bignum_Multiplication_bn_mul_u32(
  aLen: u32,
  a: &[u32],
  bLen: u32,
  b: &[u32],
  res: &mut [u32]
)
{
  (res[0usize..aLen.wrapping_add(bLen) as usize]).copy_from_slice(
    &vec![0u32; aLen.wrapping_add(bLen) as usize].into_boxed_slice()
  );
  for i0 in 0u32..bLen
  {
    let bj: u32 = b[i0 as usize];
    let res_j: (&mut [u32], &mut [u32]) = res.split_at_mut(i0 as usize);
    let mut c: u32 = 0u32;
    for i in 0u32..aLen.wrapping_div(4u32)
    {
      let a_i: u32 = a[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u32], &mut [u32]) = res_j.1.split_at_mut(4u32.wrapping_mul(i) as usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u32(a_i, bj, c, res_i0.1);
      let a_i0: u32 = a[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u32], &mut [u32]) = res_i0.1.split_at_mut(1usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u32(a_i0, bj, c, res_i1.1);
      let a_i1: u32 = a[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u32], &mut [u32]) = res_i1.1.split_at_mut(1usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u32(a_i1, bj, c, res_i2.1);
      let a_i2: u32 = a[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u32], &mut [u32]) = res_i2.1.split_at_mut(1usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u32(a_i2, bj, c, res_i.1)
    };
    for i in aLen.wrapping_div(4u32).wrapping_mul(4u32)..aLen
    {
      let a_i: u32 = a[i as usize];
      let res_i: (&mut [u32], &mut [u32]) = res_j.1.split_at_mut(i as usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u32(a_i, bj, c, res_i.1)
    };
    let r: u32 = c;
    res[aLen.wrapping_add(i0) as usize] = r
  }
}

#[inline] pub fn Hacl_Bignum_Multiplication_bn_mul_u64(
  aLen: u32,
  a: &[u64],
  bLen: u32,
  b: &[u64],
  res: &mut [u64]
)
{
  (res[0usize..aLen.wrapping_add(bLen) as usize]).copy_from_slice(
    &vec![0u64; aLen.wrapping_add(bLen) as usize].into_boxed_slice()
  );
  for i0 in 0u32..bLen
  {
    let bj: u64 = b[i0 as usize];
    let res_j: (&mut [u64], &mut [u64]) = res.split_at_mut(i0 as usize);
    let mut c: u64 = 0u64;
    for i in 0u32..aLen.wrapping_div(4u32)
    {
      let a_i: u64 = a[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u64], &mut [u64]) = res_j.1.split_at_mut(4u32.wrapping_mul(i) as usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u64(a_i, bj, c, res_i0.1);
      let a_i0: u64 = a[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u64(a_i0, bj, c, res_i1.1);
      let a_i1: u64 = a[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u64(a_i1, bj, c, res_i2.1);
      let a_i2: u64 = a[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u64(a_i2, bj, c, res_i.1)
    };
    for i in aLen.wrapping_div(4u32).wrapping_mul(4u32)..aLen
    {
      let a_i: u64 = a[i as usize];
      let res_i: (&mut [u64], &mut [u64]) = res_j.1.split_at_mut(i as usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u64(a_i, bj, c, res_i.1)
    };
    let r: u64 = c;
    res[aLen.wrapping_add(i0) as usize] = r
  }
}

#[inline] pub fn Hacl_Bignum_Multiplication_bn_sqr_u32(aLen: u32, a: &[u32], res: &mut [u32])
{
  (res[0usize..aLen.wrapping_add(aLen) as usize]).copy_from_slice(
    &vec![0u32; aLen.wrapping_add(aLen) as usize].into_boxed_slice()
  );
  for i0 in 0u32..aLen
  {
    let a_j: u32 = a[i0 as usize];
    let ab: (&[u32], &[u32]) = a.split_at(0usize);
    let res_j: (&mut [u32], &mut [u32]) = res.split_at_mut(i0 as usize);
    let mut c: u32 = 0u32;
    for i in 0u32..i0.wrapping_div(4u32)
    {
      let a_i: u32 = ab.1[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u32], &mut [u32]) = res_j.1.split_at_mut(4u32.wrapping_mul(i) as usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u32(a_i, a_j, c, res_i0.1);
      let a_i0: u32 = ab.1[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u32], &mut [u32]) = res_i0.1.split_at_mut(1usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u32(a_i0, a_j, c, res_i1.1);
      let a_i1: u32 = ab.1[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u32], &mut [u32]) = res_i1.1.split_at_mut(1usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u32(a_i1, a_j, c, res_i2.1);
      let a_i2: u32 = ab.1[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u32], &mut [u32]) = res_i2.1.split_at_mut(1usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u32(a_i2, a_j, c, res_i.1)
    };
    for i in i0.wrapping_div(4u32).wrapping_mul(4u32)..i0
    {
      let a_i: u32 = ab.1[i as usize];
      let res_i: (&mut [u32], &mut [u32]) = res_j.1.split_at_mut(i as usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u32(a_i, a_j, c, res_i.1)
    };
    let r: u32 = c;
    res[i0.wrapping_add(i0) as usize] = r
  };
  let mut a_copy0: Box<[u32]> = vec![0u32; aLen.wrapping_add(aLen) as usize].into_boxed_slice();
  let mut b_copy0: Box<[u32]> = vec![0u32; aLen.wrapping_add(aLen) as usize].into_boxed_slice();
  ((&mut a_copy0)[0usize..aLen.wrapping_add(aLen) as usize]).copy_from_slice(
    &res[0usize..aLen.wrapping_add(aLen) as usize]
  );
  ((&mut b_copy0)[0usize..aLen.wrapping_add(aLen) as usize]).copy_from_slice(
    &res[0usize..aLen.wrapping_add(aLen) as usize]
  );
  let r: u32 =
      Hacl_Bignum_Addition_bn_add_eq_len_u32(aLen.wrapping_add(aLen), &a_copy0, &b_copy0, res);
  let c0: u32 = r;
  crate::lowstar::ignore::ignore::<u32>(c0);
  let mut tmp: Box<[u32]> = vec![0u32; aLen.wrapping_add(aLen) as usize].into_boxed_slice();
  for i in 0u32..aLen
  {
    let res1: u64 = (a[i as usize] as u64).wrapping_mul(a[i as usize] as u64);
    let hi: u32 = res1.wrapping_shr(32u32) as u32;
    let lo: u32 = res1 as u32;
    (&mut tmp)[2u32.wrapping_mul(i) as usize] = lo;
    (&mut tmp)[2u32.wrapping_mul(i).wrapping_add(1u32) as usize] = hi
  };
  let mut a_copy: Box<[u32]> = vec![0u32; aLen.wrapping_add(aLen) as usize].into_boxed_slice();
  let mut b_copy: Box<[u32]> = vec![0u32; aLen.wrapping_add(aLen) as usize].into_boxed_slice();
  ((&mut a_copy)[0usize..aLen.wrapping_add(aLen) as usize]).copy_from_slice(
    &res[0usize..aLen.wrapping_add(aLen) as usize]
  );
  ((&mut b_copy)[0usize..aLen.wrapping_add(aLen) as usize]).copy_from_slice(
    &(&tmp)[0usize..aLen.wrapping_add(aLen) as usize]
  );
  let r0: u32 =
      Hacl_Bignum_Addition_bn_add_eq_len_u32(aLen.wrapping_add(aLen), &a_copy, &b_copy, res);
  let c1: u32 = r0;
  crate::lowstar::ignore::ignore::<u32>(c1)
}

#[inline] pub fn Hacl_Bignum_Multiplication_bn_sqr_u64(aLen: u32, a: &[u64], res: &mut [u64])
{
  (res[0usize..aLen.wrapping_add(aLen) as usize]).copy_from_slice(
    &vec![0u64; aLen.wrapping_add(aLen) as usize].into_boxed_slice()
  );
  for i0 in 0u32..aLen
  {
    let a_j: u64 = a[i0 as usize];
    let ab: (&[u64], &[u64]) = a.split_at(0usize);
    let res_j: (&mut [u64], &mut [u64]) = res.split_at_mut(i0 as usize);
    let mut c: u64 = 0u64;
    for i in 0u32..i0.wrapping_div(4u32)
    {
      let a_i: u64 = ab.1[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u64], &mut [u64]) = res_j.1.split_at_mut(4u32.wrapping_mul(i) as usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u64(a_i, a_j, c, res_i0.1);
      let a_i0: u64 = ab.1[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u64(a_i0, a_j, c, res_i1.1);
      let a_i1: u64 = ab.1[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u64(a_i1, a_j, c, res_i2.1);
      let a_i2: u64 = ab.1[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u64(a_i2, a_j, c, res_i.1)
    };
    for i in i0.wrapping_div(4u32).wrapping_mul(4u32)..i0
    {
      let a_i: u64 = ab.1[i as usize];
      let res_i: (&mut [u64], &mut [u64]) = res_j.1.split_at_mut(i as usize);
      c = Hacl_Bignum_Base_mul_wide_add2_u64(a_i, a_j, c, res_i.1)
    };
    let r: u64 = c;
    res[i0.wrapping_add(i0) as usize] = r
  };
  let mut a_copy0: Box<[u64]> = vec![0u64; aLen.wrapping_add(aLen) as usize].into_boxed_slice();
  let mut b_copy0: Box<[u64]> = vec![0u64; aLen.wrapping_add(aLen) as usize].into_boxed_slice();
  ((&mut a_copy0)[0usize..aLen.wrapping_add(aLen) as usize]).copy_from_slice(
    &res[0usize..aLen.wrapping_add(aLen) as usize]
  );
  ((&mut b_copy0)[0usize..aLen.wrapping_add(aLen) as usize]).copy_from_slice(
    &res[0usize..aLen.wrapping_add(aLen) as usize]
  );
  let r: u64 =
      Hacl_Bignum_Addition_bn_add_eq_len_u64(aLen.wrapping_add(aLen), &a_copy0, &b_copy0, res);
  let c0: u64 = r;
  crate::lowstar::ignore::ignore::<u64>(c0);
  let mut tmp: Box<[u64]> = vec![0u64; aLen.wrapping_add(aLen) as usize].into_boxed_slice();
  for i in 0u32..aLen
  {
    let res1: crate::types::FStar_UInt128_uint128 =
        crate::hacl_krmllib::FStar_UInt128_mul_wide(a[i as usize], a[i as usize]);
    let hi: u64 =
        crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
          crate::hacl_krmllib::FStar_UInt128_shift_right(res1, 64u32)
        );
    let lo: u64 = crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(res1);
    (&mut tmp)[2u32.wrapping_mul(i) as usize] = lo;
    (&mut tmp)[2u32.wrapping_mul(i).wrapping_add(1u32) as usize] = hi
  };
  let mut a_copy: Box<[u64]> = vec![0u64; aLen.wrapping_add(aLen) as usize].into_boxed_slice();
  let mut b_copy: Box<[u64]> = vec![0u64; aLen.wrapping_add(aLen) as usize].into_boxed_slice();
  ((&mut a_copy)[0usize..aLen.wrapping_add(aLen) as usize]).copy_from_slice(
    &res[0usize..aLen.wrapping_add(aLen) as usize]
  );
  ((&mut b_copy)[0usize..aLen.wrapping_add(aLen) as usize]).copy_from_slice(
    &(&tmp)[0usize..aLen.wrapping_add(aLen) as usize]
  );
  let r0: u64 =
      Hacl_Bignum_Addition_bn_add_eq_len_u64(aLen.wrapping_add(aLen), &a_copy, &b_copy, res);
  let c1: u64 = r0;
  crate::lowstar::ignore::ignore::<u64>(c1)
}
