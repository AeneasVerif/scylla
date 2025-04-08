#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]

pub fn Hacl_Bignum4096_add(a: &[u64], b: &[u64], res: &mut [u64]) -> u64
{
  let mut c: u64 = 0u64;
  for i in 0u32..16u32
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
  return c
}

pub fn Hacl_Bignum4096_sub(a: &[u64], b: &[u64], res: &mut [u64]) -> u64
{
  let mut c: u64 = 0u64;
  for i in 0u32..16u32
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
  return c
}

pub fn Hacl_Bignum4096_add_mod(n: &[u64], a: &[u64], b: &[u64], res: &mut [u64])
{
  let mut c0: u64 = 0u64;
  for i in 0u32..16u32
  {
    let t1: u64 = a[4u32.wrapping_mul(i) as usize];
    let t20: u64 = b[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = res.split_at_mut(4u32.wrapping_mul(i) as usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t1, t20, res_i0.1);
    let t10: u64 = a[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u64 = b[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t10, t21, res_i1.1);
    let t11: u64 = a[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u64 = b[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t11, t22, res_i2.1);
    let t12: u64 = a[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u64 = b[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t12, t2, res_i.1)
  };
  let c00: u64 = c0;
  let mut tmp: [u64; 64] = [0u64; 64usize];
  let mut c: u64 = 0u64;
  for i in 0u32..16u32
  {
    let t1: u64 = res[4u32.wrapping_mul(i) as usize];
    let t20: u64 = n[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(4u32.wrapping_mul(i) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, t20, res_i0.1);
    let t10: u64 = res[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u64 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t10, t21, res_i1.1);
    let t11: u64 = res[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u64 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t11, t22, res_i2.1);
    let t12: u64 = res[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u64 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t12, t2, res_i.1)
  };
  let c1: u64 = c;
  let c2: u64 = c00.wrapping_sub(c1);
  for i in 0u32..64u32
  {
    let x: u64 = c2 & res[i as usize] | ! c2 & tmp[i as usize];
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn Hacl_Bignum4096_sub_mod(n: &[u64], a: &[u64], b: &[u64], res: &mut [u64])
{
  let mut c0: u64 = 0u64;
  for i in 0u32..16u32
  {
    let t1: u64 = a[4u32.wrapping_mul(i) as usize];
    let t20: u64 = b[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = res.split_at_mut(4u32.wrapping_mul(i) as usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t1, t20, res_i0.1);
    let t10: u64 = a[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u64 = b[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t10, t21, res_i1.1);
    let t11: u64 = a[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u64 = b[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t11, t22, res_i2.1);
    let t12: u64 = a[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u64 = b[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t12, t2, res_i.1)
  };
  let c00: u64 = c0;
  let mut tmp: [u64; 64] = [0u64; 64usize];
  let mut c: u64 = 0u64;
  for i in 0u32..16u32
  {
    let t1: u64 = res[4u32.wrapping_mul(i) as usize];
    let t20: u64 = n[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(4u32.wrapping_mul(i) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t1, t20, res_i0.1);
    let t10: u64 = res[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u64 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t10, t21, res_i1.1);
    let t11: u64 = res[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u64 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t11, t22, res_i2.1);
    let t12: u64 = res[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u64 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t12, t2, res_i.1)
  };
  let c1: u64 = c;
  crate::lowstar::ignore::ignore::<u64>(c1);
  let c2: u64 = 0u64.wrapping_sub(c00);
  for i in 0u32..64u32
  {
    let x: u64 = c2 & tmp[i as usize] | ! c2 & res[i as usize];
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn Hacl_Bignum4096_mul(a: &[u64], b: &[u64], res: &mut [u64])
{
  let mut tmp: [u64; 256] = [0u64; 256usize];
  crate::hacl::bignum::Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64(64u32, a, b, &mut tmp, res)
}

pub fn Hacl_Bignum4096_sqr(a: &[u64], res: &mut [u64])
{
  let mut tmp: [u64; 256] = [0u64; 256usize];
  crate::hacl::bignum::Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint64(64u32, a, &mut tmp, res)
}

#[inline] pub fn precompr2(nBits: u32, n: &[u64], res: &mut [u64])
{
  (res[0usize..64usize]).copy_from_slice(&[0u64; 64usize]);
  let i: u32 = nBits.wrapping_div(64u32);
  let j: u32 = nBits.wrapping_rem(64u32);
  res[i as usize] |= 1u64.wrapping_shl(j);
  for i0 in 0u32..8192u32.wrapping_sub(nBits)
  {
    let mut a_copy: [u64; 64] = [0u64; 64usize];
    let mut b_copy: [u64; 64] = [0u64; 64usize];
    (a_copy[0usize..64usize]).copy_from_slice(&res[0usize..64usize]);
    (b_copy[0usize..64usize]).copy_from_slice(&res[0usize..64usize]);
    Hacl_Bignum4096_add_mod(n, &a_copy, &b_copy, res)
  }
}

#[inline] pub fn reduction(n: &[u64], nInv: u64, c: &mut [u64], res: &mut [u64])
{
  let mut c0: u64 = 0u64;
  for i0 in 0u32..64u32
  {
    let qj: u64 = nInv.wrapping_mul(c[i0 as usize]);
    let res_j0: (&mut [u64], &mut [u64]) = c.split_at_mut(i0 as usize);
    let mut c1: u64 = 0u64;
    for i in 0u32..16u32
    {
      let a_i: u64 = n[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u64], &mut [u64]) = res_j0.1.split_at_mut(4u32.wrapping_mul(i) as usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i, qj, c1, res_i0.1);
      let a_i0: u64 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i0, qj, c1, res_i1.1);
      let a_i1: u64 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i1, qj, c1, res_i2.1);
      let a_i2: u64 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i2, qj, c1, res_i.1)
    };
    let r: u64 = c1;
    let c10: u64 = r;
    let res_j: u64 = c[64u32.wrapping_add(i0) as usize];
    let resb: (&mut [u64], &mut [u64]) = c.split_at_mut(i0 as usize + 64usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, c10, res_j, resb.1)
  };
  (res[0usize..64usize]).copy_from_slice(&(&c[64usize..])[0usize..64usize]);
  let c00: u64 = c0;
  let mut tmp: [u64; 64] = [0u64; 64usize];
  let mut c1: u64 = 0u64;
  for i in 0u32..16u32
  {
    let t1: u64 = res[4u32.wrapping_mul(i) as usize];
    let t20: u64 = n[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(4u32.wrapping_mul(i) as usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c1, t1, t20, res_i0.1);
    let t10: u64 = res[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u64 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c1, t10, t21, res_i1.1);
    let t11: u64 = res[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u64 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c1, t11, t22, res_i2.1);
    let t12: u64 = res[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u64 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c1, t12, t2, res_i.1)
  };
  let c10: u64 = c1;
  let c2: u64 = c00.wrapping_sub(c10);
  for i in 0u32..64u32
  {
    let x: u64 = c2 & res[i as usize] | ! c2 & tmp[i as usize];
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

#[inline] pub fn to(n: &[u64], nInv: u64, r2: &[u64], a: &[u64], aM: &mut [u64])
{
  let mut c: [u64; 128] = [0u64; 128usize];
  Hacl_Bignum4096_mul(a, r2, &mut c);
  reduction(n, nInv, &mut c, aM)
}

#[inline] pub fn from(n: &[u64], nInv_u64: u64, aM: &[u64], a: &mut [u64])
{
  let mut tmp: [u64; 128] = [0u64; 128usize];
  (tmp[0usize..64usize]).copy_from_slice(&aM[0usize..64usize]);
  reduction(n, nInv_u64, &mut tmp, a)
}

#[inline] pub fn areduction(n: &[u64], nInv: u64, c: &mut [u64], res: &mut [u64])
{
  let mut c0: u64 = 0u64;
  for i0 in 0u32..64u32
  {
    let qj: u64 = nInv.wrapping_mul(c[i0 as usize]);
    let res_j0: (&mut [u64], &mut [u64]) = c.split_at_mut(i0 as usize);
    let mut c1: u64 = 0u64;
    for i in 0u32..16u32
    {
      let a_i: u64 = n[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u64], &mut [u64]) = res_j0.1.split_at_mut(4u32.wrapping_mul(i) as usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i, qj, c1, res_i0.1);
      let a_i0: u64 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i0, qj, c1, res_i1.1);
      let a_i1: u64 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i1, qj, c1, res_i2.1);
      let a_i2: u64 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i2, qj, c1, res_i.1)
    };
    let r: u64 = c1;
    let c10: u64 = r;
    let res_j: u64 = c[64u32.wrapping_add(i0) as usize];
    let resb: (&mut [u64], &mut [u64]) = c.split_at_mut(i0 as usize + 64usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, c10, res_j, resb.1)
  };
  (res[0usize..64usize]).copy_from_slice(&(&c[64usize..])[0usize..64usize]);
  let c00: u64 = c0;
  let mut tmp: [u64; 64] = [0u64; 64usize];
  let c1: u64 = Hacl_Bignum4096_sub(res, n, &mut tmp);
  crate::lowstar::ignore::ignore::<u64>(c1);
  let m: u64 = 0u64.wrapping_sub(c00);
  for i in 0u32..64u32
  {
    let x: u64 = m & tmp[i as usize] | ! m & res[i as usize];
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

#[inline] pub fn amont_mul(n: &[u64], nInv_u64: u64, aM: &[u64], bM: &[u64], resM: &mut [u64])
{
  let mut c: [u64; 128] = [0u64; 128usize];
  Hacl_Bignum4096_mul(aM, bM, &mut c);
  areduction(n, nInv_u64, &mut c, resM)
}

#[inline] pub fn amont_sqr(n: &[u64], nInv_u64: u64, aM: &[u64], resM: &mut [u64])
{
  let mut c: [u64; 128] = [0u64; 128usize];
  Hacl_Bignum4096_sqr(aM, &mut c);
  areduction(n, nInv_u64, &mut c, resM)
}

#[inline] pub fn bn_slow_precomp(n: &[u64], mu: u64, r2: &[u64], a: &[u64], res: &mut [u64])
{
  let mut a_mod: [u64; 64] = [0u64; 64usize];
  let mut a1: [u64; 128] = [0u64; 128usize];
  (a1[0usize..128usize]).copy_from_slice(&a[0usize..128usize]);
  areduction(n, mu, &mut a1, &mut a_mod);
  to(n, mu, r2, &a_mod, res)
}

pub fn Hacl_Bignum4096_mod(n: &[u64], a: &[u64], res: &mut [u64]) -> bool
{
  let mut one: [u64; 64] = [0u64; 64usize];
  (&mut one)[0usize] = 1u64;
  let bit0: u64 = n[0usize] & 1u64;
  let m0: u64 = 0u64.wrapping_sub(bit0);
  let mut acc: u64 = 0u64;
  for i in 0u32..64u32
  {
    let beq: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask((&one)[i as usize], n[i as usize]);
    let blt: u64 = ! crate::hacl_krmllib::FStar_UInt64_gte_mask((&one)[i as usize], n[i as usize]);
    acc = beq & acc | ! beq & blt
  };
  let m1: u64 = acc;
  let is_valid_m: u64 = m0 & m1;
  let nBits: u32 =
      64u32.wrapping_mul(
        crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_top_index_u64(64u32, n) as u32
      );
  if is_valid_m == 18446744073709551615u64
  {
    let mut r2: [u64; 64] = [0u64; 64usize];
    precompr2(nBits, n, &mut r2);
    let mu: u64 = crate::hacl::bignum::Hacl_Bignum_ModInvLimb_mod_inv_uint64(n[0usize]);
    bn_slow_precomp(n, mu, &r2, a, res)
  }
  else
  { (res[0usize..64usize]).copy_from_slice(&[0u64; 64usize]) };
  return is_valid_m == 18446744073709551615u64
}

pub fn exp_check(n: &[u64], a: &[u64], bBits: u32, b: &[u64]) -> u64
{
  let mut one: [u64; 64] = [0u64; 64usize];
  (&mut one)[0usize] = 1u64;
  let bit0: u64 = n[0usize] & 1u64;
  let m0: u64 = 0u64.wrapping_sub(bit0);
  let mut acc0: u64 = 0u64;
  for i in 0u32..64u32
  {
    let beq: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask((&one)[i as usize], n[i as usize]);
    let blt: u64 = ! crate::hacl_krmllib::FStar_UInt64_gte_mask((&one)[i as usize], n[i as usize]);
    acc0 = beq & acc0 | ! beq & blt
  };
  let m10: u64 = acc0;
  let m00: u64 = m0 & m10;
  let mut bLen: u32;
  if bBits == 0u32
  { bLen = 1u32 }
  else
  { bLen = bBits.wrapping_sub(1u32).wrapping_div(64u32).wrapping_add(1u32) };
  let mut m1: u64;
  if bBits < 64u32.wrapping_mul(bLen)
  {
    let mut b2: Box<[u64]> = vec![0u64; bLen as usize].into_boxed_slice();
    let i0: u32 = bBits.wrapping_div(64u32);
    let j: u32 = bBits.wrapping_rem(64u32);
    (&mut b2)[i0 as usize] = (&b2)[i0 as usize] | 1u64.wrapping_shl(j);
    let mut acc: u64 = 0u64;
    for i in 0u32..bLen
    {
      let beq: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(b[i as usize], (&b2)[i as usize]);
      let blt: u64 = ! crate::hacl_krmllib::FStar_UInt64_gte_mask(b[i as usize], (&b2)[i as usize]);
      acc = beq & acc | ! beq & blt
    };
    let res: u64 = acc;
    m1 = res
  }
  else
  { m1 = 18446744073709551615u64 };
  let mut acc: u64 = 0u64;
  for i in 0u32..64u32
  {
    let beq: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(a[i as usize], n[i as usize]);
    let blt: u64 = ! crate::hacl_krmllib::FStar_UInt64_gte_mask(a[i as usize], n[i as usize]);
    acc = beq & acc | ! beq & blt
  };
  let m2: u64 = acc;
  let m: u64 = m1 & m2;
  return m00 & m
}

#[inline] pub fn exp_vartime_precomp(
  n: &[u64],
  mu: u64,
  r2: &[u64],
  a: &[u64],
  bBits: u32,
  b: &[u64],
  res: &mut [u64]
)
{
  if bBits < 200u32
  {
    let mut aM: [u64; 64] = [0u64; 64usize];
    to(n, mu, r2, a, &mut aM);
    let mut resM: [u64; 64] = [0u64; 64usize];
    let mut ctx: [u64; 128] = [0u64; 128usize];
    (ctx[0usize..64usize]).copy_from_slice(&n[0usize..64usize]);
    ((&mut ctx[64usize..])[0usize..64usize]).copy_from_slice(&r2[0usize..64usize]);
    let ctx_n0: (&[u64], &[u64]) = ctx.split_at(0usize);
    let ctx_r2: (&[u64], &[u64]) = ctx_n0.1.split_at(64usize);
    from(ctx_r2.0, mu, ctx_r2.1, &mut resM);
    crate::lowstar::ignore::ignore::<[u64; 128]>(ctx);
    for i in 0u32..bBits
    {
      let i1: u32 = i.wrapping_div(64u32);
      let j: u32 = i.wrapping_rem(64u32);
      let tmp: u64 = b[i1 as usize];
      let bit: u64 = tmp.wrapping_shr(j) & 1u64;
      if bit != 0u64
      {
        let mut aM_copy: [u64; 64] = [0u64; 64usize];
        (aM_copy[0usize..64usize]).copy_from_slice(&resM[0usize..64usize]);
        let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
        amont_mul(ctx_n.1, mu, &aM_copy, &aM, &mut resM);
        crate::lowstar::ignore::ignore::<[u64; 128]>(ctx)
      };
      let mut aM_copy: [u64; 64] = [0u64; 64usize];
      (aM_copy[0usize..64usize]).copy_from_slice(&aM[0usize..64usize]);
      let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
      amont_sqr(ctx_n.1, mu, &aM_copy, &mut aM);
      crate::lowstar::ignore::ignore::<[u64; 128]>(ctx)
    };
    from(n, mu, &resM, res);
    return ()
  };
  let mut aM: [u64; 64] = [0u64; 64usize];
  to(n, mu, r2, a, &mut aM);
  let mut resM: [u64; 64] = [0u64; 64usize];
  let mut bLen: u32;
  if bBits == 0u32
  { bLen = 1u32 }
  else
  { bLen = bBits.wrapping_sub(1u32).wrapping_div(64u32).wrapping_add(1u32) };
  let mut ctx: [u64; 128] = [0u64; 128usize];
  (ctx[0usize..64usize]).copy_from_slice(&n[0usize..64usize]);
  ((&mut ctx[64usize..])[0usize..64usize]).copy_from_slice(&r2[0usize..64usize]);
  let mut table: [u64; 1024] = [0u64; 1024usize];
  let mut tmp: [u64; 64] = [0u64; 64usize];
  let t0: (&mut [u64], &mut [u64]) = table.split_at_mut(0usize);
  let t1: (&mut [u64], &mut [u64]) = t0.1.split_at_mut(64usize);
  let ctx_n0: (&[u64], &[u64]) = ctx.split_at(0usize);
  let ctx_r20: (&[u64], &[u64]) = ctx_n0.1.split_at(64usize);
  from(ctx_r20.0, mu, ctx_r20.1, t1.0);
  crate::lowstar::ignore::ignore::<[u64; 128]>(ctx);
  (t1.1[0usize..64usize]).copy_from_slice(&aM[0usize..64usize]);
  crate::lowstar::ignore::ignore::<[u64; 1024]>(table);
  for i in 0u32..7u32
  {
    let t11: (&[u64], &[u64]) = table.split_at(i.wrapping_add(1u32).wrapping_mul(64u32) as usize);
    let mut aM_copy0: [u64; 64] = [0u64; 64usize];
    (aM_copy0[0usize..64usize]).copy_from_slice(&t11.1[0usize..64usize]);
    let ctx_n1: (&[u64], &[u64]) = ctx.split_at(0usize);
    amont_sqr(ctx_n1.1, mu, &aM_copy0, &mut tmp);
    crate::lowstar::ignore::ignore::<[u64; 128]>(ctx);
    ((&mut table[2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(64u32) as usize..])[0usize..64usize]).copy_from_slice(
      &tmp[0usize..64usize]
    );
    let t2: (&[u64], &[u64]) =
        table.split_at(2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(64u32) as usize);
    let mut aM_copy: [u64; 64] = [0u64; 64usize];
    (aM_copy[0usize..64usize]).copy_from_slice(&aM[0usize..64usize]);
    let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
    amont_mul(ctx_n.1, mu, &aM_copy, t2.1, &mut tmp);
    crate::lowstar::ignore::ignore::<[u64; 128]>(ctx);
    ((&mut table[2u32.wrapping_mul(i).wrapping_add(3u32).wrapping_mul(64u32) as usize..])[0usize..64usize]).copy_from_slice(
      &tmp[0usize..64usize]
    )
  };
  if bBits.wrapping_rem(4u32) != 0u32
  {
    let i: u32 = bBits.wrapping_div(4u32).wrapping_mul(4u32);
    let bits_c: u64 = crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(bLen, b, i, 4u32);
    let bits_l32: u32 = bits_c as u32;
    let a_bits_l: (&[u64], &[u64]) = table.split_at(bits_l32.wrapping_mul(64u32) as usize);
    (resM[0usize..64usize]).copy_from_slice(&a_bits_l.1[0usize..64usize])
  }
  else
  {
    let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
    let ctx_r2: (&[u64], &[u64]) = ctx_n.1.split_at(64usize);
    from(ctx_r2.0, mu, ctx_r2.1, &mut resM);
    crate::lowstar::ignore::ignore::<[u64; 128]>(ctx)
  };
  let mut tmp0: [u64; 64] = [0u64; 64usize];
  for i in 0u32..bBits.wrapping_div(4u32)
  {
    for i0 in 0u32..4u32
    {
      let mut aM_copy: [u64; 64] = [0u64; 64usize];
      (aM_copy[0usize..64usize]).copy_from_slice(&resM[0usize..64usize]);
      let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
      amont_sqr(ctx_n.1, mu, &aM_copy, &mut resM);
      crate::lowstar::ignore::ignore::<[u64; 128]>(ctx)
    };
    let k: u32 =
        bBits.wrapping_sub(bBits.wrapping_rem(4u32)).wrapping_sub(4u32.wrapping_mul(i)).wrapping_sub(
          4u32
        );
    let bits_l: u64 = crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(bLen, b, k, 4u32);
    crate::lowstar::ignore::ignore::<[u64; 1024]>(table);
    let bits_l32: u32 = bits_l as u32;
    let a_bits_l: (&[u64], &[u64]) = table.split_at(bits_l32.wrapping_mul(64u32) as usize);
    (tmp0[0usize..64usize]).copy_from_slice(&a_bits_l.1[0usize..64usize]);
    let mut aM_copy: [u64; 64] = [0u64; 64usize];
    (aM_copy[0usize..64usize]).copy_from_slice(&resM[0usize..64usize]);
    let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
    amont_mul(ctx_n.1, mu, &aM_copy, &tmp0, &mut resM);
    crate::lowstar::ignore::ignore::<[u64; 128]>(ctx)
  };
  from(n, mu, &resM, res)
}

#[inline] pub fn exp_consttime_precomp(
  n: &[u64],
  mu: u64,
  r2: &[u64],
  a: &[u64],
  bBits: u32,
  b: &[u64],
  res: &mut [u64]
)
{
  if bBits < 200u32
  {
    let mut aM: [u64; 64] = [0u64; 64usize];
    to(n, mu, r2, a, &mut aM);
    let mut resM: [u64; 64] = [0u64; 64usize];
    let mut ctx: [u64; 128] = [0u64; 128usize];
    (ctx[0usize..64usize]).copy_from_slice(&n[0usize..64usize]);
    ((&mut ctx[64usize..])[0usize..64usize]).copy_from_slice(&r2[0usize..64usize]);
    let mut sw: u64 = 0u64;
    let ctx_n0: (&[u64], &[u64]) = ctx.split_at(0usize);
    let ctx_r2: (&[u64], &[u64]) = ctx_n0.1.split_at(64usize);
    from(ctx_r2.0, mu, ctx_r2.1, &mut resM);
    crate::lowstar::ignore::ignore::<[u64; 128]>(ctx);
    for i0 in 0u32..bBits
    {
      let i1: u32 = bBits.wrapping_sub(i0).wrapping_sub(1u32).wrapping_div(64u32);
      let j: u32 = bBits.wrapping_sub(i0).wrapping_sub(1u32).wrapping_rem(64u32);
      let tmp: u64 = b[i1 as usize];
      let bit: u64 = tmp.wrapping_shr(j) & 1u64;
      let sw1: u64 = bit ^ sw;
      for i in 0u32..64u32
      {
        let dummy: u64 = 0u64.wrapping_sub(sw1) & (resM[i as usize] ^ aM[i as usize]);
        resM[i as usize] ^= dummy;
        aM[i as usize] ^= dummy
      };
      let mut aM_copy: [u64; 64] = [0u64; 64usize];
      (aM_copy[0usize..64usize]).copy_from_slice(&aM[0usize..64usize]);
      let ctx_n1: (&[u64], &[u64]) = ctx.split_at(0usize);
      amont_mul(ctx_n1.1, mu, &aM_copy, &resM, &mut aM);
      crate::lowstar::ignore::ignore::<[u64; 128]>(ctx);
      let mut aM_copy0: [u64; 64] = [0u64; 64usize];
      (aM_copy0[0usize..64usize]).copy_from_slice(&resM[0usize..64usize]);
      let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
      amont_sqr(ctx_n.1, mu, &aM_copy0, &mut resM);
      crate::lowstar::ignore::ignore::<[u64; 128]>(ctx);
      sw = bit
    };
    let sw0: u64 = sw;
    for i in 0u32..64u32
    {
      let dummy: u64 = 0u64.wrapping_sub(sw0) & (resM[i as usize] ^ aM[i as usize]);
      resM[i as usize] ^= dummy;
      aM[i as usize] ^= dummy
    };
    from(n, mu, &resM, res);
    return ()
  };
  let mut aM: [u64; 64] = [0u64; 64usize];
  to(n, mu, r2, a, &mut aM);
  let mut resM: [u64; 64] = [0u64; 64usize];
  let mut bLen: u32;
  if bBits == 0u32
  { bLen = 1u32 }
  else
  { bLen = bBits.wrapping_sub(1u32).wrapping_div(64u32).wrapping_add(1u32) };
  let mut ctx: [u64; 128] = [0u64; 128usize];
  (ctx[0usize..64usize]).copy_from_slice(&n[0usize..64usize]);
  ((&mut ctx[64usize..])[0usize..64usize]).copy_from_slice(&r2[0usize..64usize]);
  let mut table: [u64; 1024] = [0u64; 1024usize];
  let mut tmp: [u64; 64] = [0u64; 64usize];
  let t0: (&mut [u64], &mut [u64]) = table.split_at_mut(0usize);
  let t1: (&mut [u64], &mut [u64]) = t0.1.split_at_mut(64usize);
  let ctx_n0: (&[u64], &[u64]) = ctx.split_at(0usize);
  let ctx_r20: (&[u64], &[u64]) = ctx_n0.1.split_at(64usize);
  from(ctx_r20.0, mu, ctx_r20.1, t1.0);
  crate::lowstar::ignore::ignore::<[u64; 128]>(ctx);
  (t1.1[0usize..64usize]).copy_from_slice(&aM[0usize..64usize]);
  crate::lowstar::ignore::ignore::<[u64; 1024]>(table);
  for i in 0u32..7u32
  {
    let t11: (&[u64], &[u64]) = table.split_at(i.wrapping_add(1u32).wrapping_mul(64u32) as usize);
    let mut aM_copy0: [u64; 64] = [0u64; 64usize];
    (aM_copy0[0usize..64usize]).copy_from_slice(&t11.1[0usize..64usize]);
    let ctx_n1: (&[u64], &[u64]) = ctx.split_at(0usize);
    amont_sqr(ctx_n1.1, mu, &aM_copy0, &mut tmp);
    crate::lowstar::ignore::ignore::<[u64; 128]>(ctx);
    ((&mut table[2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(64u32) as usize..])[0usize..64usize]).copy_from_slice(
      &tmp[0usize..64usize]
    );
    let t2: (&[u64], &[u64]) =
        table.split_at(2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(64u32) as usize);
    let mut aM_copy: [u64; 64] = [0u64; 64usize];
    (aM_copy[0usize..64usize]).copy_from_slice(&aM[0usize..64usize]);
    let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
    amont_mul(ctx_n.1, mu, &aM_copy, t2.1, &mut tmp);
    crate::lowstar::ignore::ignore::<[u64; 128]>(ctx);
    ((&mut table[2u32.wrapping_mul(i).wrapping_add(3u32).wrapping_mul(64u32) as usize..])[0usize..64usize]).copy_from_slice(
      &tmp[0usize..64usize]
    )
  };
  if bBits.wrapping_rem(4u32) != 0u32
  {
    let i0: u32 = bBits.wrapping_div(4u32).wrapping_mul(4u32);
    let bits_c: u64 = crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(bLen, b, i0, 4u32);
    (resM[0usize..64usize]).copy_from_slice(&(&table)[0usize..64usize]);
    for i1 in 0u32..15u32
    {
      let c: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(bits_c, i1.wrapping_add(1u32) as u64);
      let res_j: (&[u64], &[u64]) =
          table.split_at(i1.wrapping_add(1u32).wrapping_mul(64u32) as usize);
      for i in 0u32..64u32
      {
        let x: u64 = c & res_j.1[i as usize] | ! c & resM[i as usize];
        let os: (&mut [u64], &mut [u64]) = resM.split_at_mut(0usize);
        os.1[i as usize] = x
      }
    }
  }
  else
  {
    let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
    let ctx_r2: (&[u64], &[u64]) = ctx_n.1.split_at(64usize);
    from(ctx_r2.0, mu, ctx_r2.1, &mut resM);
    crate::lowstar::ignore::ignore::<[u64; 128]>(ctx)
  };
  let mut tmp0: [u64; 64] = [0u64; 64usize];
  for i0 in 0u32..bBits.wrapping_div(4u32)
  {
    for i in 0u32..4u32
    {
      let mut aM_copy: [u64; 64] = [0u64; 64usize];
      (aM_copy[0usize..64usize]).copy_from_slice(&resM[0usize..64usize]);
      let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
      amont_sqr(ctx_n.1, mu, &aM_copy, &mut resM);
      crate::lowstar::ignore::ignore::<[u64; 128]>(ctx)
    };
    let k: u32 =
        bBits.wrapping_sub(bBits.wrapping_rem(4u32)).wrapping_sub(4u32.wrapping_mul(i0)).wrapping_sub(
          4u32
        );
    let bits_l: u64 = crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(bLen, b, k, 4u32);
    crate::lowstar::ignore::ignore::<[u64; 1024]>(table);
    (tmp0[0usize..64usize]).copy_from_slice(&(&table)[0usize..64usize]);
    for i1 in 0u32..15u32
    {
      let c: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(bits_l, i1.wrapping_add(1u32) as u64);
      let res_j: (&[u64], &[u64]) =
          table.split_at(i1.wrapping_add(1u32).wrapping_mul(64u32) as usize);
      for i in 0u32..64u32
      {
        let x: u64 = c & res_j.1[i as usize] | ! c & tmp0[i as usize];
        let os: (&mut [u64], &mut [u64]) = tmp0.split_at_mut(0usize);
        os.1[i as usize] = x
      }
    };
    let mut aM_copy: [u64; 64] = [0u64; 64usize];
    (aM_copy[0usize..64usize]).copy_from_slice(&resM[0usize..64usize]);
    let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
    amont_mul(ctx_n.1, mu, &aM_copy, &tmp0, &mut resM);
    crate::lowstar::ignore::ignore::<[u64; 128]>(ctx)
  };
  from(n, mu, &resM, res)
}

#[inline] pub fn exp_vartime(
  nBits: u32,
  n: &[u64],
  a: &[u64],
  bBits: u32,
  b: &[u64],
  res: &mut [u64]
)
{
  let mut r2: [u64; 64] = [0u64; 64usize];
  precompr2(nBits, n, &mut r2);
  let mu: u64 = crate::hacl::bignum::Hacl_Bignum_ModInvLimb_mod_inv_uint64(n[0usize]);
  exp_vartime_precomp(n, mu, &r2, a, bBits, b, res)
}

#[inline] pub fn exp_consttime(
  nBits: u32,
  n: &[u64],
  a: &[u64],
  bBits: u32,
  b: &[u64],
  res: &mut [u64]
)
{
  let mut r2: [u64; 64] = [0u64; 64usize];
  precompr2(nBits, n, &mut r2);
  let mu: u64 = crate::hacl::bignum::Hacl_Bignum_ModInvLimb_mod_inv_uint64(n[0usize]);
  exp_consttime_precomp(n, mu, &r2, a, bBits, b, res)
}

pub fn Hacl_Bignum4096_mod_exp_vartime(
  n: &[u64],
  a: &[u64],
  bBits: u32,
  b: &[u64],
  res: &mut [u64]
) ->
    bool
{
  let is_valid_m: u64 = exp_check(n, a, bBits, b);
  let nBits: u32 =
      64u32.wrapping_mul(
        crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_top_index_u64(64u32, n) as u32
      );
  if is_valid_m == 18446744073709551615u64
  { exp_vartime(nBits, n, a, bBits, b, res) }
  else
  { (res[0usize..64usize]).copy_from_slice(&[0u64; 64usize]) };
  return is_valid_m == 18446744073709551615u64
}

pub fn Hacl_Bignum4096_mod_exp_consttime(
  n: &[u64],
  a: &[u64],
  bBits: u32,
  b: &[u64],
  res: &mut [u64]
) ->
    bool
{
  let is_valid_m: u64 = exp_check(n, a, bBits, b);
  let nBits: u32 =
      64u32.wrapping_mul(
        crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_top_index_u64(64u32, n) as u32
      );
  if is_valid_m == 18446744073709551615u64
  { exp_consttime(nBits, n, a, bBits, b, res) }
  else
  { (res[0usize..64usize]).copy_from_slice(&[0u64; 64usize]) };
  return is_valid_m == 18446744073709551615u64
}

pub fn Hacl_Bignum4096_mod_inv_prime_vartime(n: &[u64], a: &[u64], res: &mut [u64]) -> bool
{
  let mut one: [u64; 64] = [0u64; 64usize];
  (&mut one)[0usize] = 1u64;
  let bit0: u64 = n[0usize] & 1u64;
  let m0: u64 = 0u64.wrapping_sub(bit0);
  let mut acc0: u64 = 0u64;
  for i in 0u32..64u32
  {
    let beq: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask((&one)[i as usize], n[i as usize]);
    let blt: u64 = ! crate::hacl_krmllib::FStar_UInt64_gte_mask((&one)[i as usize], n[i as usize]);
    acc0 = beq & acc0 | ! beq & blt
  };
  let m1: u64 = acc0;
  let m00: u64 = m0 & m1;
  let bn_zero: [u64; 64] = [0u64; 64usize];
  let mut mask: u64 = 18446744073709551615u64;
  for i in 0u32..64u32
  {
    let uu____0: u64 =
        crate::hacl_krmllib::FStar_UInt64_eq_mask(a[i as usize], bn_zero[i as usize]);
    mask = uu____0 & mask
  };
  let mask1: u64 = mask;
  let res10: u64 = mask1;
  let m10: u64 = res10;
  let mut acc: u64 = 0u64;
  for i in 0u32..64u32
  {
    let beq: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(a[i as usize], n[i as usize]);
    let blt: u64 = ! crate::hacl_krmllib::FStar_UInt64_gte_mask(a[i as usize], n[i as usize]);
    acc = beq & acc | ! beq & blt
  };
  let m2: u64 = acc;
  let is_valid_m: u64 = m00 & ! m10 & m2;
  let nBits: u32 =
      64u32.wrapping_mul(
        crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_top_index_u64(64u32, n) as u32
      );
  if is_valid_m == 18446744073709551615u64
  {
    let mut n2: [u64; 64] = [0u64; 64usize];
    let c0: u64 =
        crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(
          0u64,
          n[0usize],
          2u64,
          &mut n2
        );
    let a1: (&[u64], &[u64]) = n.split_at(1usize);
    let res1: (&mut [u64], &mut [u64]) = n2.split_at_mut(1usize);
    let mut c: u64 = c0;
    for i in 0u32..15u32
    {
      let t1: u64 = a1.1[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u64], &mut [u64]) = res1.1.split_at_mut(4u32.wrapping_mul(i) as usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, 0u64, res_i0.1);
      let t10: u64 = a1.1[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t10, 0u64, res_i1.1);
      let t11: u64 = a1.1[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t11, 0u64, res_i2.1);
      let t12: u64 = a1.1[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t12, 0u64, res_i.1)
    };
    for i in 60u32..63u32
    {
      let t1: u64 = a1.1[i as usize];
      let res_i: (&mut [u64], &mut [u64]) = res1.1.split_at_mut(i as usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, 0u64, res_i.1)
    };
    let c1: u64 = c;
    let c2: u64 = c1;
    crate::lowstar::ignore::ignore::<u64>(c2);
    exp_vartime(nBits, n, a, 4096u32, &n2, res)
  }
  else
  { (res[0usize..64usize]).copy_from_slice(&[0u64; 64usize]) };
  return is_valid_m == 18446744073709551615u64
}

pub fn Hacl_Bignum4096_mont_ctx_init(n: &[u64]) ->
    Box<[crate::hacl::bignum::Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64]>
{
  let mut r2: Box<[u64]> = vec![0u64; 64usize].into_boxed_slice();
  let mut n1: Box<[u64]> = vec![0u64; 64usize].into_boxed_slice();
  let r21: (&mut [u64], &mut [u64]) = r2.split_at_mut(0usize);
  let n11: (&mut [u64], &mut [u64]) = n1.split_at_mut(0usize);
  (n11.1[0usize..64usize]).copy_from_slice(&n[0usize..64usize]);
  let nBits: u32 =
      64u32.wrapping_mul(
        crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_top_index_u64(64u32, n) as u32
      );
  precompr2(nBits, n, r21.1);
  let mu: u64 = crate::hacl::bignum::Hacl_Bignum_ModInvLimb_mod_inv_uint64(n[0usize]);
  let res: crate::hacl::bignum::Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64 =
      crate::hacl::bignum::Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64
      { len: 64u32, n: (*n11.1).into(), mu, r2: (*r21.1).into() };
  let buf: Box<[crate::hacl::bignum::Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64]> =
      vec![res].into_boxed_slice();
  return buf
}

pub fn Hacl_Bignum4096_mont_ctx_free(
  k: &[crate::hacl::bignum::Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64]
)
{
  let uu____0: &crate::hacl::bignum::Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64 = &k[0usize];
  let n: &[u64] = &uu____0.n;
  let r2: &[u64] = &uu____0.r2;
  ()
}

pub fn Hacl_Bignum4096_mod_precomp(
  k: &[crate::hacl::bignum::Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64],
  a: &[u64],
  res: &mut [u64]
)
{
  let n: &[u64] = &(k[0usize]).n;
  let mu: u64 = (k[0usize]).mu;
  let r2: &[u64] = &(k[0usize]).r2;
  bn_slow_precomp(n, mu, r2, a, res)
}

pub fn Hacl_Bignum4096_mod_exp_vartime_precomp(
  k: &[crate::hacl::bignum::Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64],
  a: &[u64],
  bBits: u32,
  b: &[u64],
  res: &mut [u64]
)
{
  let n: &[u64] = &(k[0usize]).n;
  let mu: u64 = (k[0usize]).mu;
  let r2: &[u64] = &(k[0usize]).r2;
  exp_vartime_precomp(n, mu, r2, a, bBits, b, res)
}

pub fn Hacl_Bignum4096_mod_exp_consttime_precomp(
  k: &[crate::hacl::bignum::Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64],
  a: &[u64],
  bBits: u32,
  b: &[u64],
  res: &mut [u64]
)
{
  let n: &[u64] = &(k[0usize]).n;
  let mu: u64 = (k[0usize]).mu;
  let r2: &[u64] = &(k[0usize]).r2;
  exp_consttime_precomp(n, mu, r2, a, bBits, b, res)
}

pub fn Hacl_Bignum4096_mod_inv_prime_vartime_precomp(
  k: &[crate::hacl::bignum::Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64],
  a: &[u64],
  res: &mut [u64]
)
{
  let n: &[u64] = &(k[0usize]).n;
  let mu: u64 = (k[0usize]).mu;
  let r2: &[u64] = &(k[0usize]).r2;
  let mut n2: [u64; 64] = [0u64; 64usize];
  let c0: u64 =
      crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(0u64, n[0usize], 2u64, &mut n2);
  let a1: (&[u64], &[u64]) = n.split_at(1usize);
  let res1: (&mut [u64], &mut [u64]) = n2.split_at_mut(1usize);
  let mut c: u64 = c0;
  for i in 0u32..15u32
  {
    let t1: u64 = a1.1[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = res1.1.split_at_mut(4u32.wrapping_mul(i) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, 0u64, res_i0.1);
    let t10: u64 = a1.1[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = res_i0.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t10, 0u64, res_i1.1);
    let t11: u64 = a1.1[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = res_i1.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t11, 0u64, res_i2.1);
    let t12: u64 = a1.1[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = res_i2.1.split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t12, 0u64, res_i.1)
  };
  for i in 60u32..63u32
  {
    let t1: u64 = a1.1[i as usize];
    let res_i: (&mut [u64], &mut [u64]) = res1.1.split_at_mut(i as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, 0u64, res_i.1)
  };
  let c1: u64 = c;
  let c2: u64 = c1;
  crate::lowstar::ignore::ignore::<u64>(c2);
  exp_vartime_precomp(n, mu, r2, a, 4096u32, &n2, res)
}

pub fn Hacl_Bignum4096_new_bn_from_bytes_be(len: u32, b: &[u8]) -> Box<[u64]>
{
  if len == 0u32 || len.wrapping_sub(1u32).wrapping_div(8u32).wrapping_add(1u32) > 536870911u32
  { return [].into() };
  let mut res: Box<[u64]> =
      vec![0u64; len.wrapping_sub(1u32).wrapping_div(8u32).wrapping_add(1u32) as usize].into_boxed_slice(

      );
  let res1: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
  let res2: (&mut [u64], &mut [u64]) = res1.1.split_at_mut(0usize);
  let bnLen: u32 = len.wrapping_sub(1u32).wrapping_div(8u32).wrapping_add(1u32);
  let tmpLen: u32 = 8u32.wrapping_mul(bnLen);
  let mut tmp: Box<[u8]> = vec![0u8; tmpLen as usize].into_boxed_slice();
  ((&mut (&mut tmp)[tmpLen.wrapping_sub(len) as usize..])[0usize..len as usize]).copy_from_slice(
    &b[0usize..len as usize]
  );
  for i in 0u32..bnLen
  {
    let u: u64 =
        crate::lowstar_endianness::load64_be(
          &(&tmp)[bnLen.wrapping_sub(i).wrapping_sub(1u32).wrapping_mul(8u32) as usize..]
        );
    let x: u64 = u;
    let os: (&mut [u64], &mut [u64]) = res2.1.split_at_mut(0usize);
    os.1[i as usize] = x
  };
  return (*res2.1).into()
}

pub fn Hacl_Bignum4096_new_bn_from_bytes_le(len: u32, b: &[u8]) -> Box<[u64]>
{
  if len == 0u32 || len.wrapping_sub(1u32).wrapping_div(8u32).wrapping_add(1u32) > 536870911u32
  { return [].into() };
  let mut res: Box<[u64]> =
      vec![0u64; len.wrapping_sub(1u32).wrapping_div(8u32).wrapping_add(1u32) as usize].into_boxed_slice(

      );
  let res1: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
  let res2: (&mut [u64], &mut [u64]) = res1.1.split_at_mut(0usize);
  let bnLen: u32 = len.wrapping_sub(1u32).wrapping_div(8u32).wrapping_add(1u32);
  let tmpLen: u32 = 8u32.wrapping_mul(bnLen);
  let mut tmp: Box<[u8]> = vec![0u8; tmpLen as usize].into_boxed_slice();
  ((&mut tmp)[0usize..len as usize]).copy_from_slice(&b[0usize..len as usize]);
  for i in 0u32..len.wrapping_sub(1u32).wrapping_div(8u32).wrapping_add(1u32)
  {
    let bj: (&[u8], &[u8]) = tmp.split_at(i.wrapping_mul(8u32) as usize);
    let u: u64 = crate::lowstar_endianness::load64_le(bj.1);
    let r1: u64 = u;
    let x: u64 = r1;
    let os: (&mut [u64], &mut [u64]) = res2.1.split_at_mut(0usize);
    os.1[i as usize] = x
  };
  return (*res2.1).into()
}

pub fn Hacl_Bignum4096_bn_to_bytes_be(b: &[u64], res: &mut [u8])
{
  let tmp: [u8; 512] = [0u8; 512usize];
  crate::lowstar::ignore::ignore::<[u8; 512]>(tmp);
  for i in 0u32..64u32
  {
    crate::lowstar_endianness::store64_be(
      &mut res[i.wrapping_mul(8u32) as usize..],
      b[64u32.wrapping_sub(i).wrapping_sub(1u32) as usize]
    )
  }
}

pub fn Hacl_Bignum4096_bn_to_bytes_le(b: &[u64], res: &mut [u8])
{
  let tmp: [u8; 512] = [0u8; 512usize];
  crate::lowstar::ignore::ignore::<[u8; 512]>(tmp);
  for i in 0u32..64u32
  {
    crate::lowstar_endianness::store64_le(&mut res[i.wrapping_mul(8u32) as usize..], b[i as usize])
  }
}

pub fn Hacl_Bignum4096_lt_mask(a: &[u64], b: &[u64]) -> u64
{
  let mut acc: u64 = 0u64;
  for i in 0u32..64u32
  {
    let beq: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(a[i as usize], b[i as usize]);
    let blt: u64 = ! crate::hacl_krmllib::FStar_UInt64_gte_mask(a[i as usize], b[i as usize]);
    acc = beq & acc | ! beq & blt
  };
  return acc
}

pub fn Hacl_Bignum4096_eq_mask(a: &[u64], b: &[u64]) -> u64
{
  let mut mask: u64 = 18446744073709551615u64;
  for i in 0u32..64u32
  {
    let uu____0: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(a[i as usize], b[i as usize]);
    mask = uu____0 & mask
  };
  let mask1: u64 = mask;
  return mask1
}
