#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

pub fn Hacl_Bignum_AlmostMontgomery_bn_almost_mont_reduction_u32(
  len: u32,
  n: &[u32],
  nInv: u32,
  c: &mut [u32],
  res: &mut [u32]
)
{
  let mut c0: u32 = 0u32;
  for i0 in 0u32..len
  {
    let qj: u32 = nInv.wrapping_mul(c[i0 as usize]);
    let res_j0: (&mut [u32], &mut [u32]) = c.split_at_mut(i0 as usize);
    let mut c1: u32 = 0u32;
    for i in 0u32..len.wrapping_div(4u32)
    {
      let a_i: u32 = n[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u32], &mut [u32]) = (res_j0.1).split_at_mut(4u32.wrapping_mul(i) as usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u32(a_i, qj, c1, res_i0.1);
      let a_i0: u32 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u32], &mut [u32]) = (res_i0.1).split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u32(a_i0, qj, c1, res_i1.1);
      let a_i1: u32 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u32], &mut [u32]) = (res_i1.1).split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u32(a_i1, qj, c1, res_i2.1);
      let a_i2: u32 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u32], &mut [u32]) = (res_i2.1).split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u32(a_i2, qj, c1, res_i.1)
    };
    for i in len.wrapping_div(4u32).wrapping_mul(4u32)..len
    {
      let a_i: u32 = n[i as usize];
      let res_i: (&mut [u32], &mut [u32]) = (res_j0.1).split_at_mut(i as usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u32(a_i, qj, c1, res_i.1)
    };
    let r: u32 = c1;
    let c10: u32 = r;
    let res_j: u32 = c[len.wrapping_add(i0) as usize];
    let resb: (&mut [u32], &mut [u32]) = c.split_at_mut((len as usize).wrapping_add(i0 as usize));
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c0, c10, res_j, resb.1)
  };
  (res[0usize..len.wrapping_add(len).wrapping_sub(len) as usize]).copy_from_slice(
    &(&c[len as usize..])[0usize..len.wrapping_add(len).wrapping_sub(len) as usize]
  );
  let c00: u32 = c0;
  let mut tmp: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  let c1: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u32(len, res, n, &mut tmp);
  crate::lowstar::ignore::ignore::<u32>(c1);
  let m: u32 = 0u32.wrapping_sub(c00);
  for i in 0u32..len
  {
    let x: u32 = m & (&tmp)[i as usize] | ! m & res[i as usize];
    let os: (&mut [u32], &mut [u32]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn Hacl_Bignum_AlmostMontgomery_bn_almost_mont_reduction_u64(
  len: u32,
  n: &[u64],
  nInv: u64,
  c: &mut [u64],
  res: &mut [u64]
)
{
  let mut c0: u64 = 0u64;
  for i0 in 0u32..len
  {
    let qj: u64 = nInv.wrapping_mul(c[i0 as usize]);
    let res_j0: (&mut [u64], &mut [u64]) = c.split_at_mut(i0 as usize);
    let mut c1: u64 = 0u64;
    for i in 0u32..len.wrapping_div(4u32)
    {
      let a_i: u64 = n[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u64], &mut [u64]) = (res_j0.1).split_at_mut(4u32.wrapping_mul(i) as usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i, qj, c1, res_i0.1);
      let a_i0: u64 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i0, qj, c1, res_i1.1);
      let a_i1: u64 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i1, qj, c1, res_i2.1);
      let a_i2: u64 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i2, qj, c1, res_i.1)
    };
    for i in len.wrapping_div(4u32).wrapping_mul(4u32)..len
    {
      let a_i: u64 = n[i as usize];
      let res_i: (&mut [u64], &mut [u64]) = (res_j0.1).split_at_mut(i as usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i, qj, c1, res_i.1)
    };
    let r: u64 = c1;
    let c10: u64 = r;
    let res_j: u64 = c[len.wrapping_add(i0) as usize];
    let resb: (&mut [u64], &mut [u64]) = c.split_at_mut((len as usize).wrapping_add(i0 as usize));
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, c10, res_j, resb.1)
  };
  (res[0usize..len.wrapping_add(len).wrapping_sub(len) as usize]).copy_from_slice(
    &(&c[len as usize..])[0usize..len.wrapping_add(len).wrapping_sub(len) as usize]
  );
  let c00: u64 = c0;
  let mut tmp: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  let c1: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u64(len, res, n, &mut tmp);
  crate::lowstar::ignore::ignore::<u64>(c1);
  let m: u64 = 0u64.wrapping_sub(c00);
  for i in 0u32..len
  {
    let x: u64 = m & (&tmp)[i as usize] | ! m & res[i as usize];
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn Hacl_Bignum_Exponentiation_bn_check_mod_exp_u32(
  len: u32,
  n: &[u32],
  a: &[u32],
  bBits: u32,
  b: &[u32]
) ->
    u32
{
  let mut one: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  ((&mut one)[0usize..len as usize]).copy_from_slice(
    &vec![0u32; len as usize].into_boxed_slice()
  );
  (&mut one)[0usize] = 1u32;
  let bit0: u32 = n[0usize] & 1u32;
  let m0: u32 = 0u32.wrapping_sub(bit0);
  let mut acc0: u32 = 0u32;
  for i in 0u32..len
  {
    let beq: u32 = crate::hacl_krmllib::FStar_UInt32_eq_mask((&one)[i as usize], n[i as usize]);
    let blt: u32 = ! crate::hacl_krmllib::FStar_UInt32_gte_mask((&one)[i as usize], n[i as usize]);
    acc0 = beq & acc0 | ! beq & blt
  };
  let m10: u32 = acc0;
  let m00: u32 = m0 & m10;
  let mut bLen: u32;
  if bBits == 0u32
  { bLen = 1u32 }
  else
  { bLen = bBits.wrapping_sub(1u32).wrapping_div(32u32).wrapping_add(1u32) };
  let mut m1: u32;
  if bBits < 32u32.wrapping_mul(bLen)
  {
    let mut b2: Box<[u32]> = vec![0u32; bLen as usize].into_boxed_slice();
    let i0: u32 = bBits.wrapping_div(32u32);
    let j: u32 = bBits.wrapping_rem(32u32);
    (&mut b2)[i0 as usize] = (&b2)[i0 as usize] | 1u32.wrapping_shl(j);
    let mut acc: u32 = 0u32;
    for i in 0u32..bLen
    {
      let beq: u32 = crate::hacl_krmllib::FStar_UInt32_eq_mask(b[i as usize], (&b2)[i as usize]);
      let blt: u32 = ! crate::hacl_krmllib::FStar_UInt32_gte_mask(b[i as usize], (&b2)[i as usize]);
      acc = beq & acc | ! beq & blt
    };
    let res: u32 = acc;
    m1 = res
  }
  else
  { m1 = 4294967295u32 };
  let mut acc: u32 = 0u32;
  for i in 0u32..len
  {
    let beq: u32 = crate::hacl_krmllib::FStar_UInt32_eq_mask(a[i as usize], n[i as usize]);
    let blt: u32 = ! crate::hacl_krmllib::FStar_UInt32_gte_mask(a[i as usize], n[i as usize]);
    acc = beq & acc | ! beq & blt
  };
  let m2: u32 = acc;
  let m: u32 = m1 & m2;
  return m00 & m
}

pub fn Hacl_Bignum_Exponentiation_bn_check_mod_exp_u64(
  len: u32,
  n: &[u64],
  a: &[u64],
  bBits: u32,
  b: &[u64]
) ->
    u64
{
  let mut one: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  ((&mut one)[0usize..len as usize]).copy_from_slice(
    &vec![0u64; len as usize].into_boxed_slice()
  );
  (&mut one)[0usize] = 1u64;
  let bit0: u64 = n[0usize] & 1u64;
  let m0: u64 = 0u64.wrapping_sub(bit0);
  let mut acc0: u64 = 0u64;
  for i in 0u32..len
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
  for i in 0u32..len
  {
    let beq: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(a[i as usize], n[i as usize]);
    let blt: u64 = ! crate::hacl_krmllib::FStar_UInt64_gte_mask(a[i as usize], n[i as usize]);
    acc = beq & acc | ! beq & blt
  };
  let m2: u64 = acc;
  let m: u64 = m1 & m2;
  return m00 & m
}

pub fn Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32(
  len: u32,
  n: &[u32],
  mu: u32,
  r2: &[u32],
  a: &[u32],
  bBits: u32,
  b: &[u32],
  res: &mut [u32]
)
{
  if bBits < 200u32
  {
    let mut aM: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
    Hacl_Bignum_Montgomery_bn_to_mont_u32(len, n, mu, r2, a, &mut aM);
    let mut resM: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
    let mut ctx: Box<[u32]> = vec![0u32; len.wrapping_add(len) as usize].into_boxed_slice();
    ((&mut ctx)[0usize..len as usize]).copy_from_slice(&n[0usize..len as usize]);
    ((&mut (&mut ctx)[len as usize..])[0usize..len as usize]).copy_from_slice(
      &r2[0usize..len as usize]
    );
    let mut sw: u32 = 0u32;
    let ctx_n0: (&[u32], &[u32]) = ctx.split_at(0usize);
    let ctx_r2: (&[u32], &[u32]) = (ctx_n0.1).split_at(len as usize);
    Hacl_Bignum_Montgomery_bn_from_mont_u32(len, ctx_r2.0, mu, ctx_r2.1, &mut resM);
    crate::lowstar::ignore::ignore::<&[u32]>(&ctx);
    for i0 in 0u32..bBits
    {
      let i1: u32 = bBits.wrapping_sub(i0).wrapping_sub(1u32).wrapping_div(32u32);
      let j: u32 = bBits.wrapping_sub(i0).wrapping_sub(1u32).wrapping_rem(32u32);
      let tmp: u32 = b[i1 as usize];
      let bit: u32 = tmp.wrapping_shr(j) & 1u32;
      let sw1: u32 = bit ^ sw;
      for i in 0u32..len
      {
        let dummy: u32 = 0u32.wrapping_sub(sw1) & ((&resM)[i as usize] ^ (&aM)[i as usize]);
        (&mut resM)[i as usize] = (&resM)[i as usize] ^ dummy;
        (&mut aM)[i as usize] = (&aM)[i as usize] ^ dummy
      };
      let mut aM_copy: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
      ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&aM)[0usize..len as usize]);
      let ctx_n1: (&[u32], &[u32]) = ctx.split_at(0usize);
      bn_almost_mont_mul_u32(len, ctx_n1.1, mu, &aM_copy, &resM, &mut aM);
      crate::lowstar::ignore::ignore::<&[u32]>(&ctx);
      let mut aM_copy0: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
      ((&mut aM_copy0)[0usize..len as usize]).copy_from_slice(&(&resM)[0usize..len as usize]);
      let ctx_n: (&[u32], &[u32]) = ctx.split_at(0usize);
      bn_almost_mont_sqr_u32(len, ctx_n.1, mu, &aM_copy0, &mut resM);
      crate::lowstar::ignore::ignore::<&[u32]>(&ctx);
      sw = bit
    };
    let sw0: u32 = sw;
    for i in 0u32..len
    {
      let dummy: u32 = 0u32.wrapping_sub(sw0) & ((&resM)[i as usize] ^ (&aM)[i as usize]);
      (&mut resM)[i as usize] = (&resM)[i as usize] ^ dummy;
      (&mut aM)[i as usize] = (&aM)[i as usize] ^ dummy
    };
    Hacl_Bignum_Montgomery_bn_from_mont_u32(len, n, mu, &resM, res);
    return ()
  };
  let mut aM: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  Hacl_Bignum_Montgomery_bn_to_mont_u32(len, n, mu, r2, a, &mut aM);
  let mut resM: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  let mut bLen: u32;
  if bBits == 0u32
  { bLen = 1u32 }
  else
  { bLen = bBits.wrapping_sub(1u32).wrapping_div(32u32).wrapping_add(1u32) };
  let mut ctx: Box<[u32]> = vec![0u32; len.wrapping_add(len) as usize].into_boxed_slice();
  ((&mut ctx)[0usize..len as usize]).copy_from_slice(&n[0usize..len as usize]);
  ((&mut (&mut ctx)[len as usize..])[0usize..len as usize]).copy_from_slice(
    &r2[0usize..len as usize]
  );
  let mut table: Box<[u32]> = vec![0u32; 16u32.wrapping_mul(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  let t0: (&mut [u32], &mut [u32]) = table.split_at_mut(0usize);
  let t1: (&mut [u32], &mut [u32]) = (t0.1).split_at_mut(len as usize);
  let ctx_n0: (&[u32], &[u32]) = ctx.split_at(0usize);
  let ctx_r20: (&[u32], &[u32]) = (ctx_n0.1).split_at(len as usize);
  Hacl_Bignum_Montgomery_bn_from_mont_u32(len, ctx_r20.0, mu, ctx_r20.1, t1.0);
  crate::lowstar::ignore::ignore::<&[u32]>(&ctx);
  (t1.1[0usize..len as usize]).copy_from_slice(&(&aM)[0usize..len as usize]);
  crate::lowstar::ignore::ignore::<&[u32]>(&table);
  for i in 0u32..7u32
  {
    let t11: (&[u32], &[u32]) = table.split_at(i.wrapping_add(1u32).wrapping_mul(len) as usize);
    let mut aM_copy0: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
    ((&mut aM_copy0)[0usize..len as usize]).copy_from_slice(&t11.1[0usize..len as usize]);
    let ctx_n1: (&[u32], &[u32]) = ctx.split_at(0usize);
    bn_almost_mont_sqr_u32(len, ctx_n1.1, mu, &aM_copy0, &mut tmp);
    crate::lowstar::ignore::ignore::<&[u32]>(&ctx);
    ((&mut (&mut table)[2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(len) as usize..])[0usize..len
    as
    usize]).copy_from_slice(&(&tmp)[0usize..len as usize]);
    let t2: (&[u32], &[u32]) =
        table.split_at(2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(len) as usize);
    let mut aM_copy: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
    ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&aM)[0usize..len as usize]);
    let ctx_n: (&[u32], &[u32]) = ctx.split_at(0usize);
    bn_almost_mont_mul_u32(len, ctx_n.1, mu, &aM_copy, t2.1, &mut tmp);
    crate::lowstar::ignore::ignore::<&[u32]>(&ctx);
    ((&mut (&mut table)[2u32.wrapping_mul(i).wrapping_add(3u32).wrapping_mul(len) as usize..])[0usize..len
    as
    usize]).copy_from_slice(&(&tmp)[0usize..len as usize])
  };
  if bBits.wrapping_rem(4u32) != 0u32
  {
    let i0: u32 = bBits.wrapping_div(4u32).wrapping_mul(4u32);
    let bits_c: u32 = crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u32(bLen, b, i0, 4u32);
    ((&mut resM)[0usize..len as usize]).copy_from_slice(
      &(&(&table)[0u32.wrapping_mul(len) as usize..])[0usize..len as usize]
    );
    for i1 in 0u32..15u32
    {
      let c: u32 = crate::hacl_krmllib::FStar_UInt32_eq_mask(bits_c, i1.wrapping_add(1u32));
      let res_j: (&[u32], &[u32]) =
          table.split_at(i1.wrapping_add(1u32).wrapping_mul(len) as usize);
      for i in 0u32..len
      {
        let x: u32 = c & res_j.1[i as usize] | ! c & (&resM)[i as usize];
        let os: (&mut [u32], &mut [u32]) = resM.split_at_mut(0usize);
        os.1[i as usize] = x
      }
    }
  }
  else
  {
    let ctx_n: (&[u32], &[u32]) = ctx.split_at(0usize);
    let ctx_r2: (&[u32], &[u32]) = (ctx_n.1).split_at(len as usize);
    Hacl_Bignum_Montgomery_bn_from_mont_u32(len, ctx_r2.0, mu, ctx_r2.1, &mut resM);
    crate::lowstar::ignore::ignore::<&[u32]>(&ctx)
  };
  let mut tmp0: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  for i0 in 0u32..bBits.wrapping_div(4u32)
  {
    for i in 0u32..4u32
    {
      let mut aM_copy: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
      ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&resM)[0usize..len as usize]);
      let ctx_n: (&[u32], &[u32]) = ctx.split_at(0usize);
      bn_almost_mont_sqr_u32(len, ctx_n.1, mu, &aM_copy, &mut resM);
      crate::lowstar::ignore::ignore::<&[u32]>(&ctx)
    };
    let k: u32 =
        bBits.wrapping_sub(bBits.wrapping_rem(4u32)).wrapping_sub(4u32.wrapping_mul(i0)).wrapping_sub(
          4u32
        );
    let bits_l: u32 = crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u32(bLen, b, k, 4u32);
    crate::lowstar::ignore::ignore::<&[u32]>(&table);
    ((&mut tmp0)[0usize..len as usize]).copy_from_slice(
      &(&(&table)[0u32.wrapping_mul(len) as usize..])[0usize..len as usize]
    );
    for i1 in 0u32..15u32
    {
      let c: u32 = crate::hacl_krmllib::FStar_UInt32_eq_mask(bits_l, i1.wrapping_add(1u32));
      let res_j: (&[u32], &[u32]) =
          table.split_at(i1.wrapping_add(1u32).wrapping_mul(len) as usize);
      for i in 0u32..len
      {
        let x: u32 = c & res_j.1[i as usize] | ! c & (&tmp0)[i as usize];
        let os: (&mut [u32], &mut [u32]) = tmp0.split_at_mut(0usize);
        os.1[i as usize] = x
      }
    };
    let mut aM_copy: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
    ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&resM)[0usize..len as usize]);
    let ctx_n: (&[u32], &[u32]) = ctx.split_at(0usize);
    bn_almost_mont_mul_u32(len, ctx_n.1, mu, &aM_copy, &tmp0, &mut resM);
    crate::lowstar::ignore::ignore::<&[u32]>(&ctx)
  };
  Hacl_Bignum_Montgomery_bn_from_mont_u32(len, n, mu, &resM, res)
}

pub fn Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64(
  len: u32,
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
    let mut aM: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
    Hacl_Bignum_Montgomery_bn_to_mont_u64(len, n, mu, r2, a, &mut aM);
    let mut resM: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
    let mut ctx: Box<[u64]> = vec![0u64; len.wrapping_add(len) as usize].into_boxed_slice();
    ((&mut ctx)[0usize..len as usize]).copy_from_slice(&n[0usize..len as usize]);
    ((&mut (&mut ctx)[len as usize..])[0usize..len as usize]).copy_from_slice(
      &r2[0usize..len as usize]
    );
    let mut sw: u64 = 0u64;
    let ctx_n0: (&[u64], &[u64]) = ctx.split_at(0usize);
    let ctx_r2: (&[u64], &[u64]) = (ctx_n0.1).split_at(len as usize);
    Hacl_Bignum_Montgomery_bn_from_mont_u64(len, ctx_r2.0, mu, ctx_r2.1, &mut resM);
    crate::lowstar::ignore::ignore::<&[u64]>(&ctx);
    for i0 in 0u32..bBits
    {
      let i1: u32 = bBits.wrapping_sub(i0).wrapping_sub(1u32).wrapping_div(64u32);
      let j: u32 = bBits.wrapping_sub(i0).wrapping_sub(1u32).wrapping_rem(64u32);
      let tmp: u64 = b[i1 as usize];
      let bit: u64 = tmp.wrapping_shr(j) & 1u64;
      let sw1: u64 = bit ^ sw;
      for i in 0u32..len
      {
        let dummy: u64 = 0u64.wrapping_sub(sw1) & ((&resM)[i as usize] ^ (&aM)[i as usize]);
        (&mut resM)[i as usize] = (&resM)[i as usize] ^ dummy;
        (&mut aM)[i as usize] = (&aM)[i as usize] ^ dummy
      };
      let mut aM_copy: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
      ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&aM)[0usize..len as usize]);
      let ctx_n1: (&[u64], &[u64]) = ctx.split_at(0usize);
      bn_almost_mont_mul_u64(len, ctx_n1.1, mu, &aM_copy, &resM, &mut aM);
      crate::lowstar::ignore::ignore::<&[u64]>(&ctx);
      let mut aM_copy0: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
      ((&mut aM_copy0)[0usize..len as usize]).copy_from_slice(&(&resM)[0usize..len as usize]);
      let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
      bn_almost_mont_sqr_u64(len, ctx_n.1, mu, &aM_copy0, &mut resM);
      crate::lowstar::ignore::ignore::<&[u64]>(&ctx);
      sw = bit
    };
    let sw0: u64 = sw;
    for i in 0u32..len
    {
      let dummy: u64 = 0u64.wrapping_sub(sw0) & ((&resM)[i as usize] ^ (&aM)[i as usize]);
      (&mut resM)[i as usize] = (&resM)[i as usize] ^ dummy;
      (&mut aM)[i as usize] = (&aM)[i as usize] ^ dummy
    };
    Hacl_Bignum_Montgomery_bn_from_mont_u64(len, n, mu, &resM, res);
    return ()
  };
  let mut aM: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  Hacl_Bignum_Montgomery_bn_to_mont_u64(len, n, mu, r2, a, &mut aM);
  let mut resM: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  let mut bLen: u32;
  if bBits == 0u32
  { bLen = 1u32 }
  else
  { bLen = bBits.wrapping_sub(1u32).wrapping_div(64u32).wrapping_add(1u32) };
  let mut ctx: Box<[u64]> = vec![0u64; len.wrapping_add(len) as usize].into_boxed_slice();
  ((&mut ctx)[0usize..len as usize]).copy_from_slice(&n[0usize..len as usize]);
  ((&mut (&mut ctx)[len as usize..])[0usize..len as usize]).copy_from_slice(
    &r2[0usize..len as usize]
  );
  let mut table: Box<[u64]> = vec![0u64; 16u32.wrapping_mul(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  let t0: (&mut [u64], &mut [u64]) = table.split_at_mut(0usize);
  let t1: (&mut [u64], &mut [u64]) = (t0.1).split_at_mut(len as usize);
  let ctx_n0: (&[u64], &[u64]) = ctx.split_at(0usize);
  let ctx_r20: (&[u64], &[u64]) = (ctx_n0.1).split_at(len as usize);
  Hacl_Bignum_Montgomery_bn_from_mont_u64(len, ctx_r20.0, mu, ctx_r20.1, t1.0);
  crate::lowstar::ignore::ignore::<&[u64]>(&ctx);
  (t1.1[0usize..len as usize]).copy_from_slice(&(&aM)[0usize..len as usize]);
  crate::lowstar::ignore::ignore::<&[u64]>(&table);
  for i in 0u32..7u32
  {
    let t11: (&[u64], &[u64]) = table.split_at(i.wrapping_add(1u32).wrapping_mul(len) as usize);
    let mut aM_copy0: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
    ((&mut aM_copy0)[0usize..len as usize]).copy_from_slice(&t11.1[0usize..len as usize]);
    let ctx_n1: (&[u64], &[u64]) = ctx.split_at(0usize);
    bn_almost_mont_sqr_u64(len, ctx_n1.1, mu, &aM_copy0, &mut tmp);
    crate::lowstar::ignore::ignore::<&[u64]>(&ctx);
    ((&mut (&mut table)[2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(len) as usize..])[0usize..len
    as
    usize]).copy_from_slice(&(&tmp)[0usize..len as usize]);
    let t2: (&[u64], &[u64]) =
        table.split_at(2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(len) as usize);
    let mut aM_copy: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
    ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&aM)[0usize..len as usize]);
    let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
    bn_almost_mont_mul_u64(len, ctx_n.1, mu, &aM_copy, t2.1, &mut tmp);
    crate::lowstar::ignore::ignore::<&[u64]>(&ctx);
    ((&mut (&mut table)[2u32.wrapping_mul(i).wrapping_add(3u32).wrapping_mul(len) as usize..])[0usize..len
    as
    usize]).copy_from_slice(&(&tmp)[0usize..len as usize])
  };
  if bBits.wrapping_rem(4u32) != 0u32
  {
    let i0: u32 = bBits.wrapping_div(4u32).wrapping_mul(4u32);
    let bits_c: u64 = crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(bLen, b, i0, 4u32);
    ((&mut resM)[0usize..len as usize]).copy_from_slice(
      &(&(&table)[0u32.wrapping_mul(len) as usize..])[0usize..len as usize]
    );
    for i1 in 0u32..15u32
    {
      let c: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(bits_c, i1.wrapping_add(1u32) as u64);
      let res_j: (&[u64], &[u64]) =
          table.split_at(i1.wrapping_add(1u32).wrapping_mul(len) as usize);
      for i in 0u32..len
      {
        let x: u64 = c & res_j.1[i as usize] | ! c & (&resM)[i as usize];
        let os: (&mut [u64], &mut [u64]) = resM.split_at_mut(0usize);
        os.1[i as usize] = x
      }
    }
  }
  else
  {
    let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
    let ctx_r2: (&[u64], &[u64]) = (ctx_n.1).split_at(len as usize);
    Hacl_Bignum_Montgomery_bn_from_mont_u64(len, ctx_r2.0, mu, ctx_r2.1, &mut resM);
    crate::lowstar::ignore::ignore::<&[u64]>(&ctx)
  };
  let mut tmp0: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  for i0 in 0u32..bBits.wrapping_div(4u32)
  {
    for i in 0u32..4u32
    {
      let mut aM_copy: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
      ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&resM)[0usize..len as usize]);
      let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
      bn_almost_mont_sqr_u64(len, ctx_n.1, mu, &aM_copy, &mut resM);
      crate::lowstar::ignore::ignore::<&[u64]>(&ctx)
    };
    let k: u32 =
        bBits.wrapping_sub(bBits.wrapping_rem(4u32)).wrapping_sub(4u32.wrapping_mul(i0)).wrapping_sub(
          4u32
        );
    let bits_l: u64 = crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(bLen, b, k, 4u32);
    crate::lowstar::ignore::ignore::<&[u64]>(&table);
    ((&mut tmp0)[0usize..len as usize]).copy_from_slice(
      &(&(&table)[0u32.wrapping_mul(len) as usize..])[0usize..len as usize]
    );
    for i1 in 0u32..15u32
    {
      let c: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(bits_l, i1.wrapping_add(1u32) as u64);
      let res_j: (&[u64], &[u64]) =
          table.split_at(i1.wrapping_add(1u32).wrapping_mul(len) as usize);
      for i in 0u32..len
      {
        let x: u64 = c & res_j.1[i as usize] | ! c & (&tmp0)[i as usize];
        let os: (&mut [u64], &mut [u64]) = tmp0.split_at_mut(0usize);
        os.1[i as usize] = x
      }
    };
    let mut aM_copy: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
    ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&resM)[0usize..len as usize]);
    let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
    bn_almost_mont_mul_u64(len, ctx_n.1, mu, &aM_copy, &tmp0, &mut resM);
    crate::lowstar::ignore::ignore::<&[u64]>(&ctx)
  };
  Hacl_Bignum_Montgomery_bn_from_mont_u64(len, n, mu, &resM, res)
}

pub fn Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u32(
  len: u32,
  nBits: u32,
  n: &[u32],
  a: &[u32],
  bBits: u32,
  b: &[u32],
  res: &mut [u32]
)
{
  let mut r2: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u32(len, nBits, n, &mut r2);
  let mu: u32 = Hacl_Bignum_ModInvLimb_mod_inv_uint32(n[0usize]);
  Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u32(len, n, mu, &r2, a, bBits, b, res)
}

pub fn Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_u64(
  len: u32,
  nBits: u32,
  n: &[u64],
  a: &[u64],
  bBits: u32,
  b: &[u64],
  res: &mut [u64]
)
{
  let mut r2: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u64(len, nBits, n, &mut r2);
  let mu: u64 = Hacl_Bignum_ModInvLimb_mod_inv_uint64(n[0usize]);
  Hacl_Bignum_Exponentiation_bn_mod_exp_consttime_precomp_u64(len, n, mu, &r2, a, bBits, b, res)
}

pub fn Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32(
  len: u32,
  n: &[u32],
  mu: u32,
  r2: &[u32],
  a: &[u32],
  bBits: u32,
  b: &[u32],
  res: &mut [u32]
)
{
  if bBits < 200u32
  {
    let mut aM: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
    Hacl_Bignum_Montgomery_bn_to_mont_u32(len, n, mu, r2, a, &mut aM);
    let mut resM: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
    let mut ctx: Box<[u32]> = vec![0u32; len.wrapping_add(len) as usize].into_boxed_slice();
    ((&mut ctx)[0usize..len as usize]).copy_from_slice(&n[0usize..len as usize]);
    ((&mut (&mut ctx)[len as usize..])[0usize..len as usize]).copy_from_slice(
      &r2[0usize..len as usize]
    );
    let ctx_n0: (&[u32], &[u32]) = ctx.split_at(0usize);
    let ctx_r2: (&[u32], &[u32]) = (ctx_n0.1).split_at(len as usize);
    Hacl_Bignum_Montgomery_bn_from_mont_u32(len, ctx_r2.0, mu, ctx_r2.1, &mut resM);
    crate::lowstar::ignore::ignore::<&[u32]>(&ctx);
    for i in 0u32..bBits
    {
      let i1: u32 = i.wrapping_div(32u32);
      let j: u32 = i.wrapping_rem(32u32);
      let tmp: u32 = b[i1 as usize];
      let bit: u32 = tmp.wrapping_shr(j) & 1u32;
      if bit != 0u32
      {
        let mut aM_copy: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
        ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&resM)[0usize..len as usize]);
        let ctx_n: (&[u32], &[u32]) = ctx.split_at(0usize);
        bn_almost_mont_mul_u32(len, ctx_n.1, mu, &aM_copy, &aM, &mut resM);
        crate::lowstar::ignore::ignore::<&[u32]>(&ctx)
      };
      let mut aM_copy: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
      ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&aM)[0usize..len as usize]);
      let ctx_n: (&[u32], &[u32]) = ctx.split_at(0usize);
      bn_almost_mont_sqr_u32(len, ctx_n.1, mu, &aM_copy, &mut aM);
      crate::lowstar::ignore::ignore::<&[u32]>(&ctx)
    };
    Hacl_Bignum_Montgomery_bn_from_mont_u32(len, n, mu, &resM, res);
    return ()
  };
  let mut aM: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  Hacl_Bignum_Montgomery_bn_to_mont_u32(len, n, mu, r2, a, &mut aM);
  let mut resM: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  let mut bLen: u32;
  if bBits == 0u32
  { bLen = 1u32 }
  else
  { bLen = bBits.wrapping_sub(1u32).wrapping_div(32u32).wrapping_add(1u32) };
  let mut ctx: Box<[u32]> = vec![0u32; len.wrapping_add(len) as usize].into_boxed_slice();
  ((&mut ctx)[0usize..len as usize]).copy_from_slice(&n[0usize..len as usize]);
  ((&mut (&mut ctx)[len as usize..])[0usize..len as usize]).copy_from_slice(
    &r2[0usize..len as usize]
  );
  let mut table: Box<[u32]> = vec![0u32; 16u32.wrapping_mul(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  let t0: (&mut [u32], &mut [u32]) = table.split_at_mut(0usize);
  let t1: (&mut [u32], &mut [u32]) = (t0.1).split_at_mut(len as usize);
  let ctx_n0: (&[u32], &[u32]) = ctx.split_at(0usize);
  let ctx_r20: (&[u32], &[u32]) = (ctx_n0.1).split_at(len as usize);
  Hacl_Bignum_Montgomery_bn_from_mont_u32(len, ctx_r20.0, mu, ctx_r20.1, t1.0);
  crate::lowstar::ignore::ignore::<&[u32]>(&ctx);
  (t1.1[0usize..len as usize]).copy_from_slice(&(&aM)[0usize..len as usize]);
  crate::lowstar::ignore::ignore::<&[u32]>(&table);
  for i in 0u32..7u32
  {
    let t11: (&[u32], &[u32]) = table.split_at(i.wrapping_add(1u32).wrapping_mul(len) as usize);
    let mut aM_copy0: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
    ((&mut aM_copy0)[0usize..len as usize]).copy_from_slice(&t11.1[0usize..len as usize]);
    let ctx_n1: (&[u32], &[u32]) = ctx.split_at(0usize);
    bn_almost_mont_sqr_u32(len, ctx_n1.1, mu, &aM_copy0, &mut tmp);
    crate::lowstar::ignore::ignore::<&[u32]>(&ctx);
    ((&mut (&mut table)[2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(len) as usize..])[0usize..len
    as
    usize]).copy_from_slice(&(&tmp)[0usize..len as usize]);
    let t2: (&[u32], &[u32]) =
        table.split_at(2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(len) as usize);
    let mut aM_copy: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
    ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&aM)[0usize..len as usize]);
    let ctx_n: (&[u32], &[u32]) = ctx.split_at(0usize);
    bn_almost_mont_mul_u32(len, ctx_n.1, mu, &aM_copy, t2.1, &mut tmp);
    crate::lowstar::ignore::ignore::<&[u32]>(&ctx);
    ((&mut (&mut table)[2u32.wrapping_mul(i).wrapping_add(3u32).wrapping_mul(len) as usize..])[0usize..len
    as
    usize]).copy_from_slice(&(&tmp)[0usize..len as usize])
  };
  if bBits.wrapping_rem(4u32) != 0u32
  {
    let i: u32 = bBits.wrapping_div(4u32).wrapping_mul(4u32);
    let bits_c: u32 = crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u32(bLen, b, i, 4u32);
    let bits_l32: u32 = bits_c;
    let a_bits_l: (&[u32], &[u32]) = table.split_at(bits_l32.wrapping_mul(len) as usize);
    ((&mut resM)[0usize..len as usize]).copy_from_slice(&a_bits_l.1[0usize..len as usize])
  }
  else
  {
    let ctx_n: (&[u32], &[u32]) = ctx.split_at(0usize);
    let ctx_r2: (&[u32], &[u32]) = (ctx_n.1).split_at(len as usize);
    Hacl_Bignum_Montgomery_bn_from_mont_u32(len, ctx_r2.0, mu, ctx_r2.1, &mut resM);
    crate::lowstar::ignore::ignore::<&[u32]>(&ctx)
  };
  let mut tmp0: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  for i in 0u32..bBits.wrapping_div(4u32)
  {
    for i0 in 0u32..4u32
    {
      let mut aM_copy: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
      ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&resM)[0usize..len as usize]);
      let ctx_n: (&[u32], &[u32]) = ctx.split_at(0usize);
      bn_almost_mont_sqr_u32(len, ctx_n.1, mu, &aM_copy, &mut resM);
      crate::lowstar::ignore::ignore::<&[u32]>(&ctx)
    };
    let k: u32 =
        bBits.wrapping_sub(bBits.wrapping_rem(4u32)).wrapping_sub(4u32.wrapping_mul(i)).wrapping_sub(
          4u32
        );
    let bits_l: u32 = crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u32(bLen, b, k, 4u32);
    crate::lowstar::ignore::ignore::<&[u32]>(&table);
    let bits_l32: u32 = bits_l;
    let a_bits_l: (&[u32], &[u32]) = table.split_at(bits_l32.wrapping_mul(len) as usize);
    ((&mut tmp0)[0usize..len as usize]).copy_from_slice(&a_bits_l.1[0usize..len as usize]);
    let mut aM_copy: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
    ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&resM)[0usize..len as usize]);
    let ctx_n: (&[u32], &[u32]) = ctx.split_at(0usize);
    bn_almost_mont_mul_u32(len, ctx_n.1, mu, &aM_copy, &tmp0, &mut resM);
    crate::lowstar::ignore::ignore::<&[u32]>(&ctx)
  };
  Hacl_Bignum_Montgomery_bn_from_mont_u32(len, n, mu, &resM, res)
}

pub fn Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64(
  len: u32,
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
    let mut aM: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
    Hacl_Bignum_Montgomery_bn_to_mont_u64(len, n, mu, r2, a, &mut aM);
    let mut resM: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
    let mut ctx: Box<[u64]> = vec![0u64; len.wrapping_add(len) as usize].into_boxed_slice();
    ((&mut ctx)[0usize..len as usize]).copy_from_slice(&n[0usize..len as usize]);
    ((&mut (&mut ctx)[len as usize..])[0usize..len as usize]).copy_from_slice(
      &r2[0usize..len as usize]
    );
    let ctx_n0: (&[u64], &[u64]) = ctx.split_at(0usize);
    let ctx_r2: (&[u64], &[u64]) = (ctx_n0.1).split_at(len as usize);
    Hacl_Bignum_Montgomery_bn_from_mont_u64(len, ctx_r2.0, mu, ctx_r2.1, &mut resM);
    crate::lowstar::ignore::ignore::<&[u64]>(&ctx);
    for i in 0u32..bBits
    {
      let i1: u32 = i.wrapping_div(64u32);
      let j: u32 = i.wrapping_rem(64u32);
      let tmp: u64 = b[i1 as usize];
      let bit: u64 = tmp.wrapping_shr(j) & 1u64;
      if bit != 0u64
      {
        let mut aM_copy: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
        ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&resM)[0usize..len as usize]);
        let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
        bn_almost_mont_mul_u64(len, ctx_n.1, mu, &aM_copy, &aM, &mut resM);
        crate::lowstar::ignore::ignore::<&[u64]>(&ctx)
      };
      let mut aM_copy: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
      ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&aM)[0usize..len as usize]);
      let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
      bn_almost_mont_sqr_u64(len, ctx_n.1, mu, &aM_copy, &mut aM);
      crate::lowstar::ignore::ignore::<&[u64]>(&ctx)
    };
    Hacl_Bignum_Montgomery_bn_from_mont_u64(len, n, mu, &resM, res);
    return ()
  };
  let mut aM: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  Hacl_Bignum_Montgomery_bn_to_mont_u64(len, n, mu, r2, a, &mut aM);
  let mut resM: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  let mut bLen: u32;
  if bBits == 0u32
  { bLen = 1u32 }
  else
  { bLen = bBits.wrapping_sub(1u32).wrapping_div(64u32).wrapping_add(1u32) };
  let mut ctx: Box<[u64]> = vec![0u64; len.wrapping_add(len) as usize].into_boxed_slice();
  ((&mut ctx)[0usize..len as usize]).copy_from_slice(&n[0usize..len as usize]);
  ((&mut (&mut ctx)[len as usize..])[0usize..len as usize]).copy_from_slice(
    &r2[0usize..len as usize]
  );
  let mut table: Box<[u64]> = vec![0u64; 16u32.wrapping_mul(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  let t0: (&mut [u64], &mut [u64]) = table.split_at_mut(0usize);
  let t1: (&mut [u64], &mut [u64]) = (t0.1).split_at_mut(len as usize);
  let ctx_n0: (&[u64], &[u64]) = ctx.split_at(0usize);
  let ctx_r20: (&[u64], &[u64]) = (ctx_n0.1).split_at(len as usize);
  Hacl_Bignum_Montgomery_bn_from_mont_u64(len, ctx_r20.0, mu, ctx_r20.1, t1.0);
  crate::lowstar::ignore::ignore::<&[u64]>(&ctx);
  (t1.1[0usize..len as usize]).copy_from_slice(&(&aM)[0usize..len as usize]);
  crate::lowstar::ignore::ignore::<&[u64]>(&table);
  for i in 0u32..7u32
  {
    let t11: (&[u64], &[u64]) = table.split_at(i.wrapping_add(1u32).wrapping_mul(len) as usize);
    let mut aM_copy0: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
    ((&mut aM_copy0)[0usize..len as usize]).copy_from_slice(&t11.1[0usize..len as usize]);
    let ctx_n1: (&[u64], &[u64]) = ctx.split_at(0usize);
    bn_almost_mont_sqr_u64(len, ctx_n1.1, mu, &aM_copy0, &mut tmp);
    crate::lowstar::ignore::ignore::<&[u64]>(&ctx);
    ((&mut (&mut table)[2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(len) as usize..])[0usize..len
    as
    usize]).copy_from_slice(&(&tmp)[0usize..len as usize]);
    let t2: (&[u64], &[u64]) =
        table.split_at(2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(len) as usize);
    let mut aM_copy: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
    ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&aM)[0usize..len as usize]);
    let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
    bn_almost_mont_mul_u64(len, ctx_n.1, mu, &aM_copy, t2.1, &mut tmp);
    crate::lowstar::ignore::ignore::<&[u64]>(&ctx);
    ((&mut (&mut table)[2u32.wrapping_mul(i).wrapping_add(3u32).wrapping_mul(len) as usize..])[0usize..len
    as
    usize]).copy_from_slice(&(&tmp)[0usize..len as usize])
  };
  if bBits.wrapping_rem(4u32) != 0u32
  {
    let i: u32 = bBits.wrapping_div(4u32).wrapping_mul(4u32);
    let bits_c: u64 = crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(bLen, b, i, 4u32);
    let bits_l32: u32 = bits_c as u32;
    let a_bits_l: (&[u64], &[u64]) = table.split_at(bits_l32.wrapping_mul(len) as usize);
    ((&mut resM)[0usize..len as usize]).copy_from_slice(&a_bits_l.1[0usize..len as usize])
  }
  else
  {
    let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
    let ctx_r2: (&[u64], &[u64]) = (ctx_n.1).split_at(len as usize);
    Hacl_Bignum_Montgomery_bn_from_mont_u64(len, ctx_r2.0, mu, ctx_r2.1, &mut resM);
    crate::lowstar::ignore::ignore::<&[u64]>(&ctx)
  };
  let mut tmp0: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  for i in 0u32..bBits.wrapping_div(4u32)
  {
    for i0 in 0u32..4u32
    {
      let mut aM_copy: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
      ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&resM)[0usize..len as usize]);
      let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
      bn_almost_mont_sqr_u64(len, ctx_n.1, mu, &aM_copy, &mut resM);
      crate::lowstar::ignore::ignore::<&[u64]>(&ctx)
    };
    let k: u32 =
        bBits.wrapping_sub(bBits.wrapping_rem(4u32)).wrapping_sub(4u32.wrapping_mul(i)).wrapping_sub(
          4u32
        );
    let bits_l: u64 = crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(bLen, b, k, 4u32);
    crate::lowstar::ignore::ignore::<&[u64]>(&table);
    let bits_l32: u32 = bits_l as u32;
    let a_bits_l: (&[u64], &[u64]) = table.split_at(bits_l32.wrapping_mul(len) as usize);
    ((&mut tmp0)[0usize..len as usize]).copy_from_slice(&a_bits_l.1[0usize..len as usize]);
    let mut aM_copy: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
    ((&mut aM_copy)[0usize..len as usize]).copy_from_slice(&(&resM)[0usize..len as usize]);
    let ctx_n: (&[u64], &[u64]) = ctx.split_at(0usize);
    bn_almost_mont_mul_u64(len, ctx_n.1, mu, &aM_copy, &tmp0, &mut resM);
    crate::lowstar::ignore::ignore::<&[u64]>(&ctx)
  };
  Hacl_Bignum_Montgomery_bn_from_mont_u64(len, n, mu, &resM, res)
}

pub fn Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u32(
  len: u32,
  nBits: u32,
  n: &[u32],
  a: &[u32],
  bBits: u32,
  b: &[u32],
  res: &mut [u32]
)
{
  let mut r2: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u32(len, nBits, n, &mut r2);
  let mu: u32 = Hacl_Bignum_ModInvLimb_mod_inv_uint32(n[0usize]);
  Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u32(len, n, mu, &r2, a, bBits, b, res)
}

pub fn Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_u64(
  len: u32,
  nBits: u32,
  n: &[u64],
  a: &[u64],
  bBits: u32,
  b: &[u64],
  res: &mut [u64]
)
{
  let mut r2: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u64(len, nBits, n, &mut r2);
  let mu: u64 = Hacl_Bignum_ModInvLimb_mod_inv_uint64(n[0usize]);
  Hacl_Bignum_Exponentiation_bn_mod_exp_vartime_precomp_u64(len, n, mu, &r2, a, bBits, b, res)
}

pub fn Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint32(
  aLen: u32,
  a: &[u32],
  b: &[u32],
  tmp: &mut [u32],
  res: &mut [u32]
)
{
  if aLen < 32u32 || aLen.wrapping_rem(2u32) == 1u32
  {
    crate::hacl::bignum_base::Hacl_Bignum_Multiplication_bn_mul_u32(aLen, a, aLen, b, res);
    return ()
  };
  let len2: u32 = aLen.wrapping_div(2u32);
  let a0: (&[u32], &[u32]) = a.split_at(0usize);
  let a1: (&[u32], &[u32]) = (a0.1).split_at(len2 as usize);
  let b0: (&[u32], &[u32]) = b.split_at(0usize);
  let b1: (&[u32], &[u32]) = (b0.1).split_at(len2 as usize);
  let t0: (&mut [u32], &mut [u32]) = tmp.split_at_mut(0usize);
  let t1: (&mut [u32], &mut [u32]) = (t0.1).split_at_mut(len2 as usize);
  let tmp_: (&mut [u32], &mut [u32]) = (t1.1).split_at_mut(aLen as usize - len2 as usize);
  let c0: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u32(len2, a1.0, a1.1, tmp_.1);
  let c10: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u32(len2, a1.1, a1.0, t1.0);
  for i in 0u32..len2
  {
    let x: u32 =
        0u32.wrapping_sub(c0) & t1.0[i as usize] | ! 0u32.wrapping_sub(c0) & tmp_.1[i as usize];
    let os: (&mut [u32], &mut [u32]) = (t1.0).split_at_mut(0usize);
    os.1[i as usize] = x
  };
  crate::lowstar::ignore::ignore::<u32>(c10);
  let c00: u32 = c0;
  let c010: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u32(len2, b1.0, b1.1, tmp_.1);
  let c1: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u32(len2, b1.1, b1.0, tmp_.0);
  for i in 0u32..len2
  {
    let x: u32 =
        0u32.wrapping_sub(c010) & tmp_.0[i as usize]
        |
        ! 0u32.wrapping_sub(c010) & tmp_.1[i as usize];
    let os: (&mut [u32], &mut [u32]) = (tmp_.0).split_at_mut(0usize);
    os.1[i as usize] = x
  };
  crate::lowstar::ignore::ignore::<u32>(c1);
  let c11: u32 = c010;
  let t23: (&mut [u32], &mut [u32]) = (tmp_.1).split_at_mut(0usize);
  let tmp1: (&mut [u32], &mut [u32]) =
      (t23.1).split_at_mut((aLen as usize).wrapping_add(aLen as usize) - aLen as usize);
  Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint32(len2, t1.0, tmp_.0, tmp1.1, tmp1.0);
  let r01: (&mut [u32], &mut [u32]) = res.split_at_mut(0usize);
  let r23: (&mut [u32], &mut [u32]) = (r01.1).split_at_mut(aLen as usize);
  Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint32(len2, a1.0, b1.0, tmp1.1, r23.0);
  Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint32(len2, a1.1, b1.1, tmp1.1, r23.1);
  crate::lowstar::ignore::ignore::<&[u32]>(res);
  crate::lowstar::ignore::ignore::<&[u32]>(tmp);
  let r011: (&[u32], &[u32]) = res.split_at(0usize);
  let r231: (&[u32], &[u32]) = (r011.1).split_at(aLen as usize);
  let t01: (&mut [u32], &mut [u32]) = tmp.split_at_mut(0usize);
  let t231: (&mut [u32], &mut [u32]) = (t01.1).split_at_mut(aLen as usize);
  let t45: (&mut [u32], &mut [u32]) =
      (t231.1).split_at_mut(2u32.wrapping_mul(aLen) as usize - aLen as usize);
  let t67: (&mut [u32], &mut [u32]) =
      (t45.1).split_at_mut(3u32.wrapping_mul(aLen) as usize - 2u32.wrapping_mul(aLen) as usize);
  let c2: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_add_eq_len_u32(aLen, r231.0, r231.1, t231.0);
  let c_sign: u32 = c00 ^ c11;
  let c3: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u32(aLen, t231.0, t45.0, t67.1);
  let c31: u32 = c2.wrapping_sub(c3);
  let c4: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_add_eq_len_u32(aLen, t231.0, t45.0, t67.0);
  let c41: u32 = c2.wrapping_add(c4);
  let mask: u32 = 0u32.wrapping_sub(c_sign);
  for i in 0u32..aLen
  {
    let x: u32 = mask & t67.0[i as usize] | ! mask & t67.1[i as usize];
    let os: (&mut [u32], &mut [u32]) = (t67.0).split_at_mut(0usize);
    os.1[i as usize] = x
  };
  let c5: u32 = mask & c41 | ! mask & c31;
  let aLen2: u32 = aLen.wrapping_div(2u32);
  crate::lowstar::ignore::ignore::<&[u32]>(res);
  let r0: (&mut [u32], &mut [u32]) = res.split_at_mut(aLen2 as usize);
  let mut a_copy: Box<[u32]> = vec![0u32; aLen as usize].into_boxed_slice();
  let mut b_copy: Box<[u32]> = vec![0u32; aLen as usize].into_boxed_slice();
  ((&mut a_copy)[0usize..aLen as usize]).copy_from_slice(&r0.1[0usize..aLen as usize]);
  ((&mut b_copy)[0usize..aLen as usize]).copy_from_slice(&t67.0[0usize..aLen as usize]);
  let r10: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_add_eq_len_u32(aLen, &a_copy, &b_copy, r0.1);
  let r11: u32 = r10;
  let c6: u32 = r11;
  let c60: u32 = c6;
  let c7: u32 = c5.wrapping_add(c60);
  crate::lowstar::ignore::ignore::<&[u32]>(res);
  let r: (&mut [u32], &mut [u32]) =
      res.split_at_mut((aLen as usize).wrapping_add(aLen2 as usize));
  let c01: u32 =
      crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(0u32, r.1[0usize], c7, r.1);
  let mut r1: u32;
  if 1u32 < aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2))
  {
    let res1: (&mut [u32], &mut [u32]) = (r.1).split_at_mut(1usize);
    let mut c: u32 = c01;
    for
    i
    in
    0u32..aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2)).wrapping_sub(1u32).wrapping_div(
      4u32
    )
    {
      let t11: u32 = res1.1[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u32], &mut [u32]) = (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t11, 0u32, res_i0.1);
      let t110: u32 = res1.1[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u32], &mut [u32]) =
          (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize + 1usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t110, 0u32, res_i1.1);
      let t111: u32 = res1.1[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u32], &mut [u32]) =
          (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize + 2usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t111, 0u32, res_i2.1);
      let t112: u32 = res1.1[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u32], &mut [u32]) =
          (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize + 3usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t112, 0u32, res_i.1)
    };
    for
    i
    in
    aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2)).wrapping_sub(1u32).wrapping_div(
      4u32
    ).wrapping_mul(4u32)..aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2)).wrapping_sub(
      1u32
    )
    {
      let t11: u32 = res1.1[i as usize];
      let res_i: (&mut [u32], &mut [u32]) = (res1.1).split_at_mut(i as usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t11, 0u32, res_i.1)
    };
    let c110: u32 = c;
    r1 = c110
  }
  else
  { r1 = c01 };
  let c8: u32 = r1;
  let c: u32 = c8;
  let c9: u32 = c;
  crate::lowstar::ignore::ignore::<u32>(c9)
}

pub fn Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64(
  aLen: u32,
  a: &[u64],
  b: &[u64],
  tmp: &mut [u64],
  res: &mut [u64]
)
{
  if aLen < 32u32 || aLen.wrapping_rem(2u32) == 1u32
  {
    crate::hacl::bignum_base::Hacl_Bignum_Multiplication_bn_mul_u64(aLen, a, aLen, b, res);
    return ()
  };
  let len2: u32 = aLen.wrapping_div(2u32);
  let a0: (&[u64], &[u64]) = a.split_at(0usize);
  let a1: (&[u64], &[u64]) = (a0.1).split_at(len2 as usize);
  let b0: (&[u64], &[u64]) = b.split_at(0usize);
  let b1: (&[u64], &[u64]) = (b0.1).split_at(len2 as usize);
  let t0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
  let t1: (&mut [u64], &mut [u64]) = (t0.1).split_at_mut(len2 as usize);
  let tmp_: (&mut [u64], &mut [u64]) = (t1.1).split_at_mut(aLen as usize - len2 as usize);
  let c0: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u64(len2, a1.0, a1.1, tmp_.1);
  let c10: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u64(len2, a1.1, a1.0, t1.0);
  for i in 0u32..len2
  {
    let x: u64 =
        0u64.wrapping_sub(c0) & t1.0[i as usize] | ! 0u64.wrapping_sub(c0) & tmp_.1[i as usize];
    let os: (&mut [u64], &mut [u64]) = (t1.0).split_at_mut(0usize);
    os.1[i as usize] = x
  };
  crate::lowstar::ignore::ignore::<u64>(c10);
  let c00: u64 = c0;
  let c010: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u64(len2, b1.0, b1.1, tmp_.1);
  let c1: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u64(len2, b1.1, b1.0, tmp_.0);
  for i in 0u32..len2
  {
    let x: u64 =
        0u64.wrapping_sub(c010) & tmp_.0[i as usize]
        |
        ! 0u64.wrapping_sub(c010) & tmp_.1[i as usize];
    let os: (&mut [u64], &mut [u64]) = (tmp_.0).split_at_mut(0usize);
    os.1[i as usize] = x
  };
  crate::lowstar::ignore::ignore::<u64>(c1);
  let c11: u64 = c010;
  let t23: (&mut [u64], &mut [u64]) = (tmp_.1).split_at_mut(0usize);
  let tmp1: (&mut [u64], &mut [u64]) =
      (t23.1).split_at_mut((aLen as usize).wrapping_add(aLen as usize) - aLen as usize);
  Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64(len2, t1.0, tmp_.0, tmp1.1, tmp1.0);
  let r01: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
  let r23: (&mut [u64], &mut [u64]) = (r01.1).split_at_mut(aLen as usize);
  Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64(len2, a1.0, b1.0, tmp1.1, r23.0);
  Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64(len2, a1.1, b1.1, tmp1.1, r23.1);
  crate::lowstar::ignore::ignore::<&[u64]>(res);
  crate::lowstar::ignore::ignore::<&[u64]>(tmp);
  let r011: (&[u64], &[u64]) = res.split_at(0usize);
  let r231: (&[u64], &[u64]) = (r011.1).split_at(aLen as usize);
  let t01: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
  let t231: (&mut [u64], &mut [u64]) = (t01.1).split_at_mut(aLen as usize);
  let t45: (&mut [u64], &mut [u64]) =
      (t231.1).split_at_mut(2u32.wrapping_mul(aLen) as usize - aLen as usize);
  let t67: (&mut [u64], &mut [u64]) =
      (t45.1).split_at_mut(3u32.wrapping_mul(aLen) as usize - 2u32.wrapping_mul(aLen) as usize);
  let c2: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_add_eq_len_u64(aLen, r231.0, r231.1, t231.0);
  let c_sign: u64 = c00 ^ c11;
  let c3: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u64(aLen, t231.0, t45.0, t67.1);
  let c31: u64 = c2.wrapping_sub(c3);
  let c4: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_add_eq_len_u64(aLen, t231.0, t45.0, t67.0);
  let c41: u64 = c2.wrapping_add(c4);
  let mask: u64 = 0u64.wrapping_sub(c_sign);
  for i in 0u32..aLen
  {
    let x: u64 = mask & t67.0[i as usize] | ! mask & t67.1[i as usize];
    let os: (&mut [u64], &mut [u64]) = (t67.0).split_at_mut(0usize);
    os.1[i as usize] = x
  };
  let c5: u64 = mask & c41 | ! mask & c31;
  let aLen2: u32 = aLen.wrapping_div(2u32);
  crate::lowstar::ignore::ignore::<&[u64]>(res);
  let r0: (&mut [u64], &mut [u64]) = res.split_at_mut(aLen2 as usize);
  let mut a_copy: Box<[u64]> = vec![0u64; aLen as usize].into_boxed_slice();
  let mut b_copy: Box<[u64]> = vec![0u64; aLen as usize].into_boxed_slice();
  ((&mut a_copy)[0usize..aLen as usize]).copy_from_slice(&r0.1[0usize..aLen as usize]);
  ((&mut b_copy)[0usize..aLen as usize]).copy_from_slice(&t67.0[0usize..aLen as usize]);
  let r10: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_add_eq_len_u64(aLen, &a_copy, &b_copy, r0.1);
  let r11: u64 = r10;
  let c6: u64 = r11;
  let c60: u64 = c6;
  let c7: u64 = c5.wrapping_add(c60);
  crate::lowstar::ignore::ignore::<&[u64]>(res);
  let r: (&mut [u64], &mut [u64]) =
      res.split_at_mut((aLen as usize).wrapping_add(aLen2 as usize));
  let c01: u64 =
      crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(0u64, r.1[0usize], c7, r.1);
  let mut r1: u64;
  if 1u32 < aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2))
  {
    let res1: (&mut [u64], &mut [u64]) = (r.1).split_at_mut(1usize);
    let mut c: u64 = c01;
    for
    i
    in
    0u32..aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2)).wrapping_sub(1u32).wrapping_div(
      4u32
    )
    {
      let t11: u64 = res1.1[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u64], &mut [u64]) = (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t11, 0u64, res_i0.1);
      let t110: u64 = res1.1[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) =
          (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize + 1usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t110, 0u64, res_i1.1);
      let t111: u64 = res1.1[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) =
          (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize + 2usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t111, 0u64, res_i2.1);
      let t112: u64 = res1.1[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) =
          (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize + 3usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t112, 0u64, res_i.1)
    };
    for
    i
    in
    aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2)).wrapping_sub(1u32).wrapping_div(
      4u32
    ).wrapping_mul(4u32)..aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2)).wrapping_sub(
      1u32
    )
    {
      let t11: u64 = res1.1[i as usize];
      let res_i: (&mut [u64], &mut [u64]) = (res1.1).split_at_mut(i as usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t11, 0u64, res_i.1)
    };
    let c110: u64 = c;
    r1 = c110
  }
  else
  { r1 = c01 };
  let c8: u64 = r1;
  let c: u64 = c8;
  let c9: u64 = c;
  crate::lowstar::ignore::ignore::<u64>(c9)
}

pub fn Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint32(
  aLen: u32,
  a: &[u32],
  tmp: &mut [u32],
  res: &mut [u32]
)
{
  if aLen < 32u32 || aLen.wrapping_rem(2u32) == 1u32
  {
    crate::hacl::bignum_base::Hacl_Bignum_Multiplication_bn_sqr_u32(aLen, a, res);
    return ()
  };
  let len2: u32 = aLen.wrapping_div(2u32);
  let a0: (&[u32], &[u32]) = a.split_at(0usize);
  let a1: (&[u32], &[u32]) = (a0.1).split_at(len2 as usize);
  let t0: (&mut [u32], &mut [u32]) = tmp.split_at_mut(0usize);
  let tmp_: (&mut [u32], &mut [u32]) = (t0.1).split_at_mut(aLen as usize);
  let c0: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u32(len2, a1.0, a1.1, tmp_.1);
  let c1: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u32(len2, a1.1, a1.0, tmp_.0);
  for i in 0u32..len2
  {
    let x: u32 =
        0u32.wrapping_sub(c0) & tmp_.0[i as usize] | ! 0u32.wrapping_sub(c0) & tmp_.1[i as usize];
    let os: (&mut [u32], &mut [u32]) = (tmp_.0).split_at_mut(0usize);
    os.1[i as usize] = x
  };
  crate::lowstar::ignore::ignore::<u32>(c1);
  let c00: u32 = c0;
  crate::lowstar::ignore::ignore::<u32>(c00);
  let t23: (&mut [u32], &mut [u32]) = (tmp_.1).split_at_mut(0usize);
  let tmp1: (&mut [u32], &mut [u32]) =
      (t23.1).split_at_mut((aLen as usize).wrapping_add(aLen as usize) - aLen as usize);
  Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint32(len2, tmp_.0, tmp1.1, tmp1.0);
  let r01: (&mut [u32], &mut [u32]) = res.split_at_mut(0usize);
  let r23: (&mut [u32], &mut [u32]) = (r01.1).split_at_mut(aLen as usize);
  Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint32(len2, a1.0, tmp1.1, r23.0);
  Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint32(len2, a1.1, tmp1.1, r23.1);
  crate::lowstar::ignore::ignore::<&[u32]>(res);
  crate::lowstar::ignore::ignore::<&[u32]>(tmp);
  let r011: (&[u32], &[u32]) = res.split_at(0usize);
  let r231: (&[u32], &[u32]) = (r011.1).split_at(aLen as usize);
  let t01: (&mut [u32], &mut [u32]) = tmp.split_at_mut(0usize);
  let t231: (&mut [u32], &mut [u32]) = (t01.1).split_at_mut(aLen as usize);
  let t45: (&mut [u32], &mut [u32]) =
      (t231.1).split_at_mut(2u32.wrapping_mul(aLen) as usize - aLen as usize);
  let c2: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_add_eq_len_u32(aLen, r231.0, r231.1, t231.0);
  let c3: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u32(aLen, t231.0, t45.0, t45.1);
  let c5: u32 = c2.wrapping_sub(c3);
  let aLen2: u32 = aLen.wrapping_div(2u32);
  crate::lowstar::ignore::ignore::<&[u32]>(res);
  let r0: (&mut [u32], &mut [u32]) = res.split_at_mut(aLen2 as usize);
  let mut a_copy: Box<[u32]> = vec![0u32; aLen as usize].into_boxed_slice();
  let mut b_copy: Box<[u32]> = vec![0u32; aLen as usize].into_boxed_slice();
  ((&mut a_copy)[0usize..aLen as usize]).copy_from_slice(&r0.1[0usize..aLen as usize]);
  ((&mut b_copy)[0usize..aLen as usize]).copy_from_slice(&t45.1[0usize..aLen as usize]);
  let r10: u32 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_add_eq_len_u32(aLen, &a_copy, &b_copy, r0.1);
  let r11: u32 = r10;
  let c4: u32 = r11;
  let c6: u32 = c4;
  let c7: u32 = c5.wrapping_add(c6);
  crate::lowstar::ignore::ignore::<&[u32]>(res);
  let r: (&mut [u32], &mut [u32]) =
      res.split_at_mut((aLen as usize).wrapping_add(aLen2 as usize));
  let c01: u32 =
      crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(0u32, r.1[0usize], c7, r.1);
  let mut r1: u32;
  if 1u32 < aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2))
  {
    let res1: (&mut [u32], &mut [u32]) = (r.1).split_at_mut(1usize);
    let mut c: u32 = c01;
    for
    i
    in
    0u32..aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2)).wrapping_sub(1u32).wrapping_div(
      4u32
    )
    {
      let t1: u32 = res1.1[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u32], &mut [u32]) = (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t1, 0u32, res_i0.1);
      let t10: u32 = res1.1[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u32], &mut [u32]) =
          (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize + 1usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t10, 0u32, res_i1.1);
      let t11: u32 = res1.1[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u32], &mut [u32]) =
          (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize + 2usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t11, 0u32, res_i2.1);
      let t12: u32 = res1.1[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u32], &mut [u32]) =
          (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize + 3usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t12, 0u32, res_i.1)
    };
    for
    i
    in
    aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2)).wrapping_sub(1u32).wrapping_div(
      4u32
    ).wrapping_mul(4u32)..aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2)).wrapping_sub(
      1u32
    )
    {
      let t1: u32 = res1.1[i as usize];
      let res_i: (&mut [u32], &mut [u32]) = (res1.1).split_at_mut(i as usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t1, 0u32, res_i.1)
    };
    let c10: u32 = c;
    r1 = c10
  }
  else
  { r1 = c01 };
  let c8: u32 = r1;
  let c: u32 = c8;
  let c9: u32 = c;
  crate::lowstar::ignore::ignore::<u32>(c9)
}

pub fn Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint64(
  aLen: u32,
  a: &[u64],
  tmp: &mut [u64],
  res: &mut [u64]
)
{
  if aLen < 32u32 || aLen.wrapping_rem(2u32) == 1u32
  {
    crate::hacl::bignum_base::Hacl_Bignum_Multiplication_bn_sqr_u64(aLen, a, res);
    return ()
  };
  let len2: u32 = aLen.wrapping_div(2u32);
  let a0: (&[u64], &[u64]) = a.split_at(0usize);
  let a1: (&[u64], &[u64]) = (a0.1).split_at(len2 as usize);
  let t0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
  let tmp_: (&mut [u64], &mut [u64]) = (t0.1).split_at_mut(aLen as usize);
  let c0: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u64(len2, a1.0, a1.1, tmp_.1);
  let c1: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u64(len2, a1.1, a1.0, tmp_.0);
  for i in 0u32..len2
  {
    let x: u64 =
        0u64.wrapping_sub(c0) & tmp_.0[i as usize] | ! 0u64.wrapping_sub(c0) & tmp_.1[i as usize];
    let os: (&mut [u64], &mut [u64]) = (tmp_.0).split_at_mut(0usize);
    os.1[i as usize] = x
  };
  crate::lowstar::ignore::ignore::<u64>(c1);
  let c00: u64 = c0;
  crate::lowstar::ignore::ignore::<u64>(c00);
  let t23: (&mut [u64], &mut [u64]) = (tmp_.1).split_at_mut(0usize);
  let tmp1: (&mut [u64], &mut [u64]) =
      (t23.1).split_at_mut((aLen as usize).wrapping_add(aLen as usize) - aLen as usize);
  Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint64(len2, tmp_.0, tmp1.1, tmp1.0);
  let r01: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
  let r23: (&mut [u64], &mut [u64]) = (r01.1).split_at_mut(aLen as usize);
  Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint64(len2, a1.0, tmp1.1, r23.0);
  Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint64(len2, a1.1, tmp1.1, r23.1);
  crate::lowstar::ignore::ignore::<&[u64]>(res);
  crate::lowstar::ignore::ignore::<&[u64]>(tmp);
  let r011: (&[u64], &[u64]) = res.split_at(0usize);
  let r231: (&[u64], &[u64]) = (r011.1).split_at(aLen as usize);
  let t01: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
  let t231: (&mut [u64], &mut [u64]) = (t01.1).split_at_mut(aLen as usize);
  let t45: (&mut [u64], &mut [u64]) =
      (t231.1).split_at_mut(2u32.wrapping_mul(aLen) as usize - aLen as usize);
  let c2: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_add_eq_len_u64(aLen, r231.0, r231.1, t231.0);
  let c3: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_sub_eq_len_u64(aLen, t231.0, t45.0, t45.1);
  let c5: u64 = c2.wrapping_sub(c3);
  let aLen2: u32 = aLen.wrapping_div(2u32);
  crate::lowstar::ignore::ignore::<&[u64]>(res);
  let r0: (&mut [u64], &mut [u64]) = res.split_at_mut(aLen2 as usize);
  let mut a_copy: Box<[u64]> = vec![0u64; aLen as usize].into_boxed_slice();
  let mut b_copy: Box<[u64]> = vec![0u64; aLen as usize].into_boxed_slice();
  ((&mut a_copy)[0usize..aLen as usize]).copy_from_slice(&r0.1[0usize..aLen as usize]);
  ((&mut b_copy)[0usize..aLen as usize]).copy_from_slice(&t45.1[0usize..aLen as usize]);
  let r10: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_add_eq_len_u64(aLen, &a_copy, &b_copy, r0.1);
  let r11: u64 = r10;
  let c4: u64 = r11;
  let c6: u64 = c4;
  let c7: u64 = c5.wrapping_add(c6);
  crate::lowstar::ignore::ignore::<&[u64]>(res);
  let r: (&mut [u64], &mut [u64]) =
      res.split_at_mut((aLen as usize).wrapping_add(aLen2 as usize));
  let c01: u64 =
      crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(0u64, r.1[0usize], c7, r.1);
  let mut r1: u64;
  if 1u32 < aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2))
  {
    let res1: (&mut [u64], &mut [u64]) = (r.1).split_at_mut(1usize);
    let mut c: u64 = c01;
    for
    i
    in
    0u32..aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2)).wrapping_sub(1u32).wrapping_div(
      4u32
    )
    {
      let t1: u64 = res1.1[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u64], &mut [u64]) = (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t1, 0u64, res_i0.1);
      let t10: u64 = res1.1[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) =
          (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize + 1usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t10, 0u64, res_i1.1);
      let t11: u64 = res1.1[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) =
          (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize + 2usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t11, 0u64, res_i2.1);
      let t12: u64 = res1.1[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) =
          (res1.1).split_at_mut(4u32.wrapping_mul(i) as usize + 3usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t12, 0u64, res_i.1)
    };
    for
    i
    in
    aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2)).wrapping_sub(1u32).wrapping_div(
      4u32
    ).wrapping_mul(4u32)..aLen.wrapping_add(aLen).wrapping_sub(aLen.wrapping_add(aLen2)).wrapping_sub(
      1u32
    )
    {
      let t1: u64 = res1.1[i as usize];
      let res_i: (&mut [u64], &mut [u64]) = (res1.1).split_at_mut(i as usize);
      c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t1, 0u64, res_i.1)
    };
    let c10: u64 = c;
    r1 = c10
  }
  else
  { r1 = c01 };
  let c8: u64 = r1;
  let c: u64 = c8;
  let c9: u64 = c;
  crate::lowstar::ignore::ignore::<u64>(c9)
}

pub fn Hacl_Bignum_ModInvLimb_mod_inv_uint32(n0: u32) -> u32
{
  let alpha: u32 = 2147483648u32;
  let beta: u32 = n0;
  let mut ub: u32 = 0u32;
  let mut vb: u32 = 0u32;
  ub = 1u32;
  vb = 0u32;
  for i in 0u32..32u32
  {
    let us: u32 = ub;
    let vs: u32 = vb;
    let u_is_odd: u32 = 0u32.wrapping_sub(us & 1u32);
    let beta_if_u_is_odd: u32 = beta & u_is_odd;
    ub = (us ^ beta_if_u_is_odd).wrapping_shr(1u32).wrapping_add(us & beta_if_u_is_odd);
    let alpha_if_u_is_odd: u32 = alpha & u_is_odd;
    vb = vs.wrapping_shr(1u32).wrapping_add(alpha_if_u_is_odd)
  };
  return vb
}

pub fn Hacl_Bignum_ModInvLimb_mod_inv_uint64(n0: u64) -> u64
{
  let alpha: u64 = 9223372036854775808u64;
  let beta: u64 = n0;
  let mut ub: u64 = 0u64;
  let mut vb: u64 = 0u64;
  ub = 1u64;
  vb = 0u64;
  for i in 0u32..64u32
  {
    let us: u64 = ub;
    let vs: u64 = vb;
    let u_is_odd: u64 = 0u64.wrapping_sub(us & 1u64);
    let beta_if_u_is_odd: u64 = beta & u_is_odd;
    ub = (us ^ beta_if_u_is_odd).wrapping_shr(1u32).wrapping_add(us & beta_if_u_is_odd);
    let alpha_if_u_is_odd: u64 = alpha & u_is_odd;
    vb = vs.wrapping_shr(1u32).wrapping_add(alpha_if_u_is_odd)
  };
  return vb
}

#[derive(PartialEq, Clone)]
#[repr(C)]
pub
struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u32
{ pub len: u32, pub n: Box<[u32]>, pub mu: u32, pub r2: Box<[u32]> }

#[derive(PartialEq, Clone)]
#[repr(C)]
pub
struct Hacl_Bignum_MontArithmetic_bn_mont_ctx_u64
{ pub len: u32, pub n: Box<[u64]>, pub mu: u64, pub r2: Box<[u64]> }

pub fn Hacl_Bignum_Montgomery_bn_check_modulus_u32(len: u32, n: &[u32]) -> u32
{
  let mut one: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  ((&mut one)[0usize..len as usize]).copy_from_slice(
    &vec![0u32; len as usize].into_boxed_slice()
  );
  (&mut one)[0usize] = 1u32;
  let bit0: u32 = n[0usize] & 1u32;
  let m0: u32 = 0u32.wrapping_sub(bit0);
  let mut acc: u32 = 0u32;
  for i in 0u32..len
  {
    let beq: u32 = crate::hacl_krmllib::FStar_UInt32_eq_mask((&one)[i as usize], n[i as usize]);
    let blt: u32 = ! crate::hacl_krmllib::FStar_UInt32_gte_mask((&one)[i as usize], n[i as usize]);
    acc = beq & acc | ! beq & blt
  };
  let m1: u32 = acc;
  return m0 & m1
}

pub fn Hacl_Bignum_Montgomery_bn_check_modulus_u64(len: u32, n: &[u64]) -> u64
{
  let mut one: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  ((&mut one)[0usize..len as usize]).copy_from_slice(
    &vec![0u64; len as usize].into_boxed_slice()
  );
  (&mut one)[0usize] = 1u64;
  let bit0: u64 = n[0usize] & 1u64;
  let m0: u64 = 0u64.wrapping_sub(bit0);
  let mut acc: u64 = 0u64;
  for i in 0u32..len
  {
    let beq: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask((&one)[i as usize], n[i as usize]);
    let blt: u64 = ! crate::hacl_krmllib::FStar_UInt64_gte_mask((&one)[i as usize], n[i as usize]);
    acc = beq & acc | ! beq & blt
  };
  let m1: u64 = acc;
  return m0 & m1
}

pub fn Hacl_Bignum_Montgomery_bn_from_mont_u32(
  len: u32,
  n: &[u32],
  nInv_u64: u32,
  aM: &[u32],
  a: &mut [u32]
)
{
  let mut tmp: Box<[u32]> = vec![0u32; len.wrapping_add(len) as usize].into_boxed_slice();
  ((&mut tmp)[0usize..len as usize]).copy_from_slice(&aM[0usize..len as usize]);
  bn_mont_reduction_u32(len, n, nInv_u64, &mut tmp, a)
}

pub fn Hacl_Bignum_Montgomery_bn_from_mont_u64(
  len: u32,
  n: &[u64],
  nInv_u64: u64,
  aM: &[u64],
  a: &mut [u64]
)
{
  let mut tmp: Box<[u64]> = vec![0u64; len.wrapping_add(len) as usize].into_boxed_slice();
  ((&mut tmp)[0usize..len as usize]).copy_from_slice(&aM[0usize..len as usize]);
  bn_mont_reduction_u64(len, n, nInv_u64, &mut tmp, a)
}

pub fn Hacl_Bignum_Montgomery_bn_mont_mul_u32(
  len: u32,
  n: &[u32],
  nInv_u64: u32,
  aM: &[u32],
  bM: &[u32],
  resM: &mut [u32]
)
{
  let mut c: Box<[u32]> = vec![0u32; len.wrapping_add(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u32]> = vec![0u32; 4u32.wrapping_mul(len) as usize].into_boxed_slice();
  Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint32(len, aM, bM, &mut tmp, &mut c);
  bn_mont_reduction_u32(len, n, nInv_u64, &mut c, resM)
}

pub fn Hacl_Bignum_Montgomery_bn_mont_mul_u64(
  len: u32,
  n: &[u64],
  nInv_u64: u64,
  aM: &[u64],
  bM: &[u64],
  resM: &mut [u64]
)
{
  let mut c: Box<[u64]> = vec![0u64; len.wrapping_add(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u64]> = vec![0u64; 4u32.wrapping_mul(len) as usize].into_boxed_slice();
  Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64(len, aM, bM, &mut tmp, &mut c);
  bn_mont_reduction_u64(len, n, nInv_u64, &mut c, resM)
}

pub fn Hacl_Bignum_Montgomery_bn_mont_sqr_u32(
  len: u32,
  n: &[u32],
  nInv_u64: u32,
  aM: &[u32],
  resM: &mut [u32]
)
{
  let mut c: Box<[u32]> = vec![0u32; len.wrapping_add(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u32]> = vec![0u32; 4u32.wrapping_mul(len) as usize].into_boxed_slice();
  Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint32(len, aM, &mut tmp, &mut c);
  bn_mont_reduction_u32(len, n, nInv_u64, &mut c, resM)
}

pub fn Hacl_Bignum_Montgomery_bn_mont_sqr_u64(
  len: u32,
  n: &[u64],
  nInv_u64: u64,
  aM: &[u64],
  resM: &mut [u64]
)
{
  let mut c: Box<[u64]> = vec![0u64; len.wrapping_add(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u64]> = vec![0u64; 4u32.wrapping_mul(len) as usize].into_boxed_slice();
  Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint64(len, aM, &mut tmp, &mut c);
  bn_mont_reduction_u64(len, n, nInv_u64, &mut c, resM)
}

pub fn Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u32(
  len: u32,
  nBits: u32,
  n: &[u32],
  res: &mut [u32]
)
{
  (res[0usize..len as usize]).copy_from_slice(&vec![0u32; len as usize].into_boxed_slice());
  let i: u32 = nBits.wrapping_div(32u32);
  let j: u32 = nBits.wrapping_rem(32u32);
  res[i as usize] |= 1u32.wrapping_shl(j);
  for i0 in 0u32..64u32.wrapping_mul(len).wrapping_sub(nBits)
  {
    let mut a_copy: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
    let mut b_copy: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
    ((&mut a_copy)[0usize..len as usize]).copy_from_slice(&res[0usize..len as usize]);
    ((&mut b_copy)[0usize..len as usize]).copy_from_slice(&res[0usize..len as usize]);
    Hacl_Bignum_bn_add_mod_n_u32(len, n, &a_copy, &b_copy, res)
  }
}

pub fn Hacl_Bignum_Montgomery_bn_precomp_r2_mod_n_u64(
  len: u32,
  nBits: u32,
  n: &[u64],
  res: &mut [u64]
)
{
  (res[0usize..len as usize]).copy_from_slice(&vec![0u64; len as usize].into_boxed_slice());
  let i: u32 = nBits.wrapping_div(64u32);
  let j: u32 = nBits.wrapping_rem(64u32);
  res[i as usize] |= 1u64.wrapping_shl(j);
  for i0 in 0u32..128u32.wrapping_mul(len).wrapping_sub(nBits)
  {
    let mut a_copy: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
    let mut b_copy: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
    ((&mut a_copy)[0usize..len as usize]).copy_from_slice(&res[0usize..len as usize]);
    ((&mut b_copy)[0usize..len as usize]).copy_from_slice(&res[0usize..len as usize]);
    Hacl_Bignum_bn_add_mod_n_u64(len, n, &a_copy, &b_copy, res)
  }
}

pub fn Hacl_Bignum_Montgomery_bn_to_mont_u32(
  len: u32,
  n: &[u32],
  nInv: u32,
  r2: &[u32],
  a: &[u32],
  aM: &mut [u32]
)
{
  let mut c: Box<[u32]> = vec![0u32; len.wrapping_add(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u32]> = vec![0u32; 4u32.wrapping_mul(len) as usize].into_boxed_slice();
  Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint32(len, a, r2, &mut tmp, &mut c);
  bn_mont_reduction_u32(len, n, nInv, &mut c, aM)
}

pub fn Hacl_Bignum_Montgomery_bn_to_mont_u64(
  len: u32,
  n: &[u64],
  nInv: u64,
  r2: &[u64],
  a: &[u64],
  aM: &mut [u64]
)
{
  let mut c: Box<[u64]> = vec![0u64; len.wrapping_add(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u64]> = vec![0u64; 4u32.wrapping_mul(len) as usize].into_boxed_slice();
  Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64(len, a, r2, &mut tmp, &mut c);
  bn_mont_reduction_u64(len, n, nInv, &mut c, aM)
}

pub fn Hacl_Bignum_bn_add_mod_n_u32(
  len1: u32,
  n: &[u32],
  a: &[u32],
  b: &[u32],
  res: &mut [u32]
)
{
  let mut c0: u32 = 0u32;
  for i in 0u32..len1.wrapping_div(4u32)
  {
    let t1: u32 = a[4u32.wrapping_mul(i) as usize];
    let t20: u32 = b[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u32], &mut [u32]) = res.split_at_mut(4u32.wrapping_mul(i) as usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c0, t1, t20, res_i0.1);
    let t10: u32 = a[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u32 = b[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u32], &mut [u32]) = (res_i0.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c0, t10, t21, res_i1.1);
    let t11: u32 = a[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u32 = b[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u32], &mut [u32]) = (res_i1.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c0, t11, t22, res_i2.1);
    let t12: u32 = a[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u32 = b[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u32], &mut [u32]) = (res_i2.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c0, t12, t2, res_i.1)
  };
  for i in len1.wrapping_div(4u32).wrapping_mul(4u32)..len1
  {
    let t1: u32 = a[i as usize];
    let t2: u32 = b[i as usize];
    let res_i: (&mut [u32], &mut [u32]) = res.split_at_mut(i as usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c0, t1, t2, res_i.1)
  };
  let c00: u32 = c0;
  let mut tmp: Box<[u32]> = vec![0u32; len1 as usize].into_boxed_slice();
  let mut c: u32 = 0u32;
  for i in 0u32..len1.wrapping_div(4u32)
  {
    let t1: u32 = res[4u32.wrapping_mul(i) as usize];
    let t20: u32 = n[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u32], &mut [u32]) = tmp.split_at_mut(4u32.wrapping_mul(i) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c, t1, t20, res_i0.1);
    let t10: u32 = res[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u32 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u32], &mut [u32]) = (res_i0.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c, t10, t21, res_i1.1);
    let t11: u32 = res[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u32 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u32], &mut [u32]) = (res_i1.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c, t11, t22, res_i2.1);
    let t12: u32 = res[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u32 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u32], &mut [u32]) = (res_i2.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c, t12, t2, res_i.1)
  };
  for i in len1.wrapping_div(4u32).wrapping_mul(4u32)..len1
  {
    let t1: u32 = res[i as usize];
    let t2: u32 = n[i as usize];
    let res_i: (&mut [u32], &mut [u32]) = tmp.split_at_mut(i as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c, t1, t2, res_i.1)
  };
  let c1: u32 = c;
  let c2: u32 = c00.wrapping_sub(c1);
  for i in 0u32..len1
  {
    let x: u32 = c2 & res[i as usize] | ! c2 & (&tmp)[i as usize];
    let os: (&mut [u32], &mut [u32]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn Hacl_Bignum_bn_add_mod_n_u64(
  len1: u32,
  n: &[u64],
  a: &[u64],
  b: &[u64],
  res: &mut [u64]
)
{
  let mut c0: u64 = 0u64;
  for i in 0u32..len1.wrapping_div(4u32)
  {
    let t1: u64 = a[4u32.wrapping_mul(i) as usize];
    let t20: u64 = b[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = res.split_at_mut(4u32.wrapping_mul(i) as usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t1, t20, res_i0.1);
    let t10: u64 = a[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u64 = b[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t10, t21, res_i1.1);
    let t11: u64 = a[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u64 = b[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t11, t22, res_i2.1);
    let t12: u64 = a[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u64 = b[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t12, t2, res_i.1)
  };
  for i in len1.wrapping_div(4u32).wrapping_mul(4u32)..len1
  {
    let t1: u64 = a[i as usize];
    let t2: u64 = b[i as usize];
    let res_i: (&mut [u64], &mut [u64]) = res.split_at_mut(i as usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t1, t2, res_i.1)
  };
  let c00: u64 = c0;
  let mut tmp: Box<[u64]> = vec![0u64; len1 as usize].into_boxed_slice();
  let mut c: u64 = 0u64;
  for i in 0u32..len1.wrapping_div(4u32)
  {
    let t1: u64 = res[4u32.wrapping_mul(i) as usize];
    let t20: u64 = n[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(4u32.wrapping_mul(i) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, t20, res_i0.1);
    let t10: u64 = res[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u64 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t10, t21, res_i1.1);
    let t11: u64 = res[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u64 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t11, t22, res_i2.1);
    let t12: u64 = res[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u64 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t12, t2, res_i.1)
  };
  for i in len1.wrapping_div(4u32).wrapping_mul(4u32)..len1
  {
    let t1: u64 = res[i as usize];
    let t2: u64 = n[i as usize];
    let res_i: (&mut [u64], &mut [u64]) = tmp.split_at_mut(i as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, t2, res_i.1)
  };
  let c1: u64 = c;
  let c2: u64 = c00.wrapping_sub(c1);
  for i in 0u32..len1
  {
    let x: u64 = c2 & res[i as usize] | ! c2 & (&tmp)[i as usize];
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn Hacl_Bignum_bn_sub_mod_n_u32(
  len1: u32,
  n: &[u32],
  a: &[u32],
  b: &[u32],
  res: &mut [u32]
)
{
  let mut c0: u32 = 0u32;
  for i in 0u32..len1.wrapping_div(4u32)
  {
    let t1: u32 = a[4u32.wrapping_mul(i) as usize];
    let t20: u32 = b[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u32], &mut [u32]) = res.split_at_mut(4u32.wrapping_mul(i) as usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c0, t1, t20, res_i0.1);
    let t10: u32 = a[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u32 = b[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u32], &mut [u32]) = (res_i0.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c0, t10, t21, res_i1.1);
    let t11: u32 = a[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u32 = b[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u32], &mut [u32]) = (res_i1.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c0, t11, t22, res_i2.1);
    let t12: u32 = a[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u32 = b[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u32], &mut [u32]) = (res_i2.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c0, t12, t2, res_i.1)
  };
  for i in len1.wrapping_div(4u32).wrapping_mul(4u32)..len1
  {
    let t1: u32 = a[i as usize];
    let t2: u32 = b[i as usize];
    let res_i: (&mut [u32], &mut [u32]) = res.split_at_mut(i as usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c0, t1, t2, res_i.1)
  };
  let c00: u32 = c0;
  let mut tmp: Box<[u32]> = vec![0u32; len1 as usize].into_boxed_slice();
  let mut c: u32 = 0u32;
  for i in 0u32..len1.wrapping_div(4u32)
  {
    let t1: u32 = res[4u32.wrapping_mul(i) as usize];
    let t20: u32 = n[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u32], &mut [u32]) = tmp.split_at_mut(4u32.wrapping_mul(i) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t1, t20, res_i0.1);
    let t10: u32 = res[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u32 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u32], &mut [u32]) = (res_i0.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t10, t21, res_i1.1);
    let t11: u32 = res[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u32 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u32], &mut [u32]) = (res_i1.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t11, t22, res_i2.1);
    let t12: u32 = res[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u32 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u32], &mut [u32]) = (res_i2.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t12, t2, res_i.1)
  };
  for i in len1.wrapping_div(4u32).wrapping_mul(4u32)..len1
  {
    let t1: u32 = res[i as usize];
    let t2: u32 = n[i as usize];
    let res_i: (&mut [u32], &mut [u32]) = tmp.split_at_mut(i as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c, t1, t2, res_i.1)
  };
  let c1: u32 = c;
  crate::lowstar::ignore::ignore::<u32>(c1);
  let c2: u32 = 0u32.wrapping_sub(c00);
  for i in 0u32..len1
  {
    let x: u32 = c2 & (&tmp)[i as usize] | ! c2 & res[i as usize];
    let os: (&mut [u32], &mut [u32]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn Hacl_Bignum_bn_sub_mod_n_u64(
  len1: u32,
  n: &[u64],
  a: &[u64],
  b: &[u64],
  res: &mut [u64]
)
{
  let mut c0: u64 = 0u64;
  for i in 0u32..len1.wrapping_div(4u32)
  {
    let t1: u64 = a[4u32.wrapping_mul(i) as usize];
    let t20: u64 = b[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = res.split_at_mut(4u32.wrapping_mul(i) as usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t1, t20, res_i0.1);
    let t10: u64 = a[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u64 = b[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t10, t21, res_i1.1);
    let t11: u64 = a[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u64 = b[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t11, t22, res_i2.1);
    let t12: u64 = a[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u64 = b[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t12, t2, res_i.1)
  };
  for i in len1.wrapping_div(4u32).wrapping_mul(4u32)..len1
  {
    let t1: u64 = a[i as usize];
    let t2: u64 = b[i as usize];
    let res_i: (&mut [u64], &mut [u64]) = res.split_at_mut(i as usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t1, t2, res_i.1)
  };
  let c00: u64 = c0;
  let mut tmp: Box<[u64]> = vec![0u64; len1 as usize].into_boxed_slice();
  let mut c: u64 = 0u64;
  for i in 0u32..len1.wrapping_div(4u32)
  {
    let t1: u64 = res[4u32.wrapping_mul(i) as usize];
    let t20: u64 = n[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(4u32.wrapping_mul(i) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t1, t20, res_i0.1);
    let t10: u64 = res[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u64 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t10, t21, res_i1.1);
    let t11: u64 = res[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u64 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t11, t22, res_i2.1);
    let t12: u64 = res[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u64 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t12, t2, res_i.1)
  };
  for i in len1.wrapping_div(4u32).wrapping_mul(4u32)..len1
  {
    let t1: u64 = res[i as usize];
    let t2: u64 = n[i as usize];
    let res_i: (&mut [u64], &mut [u64]) = tmp.split_at_mut(i as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t1, t2, res_i.1)
  };
  let c1: u64 = c;
  crate::lowstar::ignore::ignore::<u64>(c1);
  let c2: u64 = 0u64.wrapping_sub(c00);
  for i in 0u32..len1
  {
    let x: u64 = c2 & (&tmp)[i as usize] | ! c2 & res[i as usize];
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn bn_almost_mont_mul_u32(
  len: u32,
  n: &[u32],
  nInv_u64: u32,
  aM: &[u32],
  bM: &[u32],
  resM: &mut [u32]
)
{
  let mut c: Box<[u32]> = vec![0u32; len.wrapping_add(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u32]> = vec![0u32; 4u32.wrapping_mul(len) as usize].into_boxed_slice();
  Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint32(len, aM, bM, &mut tmp, &mut c);
  Hacl_Bignum_AlmostMontgomery_bn_almost_mont_reduction_u32(len, n, nInv_u64, &mut c, resM)
}

pub fn bn_almost_mont_mul_u64(
  len: u32,
  n: &[u64],
  nInv_u64: u64,
  aM: &[u64],
  bM: &[u64],
  resM: &mut [u64]
)
{
  let mut c: Box<[u64]> = vec![0u64; len.wrapping_add(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u64]> = vec![0u64; 4u32.wrapping_mul(len) as usize].into_boxed_slice();
  Hacl_Bignum_Karatsuba_bn_karatsuba_mul_uint64(len, aM, bM, &mut tmp, &mut c);
  Hacl_Bignum_AlmostMontgomery_bn_almost_mont_reduction_u64(len, n, nInv_u64, &mut c, resM)
}

pub fn bn_almost_mont_sqr_u32(len: u32, n: &[u32], nInv_u64: u32, aM: &[u32], resM: &mut [u32])
{
  let mut c: Box<[u32]> = vec![0u32; len.wrapping_add(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u32]> = vec![0u32; 4u32.wrapping_mul(len) as usize].into_boxed_slice();
  Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint32(len, aM, &mut tmp, &mut c);
  Hacl_Bignum_AlmostMontgomery_bn_almost_mont_reduction_u32(len, n, nInv_u64, &mut c, resM)
}

pub fn bn_almost_mont_sqr_u64(len: u32, n: &[u64], nInv_u64: u64, aM: &[u64], resM: &mut [u64])
{
  let mut c: Box<[u64]> = vec![0u64; len.wrapping_add(len) as usize].into_boxed_slice();
  let mut tmp: Box<[u64]> = vec![0u64; 4u32.wrapping_mul(len) as usize].into_boxed_slice();
  Hacl_Bignum_Karatsuba_bn_karatsuba_sqr_uint64(len, aM, &mut tmp, &mut c);
  Hacl_Bignum_AlmostMontgomery_bn_almost_mont_reduction_u64(len, n, nInv_u64, &mut c, resM)
}

pub fn bn_mont_reduction_u32(len: u32, n: &[u32], nInv: u32, c: &mut [u32], res: &mut [u32])
{
  let mut c0: u32 = 0u32;
  for i0 in 0u32..len
  {
    let qj: u32 = nInv.wrapping_mul(c[i0 as usize]);
    let res_j0: (&mut [u32], &mut [u32]) = c.split_at_mut(i0 as usize);
    let mut c1: u32 = 0u32;
    for i in 0u32..len.wrapping_div(4u32)
    {
      let a_i: u32 = n[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u32], &mut [u32]) = (res_j0.1).split_at_mut(4u32.wrapping_mul(i) as usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u32(a_i, qj, c1, res_i0.1);
      let a_i0: u32 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u32], &mut [u32]) = (res_i0.1).split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u32(a_i0, qj, c1, res_i1.1);
      let a_i1: u32 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u32], &mut [u32]) = (res_i1.1).split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u32(a_i1, qj, c1, res_i2.1);
      let a_i2: u32 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u32], &mut [u32]) = (res_i2.1).split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u32(a_i2, qj, c1, res_i.1)
    };
    for i in len.wrapping_div(4u32).wrapping_mul(4u32)..len
    {
      let a_i: u32 = n[i as usize];
      let res_i: (&mut [u32], &mut [u32]) = (res_j0.1).split_at_mut(i as usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u32(a_i, qj, c1, res_i.1)
    };
    let r: u32 = c1;
    let c10: u32 = r;
    let res_j: u32 = c[len.wrapping_add(i0) as usize];
    let resb: (&mut [u32], &mut [u32]) = c.split_at_mut((len as usize).wrapping_add(i0 as usize));
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u32(c0, c10, res_j, resb.1)
  };
  (res[0usize..len.wrapping_add(len).wrapping_sub(len) as usize]).copy_from_slice(
    &(&c[len as usize..])[0usize..len.wrapping_add(len).wrapping_sub(len) as usize]
  );
  let c00: u32 = c0;
  let mut tmp: Box<[u32]> = vec![0u32; len as usize].into_boxed_slice();
  let mut c1: u32 = 0u32;
  for i in 0u32..len.wrapping_div(4u32)
  {
    let t1: u32 = res[4u32.wrapping_mul(i) as usize];
    let t20: u32 = n[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u32], &mut [u32]) = tmp.split_at_mut(4u32.wrapping_mul(i) as usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c1, t1, t20, res_i0.1);
    let t10: u32 = res[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u32 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u32], &mut [u32]) = (res_i0.1).split_at_mut(1usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c1, t10, t21, res_i1.1);
    let t11: u32 = res[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u32 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u32], &mut [u32]) = (res_i1.1).split_at_mut(1usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c1, t11, t22, res_i2.1);
    let t12: u32 = res[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u32 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u32], &mut [u32]) = (res_i2.1).split_at_mut(1usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c1, t12, t2, res_i.1)
  };
  for i in len.wrapping_div(4u32).wrapping_mul(4u32)..len
  {
    let t1: u32 = res[i as usize];
    let t2: u32 = n[i as usize];
    let res_i: (&mut [u32], &mut [u32]) = tmp.split_at_mut(i as usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u32(c1, t1, t2, res_i.1)
  };
  let c10: u32 = c1;
  let c2: u32 = c00.wrapping_sub(c10);
  for i in 0u32..len
  {
    let x: u32 = c2 & res[i as usize] | ! c2 & (&tmp)[i as usize];
    let os: (&mut [u32], &mut [u32]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn bn_mont_reduction_u64(len: u32, n: &[u64], nInv: u64, c: &mut [u64], res: &mut [u64])
{
  let mut c0: u64 = 0u64;
  for i0 in 0u32..len
  {
    let qj: u64 = nInv.wrapping_mul(c[i0 as usize]);
    let res_j0: (&mut [u64], &mut [u64]) = c.split_at_mut(i0 as usize);
    let mut c1: u64 = 0u64;
    for i in 0u32..len.wrapping_div(4u32)
    {
      let a_i: u64 = n[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u64], &mut [u64]) = (res_j0.1).split_at_mut(4u32.wrapping_mul(i) as usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i, qj, c1, res_i0.1);
      let a_i0: u64 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i0, qj, c1, res_i1.1);
      let a_i1: u64 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i1, qj, c1, res_i2.1);
      let a_i2: u64 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i2, qj, c1, res_i.1)
    };
    for i in len.wrapping_div(4u32).wrapping_mul(4u32)..len
    {
      let a_i: u64 = n[i as usize];
      let res_i: (&mut [u64], &mut [u64]) = (res_j0.1).split_at_mut(i as usize);
      c1 = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i, qj, c1, res_i.1)
    };
    let r: u64 = c1;
    let c10: u64 = r;
    let res_j: u64 = c[len.wrapping_add(i0) as usize];
    let resb: (&mut [u64], &mut [u64]) = c.split_at_mut((len as usize).wrapping_add(i0 as usize));
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, c10, res_j, resb.1)
  };
  (res[0usize..len.wrapping_add(len).wrapping_sub(len) as usize]).copy_from_slice(
    &(&c[len as usize..])[0usize..len.wrapping_add(len).wrapping_sub(len) as usize]
  );
  let c00: u64 = c0;
  let mut tmp: Box<[u64]> = vec![0u64; len as usize].into_boxed_slice();
  let mut c1: u64 = 0u64;
  for i in 0u32..len.wrapping_div(4u32)
  {
    let t1: u64 = res[4u32.wrapping_mul(i) as usize];
    let t20: u64 = n[4u32.wrapping_mul(i) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(4u32.wrapping_mul(i) as usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c1, t1, t20, res_i0.1);
    let t10: u64 = res[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let t21: u64 = n[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c1, t10, t21, res_i1.1);
    let t11: u64 = res[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let t22: u64 = n[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c1, t11, t22, res_i2.1);
    let t12: u64 = res[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let t2: u64 = n[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c1, t12, t2, res_i.1)
  };
  for i in len.wrapping_div(4u32).wrapping_mul(4u32)..len
  {
    let t1: u64 = res[i as usize];
    let t2: u64 = n[i as usize];
    let res_i: (&mut [u64], &mut [u64]) = tmp.split_at_mut(i as usize);
    c1 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c1, t1, t2, res_i.1)
  };
  let c10: u64 = c1;
  let c2: u64 = c00.wrapping_sub(c10);
  for i in 0u32..len
  {
    let x: u64 = c2 & res[i as usize] | ! c2 & (&tmp)[i as usize];
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}
