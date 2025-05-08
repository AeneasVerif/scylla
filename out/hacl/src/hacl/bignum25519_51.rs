#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

#[inline] pub fn Hacl_Impl_Curve25519_Field51_cswap2(bit: u64, p1: &mut [u64], p2: &mut [u64])
{
  let mask: u64 = 0u64.wrapping_sub(bit);
  for i in 0u32..10u32
  {
    let dummy: u64 = mask & (p1[i as usize] ^ p2[i as usize]);
    p1[i as usize] ^= dummy;
    p2[i as usize] ^= dummy
  }
}

#[inline] pub fn Hacl_Impl_Curve25519_Field51_fadd(out: &mut [u64], f1: &[u64], f2: &[u64])
{
  let f10: u64 = f1[0usize];
  let f20: u64 = f2[0usize];
  let f11: u64 = f1[1usize];
  let f21: u64 = f2[1usize];
  let f12: u64 = f1[2usize];
  let f22: u64 = f2[2usize];
  let f13: u64 = f1[3usize];
  let f23: u64 = f2[3usize];
  let f14: u64 = f1[4usize];
  let f24: u64 = f2[4usize];
  out[0usize] = f10.wrapping_add(f20);
  out[1usize] = f11.wrapping_add(f21);
  out[2usize] = f12.wrapping_add(f22);
  out[3usize] = f13.wrapping_add(f23);
  out[4usize] = f14.wrapping_add(f24)
}

#[inline] pub fn Hacl_Impl_Curve25519_Field51_fmul(
  out: &mut [u64],
  f1: &[u64],
  f2: &[u64],
  uu___: &[crate::types::FStar_UInt128_uint128]
)
{
  crate::lowstar::ignore::ignore::<&[crate::types::FStar_UInt128_uint128]>(uu___);
  let f10: u64 = f1[0usize];
  let f11: u64 = f1[1usize];
  let f12: u64 = f1[2usize];
  let f13: u64 = f1[3usize];
  let f14: u64 = f1[4usize];
  let f20: u64 = f2[0usize];
  let f21: u64 = f2[1usize];
  let f22: u64 = f2[2usize];
  let f23: u64 = f2[3usize];
  let f24: u64 = f2[4usize];
  let tmp1: u64 = f21.wrapping_mul(19u64);
  let tmp2: u64 = f22.wrapping_mul(19u64);
  let tmp3: u64 = f23.wrapping_mul(19u64);
  let tmp4: u64 = f24.wrapping_mul(19u64);
  let o00: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f10, f20);
  let o10: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f10, f21);
  let o20: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f10, f22);
  let o30: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f10, f23);
  let o40: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f10, f24);
  let o01: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o00,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f11, tmp4)
      );
  let o11: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o10,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f11, f20)
      );
  let o21: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o20,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f11, f21)
      );
  let o31: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o30,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f11, f22)
      );
  let o41: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o40,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f11, f23)
      );
  let o02: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o01,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f12, tmp3)
      );
  let o12: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o11,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f12, tmp4)
      );
  let o22: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o21,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f12, f20)
      );
  let o32: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o31,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f12, f21)
      );
  let o42: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o41,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f12, f22)
      );
  let o03: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o02,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f13, tmp2)
      );
  let o13: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o12,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f13, tmp3)
      );
  let o23: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o22,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f13, tmp4)
      );
  let o33: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o32,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f13, f20)
      );
  let o43: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o42,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f13, f21)
      );
  let o04: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o03,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f14, tmp1)
      );
  let o14: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o13,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f14, tmp2)
      );
  let o24: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o23,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f14, tmp3)
      );
  let o34: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o33,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f14, tmp4)
      );
  let o44: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o43,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f14, f20)
      );
  let tmp_w0: crate::types::FStar_UInt128_uint128 = o04;
  let tmp_w1: crate::types::FStar_UInt128_uint128 = o14;
  let tmp_w2: crate::types::FStar_UInt128_uint128 = o24;
  let tmp_w3: crate::types::FStar_UInt128_uint128 = o34;
  let tmp_w4: crate::types::FStar_UInt128_uint128 = o44;
  let l_: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w0,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(0u64)
      );
  let tmp01: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_) & 2251799813685247u64;
  let c0: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_, 51u32)
      );
  let l_0: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w1,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c0)
      );
  let tmp11: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_0) & 2251799813685247u64;
  let c1: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_0, 51u32)
      );
  let l_1: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w2,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c1)
      );
  let tmp21: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_1) & 2251799813685247u64;
  let c2: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_1, 51u32)
      );
  let l_2: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w3,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c2)
      );
  let tmp31: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_2) & 2251799813685247u64;
  let c3: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_2, 51u32)
      );
  let l_3: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w4,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c3)
      );
  let tmp41: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_3) & 2251799813685247u64;
  let c4: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_3, 51u32)
      );
  let l_4: u64 = tmp01.wrapping_add(c4.wrapping_mul(19u64));
  let tmp0_: u64 = l_4 & 2251799813685247u64;
  let c5: u64 = l_4.wrapping_shr(51u32);
  let o0: u64 = tmp0_;
  let o1: u64 = tmp11.wrapping_add(c5);
  let o2: u64 = tmp21;
  let o3: u64 = tmp31;
  let o4: u64 = tmp41;
  out[0usize] = o0;
  out[1usize] = o1;
  out[2usize] = o2;
  out[3usize] = o3;
  out[4usize] = o4
}

#[inline] pub fn Hacl_Impl_Curve25519_Field51_fmul1(out: &mut [u64], f1: &[u64], f2: u64)
{
  let f10: u64 = f1[0usize];
  let f11: u64 = f1[1usize];
  let f12: u64 = f1[2usize];
  let f13: u64 = f1[3usize];
  let f14: u64 = f1[4usize];
  let tmp_w0: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f2, f10);
  let tmp_w1: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f2, f11);
  let tmp_w2: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f2, f12);
  let tmp_w3: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f2, f13);
  let tmp_w4: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f2, f14);
  let l_: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w0,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(0u64)
      );
  let tmp0: u64 = crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_) & 2251799813685247u64;
  let c0: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_, 51u32)
      );
  let l_0: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w1,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c0)
      );
  let tmp1: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_0) & 2251799813685247u64;
  let c1: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_0, 51u32)
      );
  let l_1: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w2,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c1)
      );
  let tmp2: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_1) & 2251799813685247u64;
  let c2: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_1, 51u32)
      );
  let l_2: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w3,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c2)
      );
  let tmp3: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_2) & 2251799813685247u64;
  let c3: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_2, 51u32)
      );
  let l_3: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w4,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c3)
      );
  let tmp4: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_3) & 2251799813685247u64;
  let c4: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_3, 51u32)
      );
  let l_4: u64 = tmp0.wrapping_add(c4.wrapping_mul(19u64));
  let tmp0_: u64 = l_4 & 2251799813685247u64;
  let c5: u64 = l_4.wrapping_shr(51u32);
  let o0: u64 = tmp0_;
  let o1: u64 = tmp1.wrapping_add(c5);
  let o2: u64 = tmp2;
  let o3: u64 = tmp3;
  let o4: u64 = tmp4;
  out[0usize] = o0;
  out[1usize] = o1;
  out[2usize] = o2;
  out[3usize] = o3;
  out[4usize] = o4
}

#[inline] pub fn Hacl_Impl_Curve25519_Field51_fmul2(
  out: &mut [u64],
  f1: &[u64],
  f2: &[u64],
  uu___: &[crate::types::FStar_UInt128_uint128]
)
{
  crate::lowstar::ignore::ignore::<&[crate::types::FStar_UInt128_uint128]>(uu___);
  let f10: u64 = f1[0usize];
  let f11: u64 = f1[1usize];
  let f12: u64 = f1[2usize];
  let f13: u64 = f1[3usize];
  let f14: u64 = f1[4usize];
  let f20: u64 = f2[0usize];
  let f21: u64 = f2[1usize];
  let f22: u64 = f2[2usize];
  let f23: u64 = f2[3usize];
  let f24: u64 = f2[4usize];
  let f30: u64 = f1[5usize];
  let f31: u64 = f1[6usize];
  let f32: u64 = f1[7usize];
  let f33: u64 = f1[8usize];
  let f34: u64 = f1[9usize];
  let f40: u64 = f2[5usize];
  let f41: u64 = f2[6usize];
  let f42: u64 = f2[7usize];
  let f43: u64 = f2[8usize];
  let f44: u64 = f2[9usize];
  let tmp11: u64 = f21.wrapping_mul(19u64);
  let tmp12: u64 = f22.wrapping_mul(19u64);
  let tmp13: u64 = f23.wrapping_mul(19u64);
  let tmp14: u64 = f24.wrapping_mul(19u64);
  let tmp21: u64 = f41.wrapping_mul(19u64);
  let tmp22: u64 = f42.wrapping_mul(19u64);
  let tmp23: u64 = f43.wrapping_mul(19u64);
  let tmp24: u64 = f44.wrapping_mul(19u64);
  let o00: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f10, f20);
  let o15: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f10, f21);
  let o25: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f10, f22);
  let o30: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f10, f23);
  let o40: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f10, f24);
  let o010: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o00,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f11, tmp14)
      );
  let o110: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o15,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f11, f20)
      );
  let o210: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o25,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f11, f21)
      );
  let o310: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o30,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f11, f22)
      );
  let o410: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o40,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f11, f23)
      );
  let o020: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o010,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f12, tmp13)
      );
  let o120: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o110,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f12, tmp14)
      );
  let o220: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o210,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f12, f20)
      );
  let o320: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o310,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f12, f21)
      );
  let o420: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o410,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f12, f22)
      );
  let o030: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o020,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f13, tmp12)
      );
  let o130: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o120,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f13, tmp13)
      );
  let o230: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o220,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f13, tmp14)
      );
  let o330: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o320,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f13, f20)
      );
  let o430: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o420,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f13, f21)
      );
  let o040: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o030,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f14, tmp11)
      );
  let o140: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o130,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f14, tmp12)
      );
  let o240: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o230,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f14, tmp13)
      );
  let o340: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o330,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f14, tmp14)
      );
  let o440: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o430,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f14, f20)
      );
  let tmp_w10: crate::types::FStar_UInt128_uint128 = o040;
  let tmp_w11: crate::types::FStar_UInt128_uint128 = o140;
  let tmp_w12: crate::types::FStar_UInt128_uint128 = o240;
  let tmp_w13: crate::types::FStar_UInt128_uint128 = o340;
  let tmp_w14: crate::types::FStar_UInt128_uint128 = o440;
  let o0: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f30, f40);
  let o1: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f30, f41);
  let o2: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f30, f42);
  let o3: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f30, f43);
  let o4: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_mul_wide(f30, f44);
  let o01: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o0,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f31, tmp24)
      );
  let o111: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o1,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f31, f40)
      );
  let o211: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o2,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f31, f41)
      );
  let o31: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o3,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f31, f42)
      );
  let o41: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o4,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f31, f43)
      );
  let o02: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o01,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f32, tmp23)
      );
  let o121: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o111,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f32, tmp24)
      );
  let o221: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o211,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f32, f40)
      );
  let o32: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o31,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f32, f41)
      );
  let o42: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o41,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f32, f42)
      );
  let o03: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o02,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f33, tmp22)
      );
  let o131: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o121,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f33, tmp23)
      );
  let o231: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o221,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f33, tmp24)
      );
  let o33: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o32,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f33, f40)
      );
  let o43: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o42,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f33, f41)
      );
  let o04: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o03,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f34, tmp21)
      );
  let o141: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o131,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f34, tmp22)
      );
  let o241: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o231,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f34, tmp23)
      );
  let o34: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o33,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f34, tmp24)
      );
  let o44: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o43,
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f34, f40)
      );
  let tmp_w20: crate::types::FStar_UInt128_uint128 = o04;
  let tmp_w21: crate::types::FStar_UInt128_uint128 = o141;
  let tmp_w22: crate::types::FStar_UInt128_uint128 = o241;
  let tmp_w23: crate::types::FStar_UInt128_uint128 = o34;
  let tmp_w24: crate::types::FStar_UInt128_uint128 = o44;
  let l_: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w10,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(0u64)
      );
  let tmp00: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_) & 2251799813685247u64;
  let c00: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_, 51u32)
      );
  let l_0: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w11,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c00)
      );
  let tmp10: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_0) & 2251799813685247u64;
  let c10: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_0, 51u32)
      );
  let l_1: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w12,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c10)
      );
  let tmp20: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_1) & 2251799813685247u64;
  let c20: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_1, 51u32)
      );
  let l_2: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w13,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c20)
      );
  let tmp30: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_2) & 2251799813685247u64;
  let c30: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_2, 51u32)
      );
  let l_3: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w14,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c30)
      );
  let tmp40: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_3) & 2251799813685247u64;
  let c40: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_3, 51u32)
      );
  let l_4: u64 = tmp00.wrapping_add(c40.wrapping_mul(19u64));
  let tmp0_: u64 = l_4 & 2251799813685247u64;
  let c50: u64 = l_4.wrapping_shr(51u32);
  let o100: u64 = tmp0_;
  let o112: u64 = tmp10.wrapping_add(c50);
  let o122: u64 = tmp20;
  let o132: u64 = tmp30;
  let o142: u64 = tmp40;
  let l_5: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w20,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(0u64)
      );
  let tmp0: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_5) & 2251799813685247u64;
  let c0: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_5, 51u32)
      );
  let l_6: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w21,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c0)
      );
  let tmp1: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_6) & 2251799813685247u64;
  let c1: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_6, 51u32)
      );
  let l_7: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w22,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c1)
      );
  let tmp2: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_7) & 2251799813685247u64;
  let c2: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_7, 51u32)
      );
  let l_8: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w23,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c2)
      );
  let tmp3: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_8) & 2251799813685247u64;
  let c3: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_8, 51u32)
      );
  let l_9: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        tmp_w24,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c3)
      );
  let tmp4: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_9) & 2251799813685247u64;
  let c4: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_9, 51u32)
      );
  let l_10: u64 = tmp0.wrapping_add(c4.wrapping_mul(19u64));
  let tmp0_0: u64 = l_10 & 2251799813685247u64;
  let c5: u64 = l_10.wrapping_shr(51u32);
  let o200: u64 = tmp0_0;
  let o212: u64 = tmp1.wrapping_add(c5);
  let o222: u64 = tmp2;
  let o232: u64 = tmp3;
  let o242: u64 = tmp4;
  let o10: u64 = o100;
  let o11: u64 = o112;
  let o12: u64 = o122;
  let o13: u64 = o132;
  let o14: u64 = o142;
  let o20: u64 = o200;
  let o21: u64 = o212;
  let o22: u64 = o222;
  let o23: u64 = o232;
  let o24: u64 = o242;
  out[0usize] = o10;
  out[1usize] = o11;
  out[2usize] = o12;
  out[3usize] = o13;
  out[4usize] = o14;
  out[5usize] = o20;
  out[6usize] = o21;
  out[7usize] = o22;
  out[8usize] = o23;
  out[9usize] = o24
}

#[inline] pub fn Hacl_Impl_Curve25519_Field51_fsqr(
  out: &mut [u64],
  f: &[u64],
  uu___: &[crate::types::FStar_UInt128_uint128]
)
{
  crate::lowstar::ignore::ignore::<&[crate::types::FStar_UInt128_uint128]>(uu___);
  let f0: u64 = f[0usize];
  let f1: u64 = f[1usize];
  let f2: u64 = f[2usize];
  let f3: u64 = f[3usize];
  let f4: u64 = f[4usize];
  let d0: u64 = 2u64.wrapping_mul(f0);
  let d1: u64 = 2u64.wrapping_mul(f1);
  let d2: u64 = 38u64.wrapping_mul(f2);
  let d3: u64 = 19u64.wrapping_mul(f3);
  let d419: u64 = 19u64.wrapping_mul(f4);
  let d4: u64 = 2u64.wrapping_mul(d419);
  let s0: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(f0, f0),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d4, f1)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(d2, f3)
      );
  let s1: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d0, f1),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d4, f2)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(d3, f3)
      );
  let s2: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d0, f2),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(f1, f1)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(d4, f3)
      );
  let s3: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d0, f3),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d1, f2)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f4, d419)
      );
  let s4: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d0, f4),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d1, f3)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f2, f2)
      );
  let o00: crate::types::FStar_UInt128_uint128 = s0;
  let o10: crate::types::FStar_UInt128_uint128 = s1;
  let o20: crate::types::FStar_UInt128_uint128 = s2;
  let o30: crate::types::FStar_UInt128_uint128 = s3;
  let o40: crate::types::FStar_UInt128_uint128 = s4;
  let l_: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o00,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(0u64)
      );
  let tmp0: u64 = crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_) & 2251799813685247u64;
  let c0: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_, 51u32)
      );
  let l_0: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o10,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c0)
      );
  let tmp1: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_0) & 2251799813685247u64;
  let c1: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_0, 51u32)
      );
  let l_1: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o20,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c1)
      );
  let tmp2: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_1) & 2251799813685247u64;
  let c2: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_1, 51u32)
      );
  let l_2: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o30,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c2)
      );
  let tmp3: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_2) & 2251799813685247u64;
  let c3: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_2, 51u32)
      );
  let l_3: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o40,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c3)
      );
  let tmp4: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_3) & 2251799813685247u64;
  let c4: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_3, 51u32)
      );
  let l_4: u64 = tmp0.wrapping_add(c4.wrapping_mul(19u64));
  let tmp0_: u64 = l_4 & 2251799813685247u64;
  let c5: u64 = l_4.wrapping_shr(51u32);
  let o0: u64 = tmp0_;
  let o1: u64 = tmp1.wrapping_add(c5);
  let o2: u64 = tmp2;
  let o3: u64 = tmp3;
  let o4: u64 = tmp4;
  out[0usize] = o0;
  out[1usize] = o1;
  out[2usize] = o2;
  out[3usize] = o3;
  out[4usize] = o4
}

#[inline] pub fn Hacl_Impl_Curve25519_Field51_fsqr2(
  out: &mut [u64],
  f: &[u64],
  uu___: &[crate::types::FStar_UInt128_uint128]
)
{
  crate::lowstar::ignore::ignore::<&[crate::types::FStar_UInt128_uint128]>(uu___);
  let f10: u64 = f[0usize];
  let f11: u64 = f[1usize];
  let f12: u64 = f[2usize];
  let f13: u64 = f[3usize];
  let f14: u64 = f[4usize];
  let f20: u64 = f[5usize];
  let f21: u64 = f[6usize];
  let f22: u64 = f[7usize];
  let f23: u64 = f[8usize];
  let f24: u64 = f[9usize];
  let d00: u64 = 2u64.wrapping_mul(f10);
  let d10: u64 = 2u64.wrapping_mul(f11);
  let d20: u64 = 38u64.wrapping_mul(f12);
  let d30: u64 = 19u64.wrapping_mul(f13);
  let d4190: u64 = 19u64.wrapping_mul(f14);
  let d40: u64 = 2u64.wrapping_mul(d4190);
  let s00: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(f10, f10),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d40, f11)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(d20, f13)
      );
  let s10: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d00, f11),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d40, f12)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(d30, f13)
      );
  let s20: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d00, f12),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(f11, f11)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(d40, f13)
      );
  let s30: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d00, f13),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d10, f12)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f14, d4190)
      );
  let s40: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d00, f14),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d10, f13)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f12, f12)
      );
  let o100: crate::types::FStar_UInt128_uint128 = s00;
  let o110: crate::types::FStar_UInt128_uint128 = s10;
  let o120: crate::types::FStar_UInt128_uint128 = s20;
  let o130: crate::types::FStar_UInt128_uint128 = s30;
  let o140: crate::types::FStar_UInt128_uint128 = s40;
  let d0: u64 = 2u64.wrapping_mul(f20);
  let d1: u64 = 2u64.wrapping_mul(f21);
  let d2: u64 = 38u64.wrapping_mul(f22);
  let d3: u64 = 19u64.wrapping_mul(f23);
  let d419: u64 = 19u64.wrapping_mul(f24);
  let d4: u64 = 2u64.wrapping_mul(d419);
  let s0: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(f20, f20),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d4, f21)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(d2, f23)
      );
  let s1: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d0, f21),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d4, f22)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(d3, f23)
      );
  let s2: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d0, f22),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(f21, f21)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(d4, f23)
      );
  let s3: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d0, f23),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d1, f22)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f24, d419)
      );
  let s4: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        crate::hacl_krmllib::FStar_UInt128_add(
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d0, f24),
          crate::hacl_krmllib::FStar_UInt128_mul_wide(d1, f23)
        ),
        crate::hacl_krmllib::FStar_UInt128_mul_wide(f22, f22)
      );
  let o200: crate::types::FStar_UInt128_uint128 = s0;
  let o210: crate::types::FStar_UInt128_uint128 = s1;
  let o220: crate::types::FStar_UInt128_uint128 = s2;
  let o230: crate::types::FStar_UInt128_uint128 = s3;
  let o240: crate::types::FStar_UInt128_uint128 = s4;
  let l_: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o100,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(0u64)
      );
  let tmp00: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_) & 2251799813685247u64;
  let c00: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_, 51u32)
      );
  let l_0: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o110,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c00)
      );
  let tmp10: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_0) & 2251799813685247u64;
  let c10: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_0, 51u32)
      );
  let l_1: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o120,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c10)
      );
  let tmp20: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_1) & 2251799813685247u64;
  let c20: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_1, 51u32)
      );
  let l_2: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o130,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c20)
      );
  let tmp30: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_2) & 2251799813685247u64;
  let c30: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_2, 51u32)
      );
  let l_3: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o140,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c30)
      );
  let tmp40: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_3) & 2251799813685247u64;
  let c40: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_3, 51u32)
      );
  let l_4: u64 = tmp00.wrapping_add(c40.wrapping_mul(19u64));
  let tmp0_: u64 = l_4 & 2251799813685247u64;
  let c50: u64 = l_4.wrapping_shr(51u32);
  let o101: u64 = tmp0_;
  let o111: u64 = tmp10.wrapping_add(c50);
  let o121: u64 = tmp20;
  let o131: u64 = tmp30;
  let o141: u64 = tmp40;
  let l_5: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o200,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(0u64)
      );
  let tmp0: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_5) & 2251799813685247u64;
  let c0: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_5, 51u32)
      );
  let l_6: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o210,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c0)
      );
  let tmp1: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_6) & 2251799813685247u64;
  let c1: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_6, 51u32)
      );
  let l_7: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o220,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c1)
      );
  let tmp2: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_7) & 2251799813685247u64;
  let c2: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_7, 51u32)
      );
  let l_8: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o230,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c2)
      );
  let tmp3: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_8) & 2251799813685247u64;
  let c3: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_8, 51u32)
      );
  let l_9: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_add(
        o240,
        crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(c3)
      );
  let tmp4: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(l_9) & 2251799813685247u64;
  let c4: u64 =
      crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
        crate::hacl_krmllib::FStar_UInt128_shift_right(l_9, 51u32)
      );
  let l_10: u64 = tmp0.wrapping_add(c4.wrapping_mul(19u64));
  let tmp0_0: u64 = l_10 & 2251799813685247u64;
  let c5: u64 = l_10.wrapping_shr(51u32);
  let o201: u64 = tmp0_0;
  let o211: u64 = tmp1.wrapping_add(c5);
  let o221: u64 = tmp2;
  let o231: u64 = tmp3;
  let o241: u64 = tmp4;
  let o10: u64 = o101;
  let o11: u64 = o111;
  let o12: u64 = o121;
  let o13: u64 = o131;
  let o14: u64 = o141;
  let o20: u64 = o201;
  let o21: u64 = o211;
  let o22: u64 = o221;
  let o23: u64 = o231;
  let o24: u64 = o241;
  out[0usize] = o10;
  out[1usize] = o11;
  out[2usize] = o12;
  out[3usize] = o13;
  out[4usize] = o14;
  out[5usize] = o20;
  out[6usize] = o21;
  out[7usize] = o22;
  out[8usize] = o23;
  out[9usize] = o24
}

#[inline] pub fn Hacl_Impl_Curve25519_Field51_fsub(out: &mut [u64], f1: &[u64], f2: &[u64])
{
  let f10: u64 = f1[0usize];
  let f20: u64 = f2[0usize];
  let f11: u64 = f1[1usize];
  let f21: u64 = f2[1usize];
  let f12: u64 = f1[2usize];
  let f22: u64 = f2[2usize];
  let f13: u64 = f1[3usize];
  let f23: u64 = f2[3usize];
  let f14: u64 = f1[4usize];
  let f24: u64 = f2[4usize];
  out[0usize] = f10.wrapping_add(18014398509481832u64).wrapping_sub(f20);
  out[1usize] = f11.wrapping_add(18014398509481976u64).wrapping_sub(f21);
  out[2usize] = f12.wrapping_add(18014398509481976u64).wrapping_sub(f22);
  out[3usize] = f13.wrapping_add(18014398509481976u64).wrapping_sub(f23);
  out[4usize] = f14.wrapping_add(18014398509481976u64).wrapping_sub(f24)
}

#[inline] pub fn Hacl_Impl_Curve25519_Field51_store_felem(u64s: &mut [u64], f: &[u64])
{
  let f0: u64 = f[0usize];
  let f1: u64 = f[1usize];
  let f2: u64 = f[2usize];
  let f3: u64 = f[3usize];
  let f4: u64 = f[4usize];
  let l_: u64 = f0.wrapping_add(0u64);
  let tmp0: u64 = l_ & 2251799813685247u64;
  let c0: u64 = l_.wrapping_shr(51u32);
  let l_0: u64 = f1.wrapping_add(c0);
  let tmp1: u64 = l_0 & 2251799813685247u64;
  let c1: u64 = l_0.wrapping_shr(51u32);
  let l_1: u64 = f2.wrapping_add(c1);
  let tmp2: u64 = l_1 & 2251799813685247u64;
  let c2: u64 = l_1.wrapping_shr(51u32);
  let l_2: u64 = f3.wrapping_add(c2);
  let tmp3: u64 = l_2 & 2251799813685247u64;
  let c3: u64 = l_2.wrapping_shr(51u32);
  let l_3: u64 = f4.wrapping_add(c3);
  let tmp4: u64 = l_3 & 2251799813685247u64;
  let c4: u64 = l_3.wrapping_shr(51u32);
  let l_4: u64 = tmp0.wrapping_add(c4.wrapping_mul(19u64));
  let tmp0_: u64 = l_4 & 2251799813685247u64;
  let c5: u64 = l_4.wrapping_shr(51u32);
  let f01: u64 = tmp0_;
  let f11: u64 = tmp1.wrapping_add(c5);
  let f21: u64 = tmp2;
  let f31: u64 = tmp3;
  let f41: u64 = tmp4;
  let m0: u64 = crate::hacl_krmllib::FStar_UInt64_gte_mask(f01, 2251799813685229u64);
  let m1: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(f11, 2251799813685247u64);
  let m2: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(f21, 2251799813685247u64);
  let m3: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(f31, 2251799813685247u64);
  let m4: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(f41, 2251799813685247u64);
  let mask: u64 = m0 & m1 & m2 & m3 & m4;
  let f0_: u64 = f01.wrapping_sub(mask & 2251799813685229u64);
  let f1_: u64 = f11.wrapping_sub(mask & 2251799813685247u64);
  let f2_: u64 = f21.wrapping_sub(mask & 2251799813685247u64);
  let f3_: u64 = f31.wrapping_sub(mask & 2251799813685247u64);
  let f4_: u64 = f41.wrapping_sub(mask & 2251799813685247u64);
  let f02: u64 = f0_;
  let f12: u64 = f1_;
  let f22: u64 = f2_;
  let f32: u64 = f3_;
  let f42: u64 = f4_;
  let o00: u64 = f02 | f12.wrapping_shl(51u32);
  let o10: u64 = f12.wrapping_shr(13u32) | f22.wrapping_shl(38u32);
  let o20: u64 = f22.wrapping_shr(26u32) | f32.wrapping_shl(25u32);
  let o30: u64 = f32.wrapping_shr(39u32) | f42.wrapping_shl(12u32);
  let o0: u64 = o00;
  let o1: u64 = o10;
  let o2: u64 = o20;
  let o3: u64 = o30;
  u64s[0usize] = o0;
  u64s[1usize] = o1;
  u64s[2usize] = o2;
  u64s[3usize] = o3
}
