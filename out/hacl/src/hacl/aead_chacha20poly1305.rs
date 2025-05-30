#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

pub fn Hacl_AEAD_Chacha20Poly1305_decrypt(
  output: &mut [u8],
  input: &[u8],
  input_len: u32,
  data: &[u8],
  data_len: u32,
  key: &[u8],
  nonce: &[u8],
  tag: &[u8]
) ->
    u32
{
  let mut computed_tag: [u8; 16] = [0u8; 16usize];
  let mut tmp: [u8; 64] = [0u8; 64usize];
  let tmp_copy: [u8; 64] = [0u8; 64usize];
  crate::hacl::chacha20::Hacl_Chacha20_chacha20_encrypt(
    64u32,
    &mut tmp,
    &tmp_copy,
    key,
    nonce,
    0u32
  );
  let key1: (&[u8], &[u8]) = tmp.split_at(0usize);
  poly1305_do_32(key1.1, data_len, data, input_len, input, &mut computed_tag);
  let mut res: u8 = 255u8;
  for i in 0u32..16u32
  {
    let uu____0: u8 =
        crate::hacl_krmllib::FStar_UInt8_eq_mask(computed_tag[i as usize], tag[i as usize]);
    res = (uu____0 as u32 & res as u32) as u8
  };
  let z: u8 = res;
  if z as u32 == 255u32
  {
    crate::hacl::chacha20::Hacl_Chacha20_chacha20_encrypt(
      input_len,
      output,
      input,
      key,
      nonce,
      1u32
    );
    return 0u32
  };
  return 1u32
}

pub fn Hacl_AEAD_Chacha20Poly1305_encrypt(
  output: &mut [u8],
  tag: &mut [u8],
  input: &[u8],
  input_len: u32,
  data: &[u8],
  data_len: u32,
  key: &[u8],
  nonce: &[u8]
)
{
  crate::hacl::chacha20::Hacl_Chacha20_chacha20_encrypt(
    input_len,
    output,
    input,
    key,
    nonce,
    1u32
  );
  let mut tmp: [u8; 64] = [0u8; 64usize];
  let tmp_copy: [u8; 64] = [0u8; 64usize];
  crate::hacl::chacha20::Hacl_Chacha20_chacha20_encrypt(
    64u32,
    &mut tmp,
    &tmp_copy,
    key,
    nonce,
    0u32
  );
  let key1: (&[u8], &[u8]) = tmp.split_at(0usize);
  poly1305_do_32(key1.1, data_len, data, input_len, output, tag)
}

#[inline] pub fn poly1305_do_32(
  k: &[u8],
  aadlen: u32,
  aad: &[u8],
  mlen: u32,
  m: &[u8],
  out: &mut [u8]
)
{
  let mut ctx: [u64; 25] = [0u64; 25usize];
  let mut block: [u8; 16] = [0u8; 16usize];
  crate::hacl::mac_poly1305::Hacl_MAC_Poly1305_poly1305_init(&mut ctx, k);
  if aadlen != 0u32 { poly1305_padded_32(&mut ctx, aadlen, aad) };
  if mlen != 0u32 { poly1305_padded_32(&mut ctx, mlen, m) };
  crate::lowstar_endianness::store64_le(&mut block, aadlen as u64);
  crate::lowstar_endianness::store64_le(&mut block[8usize..], mlen as u64);
  let pre: (&mut [u64], &mut [u64]) = ctx.split_at_mut(5usize);
  let acc: (&mut [u64], &mut [u64]) = (pre.0).split_at_mut(0usize);
  let mut e: [u64; 5] = [0u64; 5usize];
  let u0: u64 = crate::lowstar_endianness::load64_le(&block);
  let lo: u64 = u0;
  let u: u64 = crate::lowstar_endianness::load64_le(&block[8usize..]);
  let hi: u64 = u;
  let f0: u64 = lo;
  let f1: u64 = hi;
  let f010: u64 = f0 & 67108863u64;
  let f110: u64 = f0.wrapping_shr(26u32) & 67108863u64;
  let f20: u64 = f0.wrapping_shr(52u32) | (f1 & 16383u64).wrapping_shl(12u32);
  let f30: u64 = f1.wrapping_shr(14u32) & 67108863u64;
  let f40: u64 = f1.wrapping_shr(40u32);
  let f01: u64 = f010;
  let f111: u64 = f110;
  let f2: u64 = f20;
  let f3: u64 = f30;
  let f41: u64 = f40;
  e[0usize] = f01;
  e[1usize] = f111;
  e[2usize] = f2;
  e[3usize] = f3;
  e[4usize] = f41;
  let b: u64 = 16777216u64;
  let mask: u64 = b;
  let f4: u64 = e[4usize];
  e[4usize] = f4 | mask;
  let r: (&[u64], &[u64]) = (pre.1).split_at(0usize);
  let r5: (&[u64], &[u64]) = (r.1).split_at(5usize);
  let r0: u64 = r5.0[0usize];
  let r1: u64 = r5.0[1usize];
  let r2: u64 = r5.0[2usize];
  let r3: u64 = r5.0[3usize];
  let r4: u64 = r5.0[4usize];
  let r51: u64 = r5.1[1usize];
  let r52: u64 = r5.1[2usize];
  let r53: u64 = r5.1[3usize];
  let r54: u64 = r5.1[4usize];
  let f10: u64 = e[0usize];
  let f11: u64 = e[1usize];
  let f12: u64 = e[2usize];
  let f13: u64 = e[3usize];
  let f14: u64 = e[4usize];
  let a0: u64 = acc.1[0usize];
  let a1: u64 = acc.1[1usize];
  let a2: u64 = acc.1[2usize];
  let a3: u64 = acc.1[3usize];
  let a4: u64 = acc.1[4usize];
  let a01: u64 = a0.wrapping_add(f10);
  let a11: u64 = a1.wrapping_add(f11);
  let a21: u64 = a2.wrapping_add(f12);
  let a31: u64 = a3.wrapping_add(f13);
  let a41: u64 = a4.wrapping_add(f14);
  let a02: u64 = r0.wrapping_mul(a01);
  let a12: u64 = r1.wrapping_mul(a01);
  let a22: u64 = r2.wrapping_mul(a01);
  let a32: u64 = r3.wrapping_mul(a01);
  let a42: u64 = r4.wrapping_mul(a01);
  let a03: u64 = a02.wrapping_add(r54.wrapping_mul(a11));
  let a13: u64 = a12.wrapping_add(r0.wrapping_mul(a11));
  let a23: u64 = a22.wrapping_add(r1.wrapping_mul(a11));
  let a33: u64 = a32.wrapping_add(r2.wrapping_mul(a11));
  let a43: u64 = a42.wrapping_add(r3.wrapping_mul(a11));
  let a04: u64 = a03.wrapping_add(r53.wrapping_mul(a21));
  let a14: u64 = a13.wrapping_add(r54.wrapping_mul(a21));
  let a24: u64 = a23.wrapping_add(r0.wrapping_mul(a21));
  let a34: u64 = a33.wrapping_add(r1.wrapping_mul(a21));
  let a44: u64 = a43.wrapping_add(r2.wrapping_mul(a21));
  let a05: u64 = a04.wrapping_add(r52.wrapping_mul(a31));
  let a15: u64 = a14.wrapping_add(r53.wrapping_mul(a31));
  let a25: u64 = a24.wrapping_add(r54.wrapping_mul(a31));
  let a35: u64 = a34.wrapping_add(r0.wrapping_mul(a31));
  let a45: u64 = a44.wrapping_add(r1.wrapping_mul(a31));
  let a06: u64 = a05.wrapping_add(r51.wrapping_mul(a41));
  let a16: u64 = a15.wrapping_add(r52.wrapping_mul(a41));
  let a26: u64 = a25.wrapping_add(r53.wrapping_mul(a41));
  let a36: u64 = a35.wrapping_add(r54.wrapping_mul(a41));
  let a46: u64 = a45.wrapping_add(r0.wrapping_mul(a41));
  let t0: u64 = a06;
  let t1: u64 = a16;
  let t2: u64 = a26;
  let t3: u64 = a36;
  let t4: u64 = a46;
  let mask26: u64 = 67108863u64;
  let z0: u64 = t0.wrapping_shr(26u32);
  let z1: u64 = t3.wrapping_shr(26u32);
  let x0: u64 = t0 & mask26;
  let x3: u64 = t3 & mask26;
  let x1: u64 = t1.wrapping_add(z0);
  let x4: u64 = t4.wrapping_add(z1);
  let z01: u64 = x1.wrapping_shr(26u32);
  let z11: u64 = x4.wrapping_shr(26u32);
  let t: u64 = z11.wrapping_shl(2u32);
  let z12: u64 = z11.wrapping_add(t);
  let x11: u64 = x1 & mask26;
  let x41: u64 = x4 & mask26;
  let x2: u64 = t2.wrapping_add(z01);
  let x01: u64 = x0.wrapping_add(z12);
  let z02: u64 = x2.wrapping_shr(26u32);
  let z13: u64 = x01.wrapping_shr(26u32);
  let x21: u64 = x2 & mask26;
  let x02: u64 = x01 & mask26;
  let x31: u64 = x3.wrapping_add(z02);
  let x12: u64 = x11.wrapping_add(z13);
  let z03: u64 = x31.wrapping_shr(26u32);
  let x32: u64 = x31 & mask26;
  let x42: u64 = x41.wrapping_add(z03);
  let o0: u64 = x02;
  let o1: u64 = x12;
  let o2: u64 = x21;
  let o3: u64 = x32;
  let o4: u64 = x42;
  acc.1[0usize] = o0;
  acc.1[1usize] = o1;
  acc.1[2usize] = o2;
  acc.1[3usize] = o3;
  acc.1[4usize] = o4;
  crate::hacl::mac_poly1305::Hacl_MAC_Poly1305_poly1305_finish(out, k, &mut ctx)
}

#[inline] pub fn poly1305_padded_32(ctx: &mut [u64], len: u32, text: &[u8])
{
  let n: u32 = len.wrapping_div(16u32);
  let r: u32 = len.wrapping_rem(16u32);
  let blocks: (&[u8], &[u8]) = text.split_at(0usize);
  let rem: (&[u8], &[u8]) = (blocks.1).split_at(n.wrapping_mul(16u32) as usize);
  let pre0: (&mut [u64], &mut [u64]) = ctx.split_at_mut(5usize);
  let acc0: (&mut [u64], &mut [u64]) = (pre0.0).split_at_mut(0usize);
  let nb: u32 = n.wrapping_mul(16u32).wrapping_div(16u32);
  let rem1: u32 = n.wrapping_mul(16u32).wrapping_rem(16u32);
  for i in 0u32..nb
  {
    let block: (&[u8], &[u8]) = (rem.0).split_at(i.wrapping_mul(16u32) as usize);
    let mut e: [u64; 5] = [0u64; 5usize];
    let u0: u64 = crate::lowstar_endianness::load64_le(block.1);
    let lo: u64 = u0;
    let u: u64 = crate::lowstar_endianness::load64_le(&block.1[8usize..]);
    let hi: u64 = u;
    let f0: u64 = lo;
    let f1: u64 = hi;
    let f010: u64 = f0 & 67108863u64;
    let f110: u64 = f0.wrapping_shr(26u32) & 67108863u64;
    let f20: u64 = f0.wrapping_shr(52u32) | (f1 & 16383u64).wrapping_shl(12u32);
    let f30: u64 = f1.wrapping_shr(14u32) & 67108863u64;
    let f40: u64 = f1.wrapping_shr(40u32);
    let f01: u64 = f010;
    let f111: u64 = f110;
    let f2: u64 = f20;
    let f3: u64 = f30;
    let f41: u64 = f40;
    e[0usize] = f01;
    e[1usize] = f111;
    e[2usize] = f2;
    e[3usize] = f3;
    e[4usize] = f41;
    let b: u64 = 16777216u64;
    let mask: u64 = b;
    let f4: u64 = e[4usize];
    e[4usize] = f4 | mask;
    let r1: (&[u64], &[u64]) = (pre0.1).split_at(0usize);
    let r5: (&[u64], &[u64]) = (r1.1).split_at(5usize);
    let r0: u64 = r5.0[0usize];
    let r11: u64 = r5.0[1usize];
    let r2: u64 = r5.0[2usize];
    let r3: u64 = r5.0[3usize];
    let r4: u64 = r5.0[4usize];
    let r51: u64 = r5.1[1usize];
    let r52: u64 = r5.1[2usize];
    let r53: u64 = r5.1[3usize];
    let r54: u64 = r5.1[4usize];
    let f10: u64 = e[0usize];
    let f11: u64 = e[1usize];
    let f12: u64 = e[2usize];
    let f13: u64 = e[3usize];
    let f14: u64 = e[4usize];
    let a0: u64 = acc0.1[0usize];
    let a1: u64 = acc0.1[1usize];
    let a2: u64 = acc0.1[2usize];
    let a3: u64 = acc0.1[3usize];
    let a4: u64 = acc0.1[4usize];
    let a01: u64 = a0.wrapping_add(f10);
    let a11: u64 = a1.wrapping_add(f11);
    let a21: u64 = a2.wrapping_add(f12);
    let a31: u64 = a3.wrapping_add(f13);
    let a41: u64 = a4.wrapping_add(f14);
    let a02: u64 = r0.wrapping_mul(a01);
    let a12: u64 = r11.wrapping_mul(a01);
    let a22: u64 = r2.wrapping_mul(a01);
    let a32: u64 = r3.wrapping_mul(a01);
    let a42: u64 = r4.wrapping_mul(a01);
    let a03: u64 = a02.wrapping_add(r54.wrapping_mul(a11));
    let a13: u64 = a12.wrapping_add(r0.wrapping_mul(a11));
    let a23: u64 = a22.wrapping_add(r11.wrapping_mul(a11));
    let a33: u64 = a32.wrapping_add(r2.wrapping_mul(a11));
    let a43: u64 = a42.wrapping_add(r3.wrapping_mul(a11));
    let a04: u64 = a03.wrapping_add(r53.wrapping_mul(a21));
    let a14: u64 = a13.wrapping_add(r54.wrapping_mul(a21));
    let a24: u64 = a23.wrapping_add(r0.wrapping_mul(a21));
    let a34: u64 = a33.wrapping_add(r11.wrapping_mul(a21));
    let a44: u64 = a43.wrapping_add(r2.wrapping_mul(a21));
    let a05: u64 = a04.wrapping_add(r52.wrapping_mul(a31));
    let a15: u64 = a14.wrapping_add(r53.wrapping_mul(a31));
    let a25: u64 = a24.wrapping_add(r54.wrapping_mul(a31));
    let a35: u64 = a34.wrapping_add(r0.wrapping_mul(a31));
    let a45: u64 = a44.wrapping_add(r11.wrapping_mul(a31));
    let a06: u64 = a05.wrapping_add(r51.wrapping_mul(a41));
    let a16: u64 = a15.wrapping_add(r52.wrapping_mul(a41));
    let a26: u64 = a25.wrapping_add(r53.wrapping_mul(a41));
    let a36: u64 = a35.wrapping_add(r54.wrapping_mul(a41));
    let a46: u64 = a45.wrapping_add(r0.wrapping_mul(a41));
    let t0: u64 = a06;
    let t1: u64 = a16;
    let t2: u64 = a26;
    let t3: u64 = a36;
    let t4: u64 = a46;
    let mask26: u64 = 67108863u64;
    let z0: u64 = t0.wrapping_shr(26u32);
    let z1: u64 = t3.wrapping_shr(26u32);
    let x0: u64 = t0 & mask26;
    let x3: u64 = t3 & mask26;
    let x1: u64 = t1.wrapping_add(z0);
    let x4: u64 = t4.wrapping_add(z1);
    let z01: u64 = x1.wrapping_shr(26u32);
    let z11: u64 = x4.wrapping_shr(26u32);
    let t: u64 = z11.wrapping_shl(2u32);
    let z12: u64 = z11.wrapping_add(t);
    let x11: u64 = x1 & mask26;
    let x41: u64 = x4 & mask26;
    let x2: u64 = t2.wrapping_add(z01);
    let x01: u64 = x0.wrapping_add(z12);
    let z02: u64 = x2.wrapping_shr(26u32);
    let z13: u64 = x01.wrapping_shr(26u32);
    let x21: u64 = x2 & mask26;
    let x02: u64 = x01 & mask26;
    let x31: u64 = x3.wrapping_add(z02);
    let x12: u64 = x11.wrapping_add(z13);
    let z03: u64 = x31.wrapping_shr(26u32);
    let x32: u64 = x31 & mask26;
    let x42: u64 = x41.wrapping_add(z03);
    let o0: u64 = x02;
    let o1: u64 = x12;
    let o2: u64 = x21;
    let o3: u64 = x32;
    let o4: u64 = x42;
    acc0.1[0usize] = o0;
    acc0.1[1usize] = o1;
    acc0.1[2usize] = o2;
    acc0.1[3usize] = o3;
    acc0.1[4usize] = o4
  };
  if rem1 > 0u32
  {
    let last: (&[u8], &[u8]) = (rem.0).split_at(nb.wrapping_mul(16u32) as usize);
    let mut e: [u64; 5] = [0u64; 5usize];
    let mut tmp: [u8; 16] = [0u8; 16usize];
    (tmp[0usize..rem1 as usize]).copy_from_slice(&last.1[0usize..rem1 as usize]);
    let u0: u64 = crate::lowstar_endianness::load64_le(&tmp);
    let lo: u64 = u0;
    let u: u64 = crate::lowstar_endianness::load64_le(&tmp[8usize..]);
    let hi: u64 = u;
    let f0: u64 = lo;
    let f1: u64 = hi;
    let f010: u64 = f0 & 67108863u64;
    let f110: u64 = f0.wrapping_shr(26u32) & 67108863u64;
    let f20: u64 = f0.wrapping_shr(52u32) | (f1 & 16383u64).wrapping_shl(12u32);
    let f30: u64 = f1.wrapping_shr(14u32) & 67108863u64;
    let f40: u64 = f1.wrapping_shr(40u32);
    let f01: u64 = f010;
    let f111: u64 = f110;
    let f2: u64 = f20;
    let f3: u64 = f30;
    let f4: u64 = f40;
    e[0usize] = f01;
    e[1usize] = f111;
    e[2usize] = f2;
    e[3usize] = f3;
    e[4usize] = f4;
    let b: u64 = 1u64.wrapping_shl(rem1.wrapping_mul(8u32).wrapping_rem(26u32));
    let mask: u64 = b;
    let fi: u64 = e[rem1.wrapping_mul(8u32).wrapping_div(26u32) as usize];
    e[rem1.wrapping_mul(8u32).wrapping_div(26u32) as usize] = fi | mask;
    let r1: (&[u64], &[u64]) = (pre0.1).split_at(0usize);
    let r5: (&[u64], &[u64]) = (r1.1).split_at(5usize);
    let r0: u64 = r5.0[0usize];
    let r11: u64 = r5.0[1usize];
    let r2: u64 = r5.0[2usize];
    let r3: u64 = r5.0[3usize];
    let r4: u64 = r5.0[4usize];
    let r51: u64 = r5.1[1usize];
    let r52: u64 = r5.1[2usize];
    let r53: u64 = r5.1[3usize];
    let r54: u64 = r5.1[4usize];
    let f10: u64 = e[0usize];
    let f11: u64 = e[1usize];
    let f12: u64 = e[2usize];
    let f13: u64 = e[3usize];
    let f14: u64 = e[4usize];
    let a0: u64 = acc0.1[0usize];
    let a1: u64 = acc0.1[1usize];
    let a2: u64 = acc0.1[2usize];
    let a3: u64 = acc0.1[3usize];
    let a4: u64 = acc0.1[4usize];
    let a01: u64 = a0.wrapping_add(f10);
    let a11: u64 = a1.wrapping_add(f11);
    let a21: u64 = a2.wrapping_add(f12);
    let a31: u64 = a3.wrapping_add(f13);
    let a41: u64 = a4.wrapping_add(f14);
    let a02: u64 = r0.wrapping_mul(a01);
    let a12: u64 = r11.wrapping_mul(a01);
    let a22: u64 = r2.wrapping_mul(a01);
    let a32: u64 = r3.wrapping_mul(a01);
    let a42: u64 = r4.wrapping_mul(a01);
    let a03: u64 = a02.wrapping_add(r54.wrapping_mul(a11));
    let a13: u64 = a12.wrapping_add(r0.wrapping_mul(a11));
    let a23: u64 = a22.wrapping_add(r11.wrapping_mul(a11));
    let a33: u64 = a32.wrapping_add(r2.wrapping_mul(a11));
    let a43: u64 = a42.wrapping_add(r3.wrapping_mul(a11));
    let a04: u64 = a03.wrapping_add(r53.wrapping_mul(a21));
    let a14: u64 = a13.wrapping_add(r54.wrapping_mul(a21));
    let a24: u64 = a23.wrapping_add(r0.wrapping_mul(a21));
    let a34: u64 = a33.wrapping_add(r11.wrapping_mul(a21));
    let a44: u64 = a43.wrapping_add(r2.wrapping_mul(a21));
    let a05: u64 = a04.wrapping_add(r52.wrapping_mul(a31));
    let a15: u64 = a14.wrapping_add(r53.wrapping_mul(a31));
    let a25: u64 = a24.wrapping_add(r54.wrapping_mul(a31));
    let a35: u64 = a34.wrapping_add(r0.wrapping_mul(a31));
    let a45: u64 = a44.wrapping_add(r11.wrapping_mul(a31));
    let a06: u64 = a05.wrapping_add(r51.wrapping_mul(a41));
    let a16: u64 = a15.wrapping_add(r52.wrapping_mul(a41));
    let a26: u64 = a25.wrapping_add(r53.wrapping_mul(a41));
    let a36: u64 = a35.wrapping_add(r54.wrapping_mul(a41));
    let a46: u64 = a45.wrapping_add(r0.wrapping_mul(a41));
    let t0: u64 = a06;
    let t1: u64 = a16;
    let t2: u64 = a26;
    let t3: u64 = a36;
    let t4: u64 = a46;
    let mask26: u64 = 67108863u64;
    let z0: u64 = t0.wrapping_shr(26u32);
    let z1: u64 = t3.wrapping_shr(26u32);
    let x0: u64 = t0 & mask26;
    let x3: u64 = t3 & mask26;
    let x1: u64 = t1.wrapping_add(z0);
    let x4: u64 = t4.wrapping_add(z1);
    let z01: u64 = x1.wrapping_shr(26u32);
    let z11: u64 = x4.wrapping_shr(26u32);
    let t: u64 = z11.wrapping_shl(2u32);
    let z12: u64 = z11.wrapping_add(t);
    let x11: u64 = x1 & mask26;
    let x41: u64 = x4 & mask26;
    let x2: u64 = t2.wrapping_add(z01);
    let x01: u64 = x0.wrapping_add(z12);
    let z02: u64 = x2.wrapping_shr(26u32);
    let z13: u64 = x01.wrapping_shr(26u32);
    let x21: u64 = x2 & mask26;
    let x02: u64 = x01 & mask26;
    let x31: u64 = x3.wrapping_add(z02);
    let x12: u64 = x11.wrapping_add(z13);
    let z03: u64 = x31.wrapping_shr(26u32);
    let x32: u64 = x31 & mask26;
    let x42: u64 = x41.wrapping_add(z03);
    let o0: u64 = x02;
    let o1: u64 = x12;
    let o2: u64 = x21;
    let o3: u64 = x32;
    let o4: u64 = x42;
    acc0.1[0usize] = o0;
    acc0.1[1usize] = o1;
    acc0.1[2usize] = o2;
    acc0.1[3usize] = o3;
    acc0.1[4usize] = o4
  };
  let mut tmp: [u8; 16] = [0u8; 16usize];
  (tmp[0usize..r as usize]).copy_from_slice(&rem.1[0usize..r as usize]);
  if r > 0u32
  {
    let pre: (&[u64], &[u64]) = (pre0.1).split_at(0usize);
    let acc: (&mut [u64], &mut [u64]) = (acc0.1).split_at_mut(0usize);
    let mut e: [u64; 5] = [0u64; 5usize];
    let u0: u64 = crate::lowstar_endianness::load64_le(&tmp);
    let lo: u64 = u0;
    let u: u64 = crate::lowstar_endianness::load64_le(&tmp[8usize..]);
    let hi: u64 = u;
    let f0: u64 = lo;
    let f1: u64 = hi;
    let f010: u64 = f0 & 67108863u64;
    let f110: u64 = f0.wrapping_shr(26u32) & 67108863u64;
    let f20: u64 = f0.wrapping_shr(52u32) | (f1 & 16383u64).wrapping_shl(12u32);
    let f30: u64 = f1.wrapping_shr(14u32) & 67108863u64;
    let f40: u64 = f1.wrapping_shr(40u32);
    let f01: u64 = f010;
    let f111: u64 = f110;
    let f2: u64 = f20;
    let f3: u64 = f30;
    let f41: u64 = f40;
    e[0usize] = f01;
    e[1usize] = f111;
    e[2usize] = f2;
    e[3usize] = f3;
    e[4usize] = f41;
    let b: u64 = 16777216u64;
    let mask: u64 = b;
    let f4: u64 = e[4usize];
    e[4usize] = f4 | mask;
    let r1: (&[u64], &[u64]) = (pre.1).split_at(0usize);
    let r5: (&[u64], &[u64]) = (r1.1).split_at(5usize);
    let r0: u64 = r5.0[0usize];
    let r11: u64 = r5.0[1usize];
    let r2: u64 = r5.0[2usize];
    let r3: u64 = r5.0[3usize];
    let r4: u64 = r5.0[4usize];
    let r51: u64 = r5.1[1usize];
    let r52: u64 = r5.1[2usize];
    let r53: u64 = r5.1[3usize];
    let r54: u64 = r5.1[4usize];
    let f10: u64 = e[0usize];
    let f11: u64 = e[1usize];
    let f12: u64 = e[2usize];
    let f13: u64 = e[3usize];
    let f14: u64 = e[4usize];
    let a0: u64 = acc.1[0usize];
    let a1: u64 = acc.1[1usize];
    let a2: u64 = acc.1[2usize];
    let a3: u64 = acc.1[3usize];
    let a4: u64 = acc.1[4usize];
    let a01: u64 = a0.wrapping_add(f10);
    let a11: u64 = a1.wrapping_add(f11);
    let a21: u64 = a2.wrapping_add(f12);
    let a31: u64 = a3.wrapping_add(f13);
    let a41: u64 = a4.wrapping_add(f14);
    let a02: u64 = r0.wrapping_mul(a01);
    let a12: u64 = r11.wrapping_mul(a01);
    let a22: u64 = r2.wrapping_mul(a01);
    let a32: u64 = r3.wrapping_mul(a01);
    let a42: u64 = r4.wrapping_mul(a01);
    let a03: u64 = a02.wrapping_add(r54.wrapping_mul(a11));
    let a13: u64 = a12.wrapping_add(r0.wrapping_mul(a11));
    let a23: u64 = a22.wrapping_add(r11.wrapping_mul(a11));
    let a33: u64 = a32.wrapping_add(r2.wrapping_mul(a11));
    let a43: u64 = a42.wrapping_add(r3.wrapping_mul(a11));
    let a04: u64 = a03.wrapping_add(r53.wrapping_mul(a21));
    let a14: u64 = a13.wrapping_add(r54.wrapping_mul(a21));
    let a24: u64 = a23.wrapping_add(r0.wrapping_mul(a21));
    let a34: u64 = a33.wrapping_add(r11.wrapping_mul(a21));
    let a44: u64 = a43.wrapping_add(r2.wrapping_mul(a21));
    let a05: u64 = a04.wrapping_add(r52.wrapping_mul(a31));
    let a15: u64 = a14.wrapping_add(r53.wrapping_mul(a31));
    let a25: u64 = a24.wrapping_add(r54.wrapping_mul(a31));
    let a35: u64 = a34.wrapping_add(r0.wrapping_mul(a31));
    let a45: u64 = a44.wrapping_add(r11.wrapping_mul(a31));
    let a06: u64 = a05.wrapping_add(r51.wrapping_mul(a41));
    let a16: u64 = a15.wrapping_add(r52.wrapping_mul(a41));
    let a26: u64 = a25.wrapping_add(r53.wrapping_mul(a41));
    let a36: u64 = a35.wrapping_add(r54.wrapping_mul(a41));
    let a46: u64 = a45.wrapping_add(r0.wrapping_mul(a41));
    let t0: u64 = a06;
    let t1: u64 = a16;
    let t2: u64 = a26;
    let t3: u64 = a36;
    let t4: u64 = a46;
    let mask26: u64 = 67108863u64;
    let z0: u64 = t0.wrapping_shr(26u32);
    let z1: u64 = t3.wrapping_shr(26u32);
    let x0: u64 = t0 & mask26;
    let x3: u64 = t3 & mask26;
    let x1: u64 = t1.wrapping_add(z0);
    let x4: u64 = t4.wrapping_add(z1);
    let z01: u64 = x1.wrapping_shr(26u32);
    let z11: u64 = x4.wrapping_shr(26u32);
    let t: u64 = z11.wrapping_shl(2u32);
    let z12: u64 = z11.wrapping_add(t);
    let x11: u64 = x1 & mask26;
    let x41: u64 = x4 & mask26;
    let x2: u64 = t2.wrapping_add(z01);
    let x01: u64 = x0.wrapping_add(z12);
    let z02: u64 = x2.wrapping_shr(26u32);
    let z13: u64 = x01.wrapping_shr(26u32);
    let x21: u64 = x2 & mask26;
    let x02: u64 = x01 & mask26;
    let x31: u64 = x3.wrapping_add(z02);
    let x12: u64 = x11.wrapping_add(z13);
    let z03: u64 = x31.wrapping_shr(26u32);
    let x32: u64 = x31 & mask26;
    let x42: u64 = x41.wrapping_add(z03);
    let o0: u64 = x02;
    let o1: u64 = x12;
    let o2: u64 = x21;
    let o3: u64 = x32;
    let o4: u64 = x42;
    acc.1[0usize] = o0;
    acc.1[1usize] = o1;
    acc.1[2usize] = o2;
    acc.1[3usize] = o3;
    acc.1[4usize] = o4;
    return ()
  }
}
