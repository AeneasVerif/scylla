#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct FStar_Pervasives_Native_option___uint8_t_ <'a>
{ pub tag: u8, pub v: &'a [u8] }

pub fn Hacl_MAC_Poly1305_mac(output: &mut [u8], input: &[u8], input_len: u32, key: &[u8])
{
  let mut ctx: [u64; 25] = [0u64; 25usize];
  Hacl_MAC_Poly1305_poly1305_init(&mut ctx, key);
  poly1305_update(&mut ctx, input_len, input);
  Hacl_MAC_Poly1305_poly1305_finish(output, key, &mut ctx)
}

pub fn Hacl_MAC_Poly1305_poly1305_finish(tag: &mut [u8], key: &[u8], ctx: &mut [u64])
{
  let acc: (&mut [u64], &mut [u64]) = ctx.split_at_mut(0usize);
  let ks: (&[u8], &[u8]) = key.split_at(16usize);
  let f0: u64 = acc.1[0usize];
  let f13: u64 = acc.1[1usize];
  let f23: u64 = acc.1[2usize];
  let f33: u64 = acc.1[3usize];
  let f40: u64 = acc.1[4usize];
  let l0: u64 = f0.wrapping_add(0u64);
  let tmp00: u64 = l0 & 67108863u64;
  let c00: u64 = l0.wrapping_shr(26u32);
  let l1: u64 = f13.wrapping_add(c00);
  let tmp10: u64 = l1 & 67108863u64;
  let c10: u64 = l1.wrapping_shr(26u32);
  let l2: u64 = f23.wrapping_add(c10);
  let tmp20: u64 = l2 & 67108863u64;
  let c20: u64 = l2.wrapping_shr(26u32);
  let l3: u64 = f33.wrapping_add(c20);
  let tmp30: u64 = l3 & 67108863u64;
  let c30: u64 = l3.wrapping_shr(26u32);
  let l4: u64 = f40.wrapping_add(c30);
  let tmp40: u64 = l4 & 67108863u64;
  let c40: u64 = l4.wrapping_shr(26u32);
  let f010: u64 = tmp00.wrapping_add(c40.wrapping_mul(5u64));
  let f110: u64 = tmp10;
  let f210: u64 = tmp20;
  let f310: u64 = tmp30;
  let f410: u64 = tmp40;
  let l: u64 = f010.wrapping_add(0u64);
  let tmp0: u64 = l & 67108863u64;
  let c0: u64 = l.wrapping_shr(26u32);
  let l5: u64 = f110.wrapping_add(c0);
  let tmp1: u64 = l5 & 67108863u64;
  let c1: u64 = l5.wrapping_shr(26u32);
  let l6: u64 = f210.wrapping_add(c1);
  let tmp2: u64 = l6 & 67108863u64;
  let c2: u64 = l6.wrapping_shr(26u32);
  let l7: u64 = f310.wrapping_add(c2);
  let tmp3: u64 = l7 & 67108863u64;
  let c3: u64 = l7.wrapping_shr(26u32);
  let l8: u64 = f410.wrapping_add(c3);
  let tmp4: u64 = l8 & 67108863u64;
  let c4: u64 = l8.wrapping_shr(26u32);
  let f02: u64 = tmp0.wrapping_add(c4.wrapping_mul(5u64));
  let f12: u64 = tmp1;
  let f22: u64 = tmp2;
  let f32: u64 = tmp3;
  let f42: u64 = tmp4;
  let mh: u64 = 67108863u64;
  let ml: u64 = 67108859u64;
  let mask: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(f42, mh);
  let mask1: u64 = mask & crate::hacl_krmllib::FStar_UInt64_eq_mask(f32, mh);
  let mask2: u64 = mask1 & crate::hacl_krmllib::FStar_UInt64_eq_mask(f22, mh);
  let mask3: u64 = mask2 & crate::hacl_krmllib::FStar_UInt64_eq_mask(f12, mh);
  let mask4: u64 = mask3 & ! ! crate::hacl_krmllib::FStar_UInt64_gte_mask(f02, ml);
  let ph: u64 = mask4 & mh;
  let pl: u64 = mask4 & ml;
  let o0: u64 = f02.wrapping_sub(pl);
  let o1: u64 = f12.wrapping_sub(ph);
  let o2: u64 = f22.wrapping_sub(ph);
  let o3: u64 = f32.wrapping_sub(ph);
  let o4: u64 = f42.wrapping_sub(ph);
  let f011: u64 = o0;
  let f111: u64 = o1;
  let f211: u64 = o2;
  let f311: u64 = o3;
  let f411: u64 = o4;
  acc.1[0usize] = f011;
  acc.1[1usize] = f111;
  acc.1[2usize] = f211;
  acc.1[3usize] = f311;
  acc.1[4usize] = f411;
  let f00: u64 = acc.1[0usize];
  let f1: u64 = acc.1[1usize];
  let f2: u64 = acc.1[2usize];
  let f3: u64 = acc.1[3usize];
  let f4: u64 = acc.1[4usize];
  let f01: u64 = f00;
  let f112: u64 = f1;
  let f212: u64 = f2;
  let f312: u64 = f3;
  let f41: u64 = f4;
  let lo: u64 = f01 | f112.wrapping_shl(26u32) | f212.wrapping_shl(52u32);
  let hi: u64 = f212.wrapping_shr(12u32) | f312.wrapping_shl(14u32) | f41.wrapping_shl(40u32);
  let f10: u64 = lo;
  let f11: u64 = hi;
  let u0: u64 = crate::lowstar_endianness::load64_le(ks.1);
  let lo0: u64 = u0;
  let u: u64 = crate::lowstar_endianness::load64_le(&ks.1[8usize..]);
  let hi0: u64 = u;
  let f20: u64 = lo0;
  let f21: u64 = hi0;
  let r0: u64 = f10.wrapping_add(f20);
  let r1: u64 = f11.wrapping_add(f21);
  let c: u64 = (r0 ^ (r0 ^ f20 | r0.wrapping_sub(f20) ^ f20)).wrapping_shr(63u32);
  let r11: u64 = r1.wrapping_add(c);
  let f30: u64 = r0;
  let f31: u64 = r11;
  crate::lowstar_endianness::store64_le(tag, f30);
  crate::lowstar_endianness::store64_le(&mut tag[8usize..], f31)
}

pub fn Hacl_MAC_Poly1305_poly1305_init(ctx: &mut [u64], key: &[u8])
{
  let acc: (&mut [u64], &mut [u64]) = ctx.split_at_mut(0usize);
  let pre: (&mut [u64], &mut [u64]) = (acc.1).split_at_mut(5usize);
  let kr: (&[u8], &[u8]) = key.split_at(0usize);
  pre.0[0usize] = 0u64;
  pre.0[1usize] = 0u64;
  pre.0[2usize] = 0u64;
  pre.0[3usize] = 0u64;
  pre.0[4usize] = 0u64;
  let u0: u64 = crate::lowstar_endianness::load64_le(kr.1);
  let lo: u64 = u0;
  let u: u64 = crate::lowstar_endianness::load64_le(&kr.1[8usize..]);
  let hi: u64 = u;
  let mask0: u64 = 1152921487695413247u64;
  let mask1: u64 = 1152921487695413244u64;
  let lo1: u64 = lo & mask0;
  let hi1: u64 = hi & mask1;
  let r: (&mut [u64], &mut [u64]) = (pre.1).split_at_mut(0usize);
  let r5: (&mut [u64], &mut [u64]) = (r.1).split_at_mut(5usize);
  let rn: (&mut [u64], &mut [u64]) = (r5.1).split_at_mut(5usize);
  let rn_5: (&mut [u64], &mut [u64]) = (rn.1).split_at_mut(5usize);
  let r_vec0: u64 = lo1;
  let r_vec1: u64 = hi1;
  let f00: u64 = r_vec0 & 67108863u64;
  let f10: u64 = r_vec0.wrapping_shr(26u32) & 67108863u64;
  let f20: u64 = r_vec0.wrapping_shr(52u32) | (r_vec1 & 16383u64).wrapping_shl(12u32);
  let f30: u64 = r_vec1.wrapping_shr(14u32) & 67108863u64;
  let f40: u64 = r_vec1.wrapping_shr(40u32);
  let f0: u64 = f00;
  let f1: u64 = f10;
  let f2: u64 = f20;
  let f3: u64 = f30;
  let f4: u64 = f40;
  r5.0[0usize] = f0;
  r5.0[1usize] = f1;
  r5.0[2usize] = f2;
  r5.0[3usize] = f3;
  r5.0[4usize] = f4;
  let f200: u64 = r5.0[0usize];
  let f21: u64 = r5.0[1usize];
  let f22: u64 = r5.0[2usize];
  let f23: u64 = r5.0[3usize];
  let f24: u64 = r5.0[4usize];
  rn.0[0usize] = f200.wrapping_mul(5u64);
  rn.0[1usize] = f21.wrapping_mul(5u64);
  rn.0[2usize] = f22.wrapping_mul(5u64);
  rn.0[3usize] = f23.wrapping_mul(5u64);
  rn.0[4usize] = f24.wrapping_mul(5u64);
  rn_5.0[0usize] = r5.0[0usize];
  rn_5.0[1usize] = r5.0[1usize];
  rn_5.0[2usize] = r5.0[2usize];
  rn_5.0[3usize] = r5.0[3usize];
  rn_5.0[4usize] = r5.0[4usize];
  rn_5.1[0usize] = rn.0[0usize];
  rn_5.1[1usize] = rn.0[1usize];
  rn_5.1[2usize] = rn.0[2usize];
  rn_5.1[3usize] = rn.0[3usize];
  rn_5.1[4usize] = rn.0[4usize]
}

#[derive(PartialEq)]
#[repr(C)]
pub
struct Hacl_MAC_Poly1305_state_t <'a>
{
  pub block_state: &'a mut [u64],
  pub buf: &'a mut [u8],
  pub total_len: u64,
  pub p_key: &'a [u8]
}

pub fn Hacl_MAC_Poly1305_update(
  state: &mut [Hacl_MAC_Poly1305_state_t],
  chunk: &[u8],
  chunk_len: u32
) ->
    u8
{
  let block_state: &mut [u64] = (state[0usize]).block_state;
  let total_len: u64 = (state[0usize]).total_len;
  if chunk_len as u64 > 4294967295u64.wrapping_sub(total_len) { return 3u8 };
  let mut sz: u32;
  if total_len.wrapping_rem(16u32 as u64) == 0u64 && total_len > 0u64
  { sz = 16u32 }
  else
  { sz = total_len.wrapping_rem(16u32 as u64) as u32 };
  if chunk_len <= 16u32.wrapping_sub(sz)
  {
    let buf: &mut [u8] = (state[0usize]).buf;
    let total_len1: u64 = (state[0usize]).total_len;
    let k_1: &[u8] = (state[0usize]).p_key;
    let mut sz1: u32;
    if total_len1.wrapping_rem(16u32 as u64) == 0u64 && total_len1 > 0u64
    { sz1 = 16u32 }
    else
    { sz1 = total_len1.wrapping_rem(16u32 as u64) as u32 };
    let buf2: (&mut [u8], &mut [u8]) = buf.split_at_mut(sz1 as usize);
    (buf2.1[0usize..chunk_len as usize]).copy_from_slice(&chunk[0usize..chunk_len as usize]);
    let total_len2: u64 = total_len1.wrapping_add(chunk_len as u64);
    (state[0usize]).total_len = total_len2;
    (state[0usize]).p_key = k_1
  }
  else if sz == 0u32
  {
    let buf: &mut [u8] = (state[0usize]).buf;
    let total_len1: u64 = (state[0usize]).total_len;
    let k_1: &[u8] = (state[0usize]).p_key;
    let mut sz1: u32;
    if total_len1.wrapping_rem(16u32 as u64) == 0u64 && total_len1 > 0u64
    { sz1 = 16u32 }
    else
    { sz1 = total_len1.wrapping_rem(16u32 as u64) as u32 };
    if sz1 != 0u32 { poly1305_update(block_state, 16u32, buf) };
    let mut ite: u32;
    if (chunk_len as u64).wrapping_rem(16u32 as u64) == 0u64 && chunk_len as u64 > 0u64
    { ite = 16u32 }
    else
    { ite = (chunk_len as u64).wrapping_rem(16u32 as u64) as u32 };
    let n_blocks: u32 = chunk_len.wrapping_sub(ite).wrapping_div(16u32);
    let data1_len: u32 = n_blocks.wrapping_mul(16u32);
    let data2_len: u32 = chunk_len.wrapping_sub(data1_len);
    let data1: (&[u8], &[u8]) = chunk.split_at(0usize);
    let data2: (&[u8], &[u8]) = (data1.1).split_at(data1_len as usize);
    poly1305_update(block_state, data1_len, data2.0);
    let dst: (&mut [u8], &mut [u8]) = buf.split_at_mut(0usize);
    (dst.1[0usize..data2_len as usize]).copy_from_slice(&data2.1[0usize..data2_len as usize]);
    (state[0usize]).total_len = total_len1.wrapping_add(chunk_len as u64);
    (state[0usize]).p_key = k_1
  }
  else
  {
    let diff: u32 = 16u32.wrapping_sub(sz);
    let chunk1: (&[u8], &[u8]) = chunk.split_at(0usize);
    let chunk2: (&[u8], &[u8]) = (chunk1.1).split_at(diff as usize);
    let buf: &mut [u8] = (state[0usize]).buf;
    let total_len10: u64 = (state[0usize]).total_len;
    let k_1: &[u8] = (state[0usize]).p_key;
    let mut sz10: u32;
    if total_len10.wrapping_rem(16u32 as u64) == 0u64 && total_len10 > 0u64
    { sz10 = 16u32 }
    else
    { sz10 = total_len10.wrapping_rem(16u32 as u64) as u32 };
    let buf2: (&mut [u8], &mut [u8]) = buf.split_at_mut(sz10 as usize);
    (buf2.1[0usize..diff as usize]).copy_from_slice(&chunk2.0[0usize..diff as usize]);
    let total_len2: u64 = total_len10.wrapping_add(diff as u64);
    (state[0usize]).total_len = total_len2;
    (state[0usize]).p_key = k_1;
    let buf0: &mut [u8] = (state[0usize]).buf;
    let total_len1: u64 = (state[0usize]).total_len;
    let k_10: &[u8] = (state[0usize]).p_key;
    let mut sz1: u32;
    if total_len1.wrapping_rem(16u32 as u64) == 0u64 && total_len1 > 0u64
    { sz1 = 16u32 }
    else
    { sz1 = total_len1.wrapping_rem(16u32 as u64) as u32 };
    if sz1 != 0u32 { poly1305_update(block_state, 16u32, buf0) };
    let mut ite: u32;
    if
    (chunk_len.wrapping_sub(diff) as u64).wrapping_rem(16u32 as u64) == 0u64
    &&
    chunk_len.wrapping_sub(diff) as u64 > 0u64
    { ite = 16u32 }
    else
    { ite = (chunk_len.wrapping_sub(diff) as u64).wrapping_rem(16u32 as u64) as u32 };
    let n_blocks: u32 = chunk_len.wrapping_sub(diff).wrapping_sub(ite).wrapping_div(16u32);
    let data1_len: u32 = n_blocks.wrapping_mul(16u32);
    let data2_len: u32 = chunk_len.wrapping_sub(diff).wrapping_sub(data1_len);
    let data1: (&[u8], &[u8]) = (chunk2.1).split_at(0usize);
    let data2: (&[u8], &[u8]) = (data1.1).split_at(data1_len as usize);
    poly1305_update(block_state, data1_len, data2.0);
    let dst: (&mut [u8], &mut [u8]) = buf0.split_at_mut(0usize);
    (dst.1[0usize..data2_len as usize]).copy_from_slice(&data2.1[0usize..data2_len as usize]);
    (state[0usize]).total_len = total_len1.wrapping_add(chunk_len.wrapping_sub(diff) as u64);
    (state[0usize]).p_key = k_10
  };
  return 0u8
}

pub fn poly1305_update(ctx: &mut [u64], len: u32, text: &[u8])
{
  let pre: (&mut [u64], &mut [u64]) = ctx.split_at_mut(5usize);
  let acc: (&mut [u64], &mut [u64]) = (pre.0).split_at_mut(0usize);
  let nb: u32 = len.wrapping_div(16u32);
  let rem: u32 = len.wrapping_rem(16u32);
  for i in 0u32..nb
  {
    let block: (&[u8], &[u8]) = text.split_at(i.wrapping_mul(16u32) as usize);
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
    acc.1[4usize] = o4
  };
  if rem > 0u32
  {
    let last: (&[u8], &[u8]) = text.split_at(nb.wrapping_mul(16u32) as usize);
    let mut e: [u64; 5] = [0u64; 5usize];
    let mut tmp: [u8; 16] = [0u8; 16usize];
    (tmp[0usize..rem as usize]).copy_from_slice(&last.1[0usize..rem as usize]);
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
    let b: u64 = 1u64.wrapping_shl(rem.wrapping_mul(8u32).wrapping_rem(26u32));
    let mask: u64 = b;
    let fi: u64 = e[rem.wrapping_mul(8u32).wrapping_div(26u32) as usize];
    e[rem.wrapping_mul(8u32).wrapping_div(26u32) as usize] = fi | mask;
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
    return ()
  }
}
