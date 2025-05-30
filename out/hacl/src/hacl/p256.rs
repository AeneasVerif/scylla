#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

pub fn Hacl_Impl_P256_DH_ecp256dh_i(public_key: &mut [u8], private_key: &[u8]) -> bool
{
  let mut tmp: [u64; 16] = [0u64; 16usize];
  let sk: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
  let pk: (&mut [u64], &mut [u64]) = (sk.1).split_at_mut(4usize);
  bn_from_bytes_be4(pk.0, private_key);
  let is_b_valid: u64 = bn_is_lt_order_and_gt_zero_mask4(pk.0);
  let mut oneq: [u64; 4] = [0u64; 4usize];
  oneq[0usize] = 1u64;
  oneq[1usize] = 0u64;
  oneq[2usize] = 0u64;
  oneq[3usize] = 0u64;
  for i in 0u32..4u32
  {
    let uu____0: u64 = oneq[i as usize];
    let x: u64 = uu____0 ^ is_b_valid & (pk.0[i as usize] ^ uu____0);
    let os: (&mut [u64], &mut [u64]) = (pk.0).split_at_mut(0usize);
    os.1[i as usize] = x
  };
  let is_sk_valid: u64 = is_b_valid;
  point_mul_g(pk.1, pk.0);
  point_store(public_key, pk.1);
  return is_sk_valid == 18446744073709551615u64
}

pub fn Hacl_Impl_P256_DH_ecp256dh_r(
  shared_secret: &mut [u8],
  their_pubkey: &[u8],
  private_key: &[u8]
) ->
    bool
{
  let mut tmp: [u64; 16] = [0u64; 16usize];
  let sk: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
  let pk: (&mut [u64], &mut [u64]) = (sk.1).split_at_mut(4usize);
  let is_pk_valid: bool = load_point_vartime(pk.1, their_pubkey);
  bn_from_bytes_be4(pk.0, private_key);
  let is_b_valid: u64 = bn_is_lt_order_and_gt_zero_mask4(pk.0);
  let mut oneq: [u64; 4] = [0u64; 4usize];
  oneq[0usize] = 1u64;
  oneq[1usize] = 0u64;
  oneq[2usize] = 0u64;
  oneq[3usize] = 0u64;
  for i in 0u32..4u32
  {
    let uu____0: u64 = oneq[i as usize];
    let x: u64 = uu____0 ^ is_b_valid & (pk.0[i as usize] ^ uu____0);
    let os: (&mut [u64], &mut [u64]) = (pk.0).split_at_mut(0usize);
    os.1[i as usize] = x
  };
  let is_sk_valid: u64 = is_b_valid;
  let mut ss_proj: [u64; 12] = [0u64; 12usize];
  if is_pk_valid
  {
    point_mul(&mut ss_proj, pk.0, pk.1);
    point_store(shared_secret, &ss_proj)
  };
  return is_sk_valid == 18446744073709551615u64 && is_pk_valid
}

pub fn Hacl_P256_compressed_to_raw(pk: &[u8], pk_raw: &mut [u8]) -> bool
{
  let mut xa: [u64; 4] = [0u64; 4usize];
  let mut ya: [u64; 4] = [0u64; 4usize];
  let b: bool = aff_point_decompress_vartime(&mut xa, &mut ya, pk);
  let pk_xb: (&[u8], &[u8]) = pk.split_at(1usize);
  if b
  {
    (pk_raw[0usize..32usize]).copy_from_slice(&pk_xb.1[0usize..32usize]);
    bn_to_bytes_be4(&mut pk_raw[32usize..], &ya)
  };
  return b
}

pub fn Hacl_P256_dh_initiator(public_key: &mut [u8], private_key: &[u8]) -> bool
{ return Hacl_Impl_P256_DH_ecp256dh_i(public_key, private_key) }

pub fn Hacl_P256_dh_responder(
  shared_secret: &mut [u8],
  their_pubkey: &[u8],
  private_key: &[u8]
) ->
    bool
{ return Hacl_Impl_P256_DH_ecp256dh_r(shared_secret, their_pubkey, private_key) }

pub fn Hacl_P256_ecdsa_sign_p256_sha2(
  signature: &mut [u8],
  msg_len: u32,
  msg: &[u8],
  private_key: &[u8],
  nonce: &[u8]
) ->
    bool
{
  let mut m_q: [u64; 4] = [0u64; 4usize];
  let mut mHash: [u8; 32] = [0u8; 32usize];
  crate::hacl::hash_sha2::Hacl_Hash_SHA2_hash_256(&mut mHash, msg, msg_len);
  crate::lowstar::ignore::ignore::<u32>(msg_len);
  let mHash32: (&[u8], &[u8]) = mHash.split_at(0usize);
  bn_from_bytes_be4(&mut m_q, mHash32.1);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&m_q[0usize..4usize]);
  qmod_short(&mut m_q, &x_copy);
  let res: bool = ecdsa_sign_msg_as_qelem(signature, &mut m_q, private_key, nonce);
  return res
}

pub fn Hacl_P256_ecdsa_sign_p256_sha384(
  signature: &mut [u8],
  msg_len: u32,
  msg: &[u8],
  private_key: &[u8],
  nonce: &[u8]
) ->
    bool
{
  let mut m_q: [u64; 4] = [0u64; 4usize];
  let mut mHash: [u8; 48] = [0u8; 48usize];
  crate::hacl::hash_sha2::Hacl_Hash_SHA2_hash_384(&mut mHash, msg, msg_len);
  crate::lowstar::ignore::ignore::<u32>(msg_len);
  let mHash32: (&[u8], &[u8]) = mHash.split_at(0usize);
  bn_from_bytes_be4(&mut m_q, mHash32.1);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&m_q[0usize..4usize]);
  qmod_short(&mut m_q, &x_copy);
  let res: bool = ecdsa_sign_msg_as_qelem(signature, &mut m_q, private_key, nonce);
  return res
}

pub fn Hacl_P256_ecdsa_sign_p256_sha512(
  signature: &mut [u8],
  msg_len: u32,
  msg: &[u8],
  private_key: &[u8],
  nonce: &[u8]
) ->
    bool
{
  let mut m_q: [u64; 4] = [0u64; 4usize];
  let mut mHash: [u8; 64] = [0u8; 64usize];
  crate::hacl::hash_sha2::Hacl_Hash_SHA2_hash_512(&mut mHash, msg, msg_len);
  crate::lowstar::ignore::ignore::<u32>(msg_len);
  let mHash32: (&[u8], &[u8]) = mHash.split_at(0usize);
  bn_from_bytes_be4(&mut m_q, mHash32.1);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&m_q[0usize..4usize]);
  qmod_short(&mut m_q, &x_copy);
  let res: bool = ecdsa_sign_msg_as_qelem(signature, &mut m_q, private_key, nonce);
  return res
}

pub fn Hacl_P256_ecdsa_sign_p256_without_hash(
  signature: &mut [u8],
  msg_len: u32,
  msg: &[u8],
  private_key: &[u8],
  nonce: &[u8]
) ->
    bool
{
  let mut m_q: [u64; 4] = [0u64; 4usize];
  let mut mHash: [u8; 32] = [0u8; 32usize];
  (mHash[0usize..32usize]).copy_from_slice(&msg[0usize..32usize]);
  crate::lowstar::ignore::ignore::<u32>(msg_len);
  let mHash32: (&[u8], &[u8]) = mHash.split_at(0usize);
  bn_from_bytes_be4(&mut m_q, mHash32.1);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&m_q[0usize..4usize]);
  qmod_short(&mut m_q, &x_copy);
  let res: bool = ecdsa_sign_msg_as_qelem(signature, &mut m_q, private_key, nonce);
  return res
}

pub fn Hacl_P256_ecdsa_verif_p256_sha2(
  msg_len: u32,
  msg: &[u8],
  public_key: &[u8],
  signature_r: &[u8],
  signature_s: &[u8]
) ->
    bool
{
  let mut m_q: [u64; 4] = [0u64; 4usize];
  let mut mHash: [u8; 32] = [0u8; 32usize];
  crate::hacl::hash_sha2::Hacl_Hash_SHA2_hash_256(&mut mHash, msg, msg_len);
  crate::lowstar::ignore::ignore::<u32>(msg_len);
  let mHash32: (&[u8], &[u8]) = mHash.split_at(0usize);
  bn_from_bytes_be4(&mut m_q, mHash32.1);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&m_q[0usize..4usize]);
  qmod_short(&mut m_q, &x_copy);
  let res: bool = ecdsa_verify_msg_as_qelem(&m_q, public_key, signature_r, signature_s);
  return res
}

pub fn Hacl_P256_ecdsa_verif_p256_sha384(
  msg_len: u32,
  msg: &[u8],
  public_key: &[u8],
  signature_r: &[u8],
  signature_s: &[u8]
) ->
    bool
{
  let mut m_q: [u64; 4] = [0u64; 4usize];
  let mut mHash: [u8; 48] = [0u8; 48usize];
  crate::hacl::hash_sha2::Hacl_Hash_SHA2_hash_384(&mut mHash, msg, msg_len);
  crate::lowstar::ignore::ignore::<u32>(msg_len);
  let mHash32: (&[u8], &[u8]) = mHash.split_at(0usize);
  bn_from_bytes_be4(&mut m_q, mHash32.1);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&m_q[0usize..4usize]);
  qmod_short(&mut m_q, &x_copy);
  let res: bool = ecdsa_verify_msg_as_qelem(&m_q, public_key, signature_r, signature_s);
  return res
}

pub fn Hacl_P256_ecdsa_verif_p256_sha512(
  msg_len: u32,
  msg: &[u8],
  public_key: &[u8],
  signature_r: &[u8],
  signature_s: &[u8]
) ->
    bool
{
  let mut m_q: [u64; 4] = [0u64; 4usize];
  let mut mHash: [u8; 64] = [0u8; 64usize];
  crate::hacl::hash_sha2::Hacl_Hash_SHA2_hash_512(&mut mHash, msg, msg_len);
  crate::lowstar::ignore::ignore::<u32>(msg_len);
  let mHash32: (&[u8], &[u8]) = mHash.split_at(0usize);
  bn_from_bytes_be4(&mut m_q, mHash32.1);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&m_q[0usize..4usize]);
  qmod_short(&mut m_q, &x_copy);
  let res: bool = ecdsa_verify_msg_as_qelem(&m_q, public_key, signature_r, signature_s);
  return res
}

pub fn Hacl_P256_ecdsa_verif_without_hash(
  msg_len: u32,
  msg: &[u8],
  public_key: &[u8],
  signature_r: &[u8],
  signature_s: &[u8]
) ->
    bool
{
  let mut m_q: [u64; 4] = [0u64; 4usize];
  let mut mHash: [u8; 32] = [0u8; 32usize];
  (mHash[0usize..32usize]).copy_from_slice(&msg[0usize..32usize]);
  crate::lowstar::ignore::ignore::<u32>(msg_len);
  let mHash32: (&[u8], &[u8]) = mHash.split_at(0usize);
  bn_from_bytes_be4(&mut m_q, mHash32.1);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&m_q[0usize..4usize]);
  qmod_short(&mut m_q, &x_copy);
  let res: bool = ecdsa_verify_msg_as_qelem(&m_q, public_key, signature_r, signature_s);
  return res
}

#[inline] pub fn Hacl_P256_point_double(res: &mut [u64], p: &[u64])
{
  let mut tmp: [u64; 20] = [0u64; 20usize];
  let x3: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
  let y3: (&mut [u64], &mut [u64]) = (x3.1).split_at_mut(4usize);
  let z3: (&mut [u64], &mut [u64]) = (y3.1).split_at_mut(4usize);
  let t0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
  let t1: (&mut [u64], &mut [u64]) = (t0.1).split_at_mut(4usize);
  let t2: (&mut [u64], &mut [u64]) = (t1.1).split_at_mut(4usize);
  let t3: (&mut [u64], &mut [u64]) = (t2.1).split_at_mut(4usize);
  let t4: (&mut [u64], &mut [u64]) = (t3.1).split_at_mut(4usize);
  let x: (&[u64], &[u64]) = p.split_at(0usize);
  let y: (&[u64], &[u64]) = (x.1).split_at(4usize);
  let z0: (&[u64], &[u64]) = (y.1).split_at(4usize);
  fsqr0(t1.0, y.0);
  fsqr0(t2.0, z0.0);
  fsqr0(t3.0, z0.1);
  fmul0(t4.0, y.0, z0.0);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&t4.0[0usize..4usize]);
  fadd0(t4.0, &x_copy, &x_copy);
  fmul0(t4.1, z0.0, z0.1);
  let x0: (&[u64], &[u64]) = (y.0).split_at(0usize);
  let z: (&[u64], &[u64]) = (z0.1).split_at(0usize);
  fmul0(z3.1, x0.1, z.1);
  let mut x_copy0: [u64; 4] = [0u64; 4usize];
  (x_copy0[0usize..4usize]).copy_from_slice(&z3.1[0usize..4usize]);
  fadd0(z3.1, &x_copy0, &x_copy0);
  fmul_by_b_coeff(z3.0, t3.0);
  let mut x_copy1: [u64; 4] = [0u64; 4usize];
  (x_copy1[0usize..4usize]).copy_from_slice(&z3.0[0usize..4usize]);
  fsub0(z3.0, &x_copy1, z3.1);
  fadd0(y3.0, z3.0, z3.0);
  let mut y_copy: [u64; 4] = [0u64; 4usize];
  (y_copy[0usize..4usize]).copy_from_slice(&z3.0[0usize..4usize]);
  fadd0(z3.0, y3.0, &y_copy);
  fsub0(y3.0, t2.0, z3.0);
  let mut y_copy0: [u64; 4] = [0u64; 4usize];
  (y_copy0[0usize..4usize]).copy_from_slice(&z3.0[0usize..4usize]);
  fadd0(z3.0, t2.0, &y_copy0);
  let mut y_copy1: [u64; 4] = [0u64; 4usize];
  (y_copy1[0usize..4usize]).copy_from_slice(&z3.0[0usize..4usize]);
  fmul0(z3.0, y3.0, &y_copy1);
  let mut x_copy2: [u64; 4] = [0u64; 4usize];
  (x_copy2[0usize..4usize]).copy_from_slice(&y3.0[0usize..4usize]);
  fmul0(y3.0, &x_copy2, t4.0);
  fadd0(t4.0, t3.0, t3.0);
  let mut x_copy3: [u64; 4] = [0u64; 4usize];
  (x_copy3[0usize..4usize]).copy_from_slice(&t3.0[0usize..4usize]);
  fadd0(t3.0, &x_copy3, t4.0);
  let mut x_copy4: [u64; 4] = [0u64; 4usize];
  (x_copy4[0usize..4usize]).copy_from_slice(&z3.1[0usize..4usize]);
  fmul_by_b_coeff(z3.1, &x_copy4);
  let mut x_copy5: [u64; 4] = [0u64; 4usize];
  (x_copy5[0usize..4usize]).copy_from_slice(&z3.1[0usize..4usize]);
  fsub0(z3.1, &x_copy5, t3.0);
  let mut x_copy6: [u64; 4] = [0u64; 4usize];
  (x_copy6[0usize..4usize]).copy_from_slice(&z3.1[0usize..4usize]);
  fsub0(z3.1, &x_copy6, t1.0);
  fadd0(t4.0, z3.1, z3.1);
  let mut x_copy7: [u64; 4] = [0u64; 4usize];
  (x_copy7[0usize..4usize]).copy_from_slice(&z3.1[0usize..4usize]);
  fadd0(z3.1, &x_copy7, t4.0);
  fadd0(t4.0, t1.0, t1.0);
  let mut y_copy2: [u64; 4] = [0u64; 4usize];
  (y_copy2[0usize..4usize]).copy_from_slice(&t1.0[0usize..4usize]);
  fadd0(t1.0, t4.0, &y_copy2);
  let mut x_copy8: [u64; 4] = [0u64; 4usize];
  (x_copy8[0usize..4usize]).copy_from_slice(&t1.0[0usize..4usize]);
  fsub0(t1.0, &x_copy8, t3.0);
  let mut x_copy9: [u64; 4] = [0u64; 4usize];
  (x_copy9[0usize..4usize]).copy_from_slice(&t1.0[0usize..4usize]);
  fmul0(t1.0, &x_copy9, z3.1);
  let mut x_copy10: [u64; 4] = [0u64; 4usize];
  (x_copy10[0usize..4usize]).copy_from_slice(&z3.0[0usize..4usize]);
  fadd0(z3.0, &x_copy10, t1.0);
  fadd0(t1.0, t4.1, t4.1);
  let mut y_copy3: [u64; 4] = [0u64; 4usize];
  (y_copy3[0usize..4usize]).copy_from_slice(&z3.1[0usize..4usize]);
  fmul0(z3.1, t1.0, &y_copy3);
  let mut x_copy11: [u64; 4] = [0u64; 4usize];
  (x_copy11[0usize..4usize]).copy_from_slice(&y3.0[0usize..4usize]);
  fsub0(y3.0, &x_copy11, z3.1);
  fmul0(z3.1, t1.0, t2.0);
  let mut x_copy12: [u64; 4] = [0u64; 4usize];
  (x_copy12[0usize..4usize]).copy_from_slice(&z3.1[0usize..4usize]);
  fadd0(z3.1, &x_copy12, &x_copy12);
  let mut x_copy13: [u64; 4] = [0u64; 4usize];
  (x_copy13[0usize..4usize]).copy_from_slice(&z3.1[0usize..4usize]);
  fadd0(z3.1, &x_copy13, &x_copy13)
}

pub fn Hacl_P256_raw_to_compressed(pk_raw: &[u8], pk: &mut [u8])
{
  let pk_x: (&[u8], &[u8]) = pk_raw.split_at(0usize);
  let pk_y: (&[u8], &[u8]) = (pk_x.1).split_at(32usize);
  let mut bn_f: [u64; 4] = [0u64; 4usize];
  bn_from_bytes_be4(&mut bn_f, pk_y.1);
  let is_odd_f: u64 = bn_f[0usize] & 1u64;
  pk[0usize] = (is_odd_f as u8 as u32).wrapping_add(2u32) as u8;
  ((&mut pk[1usize..])[0usize..32usize]).copy_from_slice(&pk_y.0[0usize..32usize])
}

pub fn Hacl_P256_raw_to_uncompressed(pk_raw: &[u8], pk: &mut [u8])
{
  pk[0usize] = 4u8;
  ((&mut pk[1usize..])[0usize..64usize]).copy_from_slice(&pk_raw[0usize..64usize])
}

pub fn Hacl_P256_uncompressed_to_raw(pk: &[u8], pk_raw: &mut [u8]) -> bool
{
  let pk0: u8 = pk[0usize];
  if pk0 as u32 != 4u32 { return false };
  (pk_raw[0usize..64usize]).copy_from_slice(&(&pk[1usize..])[0usize..64usize]);
  return true
}

pub fn Hacl_P256_validate_private_key(private_key: &[u8]) -> bool
{
  let mut bn_sk: [u64; 4] = [0u64; 4usize];
  bn_from_bytes_be4(&mut bn_sk, private_key);
  let res: u64 = bn_is_lt_order_and_gt_zero_mask4(&bn_sk);
  return res == 18446744073709551615u64
}

pub fn Hacl_P256_validate_public_key(public_key: &[u8]) -> bool
{
  let mut point_jac: [u64; 12] = [0u64; 12usize];
  let res: bool = load_point_vartime(&mut point_jac, public_key);
  return res
}

#[inline] pub fn aff_point_decompress_vartime(x: &mut [u64], y: &mut [u64], s: &[u8]) -> bool
{
  let s0: u8 = s[0usize];
  let s01: u8 = s0;
  if ! (s01 as u32 == 2u32 || s01 as u32 == 3u32) { return false };
  let xb: (&[u8], &[u8]) = s.split_at(1usize);
  bn_from_bytes_be4(x, xb.1);
  let is_x_valid: u64 = bn_is_lt_prime_mask4(x);
  let is_x_valid1: bool = is_x_valid == 18446744073709551615u64;
  let is_y_odd: bool = s01 as u32 == 3u32;
  if ! is_x_valid1 { return false };
  let mut y2M: [u64; 4] = [0u64; 4usize];
  let mut xM: [u64; 4] = [0u64; 4usize];
  let mut yM: [u64; 4] = [0u64; 4usize];
  to_mont(&mut xM, x);
  let mut tmp: [u64; 4] = [0u64; 4usize];
  fcube(&mut y2M, &xM);
  make_a_coeff(&mut tmp);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&tmp[0usize..4usize]);
  fmul0(&mut tmp, &x_copy, &xM);
  let mut y_copy: [u64; 4] = [0u64; 4usize];
  (y_copy[0usize..4usize]).copy_from_slice(&y2M[0usize..4usize]);
  fadd0(&mut y2M, &tmp, &y_copy);
  make_b_coeff(&mut tmp);
  let mut y_copy0: [u64; 4] = [0u64; 4usize];
  (y_copy0[0usize..4usize]).copy_from_slice(&y2M[0usize..4usize]);
  fadd0(&mut y2M, &tmp, &y_copy0);
  fsqrt(&mut yM, &y2M);
  from_mont(y, &yM);
  let mut x_copy0: [u64; 4] = [0u64; 4usize];
  (x_copy0[0usize..4usize]).copy_from_slice(&yM[0usize..4usize]);
  fsqr0(&mut yM, &x_copy0);
  let r: u64 = feq_mask(&yM, &y2M);
  let is_y_valid: bool = r == 18446744073709551615u64;
  let is_y_valid0: bool = is_y_valid;
  if ! is_y_valid0 { return false };
  let is_y_odd1: u64 = y[0usize] & 1u64;
  let is_y_odd2: bool = is_y_odd1 == 1u64;
  fnegate_conditional_vartime(y, is_y_odd2 != is_y_odd);
  return true
}

#[inline] pub fn aff_point_load_vartime(p: &mut [u64], b: &[u8]) -> bool
{
  let p_x: (&[u8], &[u8]) = b.split_at(0usize);
  let p_y: (&[u8], &[u8]) = (p_x.1).split_at(32usize);
  let bn_p_x: (&mut [u64], &mut [u64]) = p.split_at_mut(0usize);
  let bn_p_y: (&mut [u64], &mut [u64]) = (bn_p_x.1).split_at_mut(4usize);
  bn_from_bytes_be4(bn_p_y.0, p_y.0);
  bn_from_bytes_be4(bn_p_y.1, p_y.1);
  let px: (&[u64], &[u64]) = (bn_p_y.0).split_at(0usize);
  let py: (&[u64], &[u64]) = (bn_p_y.1).split_at(0usize);
  let lessX: u64 = bn_is_lt_prime_mask4(px.1);
  let lessY: u64 = bn_is_lt_prime_mask4(py.1);
  let res: u64 = lessX & lessY;
  let is_xy_valid: bool = res == 18446744073709551615u64;
  if ! is_xy_valid { return false };
  return is_on_curve_vartime(p)
}

#[inline] pub fn aff_point_store(res: &mut [u8], p: &[u64])
{
  let px: (&[u64], &[u64]) = p.split_at(0usize);
  let py: (&[u64], &[u64]) = (px.1).split_at(4usize);
  bn2_to_bytes_be4(res, py.0, py.1)
}

#[inline] pub fn bn2_to_bytes_be4(res: &mut [u8], x: &[u64], y: &[u64])
{
  bn_to_bytes_be4(res, x);
  bn_to_bytes_be4(&mut res[32usize..], y)
}

#[inline] pub fn bn_add_mod4(res: &mut [u64], n: &[u64], x: &[u64], y: &[u64])
{
  let mut c0: u64 = 0u64;
  {
    let t1: u64 = x[4u32.wrapping_mul(0u32) as usize];
    let t20: u64 = y[4u32.wrapping_mul(0u32) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = res.split_at_mut(4u32.wrapping_mul(0u32) as usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t1, t20, res_i0.1);
    let t10: u64 = x[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let t21: u64 = y[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t10, t21, res_i1.1);
    let t11: u64 = x[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let t22: u64 = y[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t11, t22, res_i2.1);
    let t12: u64 = x[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let t2: u64 = y[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, t12, t2, res_i.1)
  };
  let c00: u64 = c0;
  let mut tmp: [u64; 4] = [0u64; 4usize];
  let mut c: u64 = 0u64;
  {
    let t1: u64 = res[4u32.wrapping_mul(0u32) as usize];
    let t20: u64 = n[4u32.wrapping_mul(0u32) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(4u32.wrapping_mul(0u32) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, t20, res_i0.1);
    let t10: u64 = res[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let t21: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t10, t21, res_i1.1);
    let t11: u64 = res[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let t22: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t11, t22, res_i2.1);
    let t12: u64 = res[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let t2: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t12, t2, res_i.1)
  };
  let c1: u64 = c;
  let c2: u64 = c00.wrapping_sub(c1);
  for i in 0u32..4u32
  {
    let x1: u64 = c2 & res[i as usize] | ! c2 & tmp[i as usize];
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x1
  }
}

#[inline] pub fn bn_cmovznz4(res: &mut [u64], cin: u64, x: &[u64], y: &[u64])
{
  let mask: u64 = ! crate::hacl_krmllib::FStar_UInt64_eq_mask(cin, 0u64);
  for i in 0u32..4u32
  {
    let uu____0: u64 = x[i as usize];
    let x1: u64 = uu____0 ^ mask & (y[i as usize] ^ uu____0);
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x1
  }
}

#[inline] pub fn bn_from_bytes_be4(res: &mut [u64], b: &[u8])
{
  for i in 0u32..4u32
  {
    let u: u64 =
        crate::lowstar_endianness::load64_be(
          &b[4u32.wrapping_sub(i).wrapping_sub(1u32).wrapping_mul(8u32) as usize..]
        );
    let x: u64 = u;
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

#[inline] pub fn bn_is_eq_mask4(a: &[u64], b: &[u64]) -> u64
{
  let mut mask: u64 = 18446744073709551615u64;
  for i in 0u32..4u32
  {
    let uu____0: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(a[i as usize], b[i as usize]);
    mask = uu____0 & mask
  };
  let mask1: u64 = mask;
  return mask1
}

#[inline] pub fn bn_is_eq_vartime4(a: &[u64], b: &[u64]) -> bool
{
  let m: u64 = bn_is_eq_mask4(a, b);
  return m == 18446744073709551615u64
}

#[inline] pub fn bn_is_lt_order_and_gt_zero_mask4(f: &[u64]) -> u64
{
  let is_lt_order: u64 = bn_is_lt_order_mask4(f);
  let is_eq_zero: u64 = bn_is_zero_mask4(f);
  return is_lt_order & ! is_eq_zero
}

#[inline] pub fn bn_is_lt_order_mask4(f: &[u64]) -> u64
{
  let mut tmp: [u64; 4] = [0u64; 4usize];
  make_order(&mut tmp);
  let mut y_copy: [u64; 4] = [0u64; 4usize];
  (y_copy[0usize..4usize]).copy_from_slice(&tmp[0usize..4usize]);
  let c: u64 = bn_sub4(&mut tmp, f, &y_copy);
  let c0: u64 = c;
  return 0u64.wrapping_sub(c0)
}

#[inline] pub fn bn_is_lt_prime_mask4(f: &[u64]) -> u64
{
  let mut tmp: [u64; 4] = [0u64; 4usize];
  make_prime(&mut tmp);
  let mut y_copy: [u64; 4] = [0u64; 4usize];
  (y_copy[0usize..4usize]).copy_from_slice(&tmp[0usize..4usize]);
  let c: u64 = bn_sub4(&mut tmp, f, &y_copy);
  let c0: u64 = c;
  return 0u64.wrapping_sub(c0)
}

#[inline] pub fn bn_is_zero_mask4(f: &[u64]) -> u64
{
  let bn_zero: [u64; 4] = [0u64; 4usize];
  let mut mask: u64 = 18446744073709551615u64;
  for i in 0u32..4u32
  {
    let uu____0: u64 =
        crate::hacl_krmllib::FStar_UInt64_eq_mask(f[i as usize], bn_zero[i as usize]);
    mask = uu____0 & mask
  };
  let mask1: u64 = mask;
  let res: u64 = mask1;
  return res
}

#[inline] pub fn bn_is_zero_vartime4(f: &[u64]) -> bool
{
  let m: u64 = bn_is_zero_mask4(f);
  return m == 18446744073709551615u64
}

#[inline] pub fn bn_mul4(res: &mut [u64], x: &[u64], y: &[u64])
{
  (res[0usize..8usize]).copy_from_slice(&[0u64; 8usize]);
  for i0 in 0u32..4u32
  {
    let bj: u64 = y[i0 as usize];
    let res_j: (&mut [u64], &mut [u64]) = res.split_at_mut(i0 as usize);
    let mut c: u64 = 0u64;
    {
      let a_i: u64 = x[4u32.wrapping_mul(0u32) as usize];
      let res_i0: (&mut [u64], &mut [u64]) =
          (res_j.1).split_at_mut(4u32.wrapping_mul(0u32) as usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i, bj, c, res_i0.1);
      let a_i0: u64 = x[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i0, bj, c, res_i1.1);
      let a_i1: u64 = x[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i1, bj, c, res_i2.1);
      let a_i2: u64 = x[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i2, bj, c, res_i.1)
    };
    let r: u64 = c;
    res[4u32.wrapping_add(i0) as usize] = r
  }
}

#[inline] pub fn bn_sqr4(res: &mut [u64], x: &[u64])
{
  (res[0usize..8usize]).copy_from_slice(&[0u64; 8usize]);
  for i0 in 0u32..4u32
  {
    let a_j: u64 = x[i0 as usize];
    let ab: (&[u64], &[u64]) = x.split_at(0usize);
    let res_j: (&mut [u64], &mut [u64]) = res.split_at_mut(i0 as usize);
    let mut c: u64 = 0u64;
    for i in 0u32..i0.wrapping_div(4u32)
    {
      let a_i: u64 = ab.1[4u32.wrapping_mul(i) as usize];
      let res_i0: (&mut [u64], &mut [u64]) = (res_j.1).split_at_mut(4u32.wrapping_mul(i) as usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i, a_j, c, res_i0.1);
      let a_i0: u64 = ab.1[4u32.wrapping_mul(i).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i0, a_j, c, res_i1.1);
      let a_i1: u64 = ab.1[4u32.wrapping_mul(i).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i1, a_j, c, res_i2.1);
      let a_i2: u64 = ab.1[4u32.wrapping_mul(i).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i2, a_j, c, res_i.1)
    };
    for i in i0.wrapping_div(4u32).wrapping_mul(4u32)..i0
    {
      let a_i: u64 = ab.1[i as usize];
      let res_i: (&mut [u64], &mut [u64]) = (res_j.1).split_at_mut(i as usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i, a_j, c, res_i.1)
    };
    let r: u64 = c;
    res[i0.wrapping_add(i0) as usize] = r
  };
  let mut a_copy0: [u64; 8] = [0u64; 8usize];
  let mut b_copy0: [u64; 8] = [0u64; 8usize];
  (a_copy0[0usize..8usize]).copy_from_slice(&res[0usize..8usize]);
  (b_copy0[0usize..8usize]).copy_from_slice(&res[0usize..8usize]);
  let r: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_add_eq_len_u64(
        8u32,
        &a_copy0,
        &b_copy0,
        res
      );
  let c0: u64 = r;
  crate::lowstar::ignore::ignore::<u64>(c0);
  let mut tmp: [u64; 8] = [0u64; 8usize];
  for i in 0u32..4u32
  {
    let res1: crate::types::FStar_UInt128_uint128 =
        crate::hacl_krmllib::FStar_UInt128_mul_wide(x[i as usize], x[i as usize]);
    let hi: u64 =
        crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(
          crate::hacl_krmllib::FStar_UInt128_shift_right(res1, 64u32)
        );
    let lo: u64 = crate::hacl_krmllib::FStar_UInt128_uint128_to_uint64(res1);
    tmp[2u32.wrapping_mul(i) as usize] = lo;
    tmp[2u32.wrapping_mul(i).wrapping_add(1u32) as usize] = hi
  };
  let mut a_copy: [u64; 8] = [0u64; 8usize];
  let mut b_copy: [u64; 8] = [0u64; 8usize];
  (a_copy[0usize..8usize]).copy_from_slice(&res[0usize..8usize]);
  (b_copy[0usize..8usize]).copy_from_slice(&tmp[0usize..8usize]);
  let r0: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Addition_bn_add_eq_len_u64(8u32, &a_copy, &b_copy, res);
  let c1: u64 = r0;
  crate::lowstar::ignore::ignore::<u64>(c1)
}

#[inline] pub fn bn_sub4(res: &mut [u64], x: &[u64], y: &[u64]) -> u64
{
  let mut c: u64 = 0u64;
  {
    let t1: u64 = x[4u32.wrapping_mul(0u32) as usize];
    let t20: u64 = y[4u32.wrapping_mul(0u32) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = res.split_at_mut(4u32.wrapping_mul(0u32) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, t20, res_i0.1);
    let t10: u64 = x[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let t21: u64 = y[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t10, t21, res_i1.1);
    let t11: u64 = x[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let t22: u64 = y[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t11, t22, res_i2.1);
    let t12: u64 = x[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let t2: u64 = y[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t12, t2, res_i.1)
  };
  let c0: u64 = c;
  return c0
}

#[inline] pub fn bn_sub_mod4(res: &mut [u64], n: &[u64], x: &[u64], y: &[u64])
{
  let mut c0: u64 = 0u64;
  {
    let t1: u64 = x[4u32.wrapping_mul(0u32) as usize];
    let t20: u64 = y[4u32.wrapping_mul(0u32) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = res.split_at_mut(4u32.wrapping_mul(0u32) as usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t1, t20, res_i0.1);
    let t10: u64 = x[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let t21: u64 = y[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t10, t21, res_i1.1);
    let t11: u64 = x[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let t22: u64 = y[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t11, t22, res_i2.1);
    let t12: u64 = x[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let t2: u64 = y[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c0, t12, t2, res_i.1)
  };
  let c00: u64 = c0;
  let mut tmp: [u64; 4] = [0u64; 4usize];
  let mut c: u64 = 0u64;
  {
    let t1: u64 = res[4u32.wrapping_mul(0u32) as usize];
    let t20: u64 = n[4u32.wrapping_mul(0u32) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(4u32.wrapping_mul(0u32) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t1, t20, res_i0.1);
    let t10: u64 = res[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let t21: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t10, t21, res_i1.1);
    let t11: u64 = res[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let t22: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t11, t22, res_i2.1);
    let t12: u64 = res[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let t2: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c, t12, t2, res_i.1)
  };
  let c1: u64 = c;
  crate::lowstar::ignore::ignore::<u64>(c1);
  let c2: u64 = 0u64.wrapping_sub(c00);
  for i in 0u32..4u32
  {
    let x1: u64 = c2 & tmp[i as usize] | ! c2 & res[i as usize];
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x1
  }
}

#[inline] pub fn bn_to_bytes_be4(res: &mut [u8], f: &[u64])
{
  let tmp: [u8; 32] = [0u8; 32usize];
  crate::lowstar::ignore::ignore::<[u8; 32]>(tmp);
  for i in 0u32..4u32
  {
    crate::lowstar_endianness::store64_be(
      &mut res[i.wrapping_mul(8u32) as usize..],
      f[4u32.wrapping_sub(i).wrapping_sub(1u32) as usize]
    )
  }
}

#[inline] pub fn ecdsa_sign_msg_as_qelem(
  signature: &mut [u8],
  m_q: &mut [u64],
  private_key: &[u8],
  nonce: &[u8]
) ->
    bool
{
  let mut rsdk_q: [u64; 16] = [0u64; 16usize];
  let r_q: (&mut [u64], &mut [u64]) = rsdk_q.split_at_mut(0usize);
  let s_q: (&mut [u64], &mut [u64]) = (r_q.1).split_at_mut(4usize);
  let d_a: (&mut [u64], &mut [u64]) = (s_q.1).split_at_mut(4usize);
  let k_q: (&mut [u64], &mut [u64]) = (d_a.1).split_at_mut(4usize);
  bn_from_bytes_be4(k_q.0, private_key);
  let is_b_valid0: u64 = bn_is_lt_order_and_gt_zero_mask4(k_q.0);
  let mut oneq0: [u64; 4] = [0u64; 4usize];
  oneq0[0usize] = 1u64;
  oneq0[1usize] = 0u64;
  oneq0[2usize] = 0u64;
  oneq0[3usize] = 0u64;
  for i in 0u32..4u32
  {
    let uu____0: u64 = oneq0[i as usize];
    let x: u64 = uu____0 ^ is_b_valid0 & (k_q.0[i as usize] ^ uu____0);
    let os: (&mut [u64], &mut [u64]) = (k_q.0).split_at_mut(0usize);
    os.1[i as usize] = x
  };
  let is_sk_valid: u64 = is_b_valid0;
  bn_from_bytes_be4(k_q.1, nonce);
  let is_b_valid: u64 = bn_is_lt_order_and_gt_zero_mask4(k_q.1);
  let mut oneq: [u64; 4] = [0u64; 4usize];
  oneq[0usize] = 1u64;
  oneq[1usize] = 0u64;
  oneq[2usize] = 0u64;
  oneq[3usize] = 0u64;
  for i in 0u32..4u32
  {
    let uu____1: u64 = oneq[i as usize];
    let x: u64 = uu____1 ^ is_b_valid & (k_q.1[i as usize] ^ uu____1);
    let os: (&mut [u64], &mut [u64]) = (k_q.1).split_at_mut(0usize);
    os.1[i as usize] = x
  };
  let is_nonce_valid: u64 = is_b_valid;
  let are_sk_nonce_valid: u64 = is_sk_valid & is_nonce_valid;
  let mut p: [u64; 12] = [0u64; 12usize];
  point_mul_g(&mut p, k_q.1);
  to_aff_point_x(s_q.0, &p);
  let mut x_copy0: [u64; 4] = [0u64; 4usize];
  (x_copy0[0usize..4usize]).copy_from_slice(&s_q.0[0usize..4usize]);
  qmod_short(s_q.0, &x_copy0);
  let mut kinv: [u64; 4] = [0u64; 4usize];
  qinv(&mut kinv, k_q.1);
  qmul(d_a.0, s_q.0, k_q.0);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&m_q[0usize..4usize]);
  from_qmont(m_q, &x_copy);
  let mut y_copy: [u64; 4] = [0u64; 4usize];
  (y_copy[0usize..4usize]).copy_from_slice(&d_a.0[0usize..4usize]);
  qadd(d_a.0, m_q, &y_copy);
  let mut y_copy0: [u64; 4] = [0u64; 4usize];
  (y_copy0[0usize..4usize]).copy_from_slice(&d_a.0[0usize..4usize]);
  qmul(d_a.0, &kinv, &y_copy0);
  bn2_to_bytes_be4(signature, s_q.0, d_a.0);
  let is_r_zero: u64 = bn_is_zero_mask4(s_q.0);
  let is_s_zero: u64 = bn_is_zero_mask4(d_a.0);
  let m: u64 = are_sk_nonce_valid & (! is_r_zero & ! is_s_zero);
  let res: bool = m == 18446744073709551615u64;
  return res
}

#[inline] pub fn ecdsa_verify_msg_as_qelem(
  m_q: &[u64],
  public_key: &[u8],
  signature_r: &[u8],
  signature_s: &[u8]
) ->
    bool
{
  let mut tmp: [u64; 28] = [0u64; 28usize];
  let pk: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
  let r_q: (&mut [u64], &mut [u64]) = (pk.1).split_at_mut(12usize);
  let s_q: (&mut [u64], &mut [u64]) = (r_q.1).split_at_mut(4usize);
  let u1: (&mut [u64], &mut [u64]) = (s_q.1).split_at_mut(4usize);
  let u2: (&mut [u64], &mut [u64]) = (u1.1).split_at_mut(4usize);
  let is_pk_valid: bool = load_point_vartime(r_q.0, public_key);
  bn_from_bytes_be4(s_q.0, signature_r);
  bn_from_bytes_be4(u1.0, signature_s);
  let is_r_valid: u64 = bn_is_lt_order_and_gt_zero_mask4(s_q.0);
  let is_s_valid: u64 = bn_is_lt_order_and_gt_zero_mask4(u1.0);
  let is_rs_valid: bool =
      is_r_valid == 18446744073709551615u64 && is_s_valid == 18446744073709551615u64;
  if ! (is_pk_valid && is_rs_valid) { return false };
  let mut sinv: [u64; 4] = [0u64; 4usize];
  qinv(&mut sinv, u1.0);
  qmul_mont(&sinv, m_q, u2.0);
  qmul_mont(&sinv, s_q.0, u2.1);
  let mut res: [u64; 12] = [0u64; 12usize];
  point_mul_double_g(&mut res, u2.0, u2.1, r_q.0);
  if is_point_at_inf_vartime(&res) { return false };
  let mut x: [u64; 4] = [0u64; 4usize];
  to_aff_point_x(&mut x, &res);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&x[0usize..4usize]);
  qmod_short(&mut x, &x_copy);
  let res1: bool = bn_is_eq_vartime4(&x, s_q.0);
  return res1
}

#[inline] pub fn fadd0(res: &mut [u64], x: &[u64], y: &[u64])
{
  let mut n: [u64; 4] = [0u64; 4usize];
  make_prime(&mut n);
  bn_add_mod4(res, &n, x, y)
}

#[inline] pub fn fcube(res: &mut [u64], x: &[u64])
{
  fsqr0(res, x);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&res[0usize..4usize]);
  fmul0(res, &x_copy, x)
}

#[inline] pub fn feq_mask(a: &[u64], b: &[u64]) -> u64
{
  let r: u64 = bn_is_eq_mask4(a, b);
  return r
}

#[inline] pub fn finv(res: &mut [u64], a: &[u64])
{
  let mut tmp: [u64; 16] = [0u64; 16usize];
  let x30: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
  let x2: (&mut [u64], &mut [u64]) = (x30.1).split_at_mut(4usize);
  let tmp1: (&mut [u64], &mut [u64]) = (x2.1).split_at_mut(4usize);
  let tmp2: (&mut [u64], &mut [u64]) = (tmp1.1).split_at_mut(4usize);
  (tmp1.0[0usize..4usize]).copy_from_slice(&a[0usize..4usize]);
  {
    let mut x_copy: [u64; 4] = [0u64; 4usize];
    (x_copy[0usize..4usize]).copy_from_slice(&tmp1.0[0usize..4usize]);
    fsqr0(tmp1.0, &x_copy)
  };
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&tmp1.0[0usize..4usize]);
  fmul0(tmp1.0, &x_copy, a);
  (x2.0[0usize..4usize]).copy_from_slice(&tmp1.0[0usize..4usize]);
  {
    let mut x_copy0: [u64; 4] = [0u64; 4usize];
    (x_copy0[0usize..4usize]).copy_from_slice(&x2.0[0usize..4usize]);
    fsqr0(x2.0, &x_copy0)
  };
  let mut x_copy0: [u64; 4] = [0u64; 4usize];
  (x_copy0[0usize..4usize]).copy_from_slice(&x2.0[0usize..4usize]);
  fmul0(x2.0, &x_copy0, a);
  (tmp2.0[0usize..4usize]).copy_from_slice(&x2.0[0usize..4usize]);
  for i in 0u32..3u32
  {
    let mut x_copy1: [u64; 4] = [0u64; 4usize];
    (x_copy1[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
    fsqr0(tmp2.0, &x_copy1)
  };
  let mut x_copy1: [u64; 4] = [0u64; 4usize];
  (x_copy1[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
  fmul0(tmp2.0, &x_copy1, x2.0);
  (tmp2.1[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
  for i in 0u32..6u32
  {
    let mut x_copy2: [u64; 4] = [0u64; 4usize];
    (x_copy2[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
    fsqr0(tmp2.1, &x_copy2)
  };
  let mut x_copy2: [u64; 4] = [0u64; 4usize];
  (x_copy2[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
  fmul0(tmp2.1, &x_copy2, tmp2.0);
  (tmp2.0[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
  for i in 0u32..3u32
  {
    let mut x_copy3: [u64; 4] = [0u64; 4usize];
    (x_copy3[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
    fsqr0(tmp2.0, &x_copy3)
  };
  let mut x_copy3: [u64; 4] = [0u64; 4usize];
  (x_copy3[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
  fmul0(tmp2.0, &x_copy3, x2.0);
  (x2.0[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
  for i in 0u32..15u32
  {
    let mut x_copy4: [u64; 4] = [0u64; 4usize];
    (x_copy4[0usize..4usize]).copy_from_slice(&x2.0[0usize..4usize]);
    fsqr0(x2.0, &x_copy4)
  };
  let mut x_copy4: [u64; 4] = [0u64; 4usize];
  (x_copy4[0usize..4usize]).copy_from_slice(&x2.0[0usize..4usize]);
  fmul0(x2.0, &x_copy4, tmp2.0);
  (tmp2.0[0usize..4usize]).copy_from_slice(&x2.0[0usize..4usize]);
  for i in 0u32..2u32
  {
    let mut x_copy5: [u64; 4] = [0u64; 4usize];
    (x_copy5[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
    fsqr0(tmp2.0, &x_copy5)
  };
  let mut x_copy5: [u64; 4] = [0u64; 4usize];
  (x_copy5[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
  fmul0(tmp2.0, &x_copy5, tmp1.0);
  (tmp1.0[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
  for i in 0u32..32u32
  {
    let mut x_copy6: [u64; 4] = [0u64; 4usize];
    (x_copy6[0usize..4usize]).copy_from_slice(&tmp1.0[0usize..4usize]);
    fsqr0(tmp1.0, &x_copy6)
  };
  let mut x_copy6: [u64; 4] = [0u64; 4usize];
  (x_copy6[0usize..4usize]).copy_from_slice(&tmp1.0[0usize..4usize]);
  fmul0(tmp1.0, &x_copy6, a);
  for i in 0u32..128u32
  {
    let mut x_copy7: [u64; 4] = [0u64; 4usize];
    (x_copy7[0usize..4usize]).copy_from_slice(&tmp1.0[0usize..4usize]);
    fsqr0(tmp1.0, &x_copy7)
  };
  let mut x_copy7: [u64; 4] = [0u64; 4usize];
  (x_copy7[0usize..4usize]).copy_from_slice(&tmp1.0[0usize..4usize]);
  fmul0(tmp1.0, &x_copy7, tmp2.0);
  for i in 0u32..32u32
  {
    let mut x_copy8: [u64; 4] = [0u64; 4usize];
    (x_copy8[0usize..4usize]).copy_from_slice(&tmp1.0[0usize..4usize]);
    fsqr0(tmp1.0, &x_copy8)
  };
  let mut x_copy8: [u64; 4] = [0u64; 4usize];
  (x_copy8[0usize..4usize]).copy_from_slice(&tmp1.0[0usize..4usize]);
  fmul0(tmp1.0, &x_copy8, tmp2.0);
  for i in 0u32..30u32
  {
    let mut x_copy9: [u64; 4] = [0u64; 4usize];
    (x_copy9[0usize..4usize]).copy_from_slice(&tmp1.0[0usize..4usize]);
    fsqr0(tmp1.0, &x_copy9)
  };
  let mut x_copy9: [u64; 4] = [0u64; 4usize];
  (x_copy9[0usize..4usize]).copy_from_slice(&tmp1.0[0usize..4usize]);
  fmul0(tmp1.0, &x_copy9, x2.0);
  for i in 0u32..2u32
  {
    let mut x_copy10: [u64; 4] = [0u64; 4usize];
    (x_copy10[0usize..4usize]).copy_from_slice(&tmp1.0[0usize..4usize]);
    fsqr0(tmp1.0, &x_copy10)
  };
  fmul0(tmp2.0, tmp1.0, a);
  (res[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize])
}

#[inline] pub fn fmul0(res: &mut [u64], x: &[u64], y: &[u64])
{
  let mut tmp: [u64; 8] = [0u64; 8usize];
  bn_mul4(&mut tmp, x, y);
  mont_reduction(res, &mut tmp)
}

#[inline] pub fn fmul_by_b_coeff(res: &mut [u64], x: &[u64])
{
  let mut b_coeff: [u64; 4] = [0u64; 4usize];
  make_b_coeff(&mut b_coeff);
  fmul0(res, &b_coeff, x)
}

#[inline] pub fn fnegate_conditional_vartime(f: &mut [u64], is_negate: bool)
{
  let zero: [u64; 4] = [0u64; 4usize];
  if is_negate
  {
    let mut y_copy: [u64; 4] = [0u64; 4usize];
    (y_copy[0usize..4usize]).copy_from_slice(&f[0usize..4usize]);
    fsub0(f, &zero, &y_copy)
  }
}

#[inline] pub fn from_mont(res: &mut [u64], a: &[u64])
{
  let mut tmp: [u64; 8] = [0u64; 8usize];
  (tmp[0usize..4usize]).copy_from_slice(&a[0usize..4usize]);
  mont_reduction(res, &mut tmp)
}

#[inline] pub fn from_qmont(res: &mut [u64], x: &[u64])
{
  let mut tmp: [u64; 8] = [0u64; 8usize];
  (tmp[0usize..4usize]).copy_from_slice(&x[0usize..4usize]);
  qmont_reduction(res, &mut tmp)
}

#[inline] pub fn fsqr0(res: &mut [u64], x: &[u64])
{
  let mut tmp: [u64; 8] = [0u64; 8usize];
  bn_sqr4(&mut tmp, x);
  mont_reduction(res, &mut tmp)
}

#[inline] pub fn fsqrt(res: &mut [u64], a: &[u64])
{
  let mut tmp: [u64; 8] = [0u64; 8usize];
  let tmp1: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
  let tmp2: (&mut [u64], &mut [u64]) = (tmp1.1).split_at_mut(4usize);
  (tmp2.0[0usize..4usize]).copy_from_slice(&a[0usize..4usize]);
  {
    let mut x_copy: [u64; 4] = [0u64; 4usize];
    (x_copy[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
    fsqr0(tmp2.0, &x_copy)
  };
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
  fmul0(tmp2.0, &x_copy, a);
  (tmp2.1[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
  for i in 0u32..2u32
  {
    let mut x_copy0: [u64; 4] = [0u64; 4usize];
    (x_copy0[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
    fsqr0(tmp2.1, &x_copy0)
  };
  let mut x_copy0: [u64; 4] = [0u64; 4usize];
  (x_copy0[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
  fmul0(tmp2.1, &x_copy0, tmp2.0);
  (tmp2.0[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
  for i in 0u32..4u32
  {
    let mut x_copy1: [u64; 4] = [0u64; 4usize];
    (x_copy1[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
    fsqr0(tmp2.0, &x_copy1)
  };
  let mut x_copy1: [u64; 4] = [0u64; 4usize];
  (x_copy1[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
  fmul0(tmp2.0, &x_copy1, tmp2.1);
  (tmp2.1[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
  for i in 0u32..8u32
  {
    let mut x_copy2: [u64; 4] = [0u64; 4usize];
    (x_copy2[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
    fsqr0(tmp2.1, &x_copy2)
  };
  let mut x_copy2: [u64; 4] = [0u64; 4usize];
  (x_copy2[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
  fmul0(tmp2.1, &x_copy2, tmp2.0);
  (tmp2.0[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
  for i in 0u32..16u32
  {
    let mut x_copy3: [u64; 4] = [0u64; 4usize];
    (x_copy3[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
    fsqr0(tmp2.0, &x_copy3)
  };
  let mut x_copy3: [u64; 4] = [0u64; 4usize];
  (x_copy3[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
  fmul0(tmp2.0, &x_copy3, tmp2.1);
  (tmp2.1[0usize..4usize]).copy_from_slice(&tmp2.0[0usize..4usize]);
  for i in 0u32..32u32
  {
    let mut x_copy4: [u64; 4] = [0u64; 4usize];
    (x_copy4[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
    fsqr0(tmp2.1, &x_copy4)
  };
  let mut x_copy4: [u64; 4] = [0u64; 4usize];
  (x_copy4[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
  fmul0(tmp2.1, &x_copy4, a);
  for i in 0u32..96u32
  {
    let mut x_copy5: [u64; 4] = [0u64; 4usize];
    (x_copy5[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
    fsqr0(tmp2.1, &x_copy5)
  };
  let mut x_copy5: [u64; 4] = [0u64; 4usize];
  (x_copy5[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
  fmul0(tmp2.1, &x_copy5, a);
  for i in 0u32..94u32
  {
    let mut x_copy6: [u64; 4] = [0u64; 4usize];
    (x_copy6[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize]);
    fsqr0(tmp2.1, &x_copy6)
  };
  (res[0usize..4usize]).copy_from_slice(&tmp2.1[0usize..4usize])
}

#[inline] pub fn fsub0(res: &mut [u64], x: &[u64], y: &[u64])
{
  let mut n: [u64; 4] = [0u64; 4usize];
  make_prime(&mut n);
  bn_sub_mod4(res, &n, x, y)
}

#[inline] pub fn is_on_curve_vartime(p: &[u64]) -> bool
{
  let mut rp: [u64; 4] = [0u64; 4usize];
  let mut tx: [u64; 4] = [0u64; 4usize];
  let mut ty: [u64; 4] = [0u64; 4usize];
  let px: (&[u64], &[u64]) = p.split_at(0usize);
  let py: (&[u64], &[u64]) = (px.1).split_at(4usize);
  to_mont(&mut tx, py.0);
  to_mont(&mut ty, py.1);
  let mut tmp: [u64; 4] = [0u64; 4usize];
  fcube(&mut rp, &tx);
  make_a_coeff(&mut tmp);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&tmp[0usize..4usize]);
  fmul0(&mut tmp, &x_copy, &tx);
  let mut y_copy: [u64; 4] = [0u64; 4usize];
  (y_copy[0usize..4usize]).copy_from_slice(&rp[0usize..4usize]);
  fadd0(&mut rp, &tmp, &y_copy);
  make_b_coeff(&mut tmp);
  let mut y_copy0: [u64; 4] = [0u64; 4usize];
  (y_copy0[0usize..4usize]).copy_from_slice(&rp[0usize..4usize]);
  fadd0(&mut rp, &tmp, &y_copy0);
  let mut x_copy0: [u64; 4] = [0u64; 4usize];
  (x_copy0[0usize..4usize]).copy_from_slice(&ty[0usize..4usize]);
  fsqr0(&mut ty, &x_copy0);
  let r: u64 = feq_mask(&ty, &rp);
  let r0: bool = r == 18446744073709551615u64;
  return r0
}

#[inline] pub fn is_point_at_inf_vartime(p: &[u64]) -> bool
{
  let pz: (&[u64], &[u64]) = p.split_at(8usize);
  return bn_is_zero_vartime4(pz.1)
}

#[inline] pub fn load_point_vartime(p: &mut [u64], b: &[u8]) -> bool
{
  let mut p_aff: [u64; 8] = [0u64; 8usize];
  let res: bool = aff_point_load_vartime(&mut p_aff, b);
  if res { to_proj_point(p, &p_aff) };
  return res
}

#[inline] pub fn make_a_coeff(a: &mut [u64])
{
  a[0usize] = 18446744073709551612u64;
  a[1usize] = 17179869183u64;
  a[2usize] = 0u64;
  a[3usize] = 18446744056529682436u64
}

#[inline] pub fn make_b_coeff(b: &mut [u64])
{
  b[0usize] = 15608596021259845087u64;
  b[1usize] = 12461466548982526096u64;
  b[2usize] = 16546823903870267094u64;
  b[3usize] = 15866188208926050356u64
}

#[inline] pub fn make_base_point(p: &mut [u64])
{
  let x: (&mut [u64], &mut [u64]) = p.split_at_mut(0usize);
  let y: (&mut [u64], &mut [u64]) = (x.1).split_at_mut(4usize);
  let z: (&mut [u64], &mut [u64]) = (y.1).split_at_mut(4usize);
  make_g_x(y.0);
  make_g_y(z.0);
  make_fone(z.1)
}

#[inline] pub fn make_fmont_R2(n: &mut [u64])
{
  n[0usize] = 3u64;
  n[1usize] = 18446744056529682431u64;
  n[2usize] = 18446744073709551614u64;
  n[3usize] = 21474836477u64
}

#[inline] pub fn make_fone(n: &mut [u64])
{
  n[0usize] = 1u64;
  n[1usize] = 18446744069414584320u64;
  n[2usize] = 18446744073709551615u64;
  n[3usize] = 4294967294u64
}

#[inline] pub fn make_fzero(n: &mut [u64])
{
  n[0usize] = 0u64;
  n[1usize] = 0u64;
  n[2usize] = 0u64;
  n[3usize] = 0u64
}

#[inline] pub fn make_g_x(n: &mut [u64])
{
  n[0usize] = 8784043285714375740u64;
  n[1usize] = 8483257759279461889u64;
  n[2usize] = 8789745728267363600u64;
  n[3usize] = 1770019616739251654u64
}

#[inline] pub fn make_g_y(n: &mut [u64])
{
  n[0usize] = 15992936863339206154u64;
  n[1usize] = 10037038012062884956u64;
  n[2usize] = 15197544864945402661u64;
  n[3usize] = 9615747158586711429u64
}

#[inline] pub fn make_order(n: &mut [u64])
{
  n[0usize] = 17562291160714782033u64;
  n[1usize] = 13611842547513532036u64;
  n[2usize] = 18446744073709551615u64;
  n[3usize] = 18446744069414584320u64
}

#[inline] pub fn make_point_at_inf(p: &mut [u64])
{
  let x: (&mut [u64], &mut [u64]) = p.split_at_mut(0usize);
  let y: (&mut [u64], &mut [u64]) = (x.1).split_at_mut(4usize);
  let z: (&mut [u64], &mut [u64]) = (y.1).split_at_mut(4usize);
  make_fzero(y.0);
  make_fone(z.0);
  make_fzero(z.1)
}

#[inline] pub fn make_prime(n: &mut [u64])
{
  n[0usize] = 18446744073709551615u64;
  n[1usize] = 4294967295u64;
  n[2usize] = 0u64;
  n[3usize] = 18446744069414584321u64
}

#[inline] pub fn mont_reduction(res: &mut [u64], x: &mut [u64])
{
  let mut n: [u64; 4] = [0u64; 4usize];
  make_prime(&mut n);
  let mut c0: u64 = 0u64;
  for i0 in 0u32..4u32
  {
    let qj: u64 = 1u64.wrapping_mul(x[i0 as usize]);
    let res_j0: (&mut [u64], &mut [u64]) = x.split_at_mut(i0 as usize);
    let mut c: u64 = 0u64;
    {
      let a_i: u64 = n[4u32.wrapping_mul(0u32) as usize];
      let res_i0: (&mut [u64], &mut [u64]) =
          (res_j0.1).split_at_mut(4u32.wrapping_mul(0u32) as usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i, qj, c, res_i0.1);
      let a_i0: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i0, qj, c, res_i1.1);
      let a_i1: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i1, qj, c, res_i2.1);
      let a_i2: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i2, qj, c, res_i.1)
    };
    let r: u64 = c;
    let c1: u64 = r;
    let res_j: u64 = x[4u32.wrapping_add(i0) as usize];
    let resb: (&mut [u64], &mut [u64]) = x.split_at_mut(i0 as usize + 4usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, c1, res_j, resb.1)
  };
  (res[0usize..4usize]).copy_from_slice(&(&x[4usize..])[0usize..4usize]);
  let c00: u64 = c0;
  let mut tmp: [u64; 4] = [0u64; 4usize];
  let mut c: u64 = 0u64;
  {
    let t1: u64 = res[4u32.wrapping_mul(0u32) as usize];
    let t20: u64 = n[4u32.wrapping_mul(0u32) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(4u32.wrapping_mul(0u32) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, t20, res_i0.1);
    let t10: u64 = res[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let t21: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t10, t21, res_i1.1);
    let t11: u64 = res[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let t22: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t11, t22, res_i2.1);
    let t12: u64 = res[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let t2: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t12, t2, res_i.1)
  };
  let c1: u64 = c;
  let c2: u64 = c00.wrapping_sub(c1);
  for i in 0u32..4u32
  {
    let x1: u64 = c2 & res[i as usize] | ! c2 & tmp[i as usize];
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x1
  }
}

#[inline] pub fn point_add(res: &mut [u64], p: &[u64], q: &[u64])
{
  let mut tmp: [u64; 36] = [0u64; 36usize];
  let t0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
  let t1: (&mut [u64], &mut [u64]) = (t0.1).split_at_mut(24usize);
  let x3: (&mut [u64], &mut [u64]) = (t1.1).split_at_mut(0usize);
  let y3: (&mut [u64], &mut [u64]) = (x3.1).split_at_mut(4usize);
  let z3: (&mut [u64], &mut [u64]) = (y3.1).split_at_mut(4usize);
  let t01: (&mut [u64], &mut [u64]) = (t1.0).split_at_mut(0usize);
  let t11: (&mut [u64], &mut [u64]) = (t01.1).split_at_mut(4usize);
  let t2: (&mut [u64], &mut [u64]) = (t11.1).split_at_mut(4usize);
  let t3: (&mut [u64], &mut [u64]) = (t2.1).split_at_mut(4usize);
  let t4: (&mut [u64], &mut [u64]) = (t3.1).split_at_mut(4usize);
  let t5: (&mut [u64], &mut [u64]) = (t4.1).split_at_mut(4usize);
  let x1: (&[u64], &[u64]) = p.split_at(0usize);
  let y1: (&[u64], &[u64]) = (x1.1).split_at(4usize);
  let z10: (&[u64], &[u64]) = (y1.1).split_at(4usize);
  let x20: (&[u64], &[u64]) = q.split_at(0usize);
  let y20: (&[u64], &[u64]) = (x20.1).split_at(4usize);
  let z20: (&[u64], &[u64]) = (y20.1).split_at(4usize);
  fmul0(t11.0, y1.0, y20.0);
  fmul0(t2.0, z10.0, z20.0);
  fmul0(t3.0, z10.1, z20.1);
  fadd0(t4.0, y1.0, z10.0);
  fadd0(t5.0, y20.0, z20.0);
  let mut x_copy0: [u64; 4] = [0u64; 4usize];
  (x_copy0[0usize..4usize]).copy_from_slice(&t4.0[0usize..4usize]);
  fmul0(t4.0, &x_copy0, t5.0);
  fadd0(t5.0, t11.0, t2.0);
  let y10: (&[u64], &[u64]) = (z10.0).split_at(0usize);
  let z11: (&[u64], &[u64]) = (z10.1).split_at(0usize);
  let y2: (&[u64], &[u64]) = (z20.0).split_at(0usize);
  let z21: (&[u64], &[u64]) = (z20.1).split_at(0usize);
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&t4.0[0usize..4usize]);
  fsub0(t4.0, &x_copy, t5.0);
  fadd0(t5.0, y10.1, z11.1);
  fadd0(t5.1, y2.1, z21.1);
  let mut x_copy1: [u64; 4] = [0u64; 4usize];
  (x_copy1[0usize..4usize]).copy_from_slice(&t5.0[0usize..4usize]);
  fmul0(t5.0, &x_copy1, t5.1);
  fadd0(t5.1, t2.0, t3.0);
  let mut x_copy2: [u64; 4] = [0u64; 4usize];
  (x_copy2[0usize..4usize]).copy_from_slice(&t5.0[0usize..4usize]);
  fsub0(t5.0, &x_copy2, t5.1);
  let x10: (&[u64], &[u64]) = (y1.0).split_at(0usize);
  let z1: (&[u64], &[u64]) = (z11.1).split_at(0usize);
  let x2: (&[u64], &[u64]) = (y20.0).split_at(0usize);
  let z2: (&[u64], &[u64]) = (z21.1).split_at(0usize);
  fadd0(y3.0, x10.1, z1.1);
  fadd0(z3.0, x2.1, z2.1);
  let mut x_copy3: [u64; 4] = [0u64; 4usize];
  (x_copy3[0usize..4usize]).copy_from_slice(&y3.0[0usize..4usize]);
  fmul0(y3.0, &x_copy3, z3.0);
  fadd0(z3.0, t11.0, t3.0);
  let mut y_copy: [u64; 4] = [0u64; 4usize];
  (y_copy[0usize..4usize]).copy_from_slice(&z3.0[0usize..4usize]);
  fsub0(z3.0, y3.0, &y_copy);
  fmul_by_b_coeff(z3.1, t3.0);
  fsub0(y3.0, z3.0, z3.1);
  fadd0(z3.1, y3.0, y3.0);
  let mut x_copy4: [u64; 4] = [0u64; 4usize];
  (x_copy4[0usize..4usize]).copy_from_slice(&y3.0[0usize..4usize]);
  fadd0(y3.0, &x_copy4, z3.1);
  fsub0(z3.1, t2.0, y3.0);
  let mut y_copy0: [u64; 4] = [0u64; 4usize];
  (y_copy0[0usize..4usize]).copy_from_slice(&y3.0[0usize..4usize]);
  fadd0(y3.0, t2.0, &y_copy0);
  let mut x_copy5: [u64; 4] = [0u64; 4usize];
  (x_copy5[0usize..4usize]).copy_from_slice(&z3.0[0usize..4usize]);
  fmul_by_b_coeff(z3.0, &x_copy5);
  fadd0(t2.0, t3.0, t3.0);
  let mut y_copy1: [u64; 4] = [0u64; 4usize];
  (y_copy1[0usize..4usize]).copy_from_slice(&t3.0[0usize..4usize]);
  fadd0(t3.0, t2.0, &y_copy1);
  let mut x_copy6: [u64; 4] = [0u64; 4usize];
  (x_copy6[0usize..4usize]).copy_from_slice(&z3.0[0usize..4usize]);
  fsub0(z3.0, &x_copy6, t3.0);
  let mut x_copy7: [u64; 4] = [0u64; 4usize];
  (x_copy7[0usize..4usize]).copy_from_slice(&z3.0[0usize..4usize]);
  fsub0(z3.0, &x_copy7, t11.0);
  fadd0(t2.0, z3.0, z3.0);
  let mut y_copy2: [u64; 4] = [0u64; 4usize];
  (y_copy2[0usize..4usize]).copy_from_slice(&z3.0[0usize..4usize]);
  fadd0(z3.0, t2.0, &y_copy2);
  fadd0(t2.0, t11.0, t11.0);
  let mut y_copy3: [u64; 4] = [0u64; 4usize];
  (y_copy3[0usize..4usize]).copy_from_slice(&t11.0[0usize..4usize]);
  fadd0(t11.0, t2.0, &y_copy3);
  let mut x_copy8: [u64; 4] = [0u64; 4usize];
  (x_copy8[0usize..4usize]).copy_from_slice(&t11.0[0usize..4usize]);
  fsub0(t11.0, &x_copy8, t3.0);
  fmul0(t2.0, t5.0, z3.0);
  fmul0(t3.0, t11.0, z3.0);
  fmul0(z3.0, y3.0, z3.1);
  let mut x_copy9: [u64; 4] = [0u64; 4usize];
  (x_copy9[0usize..4usize]).copy_from_slice(&z3.0[0usize..4usize]);
  fadd0(z3.0, &x_copy9, t3.0);
  let mut y_copy4: [u64; 4] = [0u64; 4usize];
  (y_copy4[0usize..4usize]).copy_from_slice(&y3.0[0usize..4usize]);
  fmul0(y3.0, t4.0, &y_copy4);
  let mut x_copy10: [u64; 4] = [0u64; 4usize];
  (x_copy10[0usize..4usize]).copy_from_slice(&y3.0[0usize..4usize]);
  fsub0(y3.0, &x_copy10, t2.0);
  let mut y_copy5: [u64; 4] = [0u64; 4usize];
  (y_copy5[0usize..4usize]).copy_from_slice(&z3.1[0usize..4usize]);
  fmul0(z3.1, t5.0, &y_copy5);
  fmul0(t2.0, t4.0, t11.0);
  let mut x_copy11: [u64; 4] = [0u64; 4usize];
  (x_copy11[0usize..4usize]).copy_from_slice(&z3.1[0usize..4usize]);
  fadd0(z3.1, &x_copy11, t2.0);
  (res[0usize..12usize]).copy_from_slice(&t1.1[0usize..12usize])
}

#[inline] pub fn point_mul(res: &mut [u64], scalar: &[u64], p: &[u64])
{
  let mut table: [u64; 192] = [0u64; 192usize];
  let mut tmp: [u64; 12] = [0u64; 12usize];
  let t0: (&mut [u64], &mut [u64]) = table.split_at_mut(0usize);
  let t1: (&mut [u64], &mut [u64]) = (t0.1).split_at_mut(12usize);
  make_point_at_inf(t1.0);
  (t1.1[0usize..12usize]).copy_from_slice(&p[0usize..12usize]);
  crate::lowstar::ignore::ignore::<[u64; 192]>(table);
  for i in 0u32..7u32
  {
    let t11: (&[u64], &[u64]) = table.split_at(i.wrapping_add(1u32).wrapping_mul(12u32) as usize);
    let mut p_copy0: [u64; 12] = [0u64; 12usize];
    (p_copy0[0usize..12usize]).copy_from_slice(&t11.1[0usize..12usize]);
    Hacl_P256_point_double(&mut tmp, &p_copy0);
    ((&mut table[2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(12u32) as usize..])[0usize..12usize]).copy_from_slice(
      &tmp[0usize..12usize]
    );
    let t2: (&[u64], &[u64]) =
        table.split_at(2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(12u32) as usize);
    let mut p_copy: [u64; 12] = [0u64; 12usize];
    (p_copy[0usize..12usize]).copy_from_slice(&p[0usize..12usize]);
    point_add(&mut tmp, &p_copy, t2.1);
    ((&mut table[2u32.wrapping_mul(i).wrapping_add(3u32).wrapping_mul(12u32) as usize..])[0usize..12usize]).copy_from_slice(
      &tmp[0usize..12usize]
    )
  };
  make_point_at_inf(res);
  let mut tmp0: [u64; 12] = [0u64; 12usize];
  for i0 in 0u32..64u32
  {
    for i in 0u32..4u32
    {
      let mut p_copy: [u64; 12] = [0u64; 12usize];
      (p_copy[0usize..12usize]).copy_from_slice(&res[0usize..12usize]);
      Hacl_P256_point_double(res, &p_copy)
    };
    let k: u32 = 256u32.wrapping_sub(4u32.wrapping_mul(i0)).wrapping_sub(4u32);
    let bits_l: u64 =
        crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(4u32, scalar, k, 4u32);
    crate::lowstar::ignore::ignore::<[u64; 192]>(table);
    (tmp0[0usize..12usize]).copy_from_slice(&(&table)[0usize..12usize]);
    for i1 in 0u32..15u32
    {
      let c: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(bits_l, i1.wrapping_add(1u32) as u64);
      let res_j: (&[u64], &[u64]) =
          table.split_at(i1.wrapping_add(1u32).wrapping_mul(12u32) as usize);
      for i in 0u32..12u32
      {
        let x: u64 = c & res_j.1[i as usize] | ! c & tmp0[i as usize];
        let os: (&mut [u64], &mut [u64]) = tmp0.split_at_mut(0usize);
        os.1[i as usize] = x
      }
    };
    let mut p_copy: [u64; 12] = [0u64; 12usize];
    (p_copy[0usize..12usize]).copy_from_slice(&res[0usize..12usize]);
    point_add(res, &p_copy, &tmp0)
  }
}

#[inline] pub fn point_mul_double_g(
  res: &mut [u64],
  scalar1: &[u64],
  scalar2: &[u64],
  q2: &[u64]
)
{
  let mut q1: [u64; 12] = [0u64; 12usize];
  make_base_point(&mut q1);
  let mut table2: [u64; 384] = [0u64; 384usize];
  let mut tmp: [u64; 12] = [0u64; 12usize];
  let t0: (&mut [u64], &mut [u64]) = table2.split_at_mut(0usize);
  let t1: (&mut [u64], &mut [u64]) = (t0.1).split_at_mut(12usize);
  make_point_at_inf(t1.0);
  (t1.1[0usize..12usize]).copy_from_slice(&q2[0usize..12usize]);
  crate::lowstar::ignore::ignore::<[u64; 384]>(table2);
  for i in 0u32..15u32
  {
    let t11: (&[u64], &[u64]) = table2.split_at(i.wrapping_add(1u32).wrapping_mul(12u32) as usize);
    let mut p_copy0: [u64; 12] = [0u64; 12usize];
    (p_copy0[0usize..12usize]).copy_from_slice(&t11.1[0usize..12usize]);
    Hacl_P256_point_double(&mut tmp, &p_copy0);
    ((&mut table2[2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(12u32) as usize..])[0usize..12usize]).copy_from_slice(
      &tmp[0usize..12usize]
    );
    let t2: (&[u64], &[u64]) =
        table2.split_at(2u32.wrapping_mul(i).wrapping_add(2u32).wrapping_mul(12u32) as usize);
    let mut p_copy: [u64; 12] = [0u64; 12usize];
    (p_copy[0usize..12usize]).copy_from_slice(&q2[0usize..12usize]);
    point_add(&mut tmp, &p_copy, t2.1);
    ((&mut table2[2u32.wrapping_mul(i).wrapping_add(3u32).wrapping_mul(12u32) as usize..])[0usize..12usize]).copy_from_slice(
      &tmp[0usize..12usize]
    )
  };
  let mut tmp0: [u64; 12] = [0u64; 12usize];
  let i0: u32 = 255u32;
  let bits_c: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(4u32, scalar1, i0, 5u32);
  let bits_l32: u32 = bits_c as u32;
  let a_bits_l: &[u64] =
      &crate::hacl::p256_precomptable::Hacl_P256_PrecompTable_precomp_basepoint_table_w5[bits_l32.wrapping_mul(
        12u32
      )
      as
      usize..];
  (res[0usize..12usize]).copy_from_slice(&a_bits_l[0usize..12usize]);
  let i1: u32 = 255u32;
  let bits_c0: u64 =
      crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(4u32, scalar2, i1, 5u32);
  let bits_l320: u32 = bits_c0 as u32;
  let a_bits_l0: (&[u64], &[u64]) = table2.split_at(bits_l320.wrapping_mul(12u32) as usize);
  (tmp0[0usize..12usize]).copy_from_slice(&a_bits_l0.1[0usize..12usize]);
  let mut p_copy: [u64; 12] = [0u64; 12usize];
  (p_copy[0usize..12usize]).copy_from_slice(&res[0usize..12usize]);
  point_add(res, &p_copy, &tmp0);
  let mut tmp1: [u64; 12] = [0u64; 12usize];
  for i in 0u32..51u32
  {
    for i2 in 0u32..5u32
    {
      let mut p_copy0: [u64; 12] = [0u64; 12usize];
      (p_copy0[0usize..12usize]).copy_from_slice(&res[0usize..12usize]);
      Hacl_P256_point_double(res, &p_copy0)
    };
    let k: u32 = 255u32.wrapping_sub(5u32.wrapping_mul(i)).wrapping_sub(5u32);
    let bits_l: u64 =
        crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(4u32, scalar2, k, 5u32);
    crate::lowstar::ignore::ignore::<[u64; 384]>(table2);
    let bits_l321: u32 = bits_l as u32;
    let a_bits_l1: (&[u64], &[u64]) = table2.split_at(bits_l321.wrapping_mul(12u32) as usize);
    (tmp1[0usize..12usize]).copy_from_slice(&a_bits_l1.1[0usize..12usize]);
    let mut p_copy0: [u64; 12] = [0u64; 12usize];
    (p_copy0[0usize..12usize]).copy_from_slice(&res[0usize..12usize]);
    point_add(res, &p_copy0, &tmp1);
    let k0: u32 = 255u32.wrapping_sub(5u32.wrapping_mul(i)).wrapping_sub(5u32);
    let bits_l0: u64 =
        crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(4u32, scalar1, k0, 5u32);
    let bits_l322: u32 = bits_l0 as u32;
    let a_bits_l2: &[u64] =
        &crate::hacl::p256_precomptable::Hacl_P256_PrecompTable_precomp_basepoint_table_w5[bits_l322.wrapping_mul(
          12u32
        )
        as
        usize..];
    (tmp1[0usize..12usize]).copy_from_slice(&a_bits_l2[0usize..12usize]);
    let mut p_copy1: [u64; 12] = [0u64; 12usize];
    (p_copy1[0usize..12usize]).copy_from_slice(&res[0usize..12usize]);
    point_add(res, &p_copy1, &tmp1)
  }
}

#[inline] pub fn point_mul_g(res: &mut [u64], scalar: &[u64])
{
  let mut q1: [u64; 12] = [0u64; 12usize];
  make_base_point(&mut q1);
  let q2: [u64; 12] =
      [1499621593102562565u64, 16692369783039433128u64, 15337520135922861848u64,
          5455737214495366228u64, 17827017231032529600u64, 12413621606240782649u64,
          2290483008028286132u64, 15752017553340844820u64, 4846430910634234874u64,
          10861682798464583253u64, 15404737222404363049u64, 363586619281562022u64];
  let q3: [u64; 12] =
      [14619254753077084366u64, 13913835116514008593u64, 15060744674088488145u64,
          17668414598203068685u64, 10761169236902342334u64, 15467027479157446221u64,
          14989185522423469618u64, 14354539272510107003u64, 14298211796392133693u64,
          13270323784253711450u64, 13380964971965046957u64, 8686204248456909699u64];
  let q4: [u64; 12] =
      [7870395003430845958u64, 18001862936410067720u64, 8006461232116967215u64,
          5921313779532424762u64, 10702113371959864307u64, 8070517410642379879u64,
          7139806720777708306u64, 8253938546650739833u64, 17490482834545705718u64,
          1065249776797037500u64, 5018258455937968775u64, 14100621120178668337u64];
  let r1: (&[u64], &[u64]) = scalar.split_at(0usize);
  let r2: (&[u64], &[u64]) = (r1.1).split_at(1usize);
  let r3: (&[u64], &[u64]) = (r2.1).split_at(1usize);
  let r4: (&[u64], &[u64]) = (r3.1).split_at(1usize);
  make_point_at_inf(res);
  let mut tmp: [u64; 12] = [0u64; 12usize];
  for i in 0u32..16u32
  {
    for i0 in 0u32..4u32
    {
      let mut p_copy: [u64; 12] = [0u64; 12usize];
      (p_copy[0usize..12usize]).copy_from_slice(&res[0usize..12usize]);
      Hacl_P256_point_double(res, &p_copy)
    };
    let k: u32 = 64u32.wrapping_sub(4u32.wrapping_mul(i)).wrapping_sub(4u32);
    let bits_l: u64 =
        crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(1u32, r4.1, k, 4u32);
    precomp_get_consttime(
      &crate::hacl::p256_precomptable::Hacl_P256_PrecompTable_precomp_g_pow2_192_table_w4,
      bits_l,
      &mut tmp
    );
    let mut p_copy: [u64; 12] = [0u64; 12usize];
    (p_copy[0usize..12usize]).copy_from_slice(&res[0usize..12usize]);
    point_add(res, &p_copy, &tmp);
    let k0: u32 = 64u32.wrapping_sub(4u32.wrapping_mul(i)).wrapping_sub(4u32);
    let bits_l0: u64 =
        crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(1u32, r4.0, k0, 4u32);
    precomp_get_consttime(
      &crate::hacl::p256_precomptable::Hacl_P256_PrecompTable_precomp_g_pow2_128_table_w4,
      bits_l0,
      &mut tmp
    );
    let mut p_copy0: [u64; 12] = [0u64; 12usize];
    (p_copy0[0usize..12usize]).copy_from_slice(&res[0usize..12usize]);
    point_add(res, &p_copy0, &tmp);
    let k1: u32 = 64u32.wrapping_sub(4u32.wrapping_mul(i)).wrapping_sub(4u32);
    let bits_l1: u64 =
        crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(1u32, r3.0, k1, 4u32);
    precomp_get_consttime(
      &crate::hacl::p256_precomptable::Hacl_P256_PrecompTable_precomp_g_pow2_64_table_w4,
      bits_l1,
      &mut tmp
    );
    let mut p_copy1: [u64; 12] = [0u64; 12usize];
    (p_copy1[0usize..12usize]).copy_from_slice(&res[0usize..12usize]);
    point_add(res, &p_copy1, &tmp);
    let k2: u32 = 64u32.wrapping_sub(4u32.wrapping_mul(i)).wrapping_sub(4u32);
    let bits_l2: u64 =
        crate::hacl::bignum_base::Hacl_Bignum_Lib_bn_get_bits_u64(1u32, r2.0, k2, 4u32);
    precomp_get_consttime(
      &crate::hacl::p256_precomptable::Hacl_P256_PrecompTable_precomp_basepoint_table_w4,
      bits_l2,
      &mut tmp
    );
    let mut p_copy2: [u64; 12] = [0u64; 12usize];
    (p_copy2[0usize..12usize]).copy_from_slice(&res[0usize..12usize]);
    point_add(res, &p_copy2, &tmp)
  };
  crate::lowstar::ignore::ignore::<[u64; 12]>(q1);
  crate::lowstar::ignore::ignore::<[u64; 12]>(q2);
  crate::lowstar::ignore::ignore::<[u64; 12]>(q3);
  crate::lowstar::ignore::ignore::<[u64; 12]>(q4)
}

#[inline] pub fn point_store(res: &mut [u8], p: &[u64])
{
  let mut aff_p: [u64; 8] = [0u64; 8usize];
  to_aff_point(&mut aff_p, p);
  aff_point_store(res, &aff_p)
}

#[inline] pub fn precomp_get_consttime(table: &[u64], bits_l: u64, tmp: &mut [u64])
{
  (tmp[0usize..12usize]).copy_from_slice(&table[0usize..12usize]);
  for i0 in 0u32..15u32
  {
    let c: u64 = crate::hacl_krmllib::FStar_UInt64_eq_mask(bits_l, i0.wrapping_add(1u32) as u64);
    let res_j: (&[u64], &[u64]) =
        table.split_at(i0.wrapping_add(1u32).wrapping_mul(12u32) as usize);
    for i in 0u32..12u32
    {
      let x: u64 = c & res_j.1[i as usize] | ! c & tmp[i as usize];
      let os: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
      os.1[i as usize] = x
    }
  }
}

#[inline] pub fn qadd(res: &mut [u64], x: &[u64], y: &[u64])
{
  let mut n: [u64; 4] = [0u64; 4usize];
  make_order(&mut n);
  bn_add_mod4(res, &n, x, y)
}

#[inline] pub fn qinv(res: &mut [u64], r: &[u64])
{
  let mut tmp: [u64; 28] = [0u64; 28usize];
  let x6: (&mut [u64], &mut [u64]) = tmp.split_at_mut(0usize);
  let x_11: (&mut [u64], &mut [u64]) = (x6.1).split_at_mut(4usize);
  let x_101: (&mut [u64], &mut [u64]) = (x_11.1).split_at_mut(4usize);
  let x_111: (&mut [u64], &mut [u64]) = (x_101.1).split_at_mut(4usize);
  let x_1111: (&mut [u64], &mut [u64]) = (x_111.1).split_at_mut(4usize);
  let x_10101: (&mut [u64], &mut [u64]) = (x_1111.1).split_at_mut(4usize);
  let x_101111: (&mut [u64], &mut [u64]) = (x_10101.1).split_at_mut(4usize);
  (x_11.0[0usize..4usize]).copy_from_slice(&r[0usize..4usize]);
  {
    let mut x_copy: [u64; 4] = [0u64; 4usize];
    (x_copy[0usize..4usize]).copy_from_slice(&x_11.0[0usize..4usize]);
    qsqr(x_11.0, &x_copy)
  };
  qmul(x_101.0, x_11.0, r);
  qmul(x_111.0, x_11.0, x_101.0);
  qmul(x_1111.0, x_11.0, x_111.0);
  (x_11.0[0usize..4usize]).copy_from_slice(&x_111.0[0usize..4usize]);
  {
    let mut x_copy: [u64; 4] = [0u64; 4usize];
    (x_copy[0usize..4usize]).copy_from_slice(&x_11.0[0usize..4usize]);
    qsqr(x_11.0, &x_copy)
  };
  qmul(x_10101.0, x_111.0, x_11.0);
  {
    let mut x_copy: [u64; 4] = [0u64; 4usize];
    (x_copy[0usize..4usize]).copy_from_slice(&x_11.0[0usize..4usize]);
    qsqr(x_11.0, &x_copy)
  };
  qmul(x_101111.0, x_11.0, r);
  (x_11.0[0usize..4usize]).copy_from_slice(&x_101111.0[0usize..4usize]);
  {
    let mut x_copy: [u64; 4] = [0u64; 4usize];
    (x_copy[0usize..4usize]).copy_from_slice(&x_11.0[0usize..4usize]);
    qsqr(x_11.0, &x_copy)
  };
  qmul(x_101111.1, x_111.0, x_11.0);
  let mut y_copy: [u64; 4] = [0u64; 4usize];
  (y_copy[0usize..4usize]).copy_from_slice(&x_11.0[0usize..4usize]);
  qmul(x_11.0, x_101111.0, &y_copy);
  let mut tmp1: [u64; 4] = [0u64; 4usize];
  for i in 0u32..2u32
  {
    let mut x_copy: [u64; 4] = [0u64; 4usize];
    (x_copy[0usize..4usize]).copy_from_slice(&x_11.0[0usize..4usize]);
    qsqr(x_11.0, &x_copy)
  };
  let mut x_copy: [u64; 4] = [0u64; 4usize];
  (x_copy[0usize..4usize]).copy_from_slice(&x_11.0[0usize..4usize]);
  qmul(x_11.0, &x_copy, x_101.0);
  (tmp1[0usize..4usize]).copy_from_slice(&x_11.0[0usize..4usize]);
  for i in 0u32..8u32
  {
    let mut x_copy0: [u64; 4] = [0u64; 4usize];
    (x_copy0[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy0)
  };
  let mut x_copy0: [u64; 4] = [0u64; 4usize];
  (x_copy0[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy0, x_11.0);
  (x_11.0[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  for i in 0u32..16u32
  {
    let mut x_copy1: [u64; 4] = [0u64; 4usize];
    (x_copy1[0usize..4usize]).copy_from_slice(&x_11.0[0usize..4usize]);
    qsqr(x_11.0, &x_copy1)
  };
  let mut x_copy1: [u64; 4] = [0u64; 4usize];
  (x_copy1[0usize..4usize]).copy_from_slice(&x_11.0[0usize..4usize]);
  qmul(x_11.0, &x_copy1, &tmp1);
  (tmp1[0usize..4usize]).copy_from_slice(&x_11.0[0usize..4usize]);
  for i in 0u32..64u32
  {
    let mut x_copy2: [u64; 4] = [0u64; 4usize];
    (x_copy2[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy2)
  };
  let mut x_copy2: [u64; 4] = [0u64; 4usize];
  (x_copy2[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy2, x_11.0);
  for i in 0u32..32u32
  {
    let mut x_copy3: [u64; 4] = [0u64; 4usize];
    (x_copy3[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy3)
  };
  let mut x_copy3: [u64; 4] = [0u64; 4usize];
  (x_copy3[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy3, x_11.0);
  for i in 0u32..6u32
  {
    let mut x_copy4: [u64; 4] = [0u64; 4usize];
    (x_copy4[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy4)
  };
  let mut x_copy4: [u64; 4] = [0u64; 4usize];
  (x_copy4[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy4, x_101111.1);
  for i in 0u32..5u32
  {
    let mut x_copy5: [u64; 4] = [0u64; 4usize];
    (x_copy5[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy5)
  };
  let mut x_copy5: [u64; 4] = [0u64; 4usize];
  (x_copy5[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy5, x_1111.0);
  for i in 0u32..4u32
  {
    let mut x_copy6: [u64; 4] = [0u64; 4usize];
    (x_copy6[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy6)
  };
  let mut x_copy6: [u64; 4] = [0u64; 4usize];
  (x_copy6[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy6, x_101.0);
  for i in 0u32..5u32
  {
    let mut x_copy7: [u64; 4] = [0u64; 4usize];
    (x_copy7[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy7)
  };
  let mut x_copy7: [u64; 4] = [0u64; 4usize];
  (x_copy7[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy7, x_10101.0);
  for i in 0u32..5u32
  {
    let mut x_copy8: [u64; 4] = [0u64; 4usize];
    (x_copy8[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy8)
  };
  let mut x_copy8: [u64; 4] = [0u64; 4usize];
  (x_copy8[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy8, x_101111.0);
  for i in 0u32..4u32
  {
    let mut x_copy9: [u64; 4] = [0u64; 4usize];
    (x_copy9[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy9)
  };
  let mut x_copy9: [u64; 4] = [0u64; 4usize];
  (x_copy9[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy9, x_111.0);
  for i in 0u32..3u32
  {
    let mut x_copy10: [u64; 4] = [0u64; 4usize];
    (x_copy10[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy10)
  };
  let mut x_copy10: [u64; 4] = [0u64; 4usize];
  (x_copy10[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy10, x_111.0);
  for i in 0u32..3u32
  {
    let mut x_copy11: [u64; 4] = [0u64; 4usize];
    (x_copy11[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy11)
  };
  let mut x_copy11: [u64; 4] = [0u64; 4usize];
  (x_copy11[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy11, x_111.0);
  for i in 0u32..5u32
  {
    let mut x_copy12: [u64; 4] = [0u64; 4usize];
    (x_copy12[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy12)
  };
  let mut x_copy12: [u64; 4] = [0u64; 4usize];
  (x_copy12[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy12, x_1111.0);
  for i in 0u32..9u32
  {
    let mut x_copy13: [u64; 4] = [0u64; 4usize];
    (x_copy13[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy13)
  };
  let mut x_copy13: [u64; 4] = [0u64; 4usize];
  (x_copy13[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy13, x_101111.1);
  for i in 0u32..6u32
  {
    let mut x_copy14: [u64; 4] = [0u64; 4usize];
    (x_copy14[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy14)
  };
  let mut x_copy14: [u64; 4] = [0u64; 4usize];
  (x_copy14[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy14, x_10101.0);
  for i in 0u32..2u32
  {
    let mut x_copy15: [u64; 4] = [0u64; 4usize];
    (x_copy15[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy15)
  };
  let mut x_copy15: [u64; 4] = [0u64; 4usize];
  (x_copy15[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy15, r);
  for i in 0u32..5u32
  {
    let mut x_copy16: [u64; 4] = [0u64; 4usize];
    (x_copy16[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy16)
  };
  let mut x_copy16: [u64; 4] = [0u64; 4usize];
  (x_copy16[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy16, r);
  for i in 0u32..6u32
  {
    let mut x_copy17: [u64; 4] = [0u64; 4usize];
    (x_copy17[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy17)
  };
  let mut x_copy17: [u64; 4] = [0u64; 4usize];
  (x_copy17[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy17, x_10101.0);
  for i in 0u32..5u32
  {
    let mut x_copy18: [u64; 4] = [0u64; 4usize];
    (x_copy18[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy18)
  };
  let mut x_copy18: [u64; 4] = [0u64; 4usize];
  (x_copy18[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy18, x_1111.0);
  for i in 0u32..4u32
  {
    let mut x_copy19: [u64; 4] = [0u64; 4usize];
    (x_copy19[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy19)
  };
  let mut x_copy19: [u64; 4] = [0u64; 4usize];
  (x_copy19[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy19, x_1111.0);
  for i in 0u32..5u32
  {
    let mut x_copy20: [u64; 4] = [0u64; 4usize];
    (x_copy20[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy20)
  };
  let mut x_copy20: [u64; 4] = [0u64; 4usize];
  (x_copy20[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy20, x_1111.0);
  for i in 0u32..5u32
  {
    let mut x_copy21: [u64; 4] = [0u64; 4usize];
    (x_copy21[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy21)
  };
  let mut x_copy21: [u64; 4] = [0u64; 4usize];
  (x_copy21[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy21, x_111.0);
  for i in 0u32..3u32
  {
    let mut x_copy22: [u64; 4] = [0u64; 4usize];
    (x_copy22[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy22)
  };
  let mut x_copy22: [u64; 4] = [0u64; 4usize];
  (x_copy22[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy22, x_101.0);
  for i in 0u32..10u32
  {
    let mut x_copy23: [u64; 4] = [0u64; 4usize];
    (x_copy23[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy23)
  };
  let mut x_copy23: [u64; 4] = [0u64; 4usize];
  (x_copy23[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy23, x_101111.1);
  for i in 0u32..2u32
  {
    let mut x_copy24: [u64; 4] = [0u64; 4usize];
    (x_copy24[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy24)
  };
  let mut x_copy24: [u64; 4] = [0u64; 4usize];
  (x_copy24[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy24, x_101.0);
  for i in 0u32..5u32
  {
    let mut x_copy25: [u64; 4] = [0u64; 4usize];
    (x_copy25[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy25)
  };
  let mut x_copy25: [u64; 4] = [0u64; 4usize];
  (x_copy25[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy25, x_101.0);
  for i in 0u32..5u32
  {
    let mut x_copy26: [u64; 4] = [0u64; 4usize];
    (x_copy26[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy26)
  };
  let mut x_copy26: [u64; 4] = [0u64; 4usize];
  (x_copy26[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy26, x_101.0);
  for i in 0u32..3u32
  {
    let mut x_copy27: [u64; 4] = [0u64; 4usize];
    (x_copy27[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy27)
  };
  let mut x_copy27: [u64; 4] = [0u64; 4usize];
  (x_copy27[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy27, r);
  for i in 0u32..7u32
  {
    let mut x_copy28: [u64; 4] = [0u64; 4usize];
    (x_copy28[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy28)
  };
  let mut x_copy28: [u64; 4] = [0u64; 4usize];
  (x_copy28[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy28, x_101111.0);
  for i in 0u32..6u32
  {
    let mut x_copy29: [u64; 4] = [0u64; 4usize];
    (x_copy29[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
    qsqr(&mut tmp1, &x_copy29)
  };
  let mut x_copy29: [u64; 4] = [0u64; 4usize];
  (x_copy29[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  qmul(&mut tmp1, &x_copy29, x_10101.0);
  (x_11.0[0usize..4usize]).copy_from_slice(&tmp1[0usize..4usize]);
  (res[0usize..4usize]).copy_from_slice(&x_11.0[0usize..4usize])
}

#[inline] pub fn qmod_short(res: &mut [u64], x: &[u64])
{
  let mut tmp: [u64; 4] = [0u64; 4usize];
  make_order(&mut tmp);
  let mut y_copy: [u64; 4] = [0u64; 4usize];
  (y_copy[0usize..4usize]).copy_from_slice(&tmp[0usize..4usize]);
  let c: u64 = bn_sub4(&mut tmp, x, &y_copy);
  let c0: u64 = c;
  bn_cmovznz4(res, c0, &tmp, x)
}

#[inline] pub fn qmont_reduction(res: &mut [u64], x: &mut [u64])
{
  let mut n: [u64; 4] = [0u64; 4usize];
  make_order(&mut n);
  let mut c0: u64 = 0u64;
  for i0 in 0u32..4u32
  {
    let qj: u64 = 14758798090332847183u64.wrapping_mul(x[i0 as usize]);
    let res_j0: (&mut [u64], &mut [u64]) = x.split_at_mut(i0 as usize);
    let mut c: u64 = 0u64;
    {
      let a_i: u64 = n[4u32.wrapping_mul(0u32) as usize];
      let res_i0: (&mut [u64], &mut [u64]) =
          (res_j0.1).split_at_mut(4u32.wrapping_mul(0u32) as usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i, qj, c, res_i0.1);
      let a_i0: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
      let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i0, qj, c, res_i1.1);
      let a_i1: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
      let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i1, qj, c, res_i2.1);
      let a_i2: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
      let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
      c = crate::hacl::bignum_base::Hacl_Bignum_Base_mul_wide_add2_u64(a_i2, qj, c, res_i.1)
    };
    let r: u64 = c;
    let c1: u64 = r;
    let res_j: u64 = x[4u32.wrapping_add(i0) as usize];
    let resb: (&mut [u64], &mut [u64]) = x.split_at_mut(i0 as usize + 4usize);
    c0 = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_add_carry_u64(c0, c1, res_j, resb.1)
  };
  (res[0usize..4usize]).copy_from_slice(&(&x[4usize..])[0usize..4usize]);
  let c00: u64 = c0;
  let mut tmp: [u64; 4] = [0u64; 4usize];
  let mut c: u64 = 0u64;
  {
    let t1: u64 = res[4u32.wrapping_mul(0u32) as usize];
    let t20: u64 = n[4u32.wrapping_mul(0u32) as usize];
    let res_i0: (&mut [u64], &mut [u64]) = tmp.split_at_mut(4u32.wrapping_mul(0u32) as usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t1, t20, res_i0.1);
    let t10: u64 = res[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let t21: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(1u32) as usize];
    let res_i1: (&mut [u64], &mut [u64]) = (res_i0.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t10, t21, res_i1.1);
    let t11: u64 = res[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let t22: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(2u32) as usize];
    let res_i2: (&mut [u64], &mut [u64]) = (res_i1.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t11, t22, res_i2.1);
    let t12: u64 = res[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let t2: u64 = n[4u32.wrapping_mul(0u32).wrapping_add(3u32) as usize];
    let res_i: (&mut [u64], &mut [u64]) = (res_i2.1).split_at_mut(1usize);
    c = crate::lib_intrinsics::Lib_IntTypes_Intrinsics_sub_borrow_u64(c, t12, t2, res_i.1)
  };
  let c1: u64 = c;
  let c2: u64 = c00.wrapping_sub(c1);
  for i in 0u32..4u32
  {
    let x1: u64 = c2 & res[i as usize] | ! c2 & tmp[i as usize];
    let os: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
    os.1[i as usize] = x1
  }
}

#[inline] pub fn qmul(res: &mut [u64], x: &[u64], y: &[u64])
{
  let mut tmp: [u64; 8] = [0u64; 8usize];
  bn_mul4(&mut tmp, x, y);
  qmont_reduction(res, &mut tmp)
}

#[inline] pub fn qmul_mont(sinv: &[u64], b: &[u64], res: &mut [u64])
{
  let mut tmp: [u64; 4] = [0u64; 4usize];
  from_qmont(&mut tmp, b);
  qmul(res, sinv, &tmp)
}

#[inline] pub fn qsqr(res: &mut [u64], x: &[u64])
{
  let mut tmp: [u64; 8] = [0u64; 8usize];
  bn_sqr4(&mut tmp, x);
  qmont_reduction(res, &mut tmp)
}

#[inline] pub fn to_aff_point(res: &mut [u64], p: &[u64])
{
  let mut zinv: [u64; 4] = [0u64; 4usize];
  let px: (&[u64], &[u64]) = p.split_at(0usize);
  let py: (&[u64], &[u64]) = (px.1).split_at(4usize);
  let pz: (&[u64], &[u64]) = (py.1).split_at(4usize);
  let x: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
  let y: (&mut [u64], &mut [u64]) = (x.1).split_at_mut(4usize);
  finv(&mut zinv, pz.1);
  fmul0(y.0, py.0, &zinv);
  fmul0(y.1, pz.0, &zinv);
  let mut a_copy: [u64; 4] = [0u64; 4usize];
  (a_copy[0usize..4usize]).copy_from_slice(&y.0[0usize..4usize]);
  from_mont(y.0, &a_copy);
  let mut a_copy0: [u64; 4] = [0u64; 4usize];
  (a_copy0[0usize..4usize]).copy_from_slice(&y.1[0usize..4usize]);
  from_mont(y.1, &a_copy0)
}

#[inline] pub fn to_aff_point_x(res: &mut [u64], p: &[u64])
{
  let mut zinv: [u64; 4] = [0u64; 4usize];
  let px: (&[u64], &[u64]) = p.split_at(0usize);
  let pz: (&[u64], &[u64]) = (px.1).split_at(8usize);
  finv(&mut zinv, pz.1);
  fmul0(res, pz.0, &zinv);
  let mut a_copy: [u64; 4] = [0u64; 4usize];
  (a_copy[0usize..4usize]).copy_from_slice(&res[0usize..4usize]);
  from_mont(res, &a_copy)
}

#[inline] pub fn to_mont(res: &mut [u64], a: &[u64])
{
  let mut r2modn: [u64; 4] = [0u64; 4usize];
  make_fmont_R2(&mut r2modn);
  fmul0(res, a, &r2modn)
}

#[inline] pub fn to_proj_point(res: &mut [u64], p: &[u64])
{
  let px: (&[u64], &[u64]) = p.split_at(0usize);
  let py: (&[u64], &[u64]) = (px.1).split_at(4usize);
  let rx: (&mut [u64], &mut [u64]) = res.split_at_mut(0usize);
  let ry: (&mut [u64], &mut [u64]) = (rx.1).split_at_mut(4usize);
  let rz: (&mut [u64], &mut [u64]) = (ry.1).split_at_mut(4usize);
  to_mont(ry.0, py.0);
  to_mont(rz.0, py.1);
  make_fone(rz.1)
}
