#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

pub fn Hacl_Hash_SHA2_copy_256 <'a>(
  state: &'a [crate::hacl::streaming_types::Hacl_Streaming_MD_state_32]
) ->
    Box<[crate::hacl::streaming_types::Hacl_Streaming_MD_state_32]>
{
  let block_state0: &[u32] = &(state[0usize]).block_state;
  let buf0: &[u8] = &(state[0usize]).buf;
  let total_len0: u64 = (state[0usize]).total_len;
  let mut buf: Box<[u8]> = vec![0u8; 64usize].into_boxed_slice();
  ((&mut buf)[0usize..64usize]).copy_from_slice(&buf0[0usize..64usize]);
  let mut b: Box<[u32]> = vec![0u32; 8usize].into_boxed_slice();
  let block_state: crate::hacl::streaming_types::Hacl_Streaming_Types_optional_32 =
      crate::hacl::streaming_types::Hacl_Streaming_Types_optional_32 { tag: 1u8, v: &mut b };
  if block_state.tag as u32 == 0u32 { return [].into() };
  if block_state.tag as u32 == 1u32
  {
    let block_state1: &mut [u32] = block_state.v;
    let block_state2: (&mut [u32], &mut [u32]) = block_state1.split_at_mut(0usize);
    (block_state2.1[0usize..8usize]).copy_from_slice(&block_state0[0usize..8usize]);
    let k_: u8 = 1u8;
    match k_
    {
      0u8 => return [].into(),
      1u8 =>
        {
          let s: crate::hacl::streaming_types::Hacl_Streaming_MD_state_32 =
              crate::hacl::streaming_types::Hacl_Streaming_MD_state_32
              { block_state: (*block_state2.1).into(), buf, total_len: total_len0 };
          let p: Box<[crate::hacl::streaming_types::Hacl_Streaming_MD_state_32]> =
              vec![s].into_boxed_slice();
          return p
        },
      _ => panic!("")
    }
  };
  panic!("")
}

pub fn Hacl_Hash_SHA2_copy_512 <'a>(
  state: &'a [crate::hacl::streaming_types::Hacl_Streaming_MD_state_64]
) ->
    Box<[crate::hacl::streaming_types::Hacl_Streaming_MD_state_64]>
{
  let block_state0: &[u64] = &(state[0usize]).block_state;
  let buf0: &[u8] = &(state[0usize]).buf;
  let total_len0: u64 = (state[0usize]).total_len;
  let mut buf: Box<[u8]> = vec![0u8; 128usize].into_boxed_slice();
  ((&mut buf)[0usize..128usize]).copy_from_slice(&buf0[0usize..128usize]);
  let mut b: Box<[u64]> = vec![0u64; 8usize].into_boxed_slice();
  let block_state: crate::hacl::streaming_types::Hacl_Streaming_Types_optional_64 =
      crate::hacl::streaming_types::Hacl_Streaming_Types_optional_64 { tag: 1u8, v: &mut b };
  if block_state.tag as u32 == 0u32 { return [].into() };
  if block_state.tag as u32 == 1u32
  {
    let block_state1: &mut [u64] = block_state.v;
    let block_state2: (&mut [u64], &mut [u64]) = block_state1.split_at_mut(0usize);
    (block_state2.1[0usize..8usize]).copy_from_slice(&block_state0[0usize..8usize]);
    let k_: u8 = 1u8;
    match k_
    {
      0u8 => return [].into(),
      1u8 =>
        {
          let s: crate::hacl::streaming_types::Hacl_Streaming_MD_state_64 =
              crate::hacl::streaming_types::Hacl_Streaming_MD_state_64
              { block_state: (*block_state2.1).into(), buf, total_len: total_len0 };
          let p: Box<[crate::hacl::streaming_types::Hacl_Streaming_MD_state_64]> =
              vec![s].into_boxed_slice();
          return p
        },
      _ => panic!("")
    }
  };
  panic!("")
}

pub fn Hacl_Hash_SHA2_digest_224(
  state: &[crate::hacl::streaming_types::Hacl_Streaming_MD_state_32],
  output: &mut [u8]
)
{
  let block_state: &[u32] = &(state[0usize]).block_state;
  let buf_: &[u8] = &(state[0usize]).buf;
  let total_len: u64 = (state[0usize]).total_len;
  let mut r: u32;
  if total_len.wrapping_rem(64u32 as u64) == 0u64 && total_len > 0u64
  { r = 64u32 }
  else
  { r = total_len.wrapping_rem(64u32 as u64) as u32 };
  let buf_1: (&[u8], &[u8]) = buf_.split_at(0usize);
  let mut tmp_block_state: [u32; 8] = [0u32; 8usize];
  (tmp_block_state[0usize..8usize]).copy_from_slice(&block_state[0usize..8usize]);
  let buf_multi: (&[u8], &[u8]) = (buf_1.1).split_at(0usize);
  let mut ite: u32;
  if r.wrapping_rem(64u32) == 0u32 && r > 0u32
  { ite = 64u32 }
  else
  { ite = r.wrapping_rem(64u32) };
  let buf_last: (&[u8], &[u8]) = (buf_multi.1).split_at((r as usize).wrapping_sub(ite as usize));
  Hacl_Hash_SHA2_sha224_update_nblocks(0u32, buf_last.0, &mut tmp_block_state);
  let prev_len_last: u64 = total_len.wrapping_sub(r as u64);
  Hacl_Hash_SHA2_sha224_update_last(
    prev_len_last.wrapping_add(r as u64),
    r,
    buf_last.1,
    &mut tmp_block_state
  );
  Hacl_Hash_SHA2_sha224_finish(&tmp_block_state, output)
}

pub fn Hacl_Hash_SHA2_digest_256(
  state: &[crate::hacl::streaming_types::Hacl_Streaming_MD_state_32],
  output: &mut [u8]
)
{
  let block_state: &[u32] = &(state[0usize]).block_state;
  let buf_: &[u8] = &(state[0usize]).buf;
  let total_len: u64 = (state[0usize]).total_len;
  let mut r: u32;
  if total_len.wrapping_rem(64u32 as u64) == 0u64 && total_len > 0u64
  { r = 64u32 }
  else
  { r = total_len.wrapping_rem(64u32 as u64) as u32 };
  let buf_1: (&[u8], &[u8]) = buf_.split_at(0usize);
  let mut tmp_block_state: [u32; 8] = [0u32; 8usize];
  (tmp_block_state[0usize..8usize]).copy_from_slice(&block_state[0usize..8usize]);
  let buf_multi: (&[u8], &[u8]) = (buf_1.1).split_at(0usize);
  let mut ite: u32;
  if r.wrapping_rem(64u32) == 0u32 && r > 0u32
  { ite = 64u32 }
  else
  { ite = r.wrapping_rem(64u32) };
  let buf_last: (&[u8], &[u8]) = (buf_multi.1).split_at((r as usize).wrapping_sub(ite as usize));
  Hacl_Hash_SHA2_sha256_update_nblocks(0u32, buf_last.0, &mut tmp_block_state);
  let prev_len_last: u64 = total_len.wrapping_sub(r as u64);
  Hacl_Hash_SHA2_sha256_update_last(
    prev_len_last.wrapping_add(r as u64),
    r,
    buf_last.1,
    &mut tmp_block_state
  );
  Hacl_Hash_SHA2_sha256_finish(&tmp_block_state, output)
}

pub fn Hacl_Hash_SHA2_digest_384(
  state: &[crate::hacl::streaming_types::Hacl_Streaming_MD_state_64],
  output: &mut [u8]
)
{
  let block_state: &[u64] = &(state[0usize]).block_state;
  let buf_: &[u8] = &(state[0usize]).buf;
  let total_len: u64 = (state[0usize]).total_len;
  let mut r: u32;
  if total_len.wrapping_rem(128u32 as u64) == 0u64 && total_len > 0u64
  { r = 128u32 }
  else
  { r = total_len.wrapping_rem(128u32 as u64) as u32 };
  let buf_1: (&[u8], &[u8]) = buf_.split_at(0usize);
  let mut tmp_block_state: [u64; 8] = [0u64; 8usize];
  (tmp_block_state[0usize..8usize]).copy_from_slice(&block_state[0usize..8usize]);
  let buf_multi: (&[u8], &[u8]) = (buf_1.1).split_at(0usize);
  let mut ite: u32;
  if r.wrapping_rem(128u32) == 0u32 && r > 0u32
  { ite = 128u32 }
  else
  { ite = r.wrapping_rem(128u32) };
  let buf_last: (&[u8], &[u8]) = (buf_multi.1).split_at((r as usize).wrapping_sub(ite as usize));
  Hacl_Hash_SHA2_sha384_update_nblocks(0u32, buf_last.0, &mut tmp_block_state);
  let prev_len_last: u64 = total_len.wrapping_sub(r as u64);
  Hacl_Hash_SHA2_sha384_update_last(
    crate::hacl_krmllib::FStar_UInt128_add(
      crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(prev_len_last),
      crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(r as u64)
    ),
    r,
    buf_last.1,
    &mut tmp_block_state
  );
  Hacl_Hash_SHA2_sha384_finish(&tmp_block_state, output)
}

pub fn Hacl_Hash_SHA2_digest_512(
  state: &[crate::hacl::streaming_types::Hacl_Streaming_MD_state_64],
  output: &mut [u8]
)
{
  let block_state: &[u64] = &(state[0usize]).block_state;
  let buf_: &[u8] = &(state[0usize]).buf;
  let total_len: u64 = (state[0usize]).total_len;
  let mut r: u32;
  if total_len.wrapping_rem(128u32 as u64) == 0u64 && total_len > 0u64
  { r = 128u32 }
  else
  { r = total_len.wrapping_rem(128u32 as u64) as u32 };
  let buf_1: (&[u8], &[u8]) = buf_.split_at(0usize);
  let mut tmp_block_state: [u64; 8] = [0u64; 8usize];
  (tmp_block_state[0usize..8usize]).copy_from_slice(&block_state[0usize..8usize]);
  let buf_multi: (&[u8], &[u8]) = (buf_1.1).split_at(0usize);
  let mut ite: u32;
  if r.wrapping_rem(128u32) == 0u32 && r > 0u32
  { ite = 128u32 }
  else
  { ite = r.wrapping_rem(128u32) };
  let buf_last: (&[u8], &[u8]) = (buf_multi.1).split_at((r as usize).wrapping_sub(ite as usize));
  Hacl_Hash_SHA2_sha512_update_nblocks(0u32, buf_last.0, &mut tmp_block_state);
  let prev_len_last: u64 = total_len.wrapping_sub(r as u64);
  Hacl_Hash_SHA2_sha512_update_last(
    crate::hacl_krmllib::FStar_UInt128_add(
      crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(prev_len_last),
      crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(r as u64)
    ),
    r,
    buf_last.1,
    &mut tmp_block_state
  );
  Hacl_Hash_SHA2_sha512_finish(&tmp_block_state, output)
}

pub fn Hacl_Hash_SHA2_free_224(
  state: &[crate::hacl::streaming_types::Hacl_Streaming_MD_state_32]
)
{ Hacl_Hash_SHA2_free_256(state) }

pub fn Hacl_Hash_SHA2_free_256(
  state: &[crate::hacl::streaming_types::Hacl_Streaming_MD_state_32]
)
{
  let scrut: &crate::hacl::streaming_types::Hacl_Streaming_MD_state_32 = &state[0usize];
  let buf: &[u8] = &scrut.buf;
  let block_state: &[u32] = &scrut.block_state;
  ()
}

pub fn Hacl_Hash_SHA2_free_384(
  state: &[crate::hacl::streaming_types::Hacl_Streaming_MD_state_64]
)
{ Hacl_Hash_SHA2_free_512(state) }

pub fn Hacl_Hash_SHA2_free_512(
  state: &[crate::hacl::streaming_types::Hacl_Streaming_MD_state_64]
)
{
  let scrut: &crate::hacl::streaming_types::Hacl_Streaming_MD_state_64 = &state[0usize];
  let buf: &[u8] = &scrut.buf;
  let block_state: &[u64] = &scrut.block_state;
  ()
}

pub const Hacl_Hash_SHA2_h224: [u32; 8] =
    [3238371032u32, 914150663u32, 812702999u32, 4144912697u32, 4290775857u32, 1750603025u32,
        1694076839u32, 3204075428u32];

pub const Hacl_Hash_SHA2_h256: [u32; 8] =
    [1779033703u32, 3144134277u32, 1013904242u32, 2773480762u32, 1359893119u32, 2600822924u32,
        528734635u32, 1541459225u32];

pub const Hacl_Hash_SHA2_h384: [u64; 8] =
    [14680500436340154072u64, 7105036623409894663u64, 10473403895298186519u64,
        1526699215303891257u64, 7436329637833083697u64, 10282925794625328401u64,
        15784041429090275239u64, 5167115440072839076u64];

pub const Hacl_Hash_SHA2_h512: [u64; 8] =
    [7640891576956012808u64, 13503953896175478587u64, 4354685564936845355u64,
        11912009170470909681u64, 5840696475078001361u64, 11170449401992604703u64,
        2270897969802886507u64, 6620516959819538809u64];

pub fn Hacl_Hash_SHA2_hash_224(output: &mut [u8], input: &[u8], input_len: u32)
{
  let ib: (&[u8], &[u8]) = input.split_at(0usize);
  let rb: (&mut [u8], &mut [u8]) = output.split_at_mut(0usize);
  let mut st: [u32; 8] = [0u32; 8usize];
  Hacl_Hash_SHA2_sha224_init(&mut st);
  let rem: u32 = input_len.wrapping_rem(64u32);
  let len_: u64 = input_len as u64;
  Hacl_Hash_SHA2_sha224_update_nblocks(input_len, ib.1, &mut st);
  let rem1: u32 = input_len.wrapping_rem(64u32);
  let b0: (&[u8], &[u8]) = (ib.1).split_at(0usize);
  let lb: (&[u8], &[u8]) = (b0.1).split_at((input_len as usize).wrapping_sub(rem1 as usize));
  Hacl_Hash_SHA2_sha224_update_last(len_, rem, lb.1, &mut st);
  Hacl_Hash_SHA2_sha224_finish(&st, rb.1)
}

pub fn Hacl_Hash_SHA2_hash_256(output: &mut [u8], input: &[u8], input_len: u32)
{
  let ib: (&[u8], &[u8]) = input.split_at(0usize);
  let rb: (&mut [u8], &mut [u8]) = output.split_at_mut(0usize);
  let mut st: [u32; 8] = [0u32; 8usize];
  Hacl_Hash_SHA2_sha256_init(&mut st);
  let rem: u32 = input_len.wrapping_rem(64u32);
  let len_: u64 = input_len as u64;
  Hacl_Hash_SHA2_sha256_update_nblocks(input_len, ib.1, &mut st);
  let rem1: u32 = input_len.wrapping_rem(64u32);
  let b0: (&[u8], &[u8]) = (ib.1).split_at(0usize);
  let lb: (&[u8], &[u8]) = (b0.1).split_at((input_len as usize).wrapping_sub(rem1 as usize));
  Hacl_Hash_SHA2_sha256_update_last(len_, rem, lb.1, &mut st);
  Hacl_Hash_SHA2_sha256_finish(&st, rb.1)
}

pub fn Hacl_Hash_SHA2_hash_384(output: &mut [u8], input: &[u8], input_len: u32)
{
  let ib: (&[u8], &[u8]) = input.split_at(0usize);
  let rb: (&mut [u8], &mut [u8]) = output.split_at_mut(0usize);
  let mut st: [u64; 8] = [0u64; 8usize];
  Hacl_Hash_SHA2_sha384_init(&mut st);
  let rem: u32 = input_len.wrapping_rem(128u32);
  let len_: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(input_len as u64);
  Hacl_Hash_SHA2_sha384_update_nblocks(input_len, ib.1, &mut st);
  let rem1: u32 = input_len.wrapping_rem(128u32);
  let b0: (&[u8], &[u8]) = (ib.1).split_at(0usize);
  let lb: (&[u8], &[u8]) = (b0.1).split_at((input_len as usize).wrapping_sub(rem1 as usize));
  Hacl_Hash_SHA2_sha384_update_last(len_, rem, lb.1, &mut st);
  Hacl_Hash_SHA2_sha384_finish(&st, rb.1)
}

pub fn Hacl_Hash_SHA2_hash_512(output: &mut [u8], input: &[u8], input_len: u32)
{
  let ib: (&[u8], &[u8]) = input.split_at(0usize);
  let rb: (&mut [u8], &mut [u8]) = output.split_at_mut(0usize);
  let mut st: [u64; 8] = [0u64; 8usize];
  Hacl_Hash_SHA2_sha512_init(&mut st);
  let rem: u32 = input_len.wrapping_rem(128u32);
  let len_: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_uint64_to_uint128(input_len as u64);
  Hacl_Hash_SHA2_sha512_update_nblocks(input_len, ib.1, &mut st);
  let rem1: u32 = input_len.wrapping_rem(128u32);
  let b0: (&[u8], &[u8]) = (ib.1).split_at(0usize);
  let lb: (&[u8], &[u8]) = (b0.1).split_at((input_len as usize).wrapping_sub(rem1 as usize));
  Hacl_Hash_SHA2_sha512_update_last(len_, rem, lb.1, &mut st);
  Hacl_Hash_SHA2_sha512_finish(&st, rb.1)
}

pub const Hacl_Hash_SHA2_k224_256: [u32; 64] =
    [1116352408u32, 1899447441u32, 3049323471u32, 3921009573u32, 961987163u32, 1508970993u32,
        2453635748u32, 2870763221u32, 3624381080u32, 310598401u32, 607225278u32, 1426881987u32,
        1925078388u32, 2162078206u32, 2614888103u32, 3248222580u32, 3835390401u32, 4022224774u32,
        264347078u32, 604807628u32, 770255983u32, 1249150122u32, 1555081692u32, 1996064986u32,
        2554220882u32, 2821834349u32, 2952996808u32, 3210313671u32, 3336571891u32, 3584528711u32,
        113926993u32, 338241895u32, 666307205u32, 773529912u32, 1294757372u32, 1396182291u32,
        1695183700u32, 1986661051u32, 2177026350u32, 2456956037u32, 2730485921u32, 2820302411u32,
        3259730800u32, 3345764771u32, 3516065817u32, 3600352804u32, 4094571909u32, 275423344u32,
        430227734u32, 506948616u32, 659060556u32, 883997877u32, 958139571u32, 1322822218u32,
        1537002063u32, 1747873779u32, 1955562222u32, 2024104815u32, 2227730452u32, 2361852424u32,
        2428436474u32, 2756734187u32, 3204031479u32, 3329325298u32];

pub const Hacl_Hash_SHA2_k384_512: [u64; 80] =
    [4794697086780616226u64, 8158064640168781261u64, 13096744586834688815u64,
        16840607885511220156u64, 4131703408338449720u64, 6480981068601479193u64,
        10538285296894168987u64, 12329834152419229976u64, 15566598209576043074u64,
        1334009975649890238u64, 2608012711638119052u64, 6128411473006802146u64,
        8268148722764581231u64, 9286055187155687089u64, 11230858885718282805u64,
        13951009754708518548u64, 16472876342353939154u64, 17275323862435702243u64,
        1135362057144423861u64, 2597628984639134821u64, 3308224258029322869u64,
        5365058923640841347u64, 6679025012923562964u64, 8573033837759648693u64,
        10970295158949994411u64, 12119686244451234320u64, 12683024718118986047u64,
        13788192230050041572u64, 14330467153632333762u64, 15395433587784984357u64,
        489312712824947311u64, 1452737877330783856u64, 2861767655752347644u64,
        3322285676063803686u64, 5560940570517711597u64, 5996557281743188959u64,
        7280758554555802590u64, 8532644243296465576u64, 9350256976987008742u64,
        10552545826968843579u64, 11727347734174303076u64, 12113106623233404929u64,
        14000437183269869457u64, 14369950271660146224u64, 15101387698204529176u64,
        15463397548674623760u64, 17586052441742319658u64, 1182934255886127544u64,
        1847814050463011016u64, 2177327727835720531u64, 2830643537854262169u64,
        3796741975233480872u64, 4115178125766777443u64, 5681478168544905931u64,
        6601373596472566643u64, 7507060721942968483u64, 8399075790359081724u64,
        8693463985226723168u64, 9568029438360202098u64, 10144078919501101548u64,
        10430055236837252648u64, 11840083180663258601u64, 13761210420658862357u64,
        14299343276471374635u64, 14566680578165727644u64, 15097957966210449927u64,
        16922976911328602910u64, 17689382322260857208u64, 500013540394364858u64,
        748580250866718886u64, 1242879168328830382u64, 1977374033974150939u64,
        2944078676154940804u64, 3659926193048069267u64, 4368137639120453308u64,
        4836135668995329356u64, 5532061633213252278u64, 6448918945643986474u64,
        6902733635092675308u64, 7801388544844847127u64];

pub fn Hacl_Hash_SHA2_malloc_224 <'a>() ->
    Box<[crate::hacl::streaming_types::Hacl_Streaming_MD_state_32]>
{
  let buf: Box<[u8]> = vec![0u8; 64usize].into_boxed_slice();
  let buf1: (&[u8], &[u8]) = buf.split_at(0usize);
  let mut b: Box<[u32]> = vec![0u32; 8usize].into_boxed_slice();
  let block_state: crate::hacl::streaming_types::Hacl_Streaming_Types_optional_32 =
      crate::hacl::streaming_types::Hacl_Streaming_Types_optional_32 { tag: 1u8, v: &mut b };
  if block_state.tag as u32 == 0u32 { return [].into() };
  if block_state.tag as u32 == 1u32
  {
    let block_state1: &mut [u32] = block_state.v;
    let block_state2: (&mut [u32], &mut [u32]) = block_state1.split_at_mut(0usize);
    let k_: u8 = 1u8;
    match k_
    {
      0u8 => return [].into(),
      1u8 =>
        {
          Hacl_Hash_SHA2_sha224_init(block_state2.1);
          let s: crate::hacl::streaming_types::Hacl_Streaming_MD_state_32 =
              crate::hacl::streaming_types::Hacl_Streaming_MD_state_32
              {
                block_state: (*block_state2.1).into(),
                buf: (*buf1.1).into(),
                total_len: 0u32 as u64
              };
          let p: Box<[crate::hacl::streaming_types::Hacl_Streaming_MD_state_32]> =
              vec![s].into_boxed_slice();
          return p
        },
      _ => panic!("")
    }
  };
  panic!("")
}

pub fn Hacl_Hash_SHA2_malloc_256 <'a>() ->
    Box<[crate::hacl::streaming_types::Hacl_Streaming_MD_state_32]>
{
  let buf: Box<[u8]> = vec![0u8; 64usize].into_boxed_slice();
  let buf1: (&[u8], &[u8]) = buf.split_at(0usize);
  let mut b: Box<[u32]> = vec![0u32; 8usize].into_boxed_slice();
  let block_state: crate::hacl::streaming_types::Hacl_Streaming_Types_optional_32 =
      crate::hacl::streaming_types::Hacl_Streaming_Types_optional_32 { tag: 1u8, v: &mut b };
  if block_state.tag as u32 == 0u32 { return [].into() };
  if block_state.tag as u32 == 1u32
  {
    let block_state1: &mut [u32] = block_state.v;
    let block_state2: (&mut [u32], &mut [u32]) = block_state1.split_at_mut(0usize);
    let k_: u8 = 1u8;
    match k_
    {
      0u8 => return [].into(),
      1u8 =>
        {
          Hacl_Hash_SHA2_sha256_init(block_state2.1);
          let s: crate::hacl::streaming_types::Hacl_Streaming_MD_state_32 =
              crate::hacl::streaming_types::Hacl_Streaming_MD_state_32
              {
                block_state: (*block_state2.1).into(),
                buf: (*buf1.1).into(),
                total_len: 0u32 as u64
              };
          let p: Box<[crate::hacl::streaming_types::Hacl_Streaming_MD_state_32]> =
              vec![s].into_boxed_slice();
          return p
        },
      _ => panic!("")
    }
  };
  panic!("")
}

pub fn Hacl_Hash_SHA2_malloc_384 <'a>() ->
    Box<[crate::hacl::streaming_types::Hacl_Streaming_MD_state_64]>
{
  let buf: Box<[u8]> = vec![0u8; 128usize].into_boxed_slice();
  let buf1: (&[u8], &[u8]) = buf.split_at(0usize);
  let mut b: Box<[u64]> = vec![0u64; 8usize].into_boxed_slice();
  let block_state: crate::hacl::streaming_types::Hacl_Streaming_Types_optional_64 =
      crate::hacl::streaming_types::Hacl_Streaming_Types_optional_64 { tag: 1u8, v: &mut b };
  if block_state.tag as u32 == 0u32 { return [].into() };
  if block_state.tag as u32 == 1u32
  {
    let block_state1: &mut [u64] = block_state.v;
    let block_state2: (&mut [u64], &mut [u64]) = block_state1.split_at_mut(0usize);
    let k_: u8 = 1u8;
    match k_
    {
      0u8 => return [].into(),
      1u8 =>
        {
          Hacl_Hash_SHA2_sha384_init(block_state2.1);
          let s: crate::hacl::streaming_types::Hacl_Streaming_MD_state_64 =
              crate::hacl::streaming_types::Hacl_Streaming_MD_state_64
              {
                block_state: (*block_state2.1).into(),
                buf: (*buf1.1).into(),
                total_len: 0u32 as u64
              };
          let p: Box<[crate::hacl::streaming_types::Hacl_Streaming_MD_state_64]> =
              vec![s].into_boxed_slice();
          return p
        },
      _ => panic!("")
    }
  };
  panic!("")
}

pub fn Hacl_Hash_SHA2_malloc_512 <'a>() ->
    Box<[crate::hacl::streaming_types::Hacl_Streaming_MD_state_64]>
{
  let buf: Box<[u8]> = vec![0u8; 128usize].into_boxed_slice();
  let buf1: (&[u8], &[u8]) = buf.split_at(0usize);
  let mut b: Box<[u64]> = vec![0u64; 8usize].into_boxed_slice();
  let block_state: crate::hacl::streaming_types::Hacl_Streaming_Types_optional_64 =
      crate::hacl::streaming_types::Hacl_Streaming_Types_optional_64 { tag: 1u8, v: &mut b };
  if block_state.tag as u32 == 0u32 { return [].into() };
  if block_state.tag as u32 == 1u32
  {
    let block_state1: &mut [u64] = block_state.v;
    let block_state2: (&mut [u64], &mut [u64]) = block_state1.split_at_mut(0usize);
    let k_: u8 = 1u8;
    match k_
    {
      0u8 => return [].into(),
      1u8 =>
        {
          Hacl_Hash_SHA2_sha512_init(block_state2.1);
          let s: crate::hacl::streaming_types::Hacl_Streaming_MD_state_64 =
              crate::hacl::streaming_types::Hacl_Streaming_MD_state_64
              {
                block_state: (*block_state2.1).into(),
                buf: (*buf1.1).into(),
                total_len: 0u32 as u64
              };
          let p: Box<[crate::hacl::streaming_types::Hacl_Streaming_MD_state_64]> =
              vec![s].into_boxed_slice();
          return p
        },
      _ => panic!("")
    }
  };
  panic!("")
}

pub fn Hacl_Hash_SHA2_reset_224(
  state: &mut [crate::hacl::streaming_types::Hacl_Streaming_MD_state_32]
)
{
  let block_state: &mut [u32] = &mut (state[0usize]).block_state;
  Hacl_Hash_SHA2_sha224_init(block_state);
  let total_len: u64 = 0u32 as u64;
  (state[0usize]).total_len = total_len
}

pub fn Hacl_Hash_SHA2_reset_256(
  state: &mut [crate::hacl::streaming_types::Hacl_Streaming_MD_state_32]
)
{
  let block_state: &mut [u32] = &mut (state[0usize]).block_state;
  Hacl_Hash_SHA2_sha256_init(block_state);
  let total_len: u64 = 0u32 as u64;
  (state[0usize]).total_len = total_len
}

pub fn Hacl_Hash_SHA2_reset_384(
  state: &mut [crate::hacl::streaming_types::Hacl_Streaming_MD_state_64]
)
{
  let block_state: &mut [u64] = &mut (state[0usize]).block_state;
  Hacl_Hash_SHA2_sha384_init(block_state);
  let total_len: u64 = 0u32 as u64;
  (state[0usize]).total_len = total_len
}

pub fn Hacl_Hash_SHA2_reset_512(
  state: &mut [crate::hacl::streaming_types::Hacl_Streaming_MD_state_64]
)
{
  let block_state: &mut [u64] = &mut (state[0usize]).block_state;
  Hacl_Hash_SHA2_sha512_init(block_state);
  let total_len: u64 = 0u32 as u64;
  (state[0usize]).total_len = total_len
}

pub fn Hacl_Hash_SHA2_sha224_finish(st: &[u32], h: &mut [u8])
{
  let mut hbuf: [u8; 32] = [0u8; 32usize];
  for i in 0u32..8u32
  {
    crate::lowstar_endianness::store32_be(
      &mut hbuf[i.wrapping_mul(4u32) as usize..],
      st[i as usize]
    )
  };
  (h[0usize..28usize]).copy_from_slice(&hbuf[0usize..28usize])
}

pub fn Hacl_Hash_SHA2_sha224_init(hash: &mut [u32])
{
  for i in 0u32..8u32
  {
    let x: u32 = Hacl_Hash_SHA2_h224[i as usize];
    let os: (&mut [u32], &mut [u32]) = hash.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn Hacl_Hash_SHA2_sha224_update_last(totlen: u64, len: u32, b: &[u8], st: &mut [u32])
{ Hacl_Hash_SHA2_sha256_update_last(totlen, len, b, st) }

pub fn Hacl_Hash_SHA2_sha224_update_nblocks(len: u32, b: &[u8], st: &mut [u32])
{ Hacl_Hash_SHA2_sha256_update_nblocks(len, b, st) }

pub fn Hacl_Hash_SHA2_sha256_finish(st: &[u32], h: &mut [u8])
{
  let mut hbuf: [u8; 32] = [0u8; 32usize];
  for i in 0u32..8u32
  {
    crate::lowstar_endianness::store32_be(
      &mut hbuf[i.wrapping_mul(4u32) as usize..],
      st[i as usize]
    )
  };
  (h[0usize..32usize]).copy_from_slice(&hbuf[0usize..32usize])
}

pub fn Hacl_Hash_SHA2_sha256_init(hash: &mut [u32])
{
  for i in 0u32..8u32
  {
    let x: u32 = Hacl_Hash_SHA2_h256[i as usize];
    let os: (&mut [u32], &mut [u32]) = hash.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn Hacl_Hash_SHA2_sha256_update_last(totlen: u64, len: u32, b: &[u8], hash: &mut [u32])
{
  let mut blocks: u32;
  if len.wrapping_add(8u32).wrapping_add(1u32) <= 64u32 { blocks = 1u32 } else { blocks = 2u32 };
  let fin: u32 = blocks.wrapping_mul(64u32);
  let mut last: [u8; 128] = [0u8; 128usize];
  let mut totlen_buf: [u8; 8] = [0u8; 8usize];
  let total_len_bits: u64 = totlen.wrapping_shl(3u32);
  crate::lowstar_endianness::store64_be(&mut totlen_buf, total_len_bits);
  let b0: (&[u8], &[u8]) = b.split_at(0usize);
  (last[0usize..len as usize]).copy_from_slice(&b0.1[0usize..len as usize]);
  last[len as usize] = 128u8;
  ((&mut last[(fin as usize).wrapping_sub(8usize)..])[0usize..8usize]).copy_from_slice(
    &totlen_buf[0usize..8usize]
  );
  let last00: (&[u8], &[u8]) = last.split_at(0usize);
  let last10: (&[u8], &[u8]) = (last00.1).split_at(64usize);
  let l0: (&[u8], &[u8]) = (last10.0).split_at(0usize);
  let l1: (&[u8], &[u8]) = (last10.1).split_at(0usize);
  let lb0: (&[u8], &[u8]) = (l0.1).split_at(0usize);
  let lb1: (&[u8], &[u8]) = (l1.1).split_at(0usize);
  let last0: (&[u8], &[u8]) = (lb0.1).split_at(0usize);
  let last1: (&[u8], &[u8]) = (lb1.1).split_at(0usize);
  sha256_update(last0.1, hash);
  if blocks > 1u32
  {
    sha256_update(last1.1, hash);
    return ()
  }
}

pub fn Hacl_Hash_SHA2_sha256_update_nblocks(len: u32, b: &[u8], st: &mut [u32])
{
  let blocks: u32 = len.wrapping_div(64u32);
  for i in 0u32..blocks
  {
    let b0: (&[u8], &[u8]) = b.split_at(0usize);
    let mb: (&[u8], &[u8]) = (b0.1).split_at(i.wrapping_mul(64u32) as usize);
    sha256_update(mb.1, st)
  }
}

pub fn Hacl_Hash_SHA2_sha384_finish(st: &[u64], h: &mut [u8])
{
  let mut hbuf: [u8; 64] = [0u8; 64usize];
  for i in 0u32..8u32
  {
    crate::lowstar_endianness::store64_be(
      &mut hbuf[i.wrapping_mul(8u32) as usize..],
      st[i as usize]
    )
  };
  (h[0usize..48usize]).copy_from_slice(&hbuf[0usize..48usize])
}

pub fn Hacl_Hash_SHA2_sha384_init(hash: &mut [u64])
{
  for i in 0u32..8u32
  {
    let x: u64 = Hacl_Hash_SHA2_h384[i as usize];
    let os: (&mut [u64], &mut [u64]) = hash.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn Hacl_Hash_SHA2_sha384_update_last(
  totlen: crate::types::FStar_UInt128_uint128,
  len: u32,
  b: &[u8],
  st: &mut [u64]
)
{ Hacl_Hash_SHA2_sha512_update_last(totlen, len, b, st) }

pub fn Hacl_Hash_SHA2_sha384_update_nblocks(len: u32, b: &[u8], st: &mut [u64])
{ Hacl_Hash_SHA2_sha512_update_nblocks(len, b, st) }

pub fn Hacl_Hash_SHA2_sha512_finish(st: &[u64], h: &mut [u8])
{
  let mut hbuf: [u8; 64] = [0u8; 64usize];
  for i in 0u32..8u32
  {
    crate::lowstar_endianness::store64_be(
      &mut hbuf[i.wrapping_mul(8u32) as usize..],
      st[i as usize]
    )
  };
  (h[0usize..64usize]).copy_from_slice(&hbuf[0usize..64usize])
}

pub fn Hacl_Hash_SHA2_sha512_init(hash: &mut [u64])
{
  for i in 0u32..8u32
  {
    let x: u64 = Hacl_Hash_SHA2_h512[i as usize];
    let os: (&mut [u64], &mut [u64]) = hash.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

pub fn Hacl_Hash_SHA2_sha512_update_last(
  totlen: crate::types::FStar_UInt128_uint128,
  len: u32,
  b: &[u8],
  hash: &mut [u64]
)
{
  let mut blocks: u32;
  if len.wrapping_add(16u32).wrapping_add(1u32) <= 128u32
  { blocks = 1u32 }
  else
  { blocks = 2u32 };
  let fin: u32 = blocks.wrapping_mul(128u32);
  let mut last: [u8; 256] = [0u8; 256usize];
  let mut totlen_buf: [u8; 16] = [0u8; 16usize];
  let total_len_bits: crate::types::FStar_UInt128_uint128 =
      crate::hacl_krmllib::FStar_UInt128_shift_left(totlen, 3u32);
  crate::hacl_krmllib::store128_be(&mut totlen_buf, total_len_bits);
  let b0: (&[u8], &[u8]) = b.split_at(0usize);
  (last[0usize..len as usize]).copy_from_slice(&b0.1[0usize..len as usize]);
  last[len as usize] = 128u8;
  ((&mut last[(fin as usize).wrapping_sub(16usize)..])[0usize..16usize]).copy_from_slice(
    &totlen_buf[0usize..16usize]
  );
  let last00: (&[u8], &[u8]) = last.split_at(0usize);
  let last10: (&[u8], &[u8]) = (last00.1).split_at(128usize);
  let l0: (&[u8], &[u8]) = (last10.0).split_at(0usize);
  let l1: (&[u8], &[u8]) = (last10.1).split_at(0usize);
  let lb0: (&[u8], &[u8]) = (l0.1).split_at(0usize);
  let lb1: (&[u8], &[u8]) = (l1.1).split_at(0usize);
  let last0: (&[u8], &[u8]) = (lb0.1).split_at(0usize);
  let last1: (&[u8], &[u8]) = (lb1.1).split_at(0usize);
  sha512_update(last0.1, hash);
  if blocks > 1u32
  {
    sha512_update(last1.1, hash);
    return ()
  }
}

pub fn Hacl_Hash_SHA2_sha512_update_nblocks(len: u32, b: &[u8], st: &mut [u64])
{
  let blocks: u32 = len.wrapping_div(128u32);
  for i in 0u32..blocks
  {
    let b0: (&[u8], &[u8]) = b.split_at(0usize);
    let mb: (&[u8], &[u8]) = (b0.1).split_at(i.wrapping_mul(128u32) as usize);
    sha512_update(mb.1, st)
  }
}

pub fn Hacl_Hash_SHA2_update_224(
  state: &mut [crate::hacl::streaming_types::Hacl_Streaming_MD_state_32],
  input: &[u8],
  input_len: u32
) ->
    u8
{ return update_224_256(state, input, input_len) }

pub fn Hacl_Hash_SHA2_update_256(
  state: &mut [crate::hacl::streaming_types::Hacl_Streaming_MD_state_32],
  input: &[u8],
  input_len: u32
) ->
    u8
{ return update_224_256(state, input, input_len) }

pub fn Hacl_Hash_SHA2_update_384(
  state: &mut [crate::hacl::streaming_types::Hacl_Streaming_MD_state_64],
  input: &[u8],
  input_len: u32
) ->
    u8
{ return update_384_512(state, input, input_len) }

pub fn Hacl_Hash_SHA2_update_512(
  state: &mut [crate::hacl::streaming_types::Hacl_Streaming_MD_state_64],
  input: &[u8],
  input_len: u32
) ->
    u8
{ return update_384_512(state, input, input_len) }

#[inline] pub fn sha256_update(b: &[u8], hash: &mut [u32])
{
  let mut hash_old: [u32; 8] = [0u32; 8usize];
  let mut ws: [u32; 16] = [0u32; 16usize];
  (hash_old[0usize..8usize]).copy_from_slice(&hash[0usize..8usize]);
  let b10: (&[u8], &[u8]) = b.split_at(0usize);
  let u: u32 = crate::lowstar_endianness::load32_be(b10.1);
  ws[0usize] = u;
  let u0: u32 = crate::lowstar_endianness::load32_be(&b10.1[4usize..]);
  ws[1usize] = u0;
  let u1: u32 = crate::lowstar_endianness::load32_be(&b10.1[8usize..]);
  ws[2usize] = u1;
  let u2: u32 = crate::lowstar_endianness::load32_be(&b10.1[12usize..]);
  ws[3usize] = u2;
  let u3: u32 = crate::lowstar_endianness::load32_be(&b10.1[16usize..]);
  ws[4usize] = u3;
  let u4: u32 = crate::lowstar_endianness::load32_be(&b10.1[20usize..]);
  ws[5usize] = u4;
  let u5: u32 = crate::lowstar_endianness::load32_be(&b10.1[24usize..]);
  ws[6usize] = u5;
  let u6: u32 = crate::lowstar_endianness::load32_be(&b10.1[28usize..]);
  ws[7usize] = u6;
  let u7: u32 = crate::lowstar_endianness::load32_be(&b10.1[32usize..]);
  ws[8usize] = u7;
  let u8: u32 = crate::lowstar_endianness::load32_be(&b10.1[36usize..]);
  ws[9usize] = u8;
  let u9: u32 = crate::lowstar_endianness::load32_be(&b10.1[40usize..]);
  ws[10usize] = u9;
  let u10: u32 = crate::lowstar_endianness::load32_be(&b10.1[44usize..]);
  ws[11usize] = u10;
  let u11: u32 = crate::lowstar_endianness::load32_be(&b10.1[48usize..]);
  ws[12usize] = u11;
  let u12: u32 = crate::lowstar_endianness::load32_be(&b10.1[52usize..]);
  ws[13usize] = u12;
  let u13: u32 = crate::lowstar_endianness::load32_be(&b10.1[56usize..]);
  ws[14usize] = u13;
  let u14: u32 = crate::lowstar_endianness::load32_be(&b10.1[60usize..]);
  ws[15usize] = u14;
  for i0 in 0u32..4u32
  {
    for i in 0u32..16u32
    {
      let k_t: u32 = Hacl_Hash_SHA2_k224_256[16u32.wrapping_mul(i0).wrapping_add(i) as usize];
      let ws_t: u32 = ws[i as usize];
      let a0: u32 = hash[0usize];
      let b0: u32 = hash[1usize];
      let c0: u32 = hash[2usize];
      let d0: u32 = hash[3usize];
      let e0: u32 = hash[4usize];
      let f0: u32 = hash[5usize];
      let g0: u32 = hash[6usize];
      let h02: u32 = hash[7usize];
      let k_e_t: u32 = k_t;
      let t1: u32 =
          h02.wrapping_add(
            (e0.wrapping_shl(26u32) | e0.wrapping_shr(6u32))
            ^
            ((e0.wrapping_shl(21u32) | e0.wrapping_shr(11u32))
            ^
            (e0.wrapping_shl(7u32) | e0.wrapping_shr(25u32)))
          ).wrapping_add(e0 & f0 ^ ! e0 & g0).wrapping_add(k_e_t).wrapping_add(ws_t);
      let t2: u32 =
          ((a0.wrapping_shl(30u32) | a0.wrapping_shr(2u32))
          ^
          ((a0.wrapping_shl(19u32) | a0.wrapping_shr(13u32))
          ^
          (a0.wrapping_shl(10u32) | a0.wrapping_shr(22u32)))).wrapping_add(
            a0 & b0 ^ (a0 & c0 ^ b0 & c0)
          );
      let a1: u32 = t1.wrapping_add(t2);
      let b1: u32 = a0;
      let c1: u32 = b0;
      let d1: u32 = c0;
      let e1: u32 = d0.wrapping_add(t1);
      let f1: u32 = e0;
      let g1: u32 = f0;
      let h12: u32 = g0;
      hash[0usize] = a1;
      hash[1usize] = b1;
      hash[2usize] = c1;
      hash[3usize] = d1;
      hash[4usize] = e1;
      hash[5usize] = f1;
      hash[6usize] = g1;
      hash[7usize] = h12
    };
    if i0 < 3u32
    {
      for i in 0u32..16u32
      {
        let t16: u32 = ws[i as usize];
        let t15: u32 = ws[i.wrapping_add(1u32).wrapping_rem(16u32) as usize];
        let t7: u32 = ws[i.wrapping_add(9u32).wrapping_rem(16u32) as usize];
        let t2: u32 = ws[i.wrapping_add(14u32).wrapping_rem(16u32) as usize];
        let s1: u32 =
            (t2.wrapping_shl(15u32) | t2.wrapping_shr(17u32))
            ^
            ((t2.wrapping_shl(13u32) | t2.wrapping_shr(19u32)) ^ t2.wrapping_shr(10u32));
        let s0: u32 =
            (t15.wrapping_shl(25u32) | t15.wrapping_shr(7u32))
            ^
            ((t15.wrapping_shl(14u32) | t15.wrapping_shr(18u32)) ^ t15.wrapping_shr(3u32));
        ws[i as usize] = s1.wrapping_add(t7).wrapping_add(s0).wrapping_add(t16)
      }
    }
  };
  for i in 0u32..8u32
  {
    let x: u32 = (hash[i as usize]).wrapping_add(hash_old[i as usize]);
    let os: (&mut [u32], &mut [u32]) = hash.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

#[inline] pub fn sha512_update(b: &[u8], hash: &mut [u64])
{
  let mut hash_old: [u64; 8] = [0u64; 8usize];
  let mut ws: [u64; 16] = [0u64; 16usize];
  (hash_old[0usize..8usize]).copy_from_slice(&hash[0usize..8usize]);
  let b10: (&[u8], &[u8]) = b.split_at(0usize);
  let u: u64 = crate::lowstar_endianness::load64_be(b10.1);
  ws[0usize] = u;
  let u0: u64 = crate::lowstar_endianness::load64_be(&b10.1[8usize..]);
  ws[1usize] = u0;
  let u1: u64 = crate::lowstar_endianness::load64_be(&b10.1[16usize..]);
  ws[2usize] = u1;
  let u2: u64 = crate::lowstar_endianness::load64_be(&b10.1[24usize..]);
  ws[3usize] = u2;
  let u3: u64 = crate::lowstar_endianness::load64_be(&b10.1[32usize..]);
  ws[4usize] = u3;
  let u4: u64 = crate::lowstar_endianness::load64_be(&b10.1[40usize..]);
  ws[5usize] = u4;
  let u5: u64 = crate::lowstar_endianness::load64_be(&b10.1[48usize..]);
  ws[6usize] = u5;
  let u6: u64 = crate::lowstar_endianness::load64_be(&b10.1[56usize..]);
  ws[7usize] = u6;
  let u7: u64 = crate::lowstar_endianness::load64_be(&b10.1[64usize..]);
  ws[8usize] = u7;
  let u8: u64 = crate::lowstar_endianness::load64_be(&b10.1[72usize..]);
  ws[9usize] = u8;
  let u9: u64 = crate::lowstar_endianness::load64_be(&b10.1[80usize..]);
  ws[10usize] = u9;
  let u10: u64 = crate::lowstar_endianness::load64_be(&b10.1[88usize..]);
  ws[11usize] = u10;
  let u11: u64 = crate::lowstar_endianness::load64_be(&b10.1[96usize..]);
  ws[12usize] = u11;
  let u12: u64 = crate::lowstar_endianness::load64_be(&b10.1[104usize..]);
  ws[13usize] = u12;
  let u13: u64 = crate::lowstar_endianness::load64_be(&b10.1[112usize..]);
  ws[14usize] = u13;
  let u14: u64 = crate::lowstar_endianness::load64_be(&b10.1[120usize..]);
  ws[15usize] = u14;
  for i0 in 0u32..5u32
  {
    for i in 0u32..16u32
    {
      let k_t: u64 = Hacl_Hash_SHA2_k384_512[16u32.wrapping_mul(i0).wrapping_add(i) as usize];
      let ws_t: u64 = ws[i as usize];
      let a0: u64 = hash[0usize];
      let b0: u64 = hash[1usize];
      let c0: u64 = hash[2usize];
      let d0: u64 = hash[3usize];
      let e0: u64 = hash[4usize];
      let f0: u64 = hash[5usize];
      let g0: u64 = hash[6usize];
      let h02: u64 = hash[7usize];
      let k_e_t: u64 = k_t;
      let t1: u64 =
          h02.wrapping_add(
            (e0.wrapping_shl(50u32) | e0.wrapping_shr(14u32))
            ^
            ((e0.wrapping_shl(46u32) | e0.wrapping_shr(18u32))
            ^
            (e0.wrapping_shl(23u32) | e0.wrapping_shr(41u32)))
          ).wrapping_add(e0 & f0 ^ ! e0 & g0).wrapping_add(k_e_t).wrapping_add(ws_t);
      let t2: u64 =
          ((a0.wrapping_shl(36u32) | a0.wrapping_shr(28u32))
          ^
          ((a0.wrapping_shl(30u32) | a0.wrapping_shr(34u32))
          ^
          (a0.wrapping_shl(25u32) | a0.wrapping_shr(39u32)))).wrapping_add(
            a0 & b0 ^ (a0 & c0 ^ b0 & c0)
          );
      let a1: u64 = t1.wrapping_add(t2);
      let b1: u64 = a0;
      let c1: u64 = b0;
      let d1: u64 = c0;
      let e1: u64 = d0.wrapping_add(t1);
      let f1: u64 = e0;
      let g1: u64 = f0;
      let h12: u64 = g0;
      hash[0usize] = a1;
      hash[1usize] = b1;
      hash[2usize] = c1;
      hash[3usize] = d1;
      hash[4usize] = e1;
      hash[5usize] = f1;
      hash[6usize] = g1;
      hash[7usize] = h12
    };
    if i0 < 4u32
    {
      for i in 0u32..16u32
      {
        let t16: u64 = ws[i as usize];
        let t15: u64 = ws[i.wrapping_add(1u32).wrapping_rem(16u32) as usize];
        let t7: u64 = ws[i.wrapping_add(9u32).wrapping_rem(16u32) as usize];
        let t2: u64 = ws[i.wrapping_add(14u32).wrapping_rem(16u32) as usize];
        let s1: u64 =
            (t2.wrapping_shl(45u32) | t2.wrapping_shr(19u32))
            ^
            ((t2.wrapping_shl(3u32) | t2.wrapping_shr(61u32)) ^ t2.wrapping_shr(6u32));
        let s0: u64 =
            (t15.wrapping_shl(63u32) | t15.wrapping_shr(1u32))
            ^
            ((t15.wrapping_shl(56u32) | t15.wrapping_shr(8u32)) ^ t15.wrapping_shr(7u32));
        ws[i as usize] = s1.wrapping_add(t7).wrapping_add(s0).wrapping_add(t16)
      }
    }
  };
  for i in 0u32..8u32
  {
    let x: u64 = (hash[i as usize]).wrapping_add(hash_old[i as usize]);
    let os: (&mut [u64], &mut [u64]) = hash.split_at_mut(0usize);
    os.1[i as usize] = x
  }
}

#[inline] pub fn update_224_256(
  state: &mut [crate::hacl::streaming_types::Hacl_Streaming_MD_state_32],
  chunk: &[u8],
  chunk_len: u32
) ->
    u8
{
  let block_state: &mut [u32] = &mut (state[0usize]).block_state;
  let total_len: u64 = (state[0usize]).total_len;
  if chunk_len as u64 > 2305843009213693951u64.wrapping_sub(total_len) { return 3u8 };
  let mut sz: u32;
  if total_len.wrapping_rem(64u32 as u64) == 0u64 && total_len > 0u64
  { sz = 64u32 }
  else
  { sz = total_len.wrapping_rem(64u32 as u64) as u32 };
  if chunk_len <= 64u32.wrapping_sub(sz)
  {
    let buf: &mut [u8] = &mut (state[0usize]).buf;
    let total_len1: u64 = (state[0usize]).total_len;
    let mut sz1: u32;
    if total_len1.wrapping_rem(64u32 as u64) == 0u64 && total_len1 > 0u64
    { sz1 = 64u32 }
    else
    { sz1 = total_len1.wrapping_rem(64u32 as u64) as u32 };
    let buf2: (&mut [u8], &mut [u8]) = buf.split_at_mut(sz1 as usize);
    (buf2.1[0usize..chunk_len as usize]).copy_from_slice(&chunk[0usize..chunk_len as usize]);
    let total_len2: u64 = total_len1.wrapping_add(chunk_len as u64);
    (state[0usize]).total_len = total_len2
  }
  else if sz == 0u32
  {
    let buf: &mut [u8] = &mut (state[0usize]).buf;
    let total_len1: u64 = (state[0usize]).total_len;
    let mut sz1: u32;
    if total_len1.wrapping_rem(64u32 as u64) == 0u64 && total_len1 > 0u64
    { sz1 = 64u32 }
    else
    { sz1 = total_len1.wrapping_rem(64u32 as u64) as u32 };
    if sz1 != 0u32 { Hacl_Hash_SHA2_sha256_update_nblocks(64u32, buf, block_state) };
    let mut ite: u32;
    if (chunk_len as u64).wrapping_rem(64u32 as u64) == 0u64 && chunk_len as u64 > 0u64
    { ite = 64u32 }
    else
    { ite = (chunk_len as u64).wrapping_rem(64u32 as u64) as u32 };
    let n_blocks: u32 = chunk_len.wrapping_sub(ite).wrapping_div(64u32);
    let data1_len: u32 = n_blocks.wrapping_mul(64u32);
    let data2_len: u32 = chunk_len.wrapping_sub(data1_len);
    let data1: (&[u8], &[u8]) = chunk.split_at(0usize);
    let data2: (&[u8], &[u8]) = (data1.1).split_at(data1_len as usize);
    Hacl_Hash_SHA2_sha256_update_nblocks(
      data1_len.wrapping_div(64u32).wrapping_mul(64u32),
      data2.0,
      block_state
    );
    let dst: (&mut [u8], &mut [u8]) = buf.split_at_mut(0usize);
    (dst.1[0usize..data2_len as usize]).copy_from_slice(&data2.1[0usize..data2_len as usize]);
    (state[0usize]).total_len = total_len1.wrapping_add(chunk_len as u64)
  }
  else
  {
    let diff: u32 = 64u32.wrapping_sub(sz);
    let chunk1: (&[u8], &[u8]) = chunk.split_at(0usize);
    let chunk2: (&[u8], &[u8]) = (chunk1.1).split_at(diff as usize);
    let buf: &mut [u8] = &mut (state[0usize]).buf;
    let total_len10: u64 = (state[0usize]).total_len;
    let mut sz10: u32;
    if total_len10.wrapping_rem(64u32 as u64) == 0u64 && total_len10 > 0u64
    { sz10 = 64u32 }
    else
    { sz10 = total_len10.wrapping_rem(64u32 as u64) as u32 };
    let buf2: (&mut [u8], &mut [u8]) = buf.split_at_mut(sz10 as usize);
    (buf2.1[0usize..diff as usize]).copy_from_slice(&chunk2.0[0usize..diff as usize]);
    let total_len2: u64 = total_len10.wrapping_add(diff as u64);
    (state[0usize]).total_len = total_len2;
    let buf0: &mut [u8] = &mut (state[0usize]).buf;
    let total_len1: u64 = (state[0usize]).total_len;
    let mut sz1: u32;
    if total_len1.wrapping_rem(64u32 as u64) == 0u64 && total_len1 > 0u64
    { sz1 = 64u32 }
    else
    { sz1 = total_len1.wrapping_rem(64u32 as u64) as u32 };
    if sz1 != 0u32 { Hacl_Hash_SHA2_sha256_update_nblocks(64u32, buf0, block_state) };
    let mut ite: u32;
    if
    (chunk_len.wrapping_sub(diff) as u64).wrapping_rem(64u32 as u64) == 0u64
    &&
    chunk_len.wrapping_sub(diff) as u64 > 0u64
    { ite = 64u32 }
    else
    { ite = (chunk_len.wrapping_sub(diff) as u64).wrapping_rem(64u32 as u64) as u32 };
    let n_blocks: u32 = chunk_len.wrapping_sub(diff).wrapping_sub(ite).wrapping_div(64u32);
    let data1_len: u32 = n_blocks.wrapping_mul(64u32);
    let data2_len: u32 = chunk_len.wrapping_sub(diff).wrapping_sub(data1_len);
    let data1: (&[u8], &[u8]) = (chunk2.1).split_at(0usize);
    let data2: (&[u8], &[u8]) = (data1.1).split_at(data1_len as usize);
    Hacl_Hash_SHA2_sha256_update_nblocks(
      data1_len.wrapping_div(64u32).wrapping_mul(64u32),
      data2.0,
      block_state
    );
    let dst: (&mut [u8], &mut [u8]) = buf0.split_at_mut(0usize);
    (dst.1[0usize..data2_len as usize]).copy_from_slice(&data2.1[0usize..data2_len as usize]);
    (state[0usize]).total_len = total_len1.wrapping_add(chunk_len.wrapping_sub(diff) as u64)
  };
  return 0u8
}

#[inline] pub fn update_384_512(
  state: &mut [crate::hacl::streaming_types::Hacl_Streaming_MD_state_64],
  chunk: &[u8],
  chunk_len: u32
) ->
    u8
{
  let block_state: &mut [u64] = &mut (state[0usize]).block_state;
  let total_len: u64 = (state[0usize]).total_len;
  if chunk_len as u64 > 18446744073709551615u64.wrapping_sub(total_len) { return 3u8 };
  let mut sz: u32;
  if total_len.wrapping_rem(128u32 as u64) == 0u64 && total_len > 0u64
  { sz = 128u32 }
  else
  { sz = total_len.wrapping_rem(128u32 as u64) as u32 };
  if chunk_len <= 128u32.wrapping_sub(sz)
  {
    let buf: &mut [u8] = &mut (state[0usize]).buf;
    let total_len1: u64 = (state[0usize]).total_len;
    let mut sz1: u32;
    if total_len1.wrapping_rem(128u32 as u64) == 0u64 && total_len1 > 0u64
    { sz1 = 128u32 }
    else
    { sz1 = total_len1.wrapping_rem(128u32 as u64) as u32 };
    let buf2: (&mut [u8], &mut [u8]) = buf.split_at_mut(sz1 as usize);
    (buf2.1[0usize..chunk_len as usize]).copy_from_slice(&chunk[0usize..chunk_len as usize]);
    let total_len2: u64 = total_len1.wrapping_add(chunk_len as u64);
    (state[0usize]).total_len = total_len2
  }
  else if sz == 0u32
  {
    let buf: &mut [u8] = &mut (state[0usize]).buf;
    let total_len1: u64 = (state[0usize]).total_len;
    let mut sz1: u32;
    if total_len1.wrapping_rem(128u32 as u64) == 0u64 && total_len1 > 0u64
    { sz1 = 128u32 }
    else
    { sz1 = total_len1.wrapping_rem(128u32 as u64) as u32 };
    if sz1 != 0u32 { Hacl_Hash_SHA2_sha512_update_nblocks(128u32, buf, block_state) };
    let mut ite: u32;
    if (chunk_len as u64).wrapping_rem(128u32 as u64) == 0u64 && chunk_len as u64 > 0u64
    { ite = 128u32 }
    else
    { ite = (chunk_len as u64).wrapping_rem(128u32 as u64) as u32 };
    let n_blocks: u32 = chunk_len.wrapping_sub(ite).wrapping_div(128u32);
    let data1_len: u32 = n_blocks.wrapping_mul(128u32);
    let data2_len: u32 = chunk_len.wrapping_sub(data1_len);
    let data1: (&[u8], &[u8]) = chunk.split_at(0usize);
    let data2: (&[u8], &[u8]) = (data1.1).split_at(data1_len as usize);
    Hacl_Hash_SHA2_sha512_update_nblocks(
      data1_len.wrapping_div(128u32).wrapping_mul(128u32),
      data2.0,
      block_state
    );
    let dst: (&mut [u8], &mut [u8]) = buf.split_at_mut(0usize);
    (dst.1[0usize..data2_len as usize]).copy_from_slice(&data2.1[0usize..data2_len as usize]);
    (state[0usize]).total_len = total_len1.wrapping_add(chunk_len as u64)
  }
  else
  {
    let diff: u32 = 128u32.wrapping_sub(sz);
    let chunk1: (&[u8], &[u8]) = chunk.split_at(0usize);
    let chunk2: (&[u8], &[u8]) = (chunk1.1).split_at(diff as usize);
    let buf: &mut [u8] = &mut (state[0usize]).buf;
    let total_len10: u64 = (state[0usize]).total_len;
    let mut sz10: u32;
    if total_len10.wrapping_rem(128u32 as u64) == 0u64 && total_len10 > 0u64
    { sz10 = 128u32 }
    else
    { sz10 = total_len10.wrapping_rem(128u32 as u64) as u32 };
    let buf2: (&mut [u8], &mut [u8]) = buf.split_at_mut(sz10 as usize);
    (buf2.1[0usize..diff as usize]).copy_from_slice(&chunk2.0[0usize..diff as usize]);
    let total_len2: u64 = total_len10.wrapping_add(diff as u64);
    (state[0usize]).total_len = total_len2;
    let buf0: &mut [u8] = &mut (state[0usize]).buf;
    let total_len1: u64 = (state[0usize]).total_len;
    let mut sz1: u32;
    if total_len1.wrapping_rem(128u32 as u64) == 0u64 && total_len1 > 0u64
    { sz1 = 128u32 }
    else
    { sz1 = total_len1.wrapping_rem(128u32 as u64) as u32 };
    if sz1 != 0u32 { Hacl_Hash_SHA2_sha512_update_nblocks(128u32, buf0, block_state) };
    let mut ite: u32;
    if
    (chunk_len.wrapping_sub(diff) as u64).wrapping_rem(128u32 as u64) == 0u64
    &&
    chunk_len.wrapping_sub(diff) as u64 > 0u64
    { ite = 128u32 }
    else
    { ite = (chunk_len.wrapping_sub(diff) as u64).wrapping_rem(128u32 as u64) as u32 };
    let n_blocks: u32 = chunk_len.wrapping_sub(diff).wrapping_sub(ite).wrapping_div(128u32);
    let data1_len: u32 = n_blocks.wrapping_mul(128u32);
    let data2_len: u32 = chunk_len.wrapping_sub(diff).wrapping_sub(data1_len);
    let data1: (&[u8], &[u8]) = (chunk2.1).split_at(0usize);
    let data2: (&[u8], &[u8]) = (data1.1).split_at(data1_len as usize);
    Hacl_Hash_SHA2_sha512_update_nblocks(
      data1_len.wrapping_div(128u32).wrapping_mul(128u32),
      data2.0,
      block_state
    );
    let dst: (&mut [u8], &mut [u8]) = buf0.split_at_mut(0usize);
    (dst.1[0usize..data2_len as usize]).copy_from_slice(&data2.1[0usize..data2_len as usize]);
    (state[0usize]).total_len = total_len1.wrapping_add(chunk_len.wrapping_sub(diff) as u64)
  };
  return 0u8
}
