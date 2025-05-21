#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

pub fn CBOR_Pulse_Raw_Format_Serialize_ser_(x_: cbor_raw, out: &mut [u8], offset: usize) ->
    usize
{
  let res0: header = cbor_raw_with_perm_get_header(x_);
  let xh1: header = res0;
  let res1: usize = write_header(xh1, out, offset);
  let b0: initial_byte_t = xh1.fst;
  let mut res2: usize;
  if b0.major_type as u32 == 2u32 || b0.major_type as u32 == 3u32
  {
    let scrut: cbor_raw = x_;
    let mut c_: cbor_string =
        cbor_string { cbor_string_type: 0u8, cbor_string_size: 0u8, cbor_string_ptr: &[] };
    match scrut { cbor_raw::case_CBOR_Case_String { v } => c_ = v, _ => panic!("") };
    let s: &[u8] = c_.cbor_string_ptr;
    let x2_: &[u8] = s;
    let length: usize = len__uint8_t(x2_);
    let sp1: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        split__uint8_t(out, res1);
    let sp12: &[u8] = &sp1.snd;
    let mut sp2: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        split__uint8_t(sp12, length);
    let sp21: &mut [u8] = &mut sp2.fst;
    copy__uint8_t(sp21, x2_);
    let res: usize = res1.wrapping_add(length);
    let res00: usize = res;
    res2 = res00
  }
  else
  {
    let b: initial_byte_t = xh1.fst;
    if b.major_type as u32 == 4u32
    {
      let mut ite: bool;
      match x_ { cbor_raw::case_CBOR_Case_Array { v } => ite = true, _ => ite = false };
      if ite
      {
        let x2_: cbor_raw = x_;
        let scrut0: cbor_raw = x2_;
        let mut scrut: option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_ =
            option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_::cbor_map_None;
        match scrut0
        {
          cbor_raw::case_CBOR_Case_Array { v } =>
            {
              let a: cbor_array = v;
              scrut =
                  option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_::v
                  { v: a.cbor_array_ptr }
            },
          _ =>
            scrut =
                option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_::cbor_map_None
        };
        let mut a: &[cbor_raw] = &[];
        match scrut
        {
          option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_::v { v } => a = v,
          _ => panic!("")
        };
        let mut pres: usize = res1;
        let mut pi: usize = 0u32 as usize;
        let i0: usize = pi;
        let mut cond: bool = i0 < argument_as_uint64(xh1.fst, xh1.snd) as usize;
        while
        cond
        {
          let i: usize = pi;
          let off: usize = pres;
          let e: cbor_raw = a[i];
          let i_: usize = i.wrapping_add(1u32 as usize);
          let x2_1: cbor_raw = e;
          let res: usize = CBOR_Pulse_Raw_Format_Serialize_ser_(x2_1, out, off);
          let res00: usize = res;
          let res10: usize = res00;
          pi = i_;
          pres = res10;
          let i00: usize = pi;
          cond = i00 < argument_as_uint64(xh1.fst, xh1.snd) as usize
        };
        let res: usize = pres;
        let res00: usize = res;
        let res10: usize = res00;
        res2 = res10
      }
      else
      {
        let scrut: cbor_raw = x_;
        let mut xs: cbor_serialized =
            cbor_serialized
            {
              cbor_serialized_header: CBOR_Spec_Raw_Base_raw_uint64 { size: 0u8, value: 0u64 },
              cbor_serialized_payload: &[]
            };
        match scrut { cbor_raw::case_CBOR_Case_Serialized_Array { v } => xs = v, _ => panic!("") };
        let x2_: &[u8] = xs.cbor_serialized_payload;
        let length: usize = len__uint8_t(x2_);
        let sp1: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
            split__uint8_t(out, res1);
        let sp12: &[u8] = &sp1.snd;
        let mut sp2: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
            split__uint8_t(sp12, length);
        let sp21: &mut [u8] = &mut sp2.fst;
        copy__uint8_t(sp21, x2_);
        let res: usize = res1.wrapping_add(length);
        let res00: usize = res;
        let res10: usize = res00;
        res2 = res10
      }
    }
    else
    {
      let b1: initial_byte_t = xh1.fst;
      if b1.major_type as u32 == 5u32
      {
        let mut ite: bool;
        match x_ { cbor_raw::case_CBOR_Case_Map { v } => ite = true, _ => ite = false };
        if ite
        {
          let x2_: cbor_raw = x_;
          let scrut0: cbor_raw = x2_;
          let
          mut scrut: option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_
          =
              option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_::cbor_map_entry_None;
          match scrut0
          {
            cbor_raw::case_CBOR_Case_Map { v } =>
              {
                let a: cbor_map = v;
                scrut =
                    option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_::v
                    { v: a.cbor_map_ptr }
              },
            _ =>
              scrut =
                  option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_::cbor_map_entry_None
          };
          let mut a: &[cbor_map_entry] = &[];
          match scrut
          {
            option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_::v { v } =>
              a = v,
            _ => panic!("")
          };
          let mut pres: usize = res1;
          let mut pi: usize = 0u32 as usize;
          let i0: usize = pi;
          let mut cond: bool = i0 < argument_as_uint64(xh1.fst, xh1.snd) as usize;
          while
          cond
          {
            let i: usize = pi;
            let off: usize = pres;
            let e: cbor_map_entry = a[i];
            let i_: usize = i.wrapping_add(1u32 as usize);
            let x11: cbor_raw = e.cbor_map_entry_key;
            let res: usize = CBOR_Pulse_Raw_Format_Serialize_ser_(x11, out, off);
            let res11: usize = res;
            let x2: cbor_raw = e.cbor_map_entry_value;
            let res00: usize = CBOR_Pulse_Raw_Format_Serialize_ser_(x2, out, res11);
            let res20: usize = res00;
            let res10: usize = res20;
            pi = i_;
            pres = res10;
            let i00: usize = pi;
            cond = i00 < argument_as_uint64(xh1.fst, xh1.snd) as usize
          };
          let res: usize = pres;
          let res00: usize = res;
          let res10: usize = res00;
          res2 = res10
        }
        else
        {
          let scrut: cbor_raw = x_;
          let mut xs: cbor_serialized =
              cbor_serialized
              {
                cbor_serialized_header: CBOR_Spec_Raw_Base_raw_uint64 { size: 0u8, value: 0u64 },
                cbor_serialized_payload: &[]
              };
          match scrut { cbor_raw::case_CBOR_Case_Serialized_Map { v } => xs = v, _ => panic!("") };
          let x2_: &[u8] = xs.cbor_serialized_payload;
          let length: usize = len__uint8_t(x2_);
          let sp1: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
              split__uint8_t(out, res1);
          let sp12: &[u8] = &sp1.snd;
          let mut sp2: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
              split__uint8_t(sp12, length);
          let sp21: &mut [u8] = &mut sp2.fst;
          copy__uint8_t(sp21, x2_);
          let res: usize = res1.wrapping_add(length);
          let res00: usize = res;
          let res10: usize = res00;
          res2 = res10
        }
      }
      else
      {
        let b2: initial_byte_t = xh1.fst;
        if b2.major_type as u32 == 6u32
        {
          let mut ite: bool;
          match x_ { cbor_raw::case_CBOR_Case_Tagged { v } => ite = true, _ => ite = false };
          let mut res: usize;
          if ite
          {
            let scrut: cbor_raw = x_;
            let mut tg: cbor_tagged =
                cbor_tagged
                {
                  cbor_tagged_tag: CBOR_Spec_Raw_Base_raw_uint64 { size: 0u8, value: 0u64 },
                  cbor_tagged_ptr: &[]
                };
            match scrut { cbor_raw::case_CBOR_Case_Tagged { v } => tg = v, _ => panic!("") };
            let x2_: cbor_raw = tg.cbor_tagged_ptr[0usize];
            let res00: usize = CBOR_Pulse_Raw_Format_Serialize_ser_(x2_, out, res1);
            let res10: usize = res00;
            let res20: usize = res10;
            res = res20
          }
          else
          {
            let scrut: cbor_raw = x_;
            let mut ser: cbor_serialized =
                cbor_serialized
                {
                  cbor_serialized_header: CBOR_Spec_Raw_Base_raw_uint64 { size: 0u8, value: 0u64 },
                  cbor_serialized_payload: &[]
                };
            match scrut
            { cbor_raw::case_CBOR_Case_Serialized_Tagged { v } => ser = v, _ => panic!("") };
            let x2_: &[u8] = ser.cbor_serialized_payload;
            let length: usize = len__uint8_t(x2_);
            let sp1: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
                split__uint8_t(out, res1);
            let sp12: &[u8] = &sp1.snd;
            let mut sp2: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
                split__uint8_t(sp12, length);
            let sp21: &mut [u8] = &mut sp2.fst;
            copy__uint8_t(sp21, x2_);
            let res00: usize = res1.wrapping_add(length);
            let res10: usize = res00;
            res = res10
          };
          res2 = res
        }
        else
        { res2 = res1 }
      }
    }
  };
  let res: usize = res2;
  let res3: usize = res;
  return res3
}

pub fn CBOR_Pulse_Raw_Format_Serialize_siz_(x_: cbor_raw, out: &mut [usize]) -> bool
{
  let res0: header = cbor_raw_with_perm_get_header(x_);
  let xh1: header = res0;
  let res1: bool = size_header(xh1, out);
  let mut res3: bool;
  if res1
  {
    let b0: initial_byte_t = xh1.fst;
    let mut res2: bool;
    if b0.major_type as u32 == 2u32 || b0.major_type as u32 == 3u32
    {
      let scrut: cbor_raw = x_;
      let mut c_: cbor_string =
          cbor_string { cbor_string_type: 0u8, cbor_string_size: 0u8, cbor_string_ptr: &[] };
      match scrut { cbor_raw::case_CBOR_Case_String { v } => c_ = v, _ => panic!("") };
      let s: &[u8] = c_.cbor_string_ptr;
      let x2_: &[u8] = s;
      let length: usize = len__uint8_t(x2_);
      let cur: usize = out[0usize];
      let mut res: bool;
      if cur < length
      { res = false }
      else
      {
        out[0usize] = cur.wrapping_sub(length);
        res = true
      };
      let res00: bool = res;
      res2 = res00
    }
    else
    {
      let b: initial_byte_t = xh1.fst;
      if b.major_type as u32 == 4u32
      {
        let mut ite: bool;
        match x_ { cbor_raw::case_CBOR_Case_Array { v } => ite = true, _ => ite = false };
        if ite
        {
          let x2_: cbor_raw = x_;
          let scrut0: cbor_raw = x2_;
          let mut scrut: option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_ =
              option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_::cbor_map_None;
          match scrut0
          {
            cbor_raw::case_CBOR_Case_Array { v } =>
              {
                let a: cbor_array = v;
                scrut =
                    option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_::v
                    { v: a.cbor_array_ptr }
              },
            _ =>
              scrut =
                  option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_::cbor_map_None
          };
          let mut a: &[cbor_raw] = Default::default();
          match scrut
          {
            option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_::v { v } => a = v,
            _ => panic!("")
          };
          let mut pres: bool = true;
          let mut pi: usize = 0u32 as usize;
          let res: bool = pres;
          let i0: usize = pi;
          let mut cond: bool = res && i0 < argument_as_uint64(xh1.fst, xh1.snd) as usize;
          while
          cond
          {
            let i00: usize = pi;
            let e: cbor_raw = a[i00];
            let x2_1: cbor_raw = e;
            let res4: bool = CBOR_Pulse_Raw_Format_Serialize_siz_(x2_1, out);
            let res00: bool = res4;
            let res10: bool = res00;
            if res10
            {
              let i_: usize = i00.wrapping_add(1u32 as usize);
              pi = i_
            }
            else
            { pres = false };
            let res20: bool = pres;
            let i: usize = pi;
            cond = res20 && i < argument_as_uint64(xh1.fst, xh1.snd) as usize
          };
          let res00: bool = pres;
          let res10: bool = res00;
          let res30: bool = res10;
          res2 = res30
        }
        else
        {
          let scrut: cbor_raw = x_;
          let mut xs: cbor_serialized =
              cbor_serialized
              {
                cbor_serialized_header: CBOR_Spec_Raw_Base_raw_uint64 { size: 0u8, value: 0u64 },
                cbor_serialized_payload: &[]
              };
          match scrut { cbor_raw::case_CBOR_Case_Serialized_Array { v } => xs = v, _ => panic!("") };
          let x2_: &[u8] = xs.cbor_serialized_payload;
          let length: usize = len__uint8_t(x2_);
          let cur: usize = out[0usize];
          let mut res: bool;
          if cur < length
          { res = false }
          else
          {
            out[0usize] = cur.wrapping_sub(length);
            res = true
          };
          let res00: bool = res;
          let res10: bool = res00;
          res2 = res10
        }
      }
      else
      {
        let b1: initial_byte_t = xh1.fst;
        if b1.major_type as u32 == 5u32
        {
          let mut ite: bool;
          match x_ { cbor_raw::case_CBOR_Case_Map { v } => ite = true, _ => ite = false };
          if ite
          {
            let x2_: cbor_raw = x_;
            let scrut0: cbor_raw = x2_;
            let
            mut scrut: option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_
            =
                option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_::cbor_map_entry_None;
            match scrut0
            {
              cbor_raw::case_CBOR_Case_Map { v } =>
                {
                  let a: cbor_map = v;
                  scrut =
                      option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_::v
                      { v: a.cbor_map_ptr }
                },
              _ =>
                scrut =
                    option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_::cbor_map_entry_None
            };
            let mut a: &[cbor_map_entry] = Default::default();
            match scrut
            {
              option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_::v { v } =>
                a = v,
              _ => panic!("")
            };
            let mut pres: bool = true;
            let mut pi: usize = 0u32 as usize;
            let res00: bool = pres;
            let i0: usize = pi;
            let mut cond: bool = res00 && i0 < argument_as_uint64(xh1.fst, xh1.snd) as usize;
            while
            cond
            {
              let i00: usize = pi;
              let e: cbor_map_entry = a[i00];
              let x11: cbor_raw = e.cbor_map_entry_key;
              let res01: bool = CBOR_Pulse_Raw_Format_Serialize_siz_(x11, out);
              let res11: bool = res01;
              let mut res: bool;
              if res11
              {
                let x2: cbor_raw = e.cbor_map_entry_value;
                let res02: bool = CBOR_Pulse_Raw_Format_Serialize_siz_(x2, out);
                let res20: bool = res02;
                res = res20
              }
              else
              { res = false };
              if res
              {
                let i_: usize = i00.wrapping_add(1u32 as usize);
                pi = i_
              }
              else
              { pres = false };
              let res10: bool = pres;
              let i: usize = pi;
              cond = res10 && i < argument_as_uint64(xh1.fst, xh1.snd) as usize
            };
            let res: bool = pres;
            let res10: bool = res;
            let res30: bool = res10;
            res2 = res30
          }
          else
          {
            let scrut: cbor_raw = x_;
            let mut xs: cbor_serialized =
                cbor_serialized
                {
                  cbor_serialized_header: CBOR_Spec_Raw_Base_raw_uint64 { size: 0u8, value: 0u64 },
                  cbor_serialized_payload: &[]
                };
            match scrut { cbor_raw::case_CBOR_Case_Serialized_Map { v } => xs = v, _ => panic!("") };
            let x2_: &[u8] = xs.cbor_serialized_payload;
            let length: usize = len__uint8_t(x2_);
            let cur: usize = out[0usize];
            let mut res: bool;
            if cur < length
            { res = false }
            else
            {
              out[0usize] = cur.wrapping_sub(length);
              res = true
            };
            let res00: bool = res;
            let res10: bool = res00;
            res2 = res10
          }
        }
        else
        {
          let b2: initial_byte_t = xh1.fst;
          if b2.major_type as u32 == 6u32
          {
            let mut ite: bool;
            match x_ { cbor_raw::case_CBOR_Case_Tagged { v } => ite = true, _ => ite = false };
            let mut res00: bool;
            if ite
            {
              let scrut: cbor_raw = x_;
              let mut tg: cbor_tagged =
                  cbor_tagged
                  {
                    cbor_tagged_tag: CBOR_Spec_Raw_Base_raw_uint64 { size: 0u8, value: 0u64 },
                    cbor_tagged_ptr: &[]
                  };
              match scrut { cbor_raw::case_CBOR_Case_Tagged { v } => tg = v, _ => panic!("") };
              let x2_: cbor_raw = tg.cbor_tagged_ptr[0usize];
              let res: bool = CBOR_Pulse_Raw_Format_Serialize_siz_(x2_, out);
              let res10: bool = res;
              let res20: bool = res10;
              res00 = res20
            }
            else
            {
              let scrut: cbor_raw = x_;
              let mut ser1: cbor_serialized =
                  cbor_serialized
                  {
                    cbor_serialized_header: CBOR_Spec_Raw_Base_raw_uint64 { size: 0u8, value: 0u64 },
                    cbor_serialized_payload: &[]
                  };
              match scrut
              { cbor_raw::case_CBOR_Case_Serialized_Tagged { v } => ser1 = v, _ => panic!("") };
              let x2_: &[u8] = ser1.cbor_serialized_payload;
              let length: usize = len__uint8_t(x2_);
              let cur: usize = out[0usize];
              let mut res: bool;
              if cur < length
              { res = false }
              else
              {
                out[0usize] = cur.wrapping_sub(length);
                res = true
              };
              let res10: bool = res;
              res00 = res10
            };
            res2 = res00
          }
          else
          { res2 = true }
        }
      }
    };
    res3 = res2
  }
  else
  { res3 = false };
  let res: bool = res3;
  return res
}

#[derive(PartialEq, Clone, Copy)]
pub enum CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>
{
  case_CBOR_Raw_Iterator_Slice
  { v: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry <'a> },
  case_CBOR_Raw_Iterator_Serialized { v: &'a [u8] }
}

#[derive(PartialEq, Clone, Copy)]
pub enum CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw <'a>
{
  case_CBOR_Raw_Iterator_Slice { v: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw <'a> },
  case_CBOR_Raw_Iterator_Serialized { v: &'a [u8] }
}

pub type CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw_tags = u8;

#[derive(Default, PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct CBOR_Spec_Raw_Base_raw_uint64
{ pub size: u8, pub value: u64 }

#[derive(PartialEq, Clone, Copy)]
pub enum FStar_Pervasives_Native_option__CBOR_Pulse_Raw_Type_cbor_raw <'a>
{
  v { v: cbor_raw <'a> },
  cbor_raw_None
}

pub type
FStar_Pervasives_Native_option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw__tags
=
u8;

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>
{ pub elt: &'a [cbor_map_entry <'a>], pub len: usize }

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw <'a>
{ pub elt: &'a [cbor_raw <'a>], pub len: usize }

pub type Pulse_Lib_Slice_slice__uint8_t <'a> = &'a [u8];

#[derive(PartialEq, Clone)]
#[repr(C)]
pub
struct __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
{ pub fst: Box<[u8]>, pub snd: Box<[u8]> }

pub fn
__proj__Mkdtuple2__item___1__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(
  pair: header
) ->
    initial_byte_t
{ return pair.fst }

pub fn
__proj__Mkdtuple2__item___2__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(
  pair: header
) ->
    long_argument
{ return pair.snd }

pub fn argument_as_uint64(b: initial_byte_t, x: long_argument) -> u64
{
  let mut ite: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
  match x
  {
    long_argument::case_LongArgumentU8 { v } =>
      {
        let v0: u8 = v;
        ite = CBOR_Spec_Raw_Base_raw_uint64 { size: 1u8, value: v0 as u64 }
      },
    _ =>
      match x
      {
        long_argument::case_LongArgumentU16 { v } =>
          {
            let v0: u16 = v;
            ite = CBOR_Spec_Raw_Base_raw_uint64 { size: 2u8, value: v0 as u64 }
          },
        _ =>
          match x
          {
            long_argument::case_LongArgumentU32 { v } =>
              {
                let v0: u32 = v;
                ite = CBOR_Spec_Raw_Base_raw_uint64 { size: 3u8, value: v0 as u64 }
              },
            _ =>
              match x
              {
                long_argument::case_LongArgumentU64 { v } =>
                  {
                    let v0: u64 = v;
                    ite = CBOR_Spec_Raw_Base_raw_uint64 { size: 4u8, value: v0 }
                  },
                _ =>
                  match x
                  {
                    long_argument::case_LongArgumentOther =>
                      ite =
                          CBOR_Spec_Raw_Base_raw_uint64
                          { size: 0u8, value: b.additional_info as u64 },
                    _ => panic!("")
                  }
              }
          }
      }
  };
  return ite.value
}

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct cbor_array <'a>
{
  pub cbor_array_length: CBOR_Spec_Raw_Base_raw_uint64,
  pub cbor_array_ptr: &'a [cbor_raw <'a>]
}

pub type cbor_array_iterator <'a> =
CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw <'a>;

pub type cbor_det_array_iterator_t <'a> =
CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw <'a>;

pub type cbor_det_map_entry_t <'a> = cbor_map_entry <'a>;

pub type cbor_det_map_iterator_t <'a> =
CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>;

pub type cbor_det_t <'a> = cbor_raw <'a>;

#[derive(Default, PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct cbor_int
{ pub cbor_int_type: u8, pub cbor_int_size: u8, pub cbor_int_value: u64 }

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct cbor_map <'a>
{
  pub cbor_map_length: CBOR_Spec_Raw_Base_raw_uint64,
  pub cbor_map_ptr: &'a [cbor_map_entry <'a>]
}

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct cbor_map_entry <'a>
{ pub cbor_map_entry_key: cbor_raw <'a>, pub cbor_map_entry_value: cbor_raw <'a> }

pub type cbor_map_iterator <'a> =
CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>;

#[derive(PartialEq, Clone, Copy)]
pub enum cbor_raw <'a>
{
  case_CBOR_Case_Int { v: cbor_int },
  case_CBOR_Case_Simple { v: u8 },
  case_CBOR_Case_String { v: cbor_string <'a> },
  case_CBOR_Case_Tagged { v: cbor_tagged <'a> },
  case_CBOR_Case_Array { v: cbor_array <'a> },
  case_CBOR_Case_Map { v: cbor_map <'a> },
  case_CBOR_Case_Serialized_Tagged { v: cbor_serialized <'a> },
  case_CBOR_Case_Serialized_Array { v: cbor_serialized <'a> },
  case_CBOR_Case_Serialized_Map { v: cbor_serialized <'a> }
}

pub fn cbor_raw_get_header(xl: cbor_raw) -> header
{
  let mut ite0: bool;
  match xl { cbor_raw::case_CBOR_Case_Int { v } => ite0 = true, _ => ite0 = false };
  if ite0
  {
    let mut c_: cbor_int = Default::default();
    match xl { cbor_raw::case_CBOR_Case_Int { v } => c_ = v, _ => panic!("") };
    let ty: u8 = c_.cbor_int_type;
    let mut c_0: cbor_int = Default::default();
    match xl { cbor_raw::case_CBOR_Case_Int { v } => c_0 = v, _ => panic!("") };
    let v: CBOR_Spec_Raw_Base_raw_uint64 =
        CBOR_Spec_Raw_Base_raw_uint64 { size: c_0.cbor_int_size, value: c_0.cbor_int_value };
    return raw_uint64_as_argument(ty, v)
  }
  else
  {
    let mut ite00: bool;
    match xl { cbor_raw::case_CBOR_Case_String { v } => ite00 = true, _ => ite00 = false };
    if ite00
    {
      let mut c_: cbor_string =
          cbor_string { cbor_string_type: 0u8, cbor_string_size: 0u8, cbor_string_ptr: &[] };
      match xl { cbor_raw::case_CBOR_Case_String { v } => c_ = v, _ => panic!("") };
      let ty: u8 = c_.cbor_string_type;
      let mut c_0: cbor_string =
          cbor_string { cbor_string_type: 0u8, cbor_string_size: 0u8, cbor_string_ptr: &[] };
      match xl { cbor_raw::case_CBOR_Case_String { v } => c_0 = v, _ => panic!("") };
      let res: CBOR_Spec_Raw_Base_raw_uint64 =
          CBOR_Spec_Raw_Base_raw_uint64
          { size: c_0.cbor_string_size, value: (c_0.cbor_string_ptr).len() as u64 };
      let len: CBOR_Spec_Raw_Base_raw_uint64 = res;
      return raw_uint64_as_argument(ty, len)
    }
    else
    {
      let mut a: bool;
      match xl { cbor_raw::case_CBOR_Case_Tagged { v } => a = true, _ => a = false };
      let mut ite01: bool;
      if a
      { ite01 = true }
      else
      {
        match xl
        { cbor_raw::case_CBOR_Case_Serialized_Tagged { v } => ite01 = true, _ => ite01 = false }
      };
      if ite01
      {
        let mut tag: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
        match xl
        {
          cbor_raw::case_CBOR_Case_Tagged { v } =>
            {
              let c_: cbor_tagged = v;
              tag = c_.cbor_tagged_tag
            },
          _ =>
            match xl
            {
              cbor_raw::case_CBOR_Case_Serialized_Tagged { v } =>
                {
                  let c_: cbor_serialized = v;
                  tag = c_.cbor_serialized_header
                },
              _ => panic!("")
            }
        };
        return raw_uint64_as_argument(6u8, tag)
      }
      else
      {
        let mut a0: bool;
        match xl { cbor_raw::case_CBOR_Case_Array { v } => a0 = true, _ => a0 = false };
        let mut ite02: bool;
        if a0
        { ite02 = true }
        else
        {
          match xl
          { cbor_raw::case_CBOR_Case_Serialized_Array { v } => ite02 = true, _ => ite02 = false }
        };
        if ite02
        {
          let mut len: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
          match xl
          {
            cbor_raw::case_CBOR_Case_Array { v } =>
              {
                let c_: cbor_array = v;
                len = c_.cbor_array_length
              },
            _ =>
              match xl
              {
                cbor_raw::case_CBOR_Case_Serialized_Array { v } =>
                  {
                    let c_: cbor_serialized = v;
                    len = c_.cbor_serialized_header
                  },
                _ => panic!("")
              }
          };
          return raw_uint64_as_argument(4u8, len)
        }
        else
        {
          let mut a1: bool;
          match xl { cbor_raw::case_CBOR_Case_Map { v } => a1 = true, _ => a1 = false };
          let mut ite: bool;
          if a1
          { ite = true }
          else
          {
            match xl
            { cbor_raw::case_CBOR_Case_Serialized_Map { v } => ite = true, _ => ite = false }
          };
          if ite
          {
            let mut len: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
            match xl
            {
              cbor_raw::case_CBOR_Case_Map { v } =>
                {
                  let c_: cbor_map = v;
                  len = c_.cbor_map_length
                },
              _ =>
                match xl
                {
                  cbor_raw::case_CBOR_Case_Serialized_Map { v } =>
                    {
                      let c_: cbor_serialized = v;
                      len = c_.cbor_serialized_header
                    },
                  _ => panic!("")
                }
            };
            return raw_uint64_as_argument(5u8, len)
          }
          else
          {
            let mut v: u8;
            match xl { cbor_raw::case_CBOR_Case_Simple { v: v0 } => v = v0, _ => panic!("") };
            return simple_value_as_argument(v)
          }
        }
      }
    }
  }
}

pub type cbor_raw_serialized_iterator <'a> = &'a [u8];

pub type cbor_raw_tags = u8;

pub fn cbor_raw_with_perm_get_header(xl: cbor_raw) -> header
{
  let res: header = cbor_raw_get_header(xl);
  return res
}

pub fn cbor_serialize(x: cbor_raw, output: &mut [u8]) -> usize
{
  let res: usize = ser(x, output, 0u32 as usize);
  return res
}

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct cbor_serialized <'a>
{
  pub cbor_serialized_header: CBOR_Spec_Raw_Base_raw_uint64,
  pub cbor_serialized_payload: &'a [u8]
}

pub fn cbor_size(x: cbor_raw, bound: usize) -> usize
{
  let mut output: usize = bound;
  let res: bool = siz(x, std::slice::from_mut::<usize>(&mut output));
  if res
  {
    let rem: usize = output;
    return bound.wrapping_sub(rem)
  }
  else
  { return 0u32 as usize }
}

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct cbor_string <'a>
{ pub cbor_string_type: u8, pub cbor_string_size: u8, pub cbor_string_ptr: &'a [u8] }

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct cbor_tagged <'a>
{
  pub cbor_tagged_tag: CBOR_Spec_Raw_Base_raw_uint64,
  pub cbor_tagged_ptr: &'a [cbor_raw <'a>]
}

pub fn copy__uint8_t(dst: &mut [u8], src: &[u8])
{ (dst[0usize..src.len()]).copy_from_slice(&src[0usize..src.len()]) }

pub fn dfst__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(
  t: header
) ->
    initial_byte_t
{
  return
  __proj__Mkdtuple2__item___1__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(
    t
  )
}

pub fn dsnd__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(
  t: header
) ->
    long_argument
{
  return
  __proj__Mkdtuple2__item___2__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(
    t
  )
}

pub fn get_bitfield_gen8(x: u8, lo: u32, hi: u32) -> u8
{
  let op1: u8 = (x as u32).wrapping_shl(8u32.wrapping_sub(hi)) as u8;
  return (op1 as u32).wrapping_shr(8u32.wrapping_sub(hi).wrapping_add(lo)) as u8
}

pub fn get_header_major_type(h: header) -> u8
{
  let b: initial_byte_t = h.fst;
  return b.major_type
}

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct header
{ pub fst: initial_byte_t, pub snd: long_argument }

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct initial_byte_t
{ pub major_type: u8, pub additional_info: u8 }

pub fn jump_header(input: &[u8], offset: usize) -> usize
{
  let off1: usize = offset.wrapping_add(1u32 as usize);
  let s_: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
      split__uint8_t(input, offset);
  let s10: &[u8] = &s_.fst;
  let s20: &[u8] = &s_.snd;
  let split123: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      { fst: (*s10).into(), snd: (*s20).into() };
  let input23: &[u8] = &split123.snd;
  let consumed: usize = off1.wrapping_sub(offset);
  let s1s2: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
      split__uint8_t(input23, consumed);
  let s1: &[u8] = &s1s2.fst;
  let s2: &[u8] = &s1s2.snd;
  let res: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      { fst: (*s1).into(), snd: (*s2).into() };
  let left: &[u8] = &res.fst;
  let right: &[u8] = &res.snd;
  let split23: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      { fst: (*left).into(), snd: (*right).into() };
  let input_: &[u8] = &split23.fst;
  let x: initial_byte_t = read_initial_byte_t(input_);
  let res0: initial_byte_t = x;
  let res1: initial_byte_t = res0;
  let x0: initial_byte_t = res1;
  if x0.additional_info as u32 == 24u32
  { return off1.wrapping_add(1u32 as usize) }
  else if x0.additional_info as u32 == 25u32
  { return off1.wrapping_add(2u32 as usize) }
  else if x0.additional_info as u32 == 26u32
  { return off1.wrapping_add(4u32 as usize) }
  else if x0.additional_info as u32 == 27u32
  { return off1.wrapping_add(8u32 as usize) }
  else
  { return off1.wrapping_add(0u32 as usize) }
}

pub fn jump_raw_data_item(input: &[u8], offset: usize) -> usize
{
  let mut poffset: usize = offset;
  let mut pn: usize = 1u32 as usize;
  let n0: usize = pn;
  let mut cond: bool = n0 > 0u32 as usize;
  while
  cond
  {
    let off: usize = poffset;
    let off10: usize = jump_header(input, off);
    let s_: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        split__uint8_t(input, off);
    let s10: &[u8] = &s_.fst;
    let s20: &[u8] = &s_.snd;
    let split123: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        { fst: (*s10).into(), snd: (*s20).into() };
    let input23: &[u8] = &split123.snd;
    let consumed0: usize = off10.wrapping_sub(off);
    let s1s2: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        split__uint8_t(input23, consumed0);
    let s11: &[u8] = &s1s2.fst;
    let s21: &[u8] = &s1s2.snd;
    let res: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        { fst: (*s11).into(), snd: (*s21).into() };
    let left0: &[u8] = &res.fst;
    let right0: &[u8] = &res.snd;
    let split23: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        { fst: (*left0).into(), snd: (*right0).into() };
    let input_: &[u8] = &split23.fst;
    let res0: header = read_header(input_);
    let x: header = res0;
    let b0: initial_byte_t = x.fst;
    let mut off1: usize;
    if b0.major_type as u32 == 2u32 || b0.major_type as u32 == 3u32
    {
      let b: initial_byte_t = x.fst;
      let l: long_argument = x.snd;
      off1 = off10.wrapping_add(argument_as_uint64(b, l) as usize)
    }
    else
    { off1 = off10.wrapping_add(0u32 as usize) };
    poffset = off1;
    let s_0: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        split__uint8_t(input, off);
    let s12: &[u8] = &s_0.fst;
    let s22: &[u8] = &s_0.snd;
    let split1230: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        { fst: (*s12).into(), snd: (*s22).into() };
    let input230: &[u8] = &split1230.snd;
    let consumed: usize = off1.wrapping_sub(off);
    let s1s20: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        split__uint8_t(input230, consumed);
    let s1: &[u8] = &s1s20.fst;
    let s2: &[u8] = &s1s20.snd;
    let res1: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        { fst: (*s1).into(), snd: (*s2).into() };
    let left: &[u8] = &res1.fst;
    let right: &[u8] = &res1.snd;
    let split230: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        { fst: (*left).into(), snd: (*right).into() };
    let input1: &[u8] = &split230.fst;
    let n: usize = pn;
    let unused: usize = (len__uint8_t(input)).wrapping_sub(off1);
    crate::lowstar::ignore::ignore::<usize>(unused);
    let count: usize = jump_recursive_step_count_leaf(input1);
    pn = n.wrapping_sub(1u32 as usize).wrapping_add(count);
    let n00: usize = pn;
    cond = n00 > 0u32 as usize
  };
  return poffset
}

pub fn jump_recursive_step_count_leaf(a: &[u8]) -> usize
{
  let i: usize = jump_header(a, 0u32 as usize);
  let s: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t = split__uint8_t(a, i);
  let s1: &[u8] = &s.fst;
  let s2: &[u8] = &s.snd;
  let res: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      { fst: (*s1).into(), snd: (*s2).into() };
  let input1: &[u8] = &res.fst;
  let input2: &[u8] = &res.snd;
  let spl: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      { fst: (*input1).into(), snd: (*input2).into() };
  let input10: &[u8] = &spl.fst;
  let h: header = read_header(input10);
  let typ: u8 = get_header_major_type(h);
  if typ as u32 == 4u32
  {
    let b: initial_byte_t = h.fst;
    let l: long_argument = h.snd;
    let arg64: u64 = argument_as_uint64(b, l);
    return arg64 as usize
  }
  else if typ as u32 == 5u32
  {
    let b: initial_byte_t = h.fst;
    let l: long_argument = h.snd;
    let arg64: u64 = argument_as_uint64(b, l);
    let arg: usize = arg64 as usize;
    return arg.wrapping_add(arg)
  }
  else if typ as u32 == 6u32 { return 1u32 as usize } else { return 0u32 as usize }
}

pub fn len__uint8_t(s: &[u8]) -> usize { return s.len() }

#[derive(PartialEq, Clone, Copy)]
pub enum long_argument
{
  case_LongArgumentSimpleValue { v: u8 },
  case_LongArgumentU8 { v: u8 },
  case_LongArgumentU16 { v: u16 },
  case_LongArgumentU32 { v: u32 },
  case_LongArgumentU64 { v: u64 },
  case_LongArgumentOther
}

pub type long_argument_tags = u8;

pub fn mk_raw_uint64(x: u64) -> CBOR_Spec_Raw_Base_raw_uint64
{
  let mut size: u8;
  if x <= 23u32 as u64
  { size = 0u8 }
  else if x < 256u64
  { size = 1u8 }
  else if x < 65536u64
  { size = 2u8 }
  else if x < 4294967296u64 { size = 3u8 } else { size = 4u8 };
  return CBOR_Spec_Raw_Base_raw_uint64 { size, value: x }
}

pub fn op_Array_Access__uint8_t(a: &[u8], i: usize) -> u8 { return a[i] }

pub fn op_Array_Assignment__uint8_t(a: &mut [u8], i: usize, v: u8) { a[i] = v }

#[derive(PartialEq, Clone, Copy)]
pub enum option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_ <'a>
{
  v { v: &'a [cbor_map_entry <'a>] },
  cbor_map_entry_None
}

#[derive(PartialEq, Clone, Copy)]
pub enum option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_ <'a>
{
  v { v: &'a [cbor_raw <'a>] },
  cbor_map_None
}

pub fn raw_uint64_as_argument(t: u8, x: CBOR_Spec_Raw_Base_raw_uint64) -> header
{
  if x.size as u32 == 0u32
  {
    return
    header
    {
      fst: initial_byte_t { major_type: t, additional_info: x.value as u8 },
      snd: long_argument::case_LongArgumentOther
    }
  }
  else if x.size as u32 == 1u32
  {
    return
    header
    {
      fst: initial_byte_t { major_type: t, additional_info: 24u8 },
      snd: long_argument::case_LongArgumentU8 { v: x.value as u8 }
    }
  }
  else if x.size as u32 == 2u32
  {
    return
    header
    {
      fst: initial_byte_t { major_type: t, additional_info: 25u8 },
      snd: long_argument::case_LongArgumentU16 { v: x.value as u16 }
    }
  }
  else if x.size as u32 == 3u32
  {
    return
    header
    {
      fst: initial_byte_t { major_type: t, additional_info: 26u8 },
      snd: long_argument::case_LongArgumentU32 { v: x.value as u32 }
    }
  }
  else
  {
    return
    header
    {
      fst: initial_byte_t { major_type: t, additional_info: 27u8 },
      snd: long_argument::case_LongArgumentU64 { v: x.value }
    }
  }
}

pub fn read_header(input: &[u8]) -> header
{
  let i: usize = 1u32 as usize;
  let s: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
      split__uint8_t(input, i);
  let s1: &[u8] = &s.fst;
  let s2: &[u8] = &s.snd;
  let res: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      { fst: (*s1).into(), snd: (*s2).into() };
  let input10: &[u8] = &res.fst;
  let input20: &[u8] = &res.snd;
  let split12: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      { fst: (*input10).into(), snd: (*input20).into() };
  let input1: &[u8] = &split12.fst;
  let input2: &[u8] = &split12.snd;
  let x: initial_byte_t = read_initial_byte_t(input1);
  let res0: initial_byte_t = x;
  let x1: initial_byte_t = res0;
  let mut x2: long_argument = long_argument::case_LongArgumentOther;
  if x1.additional_info as u32 == 24u32
  {
    if x1.major_type as u32 == 7u32
    {
      let last: u8 = op_Array_Access__uint8_t(input2, 0u32 as usize);
      let res1: u8 = last;
      let x0: u8 = res1;
      let res00: long_argument = long_argument::case_LongArgumentSimpleValue { v: x0 };
      let res10: long_argument = res00;
      let res2: long_argument = res10;
      x2 = res2
    }
    else
    {
      let last: u8 = op_Array_Access__uint8_t(input2, 0u32 as usize);
      let res1: u8 = last;
      let x0: u8 = res1;
      let res00: long_argument = long_argument::case_LongArgumentU8 { v: x0 };
      let res10: long_argument = res00;
      x2 = res10
    }
  }
  else if x1.additional_info as u32 == 25u32
  {
    let pos_: usize = 1u32 as usize;
    let last: u8 = op_Array_Access__uint8_t(input2, pos_);
    let last1: u8 = op_Array_Access__uint8_t(input2, 0u32 as usize);
    let n: u16 = last1 as u16;
    let blast: u16 = last as u16;
    let res1: u16 = (blast as u32).wrapping_add((n as u32).wrapping_mul(256u32)) as u16;
    let x0: u16 = res1;
    let res00: long_argument = long_argument::case_LongArgumentU16 { v: x0 };
    let res10: long_argument = res00;
    x2 = res10
  }
  else if x1.additional_info as u32 == 26u32
  {
    let pos_: usize = 3u32 as usize;
    let last: u8 = op_Array_Access__uint8_t(input2, pos_);
    let pos_1: usize = pos_.wrapping_sub(1u32 as usize);
    let last1: u8 = op_Array_Access__uint8_t(input2, pos_1);
    let pos_2: usize = pos_1.wrapping_sub(1u32 as usize);
    let last2: u8 = op_Array_Access__uint8_t(input2, pos_2);
    let last3: u8 = op_Array_Access__uint8_t(input2, 0u32 as usize);
    let n: u32 = last3 as u32;
    let blast0: u32 = last2 as u32;
    let n0: u32 = blast0.wrapping_add(n.wrapping_mul(256u32));
    let blast1: u32 = last1 as u32;
    let n1: u32 = blast1.wrapping_add(n0.wrapping_mul(256u32));
    let blast: u32 = last as u32;
    let res1: u32 = blast.wrapping_add(n1.wrapping_mul(256u32));
    let x0: u32 = res1;
    let res00: long_argument = long_argument::case_LongArgumentU32 { v: x0 };
    let res10: long_argument = res00;
    x2 = res10
  }
  else if x1.additional_info as u32 == 27u32
  {
    let pos_: usize = 7u32 as usize;
    let last: u8 = op_Array_Access__uint8_t(input2, pos_);
    let pos_1: usize = pos_.wrapping_sub(1u32 as usize);
    let last1: u8 = op_Array_Access__uint8_t(input2, pos_1);
    let pos_2: usize = pos_1.wrapping_sub(1u32 as usize);
    let last2: u8 = op_Array_Access__uint8_t(input2, pos_2);
    let pos_3: usize = pos_2.wrapping_sub(1u32 as usize);
    let last3: u8 = op_Array_Access__uint8_t(input2, pos_3);
    let pos_4: usize = pos_3.wrapping_sub(1u32 as usize);
    let last4: u8 = op_Array_Access__uint8_t(input2, pos_4);
    let pos_5: usize = pos_4.wrapping_sub(1u32 as usize);
    let last5: u8 = op_Array_Access__uint8_t(input2, pos_5);
    let pos_6: usize = pos_5.wrapping_sub(1u32 as usize);
    let last6: u8 = op_Array_Access__uint8_t(input2, pos_6);
    let last7: u8 = op_Array_Access__uint8_t(input2, 0u32 as usize);
    let n: u64 = last7 as u64;
    let blast0: u64 = last6 as u64;
    let n0: u64 = blast0.wrapping_add(n.wrapping_mul(256u64));
    let blast1: u64 = last5 as u64;
    let n1: u64 = blast1.wrapping_add(n0.wrapping_mul(256u64));
    let blast2: u64 = last4 as u64;
    let n2: u64 = blast2.wrapping_add(n1.wrapping_mul(256u64));
    let blast3: u64 = last3 as u64;
    let n3: u64 = blast3.wrapping_add(n2.wrapping_mul(256u64));
    let blast4: u64 = last2 as u64;
    let n4: u64 = blast4.wrapping_add(n3.wrapping_mul(256u64));
    let blast5: u64 = last1 as u64;
    let n5: u64 = blast5.wrapping_add(n4.wrapping_mul(256u64));
    let blast: u64 = last as u64;
    let res1: u64 = blast.wrapping_add(n5.wrapping_mul(256u64));
    let x0: u64 = res1;
    let res00: long_argument = long_argument::case_LongArgumentU64 { v: x0 };
    let res10: long_argument = res00;
    x2 = res10
  }
  else
  { x2 = long_argument::case_LongArgumentOther };
  let res1: header = header { fst: x1, snd: x2 };
  return res1
}

pub fn read_initial_byte_t(input: &[u8]) -> initial_byte_t
{
  let last: u8 = op_Array_Access__uint8_t(input, 0u32 as usize);
  let res: u8 = last;
  let x: u8 = res;
  let res0: initial_byte_t =
      initial_byte_t
      {
        major_type: get_bitfield_gen8(x, 5u32, 8u32),
        additional_info: get_bitfield_gen8(x, 0u32, 5u32)
      };
  let res1: initial_byte_t = res0;
  let res2: initial_byte_t = res1;
  let res3: initial_byte_t = res2;
  return res3
}

pub fn ser(x1_: cbor_raw, out: &mut [u8], offset: usize) -> usize
{
  let x2_: cbor_raw = x1_;
  let res: usize = CBOR_Pulse_Raw_Format_Serialize_ser_(x2_, out, offset);
  let res0: usize = res;
  return res0
}

pub fn set_bitfield_gen8(x: u8, lo: u32, hi: u32, v: u8) -> u8
{
  let op0: u8 = 255u8;
  let op1: u8 = (op0 as u32).wrapping_shr(8u32.wrapping_sub(hi.wrapping_sub(lo))) as u8;
  let op2: u8 = (op1 as u32).wrapping_shl(lo) as u8;
  let op3: u8 = ! op2;
  let op4: u8 = (x as u32 & op3 as u32) as u8;
  let op5: u8 = (v as u32).wrapping_shl(lo) as u8;
  return (op4 as u32 | op5 as u32) as u8
}

pub fn simple_value_as_argument(x: u8) -> header
{
  if (x as u32) <= 23u32
  {
    return
    header
    {
      fst: initial_byte_t { major_type: 7u8, additional_info: x },
      snd: long_argument::case_LongArgumentOther
    }
  }
  else
  {
    return
    header
    {
      fst: initial_byte_t { major_type: 7u8, additional_info: 24u8 },
      snd: long_argument::case_LongArgumentSimpleValue { v: x }
    }
  }
}

pub fn siz(x1_: cbor_raw, out: &mut [usize]) -> bool
{
  let x2_: cbor_raw = x1_;
  let res: bool = CBOR_Pulse_Raw_Format_Serialize_siz_(x2_, out);
  let res0: bool = res;
  return res0
}

pub fn size_header(x: header, out: &mut [usize]) -> bool
{
  let xh1: initial_byte_t =
      dfst__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(x);
  let capacity0: usize = out[0usize];
  let mut res0: bool;
  if capacity0 < 1u32 as usize
  { res0 = false }
  else
  {
    out[0usize] = capacity0.wrapping_sub(1u32 as usize);
    res0 = true
  };
  let res1: bool = res0;
  if res1
  {
    let x2_: long_argument =
        dsnd__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(x);
    crate::lowstar::ignore::ignore::<long_argument>(x2_);
    let mut res00: bool;
    if xh1.additional_info as u32 == 24u32
    {
      let capacity: usize = out[0usize];
      let mut res: bool;
      if capacity < 1u32 as usize
      { res = false }
      else
      {
        out[0usize] = capacity.wrapping_sub(1u32 as usize);
        res = true
      };
      res00 = res
    }
    else if xh1.additional_info as u32 == 25u32
    {
      let capacity: usize = out[0usize];
      let mut res: bool;
      if capacity < 2u32 as usize
      { res = false }
      else
      {
        out[0usize] = capacity.wrapping_sub(2u32 as usize);
        res = true
      };
      res00 = res
    }
    else if xh1.additional_info as u32 == 26u32
    {
      let capacity: usize = out[0usize];
      let mut res: bool;
      if capacity < 4u32 as usize
      { res = false }
      else
      {
        out[0usize] = capacity.wrapping_sub(4u32 as usize);
        res = true
      };
      res00 = res
    }
    else if xh1.additional_info as u32 == 27u32
    {
      let capacity: usize = out[0usize];
      let mut res: bool;
      if capacity < 8u32 as usize
      { res = false }
      else
      {
        out[0usize] = capacity.wrapping_sub(8u32 as usize);
        res = true
      };
      res00 = res
    }
    else
    { res00 = true };
    let res2: bool = res00;
    return res2
  }
  else
  { return false }
}

pub fn split__uint8_t(s: &[u8], i: usize) ->
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
{
  let elt_: (&[u8], &[u8]) = s.split_at(i);
  let s1: &[u8] = s;
  let s2: &[u8] = elt_.1;
  return
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  { fst: (*s1).into(), snd: (*s2).into() }
}

pub fn validate_header(input: &[u8], poffset: &mut [usize]) -> bool
{
  let offset1: usize = poffset[0usize];
  let offset2: usize = poffset[0usize];
  let offset30: usize = poffset[0usize];
  let mut is_valid0: bool;
  if (len__uint8_t(input)).wrapping_sub(offset30) < 1u32 as usize
  { is_valid0 = false }
  else
  {
    poffset[0usize] = offset30.wrapping_add(1u32 as usize);
    is_valid0 = true
  };
  let mut is_valid1: bool;
  if is_valid0
  {
    let off: usize = poffset[0usize];
    let s_: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        split__uint8_t(input, offset2);
    let s10: &[u8] = &s_.fst;
    let s20: &[u8] = &s_.snd;
    let split123: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        { fst: (*s10).into(), snd: (*s20).into() };
    let input23: &[u8] = &split123.snd;
    let consumed: usize = off.wrapping_sub(offset2);
    let s1s2: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        split__uint8_t(input23, consumed);
    let s1: &[u8] = &s1s2.fst;
    let s2: &[u8] = &s1s2.snd;
    let res: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        { fst: (*s1).into(), snd: (*s2).into() };
    let left: &[u8] = &res.fst;
    let right: &[u8] = &res.snd;
    let split23: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        { fst: (*left).into(), snd: (*right).into() };
    let input_: &[u8] = &split23.fst;
    let res0: initial_byte_t = read_initial_byte_t(input_);
    let x: initial_byte_t = res0;
    let mut ite: bool;
    if x.major_type as u32 == 7u32
    { ite = (x.additional_info as u32) <= 24u32 }
    else
    { ite = true };
    is_valid1 = ite && (x.additional_info as u32) < 28u32
  }
  else
  { is_valid1 = false };
  if is_valid1
  {
    let off: usize = poffset[0usize];
    let s_: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        split__uint8_t(input, offset1);
    let s10: &[u8] = &s_.fst;
    let s20: &[u8] = &s_.snd;
    let split123: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        { fst: (*s10).into(), snd: (*s20).into() };
    let input23: &[u8] = &split123.snd;
    let consumed0: usize = off.wrapping_sub(offset1);
    let s1s2: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        split__uint8_t(input23, consumed0);
    let s11: &[u8] = &s1s2.fst;
    let s21: &[u8] = &s1s2.snd;
    let res: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        { fst: (*s11).into(), snd: (*s21).into() };
    let left0: &[u8] = &res.fst;
    let right0: &[u8] = &res.snd;
    let split23: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        { fst: (*left0).into(), snd: (*right0).into() };
    let input_: &[u8] = &split23.fst;
    let x: initial_byte_t = read_initial_byte_t(input_);
    let res0: initial_byte_t = x;
    let res1: initial_byte_t = res0;
    let x0: initial_byte_t = res1;
    if x0.additional_info as u32 == 24u32
    {
      if x0.major_type as u32 == 7u32
      {
        let offset20: usize = poffset[0usize];
        let offset3: usize = poffset[0usize];
        let mut is_valid: bool;
        if (len__uint8_t(input)).wrapping_sub(offset3) < 1u32 as usize
        { is_valid = false }
        else
        {
          poffset[0usize] = offset3.wrapping_add(1u32 as usize);
          is_valid = true
        };
        if is_valid
        {
          let off1: usize = poffset[0usize];
          let s_0: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
              split__uint8_t(input, offset20);
          let s100: &[u8] = &s_0.fst;
          let s200: &[u8] = &s_0.snd;
          let split1230: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
              __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
              { fst: (*s100).into(), snd: (*s200).into() };
          let input230: &[u8] = &split1230.snd;
          let consumed: usize = off1.wrapping_sub(offset20);
          let s1s20: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
              split__uint8_t(input230, consumed);
          let s1: &[u8] = &s1s20.fst;
          let s2: &[u8] = &s1s20.snd;
          let res2: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
              __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
              { fst: (*s1).into(), snd: (*s2).into() };
          let left: &[u8] = &res2.fst;
          let right: &[u8] = &res2.snd;
          let split230: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
              __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
              { fst: (*left).into(), snd: (*right).into() };
          let input_0: &[u8] = &split230.fst;
          let last: u8 = op_Array_Access__uint8_t(input_0, 0u32 as usize);
          let res00: u8 = last;
          let res10: u8 = res00;
          let x1: u8 = res10;
          return 32u32 <= x1 as u32
        }
        else
        { return false }
      }
      else
      {
        let offset20: usize = poffset[0usize];
        if (len__uint8_t(input)).wrapping_sub(offset20) < 1u32 as usize
        { return false }
        else
        {
          poffset[0usize] = offset20.wrapping_add(1u32 as usize);
          return true
        }
      }
    }
    else if x0.additional_info as u32 == 25u32
    {
      let offset20: usize = poffset[0usize];
      if (len__uint8_t(input)).wrapping_sub(offset20) < 2u32 as usize
      { return false }
      else
      {
        poffset[0usize] = offset20.wrapping_add(2u32 as usize);
        return true
      }
    }
    else if x0.additional_info as u32 == 26u32
    {
      let offset20: usize = poffset[0usize];
      if (len__uint8_t(input)).wrapping_sub(offset20) < 4u32 as usize
      { return false }
      else
      {
        poffset[0usize] = offset20.wrapping_add(4u32 as usize);
        return true
      }
    }
    else if x0.additional_info as u32 == 27u32
    {
      let offset20: usize = poffset[0usize];
      if (len__uint8_t(input)).wrapping_sub(offset20) < 8u32 as usize
      { return false }
      else
      {
        poffset[0usize] = offset20.wrapping_add(8u32 as usize);
        return true
      }
    }
    else
    { return true }
  }
  else
  { return false }
}

pub fn validate_raw_data_item(input: &[u8], poffset: &mut [usize]) -> bool
{
  let mut pn: usize = 1u32 as usize;
  let mut pres: bool = true;
  let res: bool = pres;
  let n0: usize = pn;
  let mut cond: bool = res && n0 > 0u32 as usize;
  while
  cond
  {
    let off: usize = poffset[0usize];
    let n00: usize = pn;
    if n00 > (len__uint8_t(input)).wrapping_sub(off)
    { pres = false }
    else
    {
      let offset1: usize = poffset[0usize];
      let is_valid1: bool = validate_header(input, poffset);
      let mut res1: bool;
      if is_valid1
      {
        let off1: usize = poffset[0usize];
        let s_: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
            split__uint8_t(input, offset1);
        let s10: &[u8] = &s_.fst;
        let s20: &[u8] = &s_.snd;
        let split123: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
            __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
            { fst: (*s10).into(), snd: (*s20).into() };
        let input23: &[u8] = &split123.snd;
        let consumed: usize = off1.wrapping_sub(offset1);
        let s1s2: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
            split__uint8_t(input23, consumed);
        let s1: &[u8] = &s1s2.fst;
        let s2: &[u8] = &s1s2.snd;
        let res0: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
            __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
            { fst: (*s1).into(), snd: (*s2).into() };
        let left: &[u8] = &res0.fst;
        let right: &[u8] = &res0.snd;
        let split23: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
            __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
            { fst: (*left).into(), snd: (*right).into() };
        let input_: &[u8] = &split23.fst;
        let res00: header = read_header(input_);
        let x: header = res00;
        let b0: initial_byte_t = x.fst;
        if b0.major_type as u32 == 2u32 || b0.major_type as u32 == 3u32
        {
          let offset2: usize = poffset[0usize];
          let b: initial_byte_t = x.fst;
          let l0: long_argument = x.snd;
          if (len__uint8_t(input)).wrapping_sub(offset2) < argument_as_uint64(b, l0) as usize
          { res1 = false }
          else
          {
            let b1: initial_byte_t = x.fst;
            let l: long_argument = x.snd;
            poffset[0usize] = offset2.wrapping_add(argument_as_uint64(b1, l) as usize);
            res1 = true
          }
        }
        else
        { res1 = true }
      }
      else
      { res1 = false };
      if ! res1
      { pres = false }
      else
      {
        let offset10: usize = poffset[0usize];
        let s_: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
            split__uint8_t(input, off);
        let s10: &[u8] = &s_.fst;
        let s20: &[u8] = &s_.snd;
        let split123: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
            __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
            { fst: (*s10).into(), snd: (*s20).into() };
        let input23: &[u8] = &split123.snd;
        let consumed: usize = offset10.wrapping_sub(off);
        let s1s2: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
            split__uint8_t(input23, consumed);
        let s1: &[u8] = &s1s2.fst;
        let s2: &[u8] = &s1s2.snd;
        let res0: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
            __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
            { fst: (*s1).into(), snd: (*s2).into() };
        let left: &[u8] = &res0.fst;
        let right: &[u8] = &res0.snd;
        let split23: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
            __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
            { fst: (*left).into(), snd: (*right).into() };
        let input1: &[u8] = &split23.fst;
        let bound: usize = (len__uint8_t(input)).wrapping_sub(off).wrapping_sub(n00);
        let res2: bool =
            validate_recursive_step_count_leaf(
              input1,
              bound,
              std::slice::from_mut::<usize>(&mut pn)
            );
        let count: usize = pn;
        if res2 || count > bound
        { pres = false }
        else
        {
          let n_: usize = n00.wrapping_sub(1u32 as usize).wrapping_add(count);
          pn = n_
        }
      }
    };
    let res0: bool = pres;
    let n: usize = pn;
    cond = res0 && n > 0u32 as usize
  };
  return pres
}

pub fn validate_recursive_step_count_leaf(a: &[u8], bound: usize, prem: &mut [usize]) -> bool
{
  let i: usize = jump_header(a, 0u32 as usize);
  let s: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t = split__uint8_t(a, i);
  let s1: &[u8] = &s.fst;
  let s2: &[u8] = &s.snd;
  let res: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      { fst: (*s1).into(), snd: (*s2).into() };
  let input1: &[u8] = &res.fst;
  let input2: &[u8] = &res.snd;
  let spl: __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t =
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      { fst: (*input1).into(), snd: (*input2).into() };
  let input10: &[u8] = &spl.fst;
  let h: header = read_header(input10);
  let typ: u8 = get_header_major_type(h);
  if typ as u32 == 4u32
  {
    let b: initial_byte_t = h.fst;
    let l: long_argument = h.snd;
    let arg64: u64 = argument_as_uint64(b, l);
    prem[0usize] = arg64 as usize;
    return false
  }
  else if typ as u32 == 5u32
  {
    let b: initial_byte_t = h.fst;
    let l: long_argument = h.snd;
    let arg64: u64 = argument_as_uint64(b, l);
    let arg: usize = arg64 as usize;
    if arg > bound
    { return true }
    else if bound.wrapping_sub(arg) < arg
    { return true }
    else
    {
      prem[0usize] = arg.wrapping_add(arg);
      return false
    }
  }
  else if typ as u32 == 6u32
  {
    prem[0usize] = 1u32 as usize;
    return false
  }
  else
  {
    prem[0usize] = 0u32 as usize;
    return false
  }
}

pub fn write_header(x: header, out: &mut [u8], offset: usize) -> usize
{
  let xh1: initial_byte_t =
      dfst__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(x);
  let pos_: usize = offset.wrapping_add(1u32 as usize);
  let n_: u8 =
      set_bitfield_gen8(
        set_bitfield_gen8(0u8, 0u32, 5u32, xh1.additional_info),
        5u32,
        8u32,
        xh1.major_type
      );
  out[pos_.wrapping_sub(1u32 as usize)] = n_;
  let res1: usize = pos_;
  let x2_: long_argument =
      dsnd__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(x);
  let mut res: usize;
  if xh1.additional_info as u32 == 24u32
  {
    if xh1.major_type as u32 == 7u32
    {
      let pos_0: usize = res1.wrapping_add(1u32 as usize);
      let mut n_0: u8;
      match x2_ { long_argument::case_LongArgumentSimpleValue { v } => n_0 = v, _ => panic!("") };
      out[pos_0.wrapping_sub(1u32 as usize)] = n_0;
      res = pos_0
    }
    else
    {
      let pos_0: usize = res1.wrapping_add(1u32 as usize);
      let mut n_0: u8;
      match x2_ { long_argument::case_LongArgumentU8 { v } => n_0 = v, _ => panic!("") };
      out[pos_0.wrapping_sub(1u32 as usize)] = n_0;
      res = pos_0
    }
  }
  else if xh1.additional_info as u32 == 25u32
  {
    let pos_0: usize = res1.wrapping_add(2u32 as usize);
    let mut ite0: u16;
    match x2_ { long_argument::case_LongArgumentU16 { v } => ite0 = v, _ => panic!("") };
    let lo: u8 = ite0 as u8;
    let mut ite: u16;
    match x2_ { long_argument::case_LongArgumentU16 { v } => ite = v, _ => panic!("") };
    let hi: u16 = (ite as u32).wrapping_div(256u32) as u16;
    let pos_1: usize = pos_0.wrapping_sub(1u32 as usize);
    let n_0: u8 = hi as u8;
    out[pos_1.wrapping_sub(1u32 as usize)] = n_0;
    out[pos_1] = lo;
    res = pos_0
  }
  else if xh1.additional_info as u32 == 26u32
  {
    let pos_0: usize = res1.wrapping_add(4u32 as usize);
    let mut ite0: u32;
    match x2_ { long_argument::case_LongArgumentU32 { v } => ite0 = v, _ => panic!("") };
    let lo: u8 = ite0 as u8;
    let mut ite: u32;
    match x2_ { long_argument::case_LongArgumentU32 { v } => ite = v, _ => panic!("") };
    let hi: u32 = ite.wrapping_div(256u32);
    let pos_1: usize = pos_0.wrapping_sub(1u32 as usize);
    let lo1: u8 = hi as u8;
    let hi1: u32 = hi.wrapping_div(256u32);
    let pos_2: usize = pos_1.wrapping_sub(1u32 as usize);
    let lo2: u8 = hi1 as u8;
    let hi2: u32 = hi1.wrapping_div(256u32);
    let pos_3: usize = pos_2.wrapping_sub(1u32 as usize);
    let n_0: u8 = hi2 as u8;
    out[pos_3.wrapping_sub(1u32 as usize)] = n_0;
    out[pos_3] = lo2;
    out[pos_2] = lo1;
    out[pos_1] = lo;
    res = pos_0
  }
  else if xh1.additional_info as u32 == 27u32
  {
    let pos_0: usize = res1.wrapping_add(8u32 as usize);
    let mut ite0: u64;
    match x2_ { long_argument::case_LongArgumentU64 { v } => ite0 = v, _ => panic!("") };
    let lo: u8 = ite0 as u8;
    let mut ite: u64;
    match x2_ { long_argument::case_LongArgumentU64 { v } => ite = v, _ => panic!("") };
    let hi: u64 = ite.wrapping_div(256u64);
    let pos_1: usize = pos_0.wrapping_sub(1u32 as usize);
    let lo1: u8 = hi as u8;
    let hi1: u64 = hi.wrapping_div(256u64);
    let pos_2: usize = pos_1.wrapping_sub(1u32 as usize);
    let lo2: u8 = hi1 as u8;
    let hi2: u64 = hi1.wrapping_div(256u64);
    let pos_3: usize = pos_2.wrapping_sub(1u32 as usize);
    let lo3: u8 = hi2 as u8;
    let hi3: u64 = hi2.wrapping_div(256u64);
    let pos_4: usize = pos_3.wrapping_sub(1u32 as usize);
    let lo4: u8 = hi3 as u8;
    let hi4: u64 = hi3.wrapping_div(256u64);
    let pos_5: usize = pos_4.wrapping_sub(1u32 as usize);
    let lo5: u8 = hi4 as u8;
    let hi5: u64 = hi4.wrapping_div(256u64);
    let pos_6: usize = pos_5.wrapping_sub(1u32 as usize);
    let lo6: u8 = hi5 as u8;
    let hi6: u64 = hi5.wrapping_div(256u64);
    let pos_7: usize = pos_6.wrapping_sub(1u32 as usize);
    let n_0: u8 = hi6 as u8;
    out[pos_7.wrapping_sub(1u32 as usize)] = n_0;
    out[pos_7] = lo6;
    out[pos_6] = lo5;
    out[pos_5] = lo4;
    out[pos_4] = lo3;
    out[pos_3] = lo2;
    out[pos_2] = lo1;
    out[pos_1] = lo;
    res = pos_0
  }
  else
  { res = res1 };
  let res2: usize = res;
  let res0: usize = res2;
  return res0
}
