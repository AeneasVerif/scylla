#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

pub fn CBOR_Pulse_Raw_Compare_impl_cbor_compare(x1: cbor_raw, x2: cbor_raw) -> i16
{
  let ty1: u8 = impl_major_type(x1);
  let ty2: u8 = impl_major_type(x2);
  let c: i16 = impl_uint8_compare(ty1, ty2);
  if c == 0i32 as i16
  {
    if ty1 as u32 == 0u32 || ty1 as u32 == 1u32
    {
      let mut c_: cbor_int = Default::default();
      match x1 { cbor_raw::case_CBOR_Case_Int { v } => c_ = v, _ => panic!("") };
      let i1: CBOR_Spec_Raw_Base_raw_uint64 =
          CBOR_Spec_Raw_Base_raw_uint64 { size: c_.cbor_int_size, value: c_.cbor_int_value };
      let mut c_0: cbor_int = Default::default();
      match x2 { cbor_raw::case_CBOR_Case_Int { v } => c_0 = v, _ => panic!("") };
      let i2: CBOR_Spec_Raw_Base_raw_uint64 =
          CBOR_Spec_Raw_Base_raw_uint64 { size: c_0.cbor_int_size, value: c_0.cbor_int_value };
      return impl_raw_uint64_compare(i1, i2)
    }
    else if ty1 as u32 == 2u32 || ty1 as u32 == 3u32
    {
      let mut c_: cbor_string =
          cbor_string { cbor_string_type: 0u8, cbor_string_size: 0u8, cbor_string_ptr: &[] };
      match x1 { cbor_raw::case_CBOR_Case_String { v } => c_ = v, _ => panic!("") };
      let res: CBOR_Spec_Raw_Base_raw_uint64 =
          CBOR_Spec_Raw_Base_raw_uint64
          { size: c_.cbor_string_size, value: len__uint8_t(c_.cbor_string_ptr) as u64 };
      let i1: CBOR_Spec_Raw_Base_raw_uint64 = res;
      let mut c_0: cbor_string =
          cbor_string { cbor_string_type: 0u8, cbor_string_size: 0u8, cbor_string_ptr: &[] };
      match x2 { cbor_raw::case_CBOR_Case_String { v } => c_0 = v, _ => panic!("") };
      let res0: CBOR_Spec_Raw_Base_raw_uint64 =
          CBOR_Spec_Raw_Base_raw_uint64
          { size: c_0.cbor_string_size, value: len__uint8_t(c_0.cbor_string_ptr) as u64 };
      let i2: CBOR_Spec_Raw_Base_raw_uint64 = res0;
      let c1: i16 = impl_raw_uint64_compare(i1, i2);
      if c1 == 0i32 as i16
      {
        let mut c_1: cbor_string =
            cbor_string { cbor_string_type: 0u8, cbor_string_size: 0u8, cbor_string_ptr: &[] };
        match x1 { cbor_raw::case_CBOR_Case_String { v } => c_1 = v, _ => panic!("") };
        let pl1: &[u8] = c_1.cbor_string_ptr;
        let mut c_00: cbor_string =
            cbor_string { cbor_string_type: 0u8, cbor_string_size: 0u8, cbor_string_ptr: &[] };
        match x2 { cbor_raw::case_CBOR_Case_String { v } => c_00 = v, _ => panic!("") };
        let pl2: &[u8] = c_00.cbor_string_ptr;
        let res1: i16 = lex_compare_bytes(pl1, pl2);
        return res1
      }
      else
      { return c1 }
    }
    else if ty1 as u32 == 6u32
    {
      let mut tag1: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
      match x1
      {
        cbor_raw::case_CBOR_Case_Tagged { v } =>
          {
            let c_: cbor_tagged = v;
            tag1 = c_.cbor_tagged_tag
          },
        _ =>
          match x1
          {
            cbor_raw::case_CBOR_Case_Serialized_Tagged { v } =>
              {
                let c_: cbor_serialized = v;
                tag1 = c_.cbor_serialized_header
              },
            _ => panic!("")
          }
      };
      let mut tag2: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
      match x2
      {
        cbor_raw::case_CBOR_Case_Tagged { v } =>
          {
            let c_: cbor_tagged = v;
            tag2 = c_.cbor_tagged_tag
          },
        _ =>
          match x2
          {
            cbor_raw::case_CBOR_Case_Serialized_Tagged { v } =>
              {
                let c_: cbor_serialized = v;
                tag2 = c_.cbor_serialized_header
              },
            _ => panic!("")
          }
      };
      let c1: i16 = impl_raw_uint64_compare(tag1, tag2);
      if c1 == 0i32 as i16
      {
        let pl1: cbor_raw = cbor_match_tagged_get_payload(x1);
        let pl2: cbor_raw = cbor_match_tagged_get_payload(x2);
        let res: i16 = CBOR_Pulse_Raw_Compare_impl_cbor_compare(pl1, pl2);
        return res
      }
      else
      { return c1 }
    }
    else if ty1 as u32 == 4u32
    {
      let mut len1: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
      match x1
      {
        cbor_raw::case_CBOR_Case_Array { v } =>
          {
            let c_: cbor_array = v;
            len1 = c_.cbor_array_length
          },
        _ =>
          match x1
          {
            cbor_raw::case_CBOR_Case_Serialized_Array { v } =>
              {
                let c_: cbor_serialized = v;
                len1 = c_.cbor_serialized_header
              },
            _ => panic!("")
          }
      };
      let mut len2: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
      match x2
      {
        cbor_raw::case_CBOR_Case_Array { v } =>
          {
            let c_: cbor_array = v;
            len2 = c_.cbor_array_length
          },
        _ =>
          match x2
          {
            cbor_raw::case_CBOR_Case_Serialized_Array { v } =>
              {
                let c_: cbor_serialized = v;
                len2 = c_.cbor_serialized_header
              },
            _ => panic!("")
          }
      };
      let c1: i16 = impl_raw_uint64_compare(len1, len2);
      if c1 == 0i32 as i16
      {
        let i1: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw =
            cbor_array_iterator_init(x1);
        let i2: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw =
            cbor_array_iterator_init(x2);
        let pl1: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw = i1;
        let pl2: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw = i2;
        let mut fin1: bool;
        match pl1
        {
          CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Slice
          { v }
          =>
            {
              let c_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = v;
              let res: bool = len__CBOR_Pulse_Raw_Type_cbor_raw(c_) == 0u32 as usize;
              let res0: bool = res;
              fin1 = res0
            },
          _ =>
            match pl1
            {
              CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Serialized
              { v }
              =>
                {
                  let c_: &[u8] = v;
                  let res: bool = cbor_serialized_array_iterator_is_empty(c_);
                  fin1 = res
                },
              _ => panic!("")
            }
        };
        let mut fin2: bool;
        match pl2
        {
          CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Slice
          { v }
          =>
            {
              let c_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = v;
              let res: bool = len__CBOR_Pulse_Raw_Type_cbor_raw(c_) == 0u32 as usize;
              let res0: bool = res;
              fin2 = res0
            },
          _ =>
            match pl2
            {
              CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Serialized
              { v }
              =>
                {
                  let c_: &[u8] = v;
                  let res: bool = cbor_serialized_array_iterator_is_empty(c_);
                  fin2 = res
                },
              _ => panic!("")
            }
        };
        let mut res0: i16;
        if fin1
        { if fin2 { res0 = 0i32 as i16 } else { res0 = 0i32.wrapping_sub(1i32) as i16 } }
        else if fin2
        { res0 = 1i32 as i16 }
        else
        {
          let mut pi1: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw =
              pl1;
          let mut pi2: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw =
              pl2;
          let mut pres: i16 = 0i32 as i16;
          let mut pfin1: bool = false;
          let res1: i16 = pres;
          let fin110: bool = pfin1;
          let mut cond: bool = res1 == 0i32 as i16 && ! fin110;
          while
          cond
          {
            let i00: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw = pi1;
            let mut elt1: cbor_raw = cbor_raw::case_CBOR_Case_Simple { v: 0u8 };
            match i00
            {
              CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Slice
              { v }
              =>
                {
                  let i: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = v;
                  let res: cbor_raw =
                      op_Array_Access__CBOR_Pulse_Raw_Type_cbor_raw(i, 0u32 as usize);
                  let
                  sp:
                  __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw
                  =
                      split__CBOR_Pulse_Raw_Type_cbor_raw(i, 1u32 as usize);
                  let s_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = sp.snd;
                  let i11: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = s_;
                  let i_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = i11;
                  pi1 =
                      CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Slice
                      { v: i_ };
                  let res00: cbor_raw = res;
                  elt1 = res00
                },
              _ =>
                match i00
                {
                  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Serialized
                  { v }
                  =>
                    {
                      let i: &[u8] = v;
                      let res: cbor_raw =
                          cbor_serialized_array_iterator_next(
                            std::slice::from_mut::<CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw>(
                              &mut pi1
                            ),
                            i
                          );
                      elt1 = res
                    },
                  _ => panic!("")
                }
            };
            let i0: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw = pi2;
            let mut elt2: cbor_raw = cbor_raw::case_CBOR_Case_Simple { v: 0u8 };
            match i0
            {
              CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Slice
              { v }
              =>
                {
                  let i: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = v;
                  let res: cbor_raw =
                      op_Array_Access__CBOR_Pulse_Raw_Type_cbor_raw(i, 0u32 as usize);
                  let
                  sp:
                  __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw
                  =
                      split__CBOR_Pulse_Raw_Type_cbor_raw(i, 1u32 as usize);
                  let s_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = sp.snd;
                  let i11: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = s_;
                  let i_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = i11;
                  pi2 =
                      CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Slice
                      { v: i_ };
                  let res00: cbor_raw = res;
                  elt2 = res00
                },
              _ =>
                match i0
                {
                  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Serialized
                  { v }
                  =>
                    {
                      let i: &[u8] = v;
                      let res: cbor_raw =
                          cbor_serialized_array_iterator_next(
                            std::slice::from_mut::<CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw>(
                              &mut pi2
                            ),
                            i
                          );
                      elt2 = res
                    },
                  _ => panic!("")
                }
            };
            let pelt1: cbor_raw = elt1;
            let pelt2: cbor_raw = elt2;
            let res: i16 = CBOR_Pulse_Raw_Compare_impl_cbor_compare(pelt1, pelt2);
            let c2: i16 = res;
            if c2 == 0i32 as i16
            {
              let i11: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw =
                  pi1;
              let mut fin11: bool;
              match i11
              {
                CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Slice
                { v }
                =>
                  {
                    let c_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = v;
                    let res2: bool = len__CBOR_Pulse_Raw_Type_cbor_raw(c_) == 0u32 as usize;
                    let res00: bool = res2;
                    fin11 = res00
                  },
                _ =>
                  match i11
                  {
                    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Serialized
                    { v }
                    =>
                      {
                        let c_: &[u8] = v;
                        let res2: bool = cbor_serialized_array_iterator_is_empty(c_);
                        fin11 = res2
                      },
                    _ => panic!("")
                  }
              };
              let i21: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw =
                  pi2;
              let mut fin21: bool;
              match i21
              {
                CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Slice
                { v }
                =>
                  {
                    let c_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = v;
                    let res2: bool = len__CBOR_Pulse_Raw_Type_cbor_raw(c_) == 0u32 as usize;
                    let res00: bool = res2;
                    fin21 = res00
                  },
                _ =>
                  match i21
                  {
                    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Serialized
                    { v }
                    =>
                      {
                        let c_: &[u8] = v;
                        let res2: bool = cbor_serialized_array_iterator_is_empty(c_);
                        fin21 = res2
                      },
                    _ => panic!("")
                  }
              };
              if fin11 == fin21
              { pfin1 = fin11 }
              else if fin11 { pres = 0i32.wrapping_sub(1i32) as i16 } else { pres = 1i32 as i16 }
            }
            else
            { pres = c2 };
            let res00: i16 = pres;
            let fin11: bool = pfin1;
            cond = res00 == 0i32 as i16 && ! fin11
          };
          res0 = pres
        };
        let res: i16 = res0;
        return res
      }
      else
      { return c1 }
    }
    else if ty1 as u32 == 5u32
    {
      let mut len1: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
      match x1
      {
        cbor_raw::case_CBOR_Case_Map { v } =>
          {
            let c_: cbor_map = v;
            len1 = c_.cbor_map_length
          },
        _ =>
          match x1
          {
            cbor_raw::case_CBOR_Case_Serialized_Map { v } =>
              {
                let c_: cbor_serialized = v;
                len1 = c_.cbor_serialized_header
              },
            _ => panic!("")
          }
      };
      let mut len2: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
      match x2
      {
        cbor_raw::case_CBOR_Case_Map { v } =>
          {
            let c_: cbor_map = v;
            len2 = c_.cbor_map_length
          },
        _ =>
          match x2
          {
            cbor_raw::case_CBOR_Case_Serialized_Map { v } =>
              {
                let c_: cbor_serialized = v;
                len2 = c_.cbor_serialized_header
              },
            _ => panic!("")
          }
      };
      let c1: i16 = impl_raw_uint64_compare(len1, len2);
      if c1 == 0i32 as i16
      {
        let i1: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry =
            cbor_map_iterator_init(x1);
        let i2: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry =
            cbor_map_iterator_init(x2);
        let pl1: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry = i1;
        let pl2: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry = i2;
        let mut fin1: bool;
        match pl1
        {
          CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Slice
          { v }
          =>
            {
              let c_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = v;
              let res: bool = len__CBOR_Pulse_Raw_Type_cbor_map_entry(c_) == 0u32 as usize;
              let res0: bool = res;
              fin1 = res0
            },
          _ =>
            match pl1
            {
              CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Serialized
              { v }
              =>
                {
                  let c_: &[u8] = v;
                  let res: bool = cbor_serialized_map_iterator_is_empty(c_);
                  fin1 = res
                },
              _ => panic!("")
            }
        };
        let mut fin2: bool;
        match pl2
        {
          CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Slice
          { v }
          =>
            {
              let c_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = v;
              let res: bool = len__CBOR_Pulse_Raw_Type_cbor_map_entry(c_) == 0u32 as usize;
              let res0: bool = res;
              fin2 = res0
            },
          _ =>
            match pl2
            {
              CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Serialized
              { v }
              =>
                {
                  let c_: &[u8] = v;
                  let res: bool = cbor_serialized_map_iterator_is_empty(c_);
                  fin2 = res
                },
              _ => panic!("")
            }
        };
        let mut res0: i16;
        if fin1
        { if fin2 { res0 = 0i32 as i16 } else { res0 = 0i32.wrapping_sub(1i32) as i16 } }
        else if fin2
        { res0 = 1i32 as i16 }
        else
        {
          let
          mut pi1: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
          =
              pl1;
          let
          mut pi2: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
          =
              pl2;
          let mut pres: i16 = 0i32 as i16;
          let mut pfin1: bool = false;
          let res1: i16 = pres;
          let fin110: bool = pfin1;
          let mut cond: bool = res1 == 0i32 as i16 && ! fin110;
          while
          cond
          {
            let i00: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry =
                pi1;
            let mut elt1: cbor_map_entry =
                cbor_map_entry
                {
                  cbor_map_entry_key: cbor_raw::case_CBOR_Case_Simple { v: 0u8 },
                  cbor_map_entry_value: cbor_raw::case_CBOR_Case_Simple { v: 0u8 }
                };
            match i00
            {
              CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Slice
              { v }
              =>
                {
                  let i: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = v;
                  let res: cbor_map_entry =
                      op_Array_Access__CBOR_Pulse_Raw_Type_cbor_map_entry(i, 0u32 as usize);
                  let
                  sp:
                  __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry
                  =
                      split__CBOR_Pulse_Raw_Type_cbor_map_entry(i, 1u32 as usize);
                  let s_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = sp.snd;
                  let i11: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = s_;
                  let i_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = i11;
                  pi1 =
                      CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Slice
                      { v: i_ };
                  let res00: cbor_map_entry = res;
                  elt1 = res00
                },
              _ =>
                match i00
                {
                  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Serialized
                  { v }
                  =>
                    {
                      let i: &[u8] = v;
                      let res: cbor_map_entry =
                          cbor_serialized_map_iterator_next(
                            std::slice::from_mut::<CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry>(
                              &mut pi1
                            ),
                            i
                          );
                      elt1 = res
                    },
                  _ => panic!("")
                }
            };
            let i0: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry =
                pi2;
            let mut elt2: cbor_map_entry =
                cbor_map_entry
                {
                  cbor_map_entry_key: cbor_raw::case_CBOR_Case_Simple { v: 0u8 },
                  cbor_map_entry_value: cbor_raw::case_CBOR_Case_Simple { v: 0u8 }
                };
            match i0
            {
              CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Slice
              { v }
              =>
                {
                  let i: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = v;
                  let res: cbor_map_entry =
                      op_Array_Access__CBOR_Pulse_Raw_Type_cbor_map_entry(i, 0u32 as usize);
                  let
                  sp:
                  __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry
                  =
                      split__CBOR_Pulse_Raw_Type_cbor_map_entry(i, 1u32 as usize);
                  let s_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = sp.snd;
                  let i11: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = s_;
                  let i_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = i11;
                  pi2 =
                      CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Slice
                      { v: i_ };
                  let res00: cbor_map_entry = res;
                  elt2 = res00
                },
              _ =>
                match i0
                {
                  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Serialized
                  { v }
                  =>
                    {
                      let i: &[u8] = v;
                      let res: cbor_map_entry =
                          cbor_serialized_map_iterator_next(
                            std::slice::from_mut::<CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry>(
                              &mut pi2
                            ),
                            i
                          );
                      elt2 = res
                    },
                  _ => panic!("")
                }
            };
            let pelt1: cbor_map_entry = elt1;
            let pelt2: cbor_map_entry = elt2;
            let c20: i16 =
                CBOR_Pulse_Raw_Compare_impl_cbor_compare(
                  pelt1.cbor_map_entry_key,
                  pelt2.cbor_map_entry_key
                );
            let mut c2: i16;
            if c20 == 0i32 as i16
            {
              let c3: i16 =
                  CBOR_Pulse_Raw_Compare_impl_cbor_compare(
                    pelt1.cbor_map_entry_value,
                    pelt2.cbor_map_entry_value
                  );
              c2 = c3
            }
            else
            { c2 = c20 };
            if c2 == 0i32 as i16
            {
              let
              i11: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
              =
                  pi1;
              let mut fin11: bool;
              match i11
              {
                CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Slice
                { v }
                =>
                  {
                    let c_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = v;
                    let res: bool = len__CBOR_Pulse_Raw_Type_cbor_map_entry(c_) == 0u32 as usize;
                    let res00: bool = res;
                    fin11 = res00
                  },
                _ =>
                  match i11
                  {
                    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Serialized
                    { v }
                    =>
                      {
                        let c_: &[u8] = v;
                        let res: bool = cbor_serialized_map_iterator_is_empty(c_);
                        fin11 = res
                      },
                    _ => panic!("")
                  }
              };
              let
              i21: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
              =
                  pi2;
              let mut fin21: bool;
              match i21
              {
                CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Slice
                { v }
                =>
                  {
                    let c_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = v;
                    let res: bool = len__CBOR_Pulse_Raw_Type_cbor_map_entry(c_) == 0u32 as usize;
                    let res00: bool = res;
                    fin21 = res00
                  },
                _ =>
                  match i21
                  {
                    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Serialized
                    { v }
                    =>
                      {
                        let c_: &[u8] = v;
                        let res: bool = cbor_serialized_map_iterator_is_empty(c_);
                        fin21 = res
                      },
                    _ => panic!("")
                  }
              };
              if fin11 == fin21
              { pfin1 = fin11 }
              else if fin11 { pres = 0i32.wrapping_sub(1i32) as i16 } else { pres = 1i32 as i16 }
            }
            else
            { pres = c2 };
            let res: i16 = pres;
            let fin11: bool = pfin1;
            cond = res == 0i32 as i16 && ! fin11
          };
          res0 = pres
        };
        let res: i16 = res0;
        return res
      }
      else
      { return c1 }
    }
    else
    {
      let mut val1: u8;
      match x1 { cbor_raw::case_CBOR_Case_Simple { v } => val1 = v, _ => panic!("") };
      let mut val2: u8;
      match x2 { cbor_raw::case_CBOR_Case_Simple { v } => val2 = v, _ => panic!("") };
      return impl_uint8_compare(val1, val2)
    }
  }
  else
  { return c }
}

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
    let sp1: (&mut [u8], &mut [u8]) = out.split_at_mut(res1);
    let sp12: &mut [u8] =
        {
          let v: &mut [u8] = sp1.1;
          v
        };
    let sp2: (&mut [u8], &mut [u8]) = sp12.split_at_mut(length);
    let sp21: &mut [u8] =
        {
          let v: &mut [u8] = sp2.0;
          v
        };
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
        let sp1: (&mut [u8], &mut [u8]) = out.split_at_mut(res1);
        let sp12: &mut [u8] =
            {
              let v: &mut [u8] = sp1.1;
              v
            };
        let sp2: (&mut [u8], &mut [u8]) = sp12.split_at_mut(length);
        let sp21: &mut [u8] =
            {
              let v: &mut [u8] = sp2.0;
              v
            };
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
          let sp1: (&mut [u8], &mut [u8]) = out.split_at_mut(res1);
          let sp12: &mut [u8] =
              {
                let v: &mut [u8] = sp1.1;
                v
              };
          let sp2: (&mut [u8], &mut [u8]) = sp12.split_at_mut(length);
          let sp21: &mut [u8] =
              {
                let v: &mut [u8] = sp2.0;
                v
              };
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
            let sp1: (&mut [u8], &mut [u8]) = out.split_at_mut(res1);
            let sp12: &mut [u8] =
                {
                  let v: &mut [u8] = sp1.1;
                  v
                };
            let sp2: (&mut [u8], &mut [u8]) = sp12.split_at_mut(length);
            let sp21: &mut [u8] =
                {
                  let v: &mut [u8] = sp2.0;
                  v
                };
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

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct
__Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry
<'a>
{
  pub fst: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>,
  pub snd: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>
}

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct
__Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw
<'a>
{
  pub fst: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw <'a>,
  pub snd: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw <'a>
}

pub type __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t <'a> =
(&'a [u8], &'a [u8]);

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

pub fn cbor_array_item <'a>(c: cbor_raw <'a>, i: u64) -> cbor_raw <'a>
{
  match c
  {
    cbor_raw::case_CBOR_Case_Serialized_Array { v } =>
      {
        let c_: cbor_serialized = v;
        let res: cbor_raw = cbor_serialized_array_item(c_, i);
        return res
      },
    _ =>
      match c
      {
        cbor_raw::case_CBOR_Case_Array { v } =>
          {
            let c_: cbor_array = v;
            return c_.cbor_array_ptr[i as usize]
          },
        _ => panic!("")
      }
  }
}

pub type cbor_array_iterator <'a> =
CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw <'a>;

pub fn cbor_array_iterator_init <'a>(c: cbor_raw <'a>) ->
    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw
    <'a>
{
  match c
  {
    cbor_raw::case_CBOR_Case_Serialized_Array { v } =>
      {
        let c_: cbor_serialized = v;
        let i_: &[u8] = cbor_serialized_array_iterator_init(c_);
        return
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Serialized
        { v: i_ }
      },
    _ =>
      match c
      {
        cbor_raw::case_CBOR_Case_Array { v } =>
          {
            let c_: cbor_array = v;
            let s: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw =
                from_array__CBOR_Pulse_Raw_Type_cbor_raw(
                  c_.cbor_array_ptr,
                  c_.cbor_array_length.value as usize
                );
            let s0: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = s;
            let i: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = s0;
            let res: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw =
                CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Slice
                { v: i };
            return res
          },
        _ => panic!("")
      }
  }
}

pub fn cbor_array_iterator_is_empty(
  c: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw
) ->
    bool
{
  match c
  {
    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Slice
    { v }
    =>
      {
        let c_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = v;
        let res: bool = len__CBOR_Pulse_Raw_Type_cbor_raw(c_) == 0u32 as usize;
        let res0: bool = res;
        return res0
      },
    _ =>
      match c
      {
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Serialized
        { v }
        =>
          {
            let c_: &[u8] = v;
            let res: bool = cbor_serialized_array_iterator_is_empty(c_);
            return res
          },
        _ => panic!("")
      }
  }
}

pub fn cbor_array_iterator_next <'b, 'a>(
  pi: &'b mut [CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw <'a>]
) ->
    cbor_raw
    <'a>
{
  let i0: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw = pi[0usize];
  match i0
  {
    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Slice
    { v }
    =>
      {
        let i1: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = v;
        let res: cbor_raw = op_Array_Access__CBOR_Pulse_Raw_Type_cbor_raw(i1, 0u32 as usize);
        let
        sp:
        __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw
        =
            split__CBOR_Pulse_Raw_Type_cbor_raw(i1, 1u32 as usize);
        let s_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = sp.snd;
        let i11: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = s_;
        let i_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw = i11;
        pi[0usize] =
            CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Slice
            { v: i_ };
        let res0: cbor_raw = res;
        return res0
      },
    _ =>
      match i0
      {
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Serialized
        { v }
        =>
          {
            let i1: &[u8] = v;
            let res: cbor_raw = cbor_serialized_array_iterator_next(pi, i1);
            return res
          },
        _ => panic!("")
      }
  }
}

pub fn cbor_det_array_iterator_is_empty(
  x: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw
) ->
    bool
{
  let res: bool = cbor_array_iterator_is_empty(x);
  return res
}

pub fn cbor_det_array_iterator_next <'b, 'a>(
  x: &'b mut [CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw <'a>]
) ->
    cbor_raw
    <'a>
{
  let res: cbor_raw = cbor_array_iterator_next(x);
  return res
}

pub fn cbor_det_array_iterator_start <'a>(x: cbor_raw <'a>) ->
    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw
    <'a>
{
  let res: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw =
      cbor_array_iterator_init(x);
  return res
}

pub type cbor_det_array_iterator_t <'a> =
CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw <'a>;

pub fn cbor_det_equal(x1: cbor_raw, x2: cbor_raw) -> bool
{
  let comp: i16 = CBOR_Pulse_Raw_Compare_impl_cbor_compare(x1, x2);
  return comp == 0i32 as i16
}

pub fn cbor_det_get_array_item <'a>(x: cbor_raw <'a>, i: u64) -> cbor_raw <'a>
{
  let res: cbor_raw = cbor_array_item(x, i);
  return res
}

pub fn cbor_det_get_array_length(x: cbor_raw) -> u64
{
  let mut res: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
  match x
  {
    cbor_raw::case_CBOR_Case_Array { v } =>
      {
        let c_: cbor_array = v;
        res = c_.cbor_array_length
      },
    _ =>
      match x
      {
        cbor_raw::case_CBOR_Case_Serialized_Array { v } =>
          {
            let c_: cbor_serialized = v;
            res = c_.cbor_serialized_header
          },
        _ => panic!("")
      }
  };
  return res.value
}

pub fn cbor_det_get_map_length(x: cbor_raw) -> u64
{
  let mut res: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
  match x
  {
    cbor_raw::case_CBOR_Case_Map { v } =>
      {
        let c_: cbor_map = v;
        res = c_.cbor_map_length
      },
    _ =>
      match x
      {
        cbor_raw::case_CBOR_Case_Serialized_Map { v } =>
          {
            let c_: cbor_serialized = v;
            res = c_.cbor_serialized_header
          },
        _ => panic!("")
      }
  };
  return res.value
}

pub fn cbor_det_get_string <'a>(x: cbor_raw <'a>) -> &'a [u8]
{
  let mut c_: cbor_string =
      cbor_string { cbor_string_type: 0u8, cbor_string_size: 0u8, cbor_string_ptr: &[] };
  match x { cbor_raw::case_CBOR_Case_String { v } => c_ = v, _ => panic!("") };
  return c_.cbor_string_ptr
}

pub fn cbor_det_get_tagged_payload <'a>(x: cbor_raw <'a>) -> cbor_raw <'a>
{
  let res: cbor_raw = cbor_match_tagged_get_payload(x);
  return res
}

pub fn cbor_det_get_tagged_tag(x: cbor_raw) -> u64
{
  let mut res: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
  match x
  {
    cbor_raw::case_CBOR_Case_Tagged { v } =>
      {
        let c_: cbor_tagged = v;
        res = c_.cbor_tagged_tag
      },
    _ =>
      match x
      {
        cbor_raw::case_CBOR_Case_Serialized_Tagged { v } =>
          {
            let c_: cbor_serialized = v;
            res = c_.cbor_serialized_header
          },
        _ => panic!("")
      }
  };
  return res.value
}

pub fn cbor_det_major_type(x: cbor_raw) -> u8
{
  let res: u8 = impl_major_type(x);
  return res
}

pub fn cbor_det_map_entry_key <'a>(x2: cbor_map_entry <'a>) -> cbor_raw <'a>
{ return x2.cbor_map_entry_key }

pub type cbor_det_map_entry_t <'a> = cbor_map_entry <'a>;

pub fn cbor_det_map_entry_value <'a>(x2: cbor_map_entry <'a>) -> cbor_raw <'a>
{ return x2.cbor_map_entry_value }

pub fn cbor_det_map_get <'a>(x: cbor_raw <'a>, k: cbor_raw <'a>) ->
    FStar_Pervasives_Native_option__CBOR_Pulse_Raw_Type_cbor_raw
    <'a>
{
  let i: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry =
      cbor_det_map_iterator_start_(x);
  let mut pi: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry = i;
  let mut pres: FStar_Pervasives_Native_option__CBOR_Pulse_Raw_Type_cbor_raw =
      FStar_Pervasives_Native_option__CBOR_Pulse_Raw_Type_cbor_raw::cbor_raw_None;
  let i_is_empty: bool = cbor_det_map_iterator_is_empty(i);
  let cont: bool = ! i_is_empty;
  let mut pcont: bool = cont;
  while
  pcont
  {
    let entry: cbor_map_entry =
        cbor_det_map_iterator_next(
          std::slice::from_mut::<CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry>(
            &mut pi
          )
        );
    let key: cbor_raw = cbor_det_map_entry_key(entry);
    let comp: i16 = impl_cbor_det_compare(key, k);
    if comp == 0i32 as i16
    {
      let value: cbor_raw = cbor_det_map_entry_value(entry);
      pres = FStar_Pervasives_Native_option__CBOR_Pulse_Raw_Type_cbor_raw::v { v: value };
      pcont = false
    }
    else if comp as i32 > 0i32 as i32
    { pcont = false }
    else
    {
      let i_: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry = pi;
      let is_empty: bool = cbor_det_map_iterator_is_empty(i_);
      let cont1: bool = ! is_empty;
      pcont = cont1
    }
  };
  return pres
}

pub fn cbor_det_map_iterator_is_empty(
  x: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
) ->
    bool
{
  let res: bool = cbor_map_iterator_is_empty(x);
  return res
}

pub fn cbor_det_map_iterator_next <'b, 'a>(
  x: &'b mut [CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>]
) ->
    cbor_map_entry
    <'a>
{
  let res: cbor_map_entry = cbor_map_iterator_next(x);
  return res
}

pub fn cbor_det_map_iterator_start <'a>(x: cbor_raw <'a>) ->
    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
    <'a>
{ return cbor_det_map_iterator_start_(x) }

pub fn cbor_det_map_iterator_start_ <'a>(x: cbor_raw <'a>) ->
    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
    <'a>
{
  let res: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry =
      cbor_map_iterator_init(x);
  return res
}

pub type cbor_det_map_iterator_t <'a> =
CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>;

pub fn cbor_det_mk_array <'a>(a: &'a [cbor_raw <'a>], len: u64) -> cbor_raw <'a>
{
  let len64: CBOR_Spec_Raw_Base_raw_uint64 = mk_raw_uint64(len);
  let res_: cbor_array = cbor_array { cbor_array_length: len64, cbor_array_ptr: a };
  return cbor_raw::case_CBOR_Case_Array { v: res_ }
}

pub fn cbor_det_mk_int64 <'a>(ty: u8, v: u64) -> cbor_raw <'a>
{
  let res: cbor_int =
      cbor_int
      {
        cbor_int_type: ty,
        cbor_int_size: (mk_raw_uint64(v)).size,
        cbor_int_value: (mk_raw_uint64(v)).value
      };
  let resi: cbor_int = res;
  let res0: cbor_raw = cbor_raw::case_CBOR_Case_Int { v: resi };
  return res0
}

pub fn cbor_det_mk_map <'a>(a: &'a mut [cbor_map_entry <'a>], len: u64) -> cbor_raw <'a>
{
  let _ignored_stmt: bool = cbor_raw_sort(a, len as usize);
  let raw_len: CBOR_Spec_Raw_Base_raw_uint64 = mk_raw_uint64(len);
  let res_: cbor_map = cbor_map { cbor_map_length: raw_len, cbor_map_ptr: a };
  return cbor_raw::case_CBOR_Case_Map { v: res_ }
}

pub fn cbor_det_mk_simple_value <'a>(v: u8) -> cbor_raw <'a>
{ return cbor_raw::case_CBOR_Case_Simple { v } }

pub fn cbor_det_mk_string <'a>(ty: u8, s: &'a [u8]) -> cbor_raw <'a>
{
  let len64: CBOR_Spec_Raw_Base_raw_uint64 = mk_raw_uint64(len__uint8_t(s) as u64);
  let ress: cbor_string =
      cbor_string { cbor_string_type: ty, cbor_string_size: len64.size, cbor_string_ptr: s };
  return cbor_raw::case_CBOR_Case_String { v: ress }
}

pub fn cbor_det_mk_tagged <'a>(tag: u64, r: &'a [cbor_raw <'a>]) -> cbor_raw <'a>
{
  let tag64: CBOR_Spec_Raw_Base_raw_uint64 = mk_raw_uint64(tag);
  let res_: cbor_tagged = cbor_tagged { cbor_tagged_tag: tag64, cbor_tagged_ptr: r };
  return cbor_raw::case_CBOR_Case_Tagged { v: res_ }
}

pub fn cbor_det_parse <'a>(input: &'a [u8], len: usize) -> cbor_raw <'a>
{
  let res: cbor_raw = cbor_parse(input, len);
  return res
}

pub fn cbor_det_read_simple_value(x: cbor_raw) -> u8
{ match x { cbor_raw::case_CBOR_Case_Simple { v } => return v, _ => panic!("") } }

pub fn cbor_det_read_uint64(x: cbor_raw) -> u64
{
  let mut c_: cbor_int = Default::default();
  match x { cbor_raw::case_CBOR_Case_Int { v } => c_ = v, _ => panic!("") };
  let res: CBOR_Spec_Raw_Base_raw_uint64 =
      CBOR_Spec_Raw_Base_raw_uint64 { size: c_.cbor_int_size, value: c_.cbor_int_value };
  return res.value
}

pub fn cbor_det_serialize(x: cbor_raw, output: &mut [u8]) -> usize
{
  let res: usize = cbor_serialize(x, output);
  return res
}

pub fn cbor_det_size(x: cbor_raw, bound: usize) -> usize
{
  let res: usize = cbor_size(x, bound);
  return res
}

pub type cbor_det_t <'a> = cbor_raw <'a>;

pub fn cbor_det_validate(input: &[u8]) -> usize
{
  let res: usize = cbor_validate_det(input);
  return res
}

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

pub fn cbor_map_entry_raw_compare(x1: cbor_map_entry, x2: cbor_map_entry) -> i16
{
  let res: i16 = cbor_raw_compare(x1.cbor_map_entry_key, x2.cbor_map_entry_key);
  return res
}

pub type cbor_map_iterator <'a> =
CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>;

pub fn cbor_map_iterator_init <'a>(c: cbor_raw <'a>) ->
    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
    <'a>
{
  match c
  {
    cbor_raw::case_CBOR_Case_Serialized_Map { v } =>
      {
        let c_: cbor_serialized = v;
        let i_: &[u8] = cbor_serialized_map_iterator_init(c_);
        return
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Serialized
        { v: i_ }
      },
    _ =>
      match c
      {
        cbor_raw::case_CBOR_Case_Map { v } =>
          {
            let c_: cbor_map = v;
            let s: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry =
                from_array__CBOR_Pulse_Raw_Type_cbor_map_entry(
                  c_.cbor_map_ptr,
                  c_.cbor_map_length.value as usize
                );
            let s0: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = s;
            let i: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = s0;
            let res: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry =
                CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Slice
                { v: i };
            return res
          },
        _ => panic!("")
      }
  }
}

pub fn cbor_map_iterator_is_empty(
  c: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
) ->
    bool
{
  match c
  {
    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Slice
    { v }
    =>
      {
        let c_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = v;
        let res: bool = len__CBOR_Pulse_Raw_Type_cbor_map_entry(c_) == 0u32 as usize;
        let res0: bool = res;
        return res0
      },
    _ =>
      match c
      {
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Serialized
        { v }
        =>
          {
            let c_: &[u8] = v;
            let res: bool = cbor_serialized_map_iterator_is_empty(c_);
            return res
          },
        _ => panic!("")
      }
  }
}

pub fn cbor_map_iterator_next <'b, 'a>(
  pi:
  &'b mut [CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>]
) ->
    cbor_map_entry
    <'a>
{
  let i0: CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry =
      pi[0usize];
  match i0
  {
    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Slice
    { v }
    =>
      {
        let i1: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = v;
        let res: cbor_map_entry =
            op_Array_Access__CBOR_Pulse_Raw_Type_cbor_map_entry(i1, 0u32 as usize);
        let
        sp:
        __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry
        =
            split__CBOR_Pulse_Raw_Type_cbor_map_entry(i1, 1u32 as usize);
        let s_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = sp.snd;
        let i11: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = s_;
        let i_: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry = i11;
        pi[0usize] =
            CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Slice
            { v: i_ };
        let res0: cbor_map_entry = res;
        return res0
      },
    _ =>
      match i0
      {
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Serialized
        { v }
        =>
          {
            let i1: &[u8] = v;
            let res: cbor_map_entry = cbor_serialized_map_iterator_next(pi, i1);
            return res
          },
        _ => panic!("")
      }
  }
}

pub fn cbor_match_serialized_tagged_get_payload <'a>(c: cbor_serialized <'a>) -> cbor_raw <'a>
{
  let res: cbor_raw = cbor_read(c.cbor_serialized_payload);
  return res
}

pub fn cbor_match_tagged_get_payload <'a>(c: cbor_raw <'a>) -> cbor_raw <'a>
{
  let mut ite: bool;
  match c { cbor_raw::case_CBOR_Case_Serialized_Tagged { v } => ite = true, _ => ite = false };
  if ite
  {
    let mut cs: cbor_serialized =
        cbor_serialized
        {
          cbor_serialized_header: CBOR_Spec_Raw_Base_raw_uint64 { size: 0u8, value: 0u64 },
          cbor_serialized_payload: &[]
        };
    match c { cbor_raw::case_CBOR_Case_Serialized_Tagged { v } => cs = v, _ => panic!("") };
    let res: cbor_raw = cbor_match_serialized_tagged_get_payload(cs);
    return res
  }
  else
  {
    let mut ct: cbor_tagged =
        cbor_tagged
        {
          cbor_tagged_tag: CBOR_Spec_Raw_Base_raw_uint64 { size: 0u8, value: 0u64 },
          cbor_tagged_ptr: &[]
        };
    match c { cbor_raw::case_CBOR_Case_Tagged { v } => ct = v, _ => panic!("") };
    return ct.cbor_tagged_ptr[0usize]
  }
}

pub fn cbor_parse <'a>(input: &'a [u8], len: usize) -> cbor_raw <'a>
{
  let s_: (&[u8], &[u8]) = input.split_at(0u32 as usize);
  let s10: &[u8] =
      {
        let v: &[u8] = s_.0;
        v
      };
  let s20: &[u8] =
      {
        let v: &[u8] = s_.1;
        v
      };
  let split123: (&[u8], &[u8]) = (s10,s20);
  let input23: &[u8] =
      {
        let v: &[u8] = split123.1;
        v
      };
  let consumed: usize = len.wrapping_sub(0u32 as usize);
  let s1s2: (&[u8], &[u8]) = input23.split_at(consumed);
  let s1: &[u8] =
      {
        let v: &[u8] = s1s2.0;
        v
      };
  let s2: &[u8] =
      {
        let v: &[u8] = s1s2.1;
        v
      };
  let res: (&[u8], &[u8]) = (s1,s2);
  let left: &[u8] =
      {
        let v: &[u8] = res.0;
        v
      };
  let right: &[u8] =
      {
        let v: &[u8] = res.1;
        v
      };
  let split23: (&[u8], &[u8]) = (left,right);
  let input1: &[u8] =
      {
        let v: &[u8] = split23.0;
        v
      };
  let res0: cbor_raw = cbor_read(input1);
  return res0
}

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

pub fn cbor_raw_compare(x1: cbor_raw, x2: cbor_raw) -> i16
{ return CBOR_Pulse_Raw_Compare_impl_cbor_compare(x1, x2) }

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

pub fn cbor_raw_ints_optimal(a: &[u8]) -> bool
{
  let i: usize = jump_header(a, 0u32 as usize);
  let s: (&[u8], &[u8]) = a.split_at(i);
  let s1: &[u8] =
      {
        let v: &[u8] = s.0;
        v
      };
  let s2: &[u8] =
      {
        let v: &[u8] = s.1;
        v
      };
  let res: (&[u8], &[u8]) = (s1,s2);
  let input1: &[u8] =
      {
        let v: &[u8] = res.0;
        v
      };
  let input2: &[u8] =
      {
        let v: &[u8] = res.1;
        v
      };
  let spl: (&[u8], &[u8]) = (input1,input2);
  let input10: &[u8] =
      {
        let v: &[u8] = spl.0;
        v
      };
  let h: header = read_header(input10);
  if get_header_major_type(h) as u32 == 7u32
  { return true }
  else
  {
    let scrut: long_argument = h.snd;
    let mut ite: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
    match scrut
    {
      long_argument::case_LongArgumentU8 { v } =>
        {
          let v0: u8 = v;
          ite = CBOR_Spec_Raw_Base_raw_uint64 { size: 1u8, value: v0 as u64 }
        },
      _ =>
        match scrut
        {
          long_argument::case_LongArgumentU16 { v } =>
            {
              let v0: u16 = v;
              ite = CBOR_Spec_Raw_Base_raw_uint64 { size: 2u8, value: v0 as u64 }
            },
          _ =>
            match scrut
            {
              long_argument::case_LongArgumentU32 { v } =>
                {
                  let v0: u32 = v;
                  ite = CBOR_Spec_Raw_Base_raw_uint64 { size: 3u8, value: v0 as u64 }
                },
              _ =>
                match scrut
                {
                  long_argument::case_LongArgumentU64 { v } =>
                    {
                      let v0: u64 = v;
                      ite = CBOR_Spec_Raw_Base_raw_uint64 { size: 4u8, value: v0 }
                    },
                  _ =>
                    match scrut
                    {
                      long_argument::case_LongArgumentOther =>
                        ite =
                            CBOR_Spec_Raw_Base_raw_uint64
                            { size: 0u8, value: h.fst.additional_info as u64 },
                      _ => panic!("")
                    }
                }
            }
        }
    };
    return impl_raw_uint64_optimal(ite)
  }
}

pub type cbor_raw_serialized_iterator <'a> = &'a [u8];

pub fn cbor_raw_sort(a: &mut [cbor_map_entry], len: usize) -> bool
{
  let res: bool = cbor_raw_sort_aux(a, 0u32 as usize, len);
  return res
}

pub fn cbor_raw_sort_aux(a: &mut [cbor_map_entry], lo: usize, hi: usize) -> bool
{
  let len: usize = hi.wrapping_sub(lo);
  if len < 2u32 as usize
  { return true }
  else
  {
    let len_half: usize = len.wrapping_div(2u32 as usize);
    let mi: usize = lo.wrapping_add(len_half);
    let res: bool = cbor_raw_sort_aux(a, lo, mi);
    if ! res
    { return false }
    else
    {
      let res1: bool = cbor_raw_sort_aux(a, mi, hi);
      if ! res1
      { return false }
      else
      {
        let mut pi1: usize = lo;
        let mut pi2: usize = mi;
        let mut pres: bool = true;
        let i10: usize = pi1;
        let i20: usize = pi2;
        let res20: bool = pres;
        let mut cond: bool = res20 && ! (i10 == i20 || i20 == hi);
        while
        cond
        {
          let i1: usize = pi1;
          let x1: cbor_map_entry = a[i1];
          let i200: usize = pi2;
          let x2: cbor_map_entry = a[i200];
          let comp: i16 = cbor_map_entry_raw_compare(x1, x2);
          if comp == 0i32 as i16
          { pres = false }
          else if (comp as i32) < 0i32 as i32
          {
            let i1_: usize = i1.wrapping_add(1u32 as usize);
            pi1 = i1_
          }
          else
          {
            let i2_: usize = i200.wrapping_add(1u32 as usize);
            let mut i1_: usize;
            if i1 == i200
            { i1_ = i2_ }
            else if i200 == i2_
            { i1_ = i1 }
            else
            {
              let mut pn: usize = i2_.wrapping_sub(i1);
              let mut pl: usize = i200.wrapping_sub(i1);
              let l30: usize = pl;
              let mut cond0: bool = l30 > 0u32 as usize;
              while
              cond0
              {
                let n: usize = pn;
                let l3: usize = pl;
                let l_: usize = n.wrapping_rem(l3);
                pn = l3;
                pl = l_;
                let l300: usize = pl;
                cond0 = l300 > 0u32 as usize
              };
              let d: usize = pn;
              let q: usize = i2_.wrapping_sub(i1).wrapping_div(d);
              let mut pi: usize = i1;
              let i0: usize = pi;
              let mut cond00: bool = i0.wrapping_sub(i1) < d;
              while
              cond00
              {
                let i: usize = pi;
                let save: cbor_map_entry = a[i];
                let mut pj: usize = 0u32 as usize;
                let mut pidx: usize = i;
                let j0: usize = pj;
                let mut cond1: bool = j0 < q.wrapping_sub(1u32 as usize);
                while
                cond1
                {
                  let j: usize = pj;
                  let idx: usize = pidx;
                  let mut idx_: usize;
                  if idx.wrapping_sub(i1) >= i2_.wrapping_sub(i200)
                  { idx_ = idx.wrapping_sub(i2_.wrapping_sub(i200)) }
                  else
                  { idx_ = idx.wrapping_add(i200).wrapping_sub(i1) };
                  let x: cbor_map_entry = a[idx_];
                  let j_: usize = j.wrapping_add(1u32 as usize);
                  a[idx] = x;
                  pj = j_;
                  pidx = idx_;
                  let j00: usize = pj;
                  cond1 = j00 < q.wrapping_sub(1u32 as usize)
                };
                let idx: usize = pidx;
                a[idx] = save;
                let i_: usize = i.wrapping_add(1u32 as usize);
                pi = i_;
                let i00: usize = pi;
                cond00 = i00.wrapping_sub(i1) < d
              };
              i1_ = i1.wrapping_add(i2_).wrapping_sub(i200)
            };
            pi1 = i1_;
            pi2 = i2_
          };
          let i100: usize = pi1;
          let i2: usize = pi2;
          let res2: bool = pres;
          cond = res2 && ! (i100 == i2 || i2 == hi)
        };
        let res2: bool = pres;
        return res2
      }
    }
  }
}

pub fn cbor_raw_sorted(a: &[u8]) -> bool
{
  let i0: usize = jump_header(a, 0u32 as usize);
  let s: (&[u8], &[u8]) = a.split_at(i0);
  let s10: &[u8] =
      {
        let v: &[u8] = s.0;
        v
      };
  let s20: &[u8] =
      {
        let v: &[u8] = s.1;
        v
      };
  let res0: (&[u8], &[u8]) = (s10,s20);
  let input10: &[u8] =
      {
        let v: &[u8] = res0.0;
        v
      };
  let input20: &[u8] =
      {
        let v: &[u8] = res0.1;
        v
      };
  let spl: (&[u8], &[u8]) = (input10,input20);
  let ah: &[u8] =
      {
        let v: &[u8] = spl.0;
        v
      };
  let ap: &[u8] =
      {
        let v: &[u8] = spl.1;
        v
      };
  let h: header = read_header(ah);
  if get_header_major_type(h) as u32 == 5u32
  {
    let b: initial_byte_t = h.fst;
    let l: long_argument = h.snd;
    let n: u64 = argument_as_uint64(b, l);
    if n as usize == 0u32 as usize
    { return true }
    else
    {
      let off10: usize = jump_raw_data_item(ap, 0u32 as usize);
      let i00: usize = jump_raw_data_item(ap, off10);
      let s100: (&[u8], &[u8]) = ap.split_at(i00);
      let s110: &[u8] =
          {
            let v: &[u8] = s100.0;
            v
          };
      let s200: &[u8] =
          {
            let v: &[u8] = s100.1;
            v
          };
      let res00: (&[u8], &[u8]) = (s110,s200);
      let input100: &[u8] =
          {
            let v: &[u8] = res00.0;
            v
          };
      let input200: &[u8] =
          {
            let v: &[u8] = res00.1;
            v
          };
      let res1: (&[u8], &[u8]) = (input100,input200);
      let input11: &[u8] =
          {
            let v: &[u8] = res1.0;
            v
          };
      let input21: &[u8] =
          {
            let v: &[u8] = res1.1;
            v
          };
      let res2: (&[u8], &[u8]) = (input11,input21);
      let pl: (&[u8], &[u8]) = res2;
      let s1: &[u8] =
          {
            let v: &[u8] = pl.0;
            v
          };
      let s2: &[u8] =
          {
            let v: &[u8] = pl.1;
            v
          };
      let mut phd: &[u8] = s1;
      let mut ptl: &[u8] = s2;
      let n_: usize = (n as usize).wrapping_sub(1u32 as usize);
      let mut pi: usize = n_;
      let mut pres: bool = true;
      let i: usize = pi;
      let res3: bool = pres;
      let mut cond: bool = res3 && i > 0u32 as usize;
      while
      cond
      {
        let stl: &[u8] = ptl;
        let off1: usize = jump_raw_data_item(stl, 0u32 as usize);
        let i1: usize = jump_raw_data_item(stl, off1);
        let s3: (&[u8], &[u8]) = stl.split_at(i1);
        let s1100: &[u8] =
            {
              let v: &[u8] = s3.0;
              v
            };
        let s210: &[u8] =
            {
              let v: &[u8] = s3.1;
              v
            };
        let res01: (&[u8], &[u8]) = (s1100,s210);
        let input101: &[u8] =
            {
              let v: &[u8] = res01.0;
              v
            };
        let input201: &[u8] =
            {
              let v: &[u8] = res01.1;
              v
            };
        let res10: (&[u8], &[u8]) = (input101,input201);
        let input1: &[u8] =
            {
              let v: &[u8] = res10.0;
              v
            };
        let input2: &[u8] =
            {
              let v: &[u8] = res10.1;
              v
            };
        let res20: (&[u8], &[u8]) = (input1,input2);
        let pl1: (&[u8], &[u8]) = res20;
        let s11: &[u8] =
            {
              let v: &[u8] = pl1.0;
              v
            };
        let s21: &[u8] =
            {
              let v: &[u8] = pl1.1;
              v
            };
        let shd: &[u8] = phd;
        let res30: bool = impl_deterministically_encoded_cbor_map_key_order(shd, s11);
        if res30
        {
          phd = s11;
          ptl = s21;
          let i2: usize = pi;
          let i_: usize = i2.wrapping_sub(1u32 as usize);
          pi = i_
        }
        else
        { pres = false };
        let i01: usize = pi;
        let res: bool = pres;
        cond = res && i01 > 0u32 as usize
      };
      return pres
    }
  }
  else
  { return true }
}

pub type cbor_raw_tags = u8;

pub fn cbor_raw_with_perm_get_header(xl: cbor_raw) -> header
{
  let res: header = cbor_raw_get_header(xl);
  return res
}

pub fn cbor_read <'a>(input: &'a [u8]) -> cbor_raw <'a>
{
  let mut ph: header =
      header
      {
        fst: initial_byte_t { major_type: 7u8, additional_info: 0u8 },
        snd: long_argument::case_LongArgumentOther
      };
  let i0: usize = jump_header(input, 0u32 as usize);
  let s: (&[u8], &[u8]) = input.split_at(i0);
  let s1: &[u8] =
      {
        let v: &[u8] = s.0;
        v
      };
  let s2: &[u8] =
      {
        let v: &[u8] = s.1;
        v
      };
  let res: (&[u8], &[u8]) = (s1,s2);
  let input1: &[u8] =
      {
        let v: &[u8] = res.0;
        v
      };
  let input2: &[u8] =
      {
        let v: &[u8] = res.1;
        v
      };
  let spl: (&[u8], &[u8]) = (input1,input2);
  let ph1: &[u8] =
      {
        let v: &[u8] = spl.0;
        v
      };
  let outc: &[u8] =
      {
        let v: &[u8] = spl.1;
        v
      };
  let h0: header = read_header(ph1);
  ph = h0;
  let pc: &[u8] = outc;
  let h: header = ph;
  let typ: u8 = h.fst.major_type;
  if typ as u32 == 0u32 || typ as u32 == 1u32
  {
    let b: initial_byte_t = h.fst;
    let l: long_argument = h.snd;
    let mut i: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
    match l
    {
      long_argument::case_LongArgumentU8 { v } =>
        {
          let v1: u8 = v;
          i = CBOR_Spec_Raw_Base_raw_uint64 { size: 1u8, value: v1 as u64 }
        },
      _ =>
        match l
        {
          long_argument::case_LongArgumentU16 { v } =>
            {
              let v1: u16 = v;
              i = CBOR_Spec_Raw_Base_raw_uint64 { size: 2u8, value: v1 as u64 }
            },
          _ =>
            match l
            {
              long_argument::case_LongArgumentU32 { v } =>
                {
                  let v1: u32 = v;
                  i = CBOR_Spec_Raw_Base_raw_uint64 { size: 3u8, value: v1 as u64 }
                },
              _ =>
                match l
                {
                  long_argument::case_LongArgumentU64 { v } =>
                    {
                      let v1: u64 = v;
                      i = CBOR_Spec_Raw_Base_raw_uint64 { size: 4u8, value: v1 }
                    },
                  _ =>
                    match l
                    {
                      long_argument::case_LongArgumentOther =>
                        i =
                            CBOR_Spec_Raw_Base_raw_uint64
                            { size: 0u8, value: b.additional_info as u64 },
                      _ => panic!("")
                    }
                }
            }
        }
    };
    let resi: cbor_int =
        cbor_int { cbor_int_type: typ, cbor_int_size: i.size, cbor_int_value: i.value };
    return cbor_raw::case_CBOR_Case_Int { v: resi }
  }
  else if typ as u32 == 3u32 || typ as u32 == 2u32
  {
    let b: initial_byte_t = h.fst;
    let l: long_argument = h.snd;
    let mut i: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
    match l
    {
      long_argument::case_LongArgumentU8 { v } =>
        {
          let v1: u8 = v;
          i = CBOR_Spec_Raw_Base_raw_uint64 { size: 1u8, value: v1 as u64 }
        },
      _ =>
        match l
        {
          long_argument::case_LongArgumentU16 { v } =>
            {
              let v1: u16 = v;
              i = CBOR_Spec_Raw_Base_raw_uint64 { size: 2u8, value: v1 as u64 }
            },
          _ =>
            match l
            {
              long_argument::case_LongArgumentU32 { v } =>
                {
                  let v1: u32 = v;
                  i = CBOR_Spec_Raw_Base_raw_uint64 { size: 3u8, value: v1 as u64 }
                },
              _ =>
                match l
                {
                  long_argument::case_LongArgumentU64 { v } =>
                    {
                      let v1: u64 = v;
                      i = CBOR_Spec_Raw_Base_raw_uint64 { size: 4u8, value: v1 }
                    },
                  _ =>
                    match l
                    {
                      long_argument::case_LongArgumentOther =>
                        i =
                            CBOR_Spec_Raw_Base_raw_uint64
                            { size: 0u8, value: b.additional_info as u64 },
                      _ => panic!("")
                    }
                }
            }
        }
    };
    let ress: cbor_string =
        cbor_string { cbor_string_type: typ, cbor_string_size: i.size, cbor_string_ptr: pc };
    return cbor_raw::case_CBOR_Case_String { v: ress }
  }
  else if typ as u32 == 6u32
  {
    let b: initial_byte_t = h.fst;
    let l: long_argument = h.snd;
    let mut tag: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
    match l
    {
      long_argument::case_LongArgumentU8 { v } =>
        {
          let v1: u8 = v;
          tag = CBOR_Spec_Raw_Base_raw_uint64 { size: 1u8, value: v1 as u64 }
        },
      _ =>
        match l
        {
          long_argument::case_LongArgumentU16 { v } =>
            {
              let v1: u16 = v;
              tag = CBOR_Spec_Raw_Base_raw_uint64 { size: 2u8, value: v1 as u64 }
            },
          _ =>
            match l
            {
              long_argument::case_LongArgumentU32 { v } =>
                {
                  let v1: u32 = v;
                  tag = CBOR_Spec_Raw_Base_raw_uint64 { size: 3u8, value: v1 as u64 }
                },
              _ =>
                match l
                {
                  long_argument::case_LongArgumentU64 { v } =>
                    {
                      let v1: u64 = v;
                      tag = CBOR_Spec_Raw_Base_raw_uint64 { size: 4u8, value: v1 }
                    },
                  _ =>
                    match l
                    {
                      long_argument::case_LongArgumentOther =>
                        tag =
                            CBOR_Spec_Raw_Base_raw_uint64
                            { size: 0u8, value: b.additional_info as u64 },
                      _ => panic!("")
                    }
                }
            }
        }
    };
    let rest: cbor_serialized =
        cbor_serialized { cbor_serialized_header: tag, cbor_serialized_payload: pc };
    return cbor_raw::case_CBOR_Case_Serialized_Tagged { v: rest }
  }
  else if typ as u32 == 4u32
  {
    let b: initial_byte_t = h.fst;
    let l: long_argument = h.snd;
    let mut len: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
    match l
    {
      long_argument::case_LongArgumentU8 { v } =>
        {
          let v1: u8 = v;
          len = CBOR_Spec_Raw_Base_raw_uint64 { size: 1u8, value: v1 as u64 }
        },
      _ =>
        match l
        {
          long_argument::case_LongArgumentU16 { v } =>
            {
              let v1: u16 = v;
              len = CBOR_Spec_Raw_Base_raw_uint64 { size: 2u8, value: v1 as u64 }
            },
          _ =>
            match l
            {
              long_argument::case_LongArgumentU32 { v } =>
                {
                  let v1: u32 = v;
                  len = CBOR_Spec_Raw_Base_raw_uint64 { size: 3u8, value: v1 as u64 }
                },
              _ =>
                match l
                {
                  long_argument::case_LongArgumentU64 { v } =>
                    {
                      let v1: u64 = v;
                      len = CBOR_Spec_Raw_Base_raw_uint64 { size: 4u8, value: v1 }
                    },
                  _ =>
                    match l
                    {
                      long_argument::case_LongArgumentOther =>
                        len =
                            CBOR_Spec_Raw_Base_raw_uint64
                            { size: 0u8, value: b.additional_info as u64 },
                      _ => panic!("")
                    }
                }
            }
        }
    };
    let resa: cbor_serialized =
        cbor_serialized { cbor_serialized_header: len, cbor_serialized_payload: pc };
    return cbor_raw::case_CBOR_Case_Serialized_Array { v: resa }
  }
  else if typ as u32 == 5u32
  {
    let b: initial_byte_t = h.fst;
    let l: long_argument = h.snd;
    let mut len: CBOR_Spec_Raw_Base_raw_uint64 = Default::default();
    match l
    {
      long_argument::case_LongArgumentU8 { v } =>
        {
          let v1: u8 = v;
          len = CBOR_Spec_Raw_Base_raw_uint64 { size: 1u8, value: v1 as u64 }
        },
      _ =>
        match l
        {
          long_argument::case_LongArgumentU16 { v } =>
            {
              let v1: u16 = v;
              len = CBOR_Spec_Raw_Base_raw_uint64 { size: 2u8, value: v1 as u64 }
            },
          _ =>
            match l
            {
              long_argument::case_LongArgumentU32 { v } =>
                {
                  let v1: u32 = v;
                  len = CBOR_Spec_Raw_Base_raw_uint64 { size: 3u8, value: v1 as u64 }
                },
              _ =>
                match l
                {
                  long_argument::case_LongArgumentU64 { v } =>
                    {
                      let v1: u64 = v;
                      len = CBOR_Spec_Raw_Base_raw_uint64 { size: 4u8, value: v1 }
                    },
                  _ =>
                    match l
                    {
                      long_argument::case_LongArgumentOther =>
                        len =
                            CBOR_Spec_Raw_Base_raw_uint64
                            { size: 0u8, value: b.additional_info as u64 },
                      _ => panic!("")
                    }
                }
            }
        }
    };
    let resa: cbor_serialized =
        cbor_serialized { cbor_serialized_header: len, cbor_serialized_payload: pc };
    return cbor_raw::case_CBOR_Case_Serialized_Map { v: resa }
  }
  else
  {
    let b: initial_byte_t = h.fst;
    let l: long_argument = h.snd;
    let mut i: u8;
    match l
    {
      long_argument::case_LongArgumentOther => i = b.additional_info,
      _ => match l { long_argument::case_LongArgumentSimpleValue { v } => i = v, _ => panic!("") }
    };
    return cbor_raw::case_CBOR_Case_Simple { v: i }
  }
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

pub fn cbor_serialized_array_item <'a>(c: cbor_serialized <'a>, i: u64) -> cbor_raw <'a>
{
  let j: usize = i as usize;
  let mut pi: usize = 0u32 as usize;
  let mut pres: &[u8] = c.cbor_serialized_payload;
  let i10: usize = pi;
  let mut cond: bool = i10 < j;
  while
  cond
  {
    let res: &[u8] = pres;
    let i1: usize = pi;
    let i2: usize = jump_raw_data_item(res, 0u32 as usize);
    let s: (&[u8], &[u8]) = res.split_at(i2);
    let s1: &[u8] =
        {
          let v: &[u8] = s.0;
          v
        };
    let s2: &[u8] =
        {
          let v: &[u8] = s.1;
          v
        };
    let res1: (&[u8], &[u8]) = (s1,s2);
    let input10: &[u8] =
        {
          let v: &[u8] = res1.0;
          v
        };
    let input20: &[u8] =
        {
          let v: &[u8] = res1.1;
          v
        };
    let res10: (&[u8], &[u8]) = (input10,input20);
    let input1: &[u8] =
        {
          let v: &[u8] = res10.0;
          v
        };
    let input2: &[u8] =
        {
          let v: &[u8] = res10.1;
          v
        };
    let spl: (&[u8], &[u8]) = (input1,input2);
    let res11: &[u8] =
        {
          let v: &[u8] = spl.1;
          v
        };
    let res2: &[u8] = res11;
    pi = i1.wrapping_add(1u32 as usize);
    pres = res2;
    let i100: usize = pi;
    cond = i100 < j
  };
  let res: &[u8] = pres;
  let i1: usize = jump_raw_data_item(res, 0u32 as usize);
  let s: (&[u8], &[u8]) = res.split_at(i1);
  let s1: &[u8] =
      {
        let v: &[u8] = s.0;
        v
      };
  let s2: &[u8] =
      {
        let v: &[u8] = s.1;
        v
      };
  let res1: (&[u8], &[u8]) = (s1,s2);
  let input10: &[u8] =
      {
        let v: &[u8] = res1.0;
        v
      };
  let input20: &[u8] =
      {
        let v: &[u8] = res1.1;
        v
      };
  let res10: (&[u8], &[u8]) = (input10,input20);
  let input1: &[u8] =
      {
        let v: &[u8] = res10.0;
        v
      };
  let input2: &[u8] =
      {
        let v: &[u8] = res10.1;
        v
      };
  let spl: (&[u8], &[u8]) = (input1,input2);
  let res11: &[u8] =
      {
        let v: &[u8] = spl.0;
        v
      };
  let res2: &[u8] = res11;
  let elt: &[u8] = res2;
  let res0: cbor_raw = cbor_read(elt);
  return res0
}

pub fn cbor_serialized_array_iterator_init <'a>(c: cbor_serialized <'a>) -> &'a [u8]
{ return c.cbor_serialized_payload }

pub fn cbor_serialized_array_iterator_is_empty(c: &[u8]) -> bool
{ return len__uint8_t(c) == 0u32 as usize }

pub fn cbor_serialized_array_iterator_next <'b, 'a>(
  pi: &'b mut [CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw <'a>],
  i: &'a [u8]
) ->
    cbor_raw
    <'a>
{
  let i1: usize = jump_raw_data_item(i, 0u32 as usize);
  let s: (&[u8], &[u8]) = i.split_at(i1);
  let s10: &[u8] =
      {
        let v: &[u8] = s.0;
        v
      };
  let s20: &[u8] =
      {
        let v: &[u8] = s.1;
        v
      };
  let res0: (&[u8], &[u8]) = (s10,s20);
  let input10: &[u8] =
      {
        let v: &[u8] = res0.0;
        v
      };
  let input20: &[u8] =
      {
        let v: &[u8] = res0.1;
        v
      };
  let res1: (&[u8], &[u8]) = (input10,input20);
  let input1: &[u8] =
      {
        let v: &[u8] = res1.0;
        v
      };
  let input2: &[u8] =
      {
        let v: &[u8] = res1.1;
        v
      };
  let sp: (&[u8], &[u8]) = (input1,input2);
  let s1: &[u8] =
      {
        let v: &[u8] = sp.0;
        v
      };
  let s2: &[u8] =
      {
        let v: &[u8] = sp.1;
        v
      };
  let res: cbor_raw = cbor_read(s1);
  let i_: &[u8] = s2;
  pi[0usize] =
      CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw::case_CBOR_Raw_Iterator_Serialized
      { v: i_ };
  return res
}

pub fn cbor_serialized_map_iterator_init <'a>(c: cbor_serialized <'a>) -> &'a [u8]
{ return c.cbor_serialized_payload }

pub fn cbor_serialized_map_iterator_is_empty(c: &[u8]) -> bool
{ return len__uint8_t(c) == 0u32 as usize }

pub fn cbor_serialized_map_iterator_next <'b, 'a>(
  pi:
  &'b mut [CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>],
  i: &'a [u8]
) ->
    cbor_map_entry
    <'a>
{
  let off1: usize = jump_raw_data_item(i, 0u32 as usize);
  let i10: usize = jump_raw_data_item(i, off1);
  let s: (&[u8], &[u8]) = i.split_at(i10);
  let s10: &[u8] =
      {
        let v: &[u8] = s.0;
        v
      };
  let s20: &[u8] =
      {
        let v: &[u8] = s.1;
        v
      };
  let res: (&[u8], &[u8]) = (s10,s20);
  let input10: &[u8] =
      {
        let v: &[u8] = res.0;
        v
      };
  let input20: &[u8] =
      {
        let v: &[u8] = res.1;
        v
      };
  let res0: (&[u8], &[u8]) = (input10,input20);
  let input11: &[u8] =
      {
        let v: &[u8] = res0.0;
        v
      };
  let input21: &[u8] =
      {
        let v: &[u8] = res0.1;
        v
      };
  let sp: (&[u8], &[u8]) = (input11,input21);
  let s1: &[u8] =
      {
        let v: &[u8] = sp.0;
        v
      };
  let s2: &[u8] =
      {
        let v: &[u8] = sp.1;
        v
      };
  let i1: usize = jump_raw_data_item(s1, 0u32 as usize);
  let s0: (&[u8], &[u8]) = s1.split_at(i1);
  let s110: &[u8] =
      {
        let v: &[u8] = s0.0;
        v
      };
  let s210: &[u8] =
      {
        let v: &[u8] = s0.1;
        v
      };
  let res3: (&[u8], &[u8]) = (s110,s210);
  let input12: &[u8] =
      {
        let v: &[u8] = res3.0;
        v
      };
  let input22: &[u8] =
      {
        let v: &[u8] = res3.1;
        v
      };
  let res4: (&[u8], &[u8]) = (input12,input22);
  let input1: &[u8] =
      {
        let v: &[u8] = res4.0;
        v
      };
  let input2: &[u8] =
      {
        let v: &[u8] = res4.1;
        v
      };
  let sp1: (&[u8], &[u8]) = (input1,input2);
  let s11: &[u8] =
      {
        let v: &[u8] = sp1.0;
        v
      };
  let s21: &[u8] =
      {
        let v: &[u8] = sp1.1;
        v
      };
  let res1: cbor_raw = cbor_read(s11);
  let res2: cbor_raw = cbor_read(s21);
  let res5: cbor_map_entry =
      cbor_map_entry { cbor_map_entry_key: res1, cbor_map_entry_value: res2 };
  let i_: &[u8] = s2;
  pi[0usize] =
      CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry::case_CBOR_Raw_Iterator_Serialized
      { v: i_ };
  return res5
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

pub fn cbor_validate(input: &[u8]) -> usize
{
  let mut poffset: usize = 0u32 as usize;
  let is_valid: bool =
      validate_raw_data_item(input, std::slice::from_mut::<usize>(&mut poffset));
  if is_valid { return poffset } else { return 0u32 as usize }
}

pub fn cbor_validate_det(input: &[u8]) -> usize
{
  let res: usize = cbor_validate_det_(input);
  return res
}

pub fn cbor_validate_det_(input: &[u8]) -> usize
{
  let len: usize = cbor_validate(input);
  if len == 0u32 as usize
  { return len }
  else
  {
    let s_: (&[u8], &[u8]) = input.split_at(0u32 as usize);
    let s10: &[u8] =
        {
          let v: &[u8] = s_.0;
          v
        };
    let s20: &[u8] =
        {
          let v: &[u8] = s_.1;
          v
        };
    let split123: (&[u8], &[u8]) = (s10,s20);
    let input23: &[u8] =
        {
          let v: &[u8] = split123.1;
          v
        };
    let consumed0: usize = len.wrapping_sub(0u32 as usize);
    let s1s2: (&[u8], &[u8]) = input23.split_at(consumed0);
    let s11: &[u8] =
        {
          let v: &[u8] = s1s2.0;
          v
        };
    let s21: &[u8] =
        {
          let v: &[u8] = s1s2.1;
          v
        };
    let res0: (&[u8], &[u8]) = (s11,s21);
    let left0: &[u8] =
        {
          let v: &[u8] = res0.0;
          v
        };
    let right0: &[u8] =
        {
          let v: &[u8] = res0.1;
          v
        };
    let split23: (&[u8], &[u8]) = (left0,right0);
    let input1: &[u8] =
        {
          let v: &[u8] = split23.0;
          v
        };
    let check: bool = false;
    std::slice::from_ref::<bool>(&check);
    let mut pn: usize = 1u32 as usize;
    let mut pres0: bool = true;
    let mut ppi0: &[u8] = input1;
    let res1: bool = pres0;
    let n0: usize = pn;
    let mut cond: bool = res1 && n0 > 0u32 as usize;
    while
    cond
    {
      let n00: usize = pn;
      let pi: &[u8] = ppi0;
      let i0: usize = jump_raw_data_item(pi, 0u32 as usize);
      let s: (&[u8], &[u8]) = pi.split_at(i0);
      let s100: &[u8] =
          {
            let v: &[u8] = s.0;
            v
          };
      let s200: &[u8] =
          {
            let v: &[u8] = s.1;
            v
          };
      let res: (&[u8], &[u8]) = (s100,s200);
      let input110: &[u8] =
          {
            let v: &[u8] = res.0;
            v
          };
      let input20: &[u8] =
          {
            let v: &[u8] = res.1;
            v
          };
      let res00: (&[u8], &[u8]) = (input110,input20);
      let input111: &[u8] =
          {
            let v: &[u8] = res00.0;
            v
          };
      let input21: &[u8] =
          {
            let v: &[u8] = res00.1;
            v
          };
      let spl: (&[u8], &[u8]) = (input111,input21);
      let res10: &[u8] =
          {
            let v: &[u8] = spl.0;
            v
          };
      let px: &[u8] = res10;
      let res2: bool = cbor_raw_ints_optimal(px);
      if ! res2
      { pres0 = false }
      else
      {
        let off1: usize = jump_header(pi, 0u32 as usize);
        let s_0: (&[u8], &[u8]) = pi.split_at(0u32 as usize);
        let s101: &[u8] =
            {
              let v: &[u8] = s_0.0;
              v
            };
        let s201: &[u8] =
            {
              let v: &[u8] = s_0.1;
              v
            };
        let split1230: (&[u8], &[u8]) = (s101,s201);
        let input230: &[u8] =
            {
              let v: &[u8] = split1230.1;
              v
            };
        let consumed: usize = off1.wrapping_sub(0u32 as usize);
        let s1s20: (&[u8], &[u8]) = input230.split_at(consumed);
        let s110: &[u8] =
            {
              let v: &[u8] = s1s20.0;
              v
            };
        let s210: &[u8] =
            {
              let v: &[u8] = s1s20.1;
              v
            };
        let res11: (&[u8], &[u8]) = (s110,s210);
        let left: &[u8] =
            {
              let v: &[u8] = res11.0;
              v
            };
        let right: &[u8] =
            {
              let v: &[u8] = res11.1;
              v
            };
        let split230: (&[u8], &[u8]) = (left,right);
        let input_: &[u8] =
            {
              let v: &[u8] = split230.0;
              v
            };
        let res100: header = read_header(input_);
        let x: header = res100;
        let b0: initial_byte_t = x.fst;
        let mut i: usize;
        if b0.major_type as u32 == 2u32 || b0.major_type as u32 == 3u32
        {
          let b: initial_byte_t = x.fst;
          let l: long_argument = x.snd;
          i = off1.wrapping_add(argument_as_uint64(b, l) as usize)
        }
        else
        { i = off1.wrapping_add(0u32 as usize) };
        let s0: (&[u8], &[u8]) = pi.split_at(i);
        let s1: &[u8] =
            {
              let v: &[u8] = s0.0;
              v
            };
        let s2: &[u8] =
            {
              let v: &[u8] = s0.1;
              v
            };
        let res110: (&[u8], &[u8]) = (s1,s2);
        let input11: &[u8] =
            {
              let v: &[u8] = res110.0;
              v
            };
        let input2: &[u8] =
            {
              let v: &[u8] = res110.1;
              v
            };
        let spl0: (&[u8], &[u8]) = (input11,input2);
        let ph: &[u8] =
            {
              let v: &[u8] = spl0.0;
              v
            };
        let pc: &[u8] =
            {
              let v: &[u8] = spl0.1;
              v
            };
        let unused: usize = len__uint8_t(pc);
        crate::lowstar::ignore::ignore::<usize>(unused);
        let count: usize = jump_recursive_step_count_leaf(ph);
        pn = n00.wrapping_sub(1u32 as usize).wrapping_add(count);
        ppi0 = pc
      };
      let res3: bool = pres0;
      let n: usize = pn;
      cond = res3 && n > 0u32 as usize
    };
    let res2: bool = pres0;
    let check1: bool = res2;
    if ! check1
    { return 0u32 as usize }
    else
    {
      let mut pn0: usize = 1u32 as usize;
      let mut pres: bool = true;
      let mut ppi: &[u8] = input1;
      let res: bool = pres;
      let n00: usize = pn0;
      let mut cond0: bool = res && n00 > 0u32 as usize;
      while
      cond0
      {
        let n01: usize = pn0;
        let pi: &[u8] = ppi;
        let i0: usize = jump_raw_data_item(pi, 0u32 as usize);
        let s: (&[u8], &[u8]) = pi.split_at(i0);
        let s100: &[u8] =
            {
              let v: &[u8] = s.0;
              v
            };
        let s200: &[u8] =
            {
              let v: &[u8] = s.1;
              v
            };
        let res3: (&[u8], &[u8]) = (s100,s200);
        let input110: &[u8] =
            {
              let v: &[u8] = res3.0;
              v
            };
        let input20: &[u8] =
            {
              let v: &[u8] = res3.1;
              v
            };
        let res00: (&[u8], &[u8]) = (input110,input20);
        let input111: &[u8] =
            {
              let v: &[u8] = res00.0;
              v
            };
        let input21: &[u8] =
            {
              let v: &[u8] = res00.1;
              v
            };
        let spl: (&[u8], &[u8]) = (input111,input21);
        let res10: &[u8] =
            {
              let v: &[u8] = spl.0;
              v
            };
        let px: &[u8] = res10;
        let res20: bool = cbor_raw_sorted(px);
        if ! res20
        { pres = false }
        else
        {
          let off1: usize = jump_header(pi, 0u32 as usize);
          let s_0: (&[u8], &[u8]) = pi.split_at(0u32 as usize);
          let s101: &[u8] =
              {
                let v: &[u8] = s_0.0;
                v
              };
          let s201: &[u8] =
              {
                let v: &[u8] = s_0.1;
                v
              };
          let split1230: (&[u8], &[u8]) = (s101,s201);
          let input230: &[u8] =
              {
                let v: &[u8] = split1230.1;
                v
              };
          let consumed: usize = off1.wrapping_sub(0u32 as usize);
          let s1s20: (&[u8], &[u8]) = input230.split_at(consumed);
          let s110: &[u8] =
              {
                let v: &[u8] = s1s20.0;
                v
              };
          let s210: &[u8] =
              {
                let v: &[u8] = s1s20.1;
                v
              };
          let res11: (&[u8], &[u8]) = (s110,s210);
          let left: &[u8] =
              {
                let v: &[u8] = res11.0;
                v
              };
          let right: &[u8] =
              {
                let v: &[u8] = res11.1;
                v
              };
          let split230: (&[u8], &[u8]) = (left,right);
          let input_: &[u8] =
              {
                let v: &[u8] = split230.0;
                v
              };
          let res100: header = read_header(input_);
          let x: header = res100;
          let b0: initial_byte_t = x.fst;
          let mut i: usize;
          if b0.major_type as u32 == 2u32 || b0.major_type as u32 == 3u32
          {
            let b: initial_byte_t = x.fst;
            let l: long_argument = x.snd;
            i = off1.wrapping_add(argument_as_uint64(b, l) as usize)
          }
          else
          { i = off1.wrapping_add(0u32 as usize) };
          let s0: (&[u8], &[u8]) = pi.split_at(i);
          let s1: &[u8] =
              {
                let v: &[u8] = s0.0;
                v
              };
          let s2: &[u8] =
              {
                let v: &[u8] = s0.1;
                v
              };
          let res110: (&[u8], &[u8]) = (s1,s2);
          let input11: &[u8] =
              {
                let v: &[u8] = res110.0;
                v
              };
          let input2: &[u8] =
              {
                let v: &[u8] = res110.1;
                v
              };
          let spl0: (&[u8], &[u8]) = (input11,input2);
          let ph: &[u8] =
              {
                let v: &[u8] = spl0.0;
                v
              };
          let pc: &[u8] =
              {
                let v: &[u8] = spl0.1;
                v
              };
          let unused: usize = len__uint8_t(pc);
          crate::lowstar::ignore::ignore::<usize>(unused);
          let count: usize = jump_recursive_step_count_leaf(ph);
          pn0 = n01.wrapping_sub(1u32 as usize).wrapping_add(count);
          ppi = pc
        };
        let res30: bool = pres;
        let n: usize = pn0;
        cond0 = res30 && n > 0u32 as usize
      };
      let res00: bool = pres;
      let check2: bool = res00;
      if ! check2 { return 0u32 as usize } else { return len }
    }
  }
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

pub fn from_array__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>(
  a: &'a [cbor_map_entry <'a>],
  alen: usize
) ->
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry
    <'a>
{
  let ptr: (&[cbor_map_entry], &[cbor_map_entry]) = a.split_at(0usize);
  return Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry { elt: ptr.1, len: alen }
}

pub fn from_array__CBOR_Pulse_Raw_Type_cbor_raw <'a>(a: &'a [cbor_raw <'a>], alen: usize) ->
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw
    <'a>
{
  let ptr: (&[cbor_raw], &[cbor_raw]) = a.split_at(0usize);
  return Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw { elt: ptr.1, len: alen }
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

pub fn impl_cbor_det_compare(x1: cbor_raw, x2: cbor_raw) -> i16
{
  let res: i16 = CBOR_Pulse_Raw_Compare_impl_cbor_compare(x1, x2);
  return res
}

pub fn impl_deterministically_encoded_cbor_map_key_order(a1: &[u8], a2: &[u8]) -> bool
{
  let i0: usize = jump_raw_data_item(a1, 0u32 as usize);
  let s: (&[u8], &[u8]) = a1.split_at(i0);
  let s10: &[u8] =
      {
        let v: &[u8] = s.0;
        v
      };
  let s20: &[u8] =
      {
        let v: &[u8] = s.1;
        v
      };
  let res: (&[u8], &[u8]) = (s10,s20);
  let input10: &[u8] =
      {
        let v: &[u8] = res.0;
        v
      };
  let input20: &[u8] =
      {
        let v: &[u8] = res.1;
        v
      };
  let res0: (&[u8], &[u8]) = (input10,input20);
  let input11: &[u8] =
      {
        let v: &[u8] = res0.0;
        v
      };
  let input21: &[u8] =
      {
        let v: &[u8] = res0.1;
        v
      };
  let spl: (&[u8], &[u8]) = (input11,input21);
  let k1: &[u8] =
      {
        let v: &[u8] = spl.0;
        v
      };
  let i: usize = jump_raw_data_item(a2, 0u32 as usize);
  let s0: (&[u8], &[u8]) = a2.split_at(i);
  let s1: &[u8] =
      {
        let v: &[u8] = s0.0;
        v
      };
  let s2: &[u8] =
      {
        let v: &[u8] = s0.1;
        v
      };
  let res1: (&[u8], &[u8]) = (s1,s2);
  let input12: &[u8] =
      {
        let v: &[u8] = res1.0;
        v
      };
  let input22: &[u8] =
      {
        let v: &[u8] = res1.1;
        v
      };
  let res2: (&[u8], &[u8]) = (input12,input22);
  let input1: &[u8] =
      {
        let v: &[u8] = res2.0;
        v
      };
  let input2: &[u8] =
      {
        let v: &[u8] = res2.1;
        v
      };
  let spl0: (&[u8], &[u8]) = (input1,input2);
  let k2: &[u8] =
      {
        let v: &[u8] = spl0.0;
        v
      };
  let res3: i16 = lex_compare_bytes(k1, k2);
  return (res3 as i32) < 0i32 as i32
}

pub fn impl_major_type(x: cbor_raw) -> u8
{
  match x
  {
    cbor_raw::case_CBOR_Case_Simple { v } => return 7u8,
    _ =>
      match x
      {
        cbor_raw::case_CBOR_Case_Int { v } =>
          {
            let mut c_: cbor_int = Default::default();
            match x { cbor_raw::case_CBOR_Case_Int { v: v0 } => c_ = v0, _ => panic!("") };
            return c_.cbor_int_type
          },
        _ =>
          match x
          {
            cbor_raw::case_CBOR_Case_String { v } =>
              {
                let mut c_: cbor_string =
                    cbor_string
                    { cbor_string_type: 0u8, cbor_string_size: 0u8, cbor_string_ptr: &[] };
                match x { cbor_raw::case_CBOR_Case_String { v: v0 } => c_ = v0, _ => panic!("") };
                return c_.cbor_string_type
              },
            _ =>
              match x
              {
                cbor_raw::case_CBOR_Case_Tagged { v } => return 6u8,
                _ =>
                  match x
                  {
                    cbor_raw::case_CBOR_Case_Serialized_Tagged { v } => return 6u8,
                    _ =>
                      match x
                      {
                        cbor_raw::case_CBOR_Case_Array { v } => return 4u8,
                        _ =>
                          match x
                          {
                            cbor_raw::case_CBOR_Case_Serialized_Array { v } => return 4u8,
                            _ =>
                              match x
                              {
                                cbor_raw::case_CBOR_Case_Map { v } => return 5u8,
                                _ =>
                                  match x
                                  {
                                    cbor_raw::case_CBOR_Case_Serialized_Map { v } => return 5u8,
                                    _ => panic!("")
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
  }
}

pub fn impl_raw_uint64_compare(
  x1: CBOR_Spec_Raw_Base_raw_uint64,
  x2: CBOR_Spec_Raw_Base_raw_uint64
) ->
    i16
{
  let c: i16 = impl_uint8_compare(x1.size, x2.size);
  if c == 0i32 as i16 { return uint64_compare(x1.value, x2.value) } else { return c }
}

pub fn impl_raw_uint64_optimal(x: CBOR_Spec_Raw_Base_raw_uint64) -> bool
{
  if (x.value <= 23u32 as u64) == (x.size as u32 == 0u32)
  {
    if (x.size as u32) <= 1u32
    { return true }
    else if x.size as u32 == 2u32
    { return 256u64 <= x.value }
    else if x.size as u32 == 3u32
    { return 65536u64 <= x.value }
    else
    { return 4294967296u64 <= x.value }
  }
  else
  { return false }
}

pub fn impl_uint8_compare(x1: u8, x2: u8) -> i16
{
  if (x1 as i32) < x2 as i32
  { return 0i32.wrapping_sub(1i32) as i16 }
  else if x1 as i32 > x2 as i32 { return 1i32 as i16 } else { return 0i32 as i16 }
}

#[derive(PartialEq, Clone, Copy)]
#[repr(C)]
pub
struct initial_byte_t
{ pub major_type: u8, pub additional_info: u8 }

pub fn jump_header(input: &[u8], offset: usize) -> usize
{
  let off1: usize = offset.wrapping_add(1u32 as usize);
  let s_: (&[u8], &[u8]) = input.split_at(offset);
  let s10: &[u8] =
      {
        let v: &[u8] = s_.0;
        v
      };
  let s20: &[u8] =
      {
        let v: &[u8] = s_.1;
        v
      };
  let split123: (&[u8], &[u8]) = (s10,s20);
  let input23: &[u8] =
      {
        let v: &[u8] = split123.1;
        v
      };
  let consumed: usize = off1.wrapping_sub(offset);
  let s1s2: (&[u8], &[u8]) = input23.split_at(consumed);
  let s1: &[u8] =
      {
        let v: &[u8] = s1s2.0;
        v
      };
  let s2: &[u8] =
      {
        let v: &[u8] = s1s2.1;
        v
      };
  let res: (&[u8], &[u8]) = (s1,s2);
  let left: &[u8] =
      {
        let v: &[u8] = res.0;
        v
      };
  let right: &[u8] =
      {
        let v: &[u8] = res.1;
        v
      };
  let split23: (&[u8], &[u8]) = (left,right);
  let input_: &[u8] =
      {
        let v: &[u8] = split23.0;
        v
      };
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
    let s_: (&[u8], &[u8]) = input.split_at(off);
    let s10: &[u8] =
        {
          let v: &[u8] = s_.0;
          v
        };
    let s20: &[u8] =
        {
          let v: &[u8] = s_.1;
          v
        };
    let split123: (&[u8], &[u8]) = (s10,s20);
    let input23: &[u8] =
        {
          let v: &[u8] = split123.1;
          v
        };
    let consumed0: usize = off10.wrapping_sub(off);
    let s1s2: (&[u8], &[u8]) = input23.split_at(consumed0);
    let s11: &[u8] =
        {
          let v: &[u8] = s1s2.0;
          v
        };
    let s21: &[u8] =
        {
          let v: &[u8] = s1s2.1;
          v
        };
    let res: (&[u8], &[u8]) = (s11,s21);
    let left0: &[u8] =
        {
          let v: &[u8] = res.0;
          v
        };
    let right0: &[u8] =
        {
          let v: &[u8] = res.1;
          v
        };
    let split23: (&[u8], &[u8]) = (left0,right0);
    let input_: &[u8] =
        {
          let v: &[u8] = split23.0;
          v
        };
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
    let s_0: (&[u8], &[u8]) = input.split_at(off);
    let s12: &[u8] =
        {
          let v: &[u8] = s_0.0;
          v
        };
    let s22: &[u8] =
        {
          let v: &[u8] = s_0.1;
          v
        };
    let split1230: (&[u8], &[u8]) = (s12,s22);
    let input230: &[u8] =
        {
          let v: &[u8] = split1230.1;
          v
        };
    let consumed: usize = off1.wrapping_sub(off);
    let s1s20: (&[u8], &[u8]) = input230.split_at(consumed);
    let s1: &[u8] =
        {
          let v: &[u8] = s1s20.0;
          v
        };
    let s2: &[u8] =
        {
          let v: &[u8] = s1s20.1;
          v
        };
    let res1: (&[u8], &[u8]) = (s1,s2);
    let left: &[u8] =
        {
          let v: &[u8] = res1.0;
          v
        };
    let right: &[u8] =
        {
          let v: &[u8] = res1.1;
          v
        };
    let split230: (&[u8], &[u8]) = (left,right);
    let input1: &[u8] =
        {
          let v: &[u8] = split230.0;
          v
        };
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
  let s: (&[u8], &[u8]) = a.split_at(i);
  let s1: &[u8] =
      {
        let v: &[u8] = s.0;
        v
      };
  let s2: &[u8] =
      {
        let v: &[u8] = s.1;
        v
      };
  let res: (&[u8], &[u8]) = (s1,s2);
  let input1: &[u8] =
      {
        let v: &[u8] = res.0;
        v
      };
  let input2: &[u8] =
      {
        let v: &[u8] = res.1;
        v
      };
  let spl: (&[u8], &[u8]) = (input1,input2);
  let input10: &[u8] =
      {
        let v: &[u8] = spl.0;
        v
      };
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

pub fn len__CBOR_Pulse_Raw_Type_cbor_map_entry(
  s: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry
) ->
    usize
{ return s.len }

pub fn len__CBOR_Pulse_Raw_Type_cbor_raw(
  s: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw
) ->
    usize
{ return s.len }

pub fn len__uint8_t(s: &[u8]) -> usize { return s.len() }

pub fn lex_compare_bytes(s1: &[u8], s2: &[u8]) -> i16
{
  let sp1: &[u8] = s1;
  let sp2: &[u8] = s2;
  let mut pi1: usize = 0u32 as usize;
  let mut pi2: usize = 0u32 as usize;
  let n1: usize = len__uint8_t(sp1);
  let n2: usize = len__uint8_t(sp2);
  let mut ite: i16;
  if (0u32 as usize) < n1
  { if (0u32 as usize) < n2 { ite = 0i32 as i16 } else { ite = 1i32 as i16 } }
  else if (0u32 as usize) < n2
  { ite = 0i32.wrapping_sub(1i32) as i16 }
  else
  { ite = 0i32 as i16 };
  let mut pres: i16 = ite;
  let res: i16 = pres;
  let i10: usize = pi1;
  let mut cond: bool = res == 0i32 as i16 && i10 < n1;
  while
  cond
  {
    let i100: usize = pi1;
    let x1: u8 = op_Array_Access__uint8_t(sp1, i100);
    let i2: usize = pi2;
    let x2: u8 = op_Array_Access__uint8_t(sp2, i2);
    let res0: i16 = impl_uint8_compare(x1, x2);
    let c: i16 = res0;
    if c == 0i32 as i16
    {
      let i1_: usize = i100.wrapping_add(1u32 as usize);
      let i2_: usize = i2.wrapping_add(1u32 as usize);
      let ci1_: bool = i1_ < n1;
      let ci2_: bool = i2_ < n2;
      if ci2_ && ! ci1_
      { pres = 0i32.wrapping_sub(1i32) as i16 }
      else if ci1_ && ! ci2_
      { pres = 1i32 as i16 }
      else
      {
        pi1 = i1_;
        pi2 = i2_
      }
    }
    else
    { pres = c };
    let res00: i16 = pres;
    let i1: usize = pi1;
    cond = res00 == 0i32 as i16 && i1 < n1
  };
  let res0: i16 = pres;
  let res1: i16 = res0;
  return res1
}

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

pub fn op_Array_Access__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>(
  a: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>,
  i: usize
) ->
    cbor_map_entry
    <'a>
{ return a.elt[i] }

pub fn op_Array_Access__CBOR_Pulse_Raw_Type_cbor_raw <'a>(
  a: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw <'a>,
  i: usize
) ->
    cbor_raw
    <'a>
{ return a.elt[i] }

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
  let s: (&[u8], &[u8]) = input.split_at(i);
  let s1: &[u8] =
      {
        let v: &[u8] = s.0;
        v
      };
  let s2: &[u8] =
      {
        let v: &[u8] = s.1;
        v
      };
  let res: (&[u8], &[u8]) = (s1,s2);
  let input10: &[u8] =
      {
        let v: &[u8] = res.0;
        v
      };
  let input20: &[u8] =
      {
        let v: &[u8] = res.1;
        v
      };
  let split12: (&[u8], &[u8]) = (input10,input20);
  let input1: &[u8] =
      {
        let v: &[u8] = split12.0;
        v
      };
  let input2: &[u8] =
      {
        let v: &[u8] = split12.1;
        v
      };
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

pub fn scylla_split <'a>(s: &'a [u8], i: usize) -> (&'a [u8], &'a [u8])
{
  let elt_: (&[u8], &[u8]) = s.split_at(i);
  let s1: &[u8] = s;
  let s2: &[u8] = elt_.1;
  return (s1,s2)
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

pub fn split__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>(
  s: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry <'a>,
  i: usize
) ->
    __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry
    <'a>
{
  let elt_: &[cbor_map_entry] = &s.elt[i..];
  let s1: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry =
      Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry { elt: s.elt, len: i };
  let s2: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry =
      Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry
      { elt: elt_, len: (s.len).wrapping_sub(i) };
  return
  __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry
  { fst: s1, snd: s2 }
}

pub fn split__CBOR_Pulse_Raw_Type_cbor_raw <'a>(
  s: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw <'a>,
  i: usize
) ->
    __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw
    <'a>
{
  let elt_: &[cbor_raw] = &s.elt[i..];
  let s1: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw =
      Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw { elt: s.elt, len: i };
  let s2: Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw =
      Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw
      { elt: elt_, len: (s.len).wrapping_sub(i) };
  return
  __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw
  { fst: s1, snd: s2 }
}

pub fn uint64_compare(x1: u64, x2: u64) -> i16
{
  if x1 < x2
  { return 0i32.wrapping_sub(1i32) as i16 }
  else if x1 > x2 { return 1i32 as i16 } else { return 0i32 as i16 }
}

pub fn uu___is_CBOR_Case_Array(projectee: cbor_raw) -> bool
{ match projectee { cbor_raw::case_CBOR_Case_Array { v } => return true, _ => return false } }

pub fn uu___is_CBOR_Case_Int(projectee: cbor_raw) -> bool
{ match projectee { cbor_raw::case_CBOR_Case_Int { v } => return true, _ => return false } }

pub fn uu___is_CBOR_Case_Map(projectee: cbor_raw) -> bool
{ match projectee { cbor_raw::case_CBOR_Case_Map { v } => return true, _ => return false } }

pub fn uu___is_CBOR_Case_Serialized_Array(projectee: cbor_raw) -> bool
{
  match projectee
  { cbor_raw::case_CBOR_Case_Serialized_Array { v } => return true, _ => return false }
}

pub fn uu___is_CBOR_Case_Serialized_Map(projectee: cbor_raw) -> bool
{
  match projectee
  { cbor_raw::case_CBOR_Case_Serialized_Map { v } => return true, _ => return false }
}

pub fn uu___is_CBOR_Case_Serialized_Tagged(projectee: cbor_raw) -> bool
{
  match projectee
  { cbor_raw::case_CBOR_Case_Serialized_Tagged { v } => return true, _ => return false }
}

pub fn uu___is_CBOR_Case_Simple(projectee: cbor_raw) -> bool
{ match projectee { cbor_raw::case_CBOR_Case_Simple { v } => return true, _ => return false } }

pub fn uu___is_CBOR_Case_String(projectee: cbor_raw) -> bool
{ match projectee { cbor_raw::case_CBOR_Case_String { v } => return true, _ => return false } }

pub fn uu___is_CBOR_Case_Tagged(projectee: cbor_raw) -> bool
{ match projectee { cbor_raw::case_CBOR_Case_Tagged { v } => return true, _ => return false } }

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
    let s_: (&[u8], &[u8]) = input.split_at(offset2);
    let s10: &[u8] =
        {
          let v: &[u8] = s_.0;
          v
        };
    let s20: &[u8] =
        {
          let v: &[u8] = s_.1;
          v
        };
    let split123: (&[u8], &[u8]) = (s10,s20);
    let input23: &[u8] =
        {
          let v: &[u8] = split123.1;
          v
        };
    let consumed: usize = off.wrapping_sub(offset2);
    let s1s2: (&[u8], &[u8]) = input23.split_at(consumed);
    let s1: &[u8] =
        {
          let v: &[u8] = s1s2.0;
          v
        };
    let s2: &[u8] =
        {
          let v: &[u8] = s1s2.1;
          v
        };
    let res: (&[u8], &[u8]) = (s1,s2);
    let left: &[u8] =
        {
          let v: &[u8] = res.0;
          v
        };
    let right: &[u8] =
        {
          let v: &[u8] = res.1;
          v
        };
    let split23: (&[u8], &[u8]) = (left,right);
    let input_: &[u8] =
        {
          let v: &[u8] = split23.0;
          v
        };
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
    let s_: (&[u8], &[u8]) = input.split_at(offset1);
    let s10: &[u8] =
        {
          let v: &[u8] = s_.0;
          v
        };
    let s20: &[u8] =
        {
          let v: &[u8] = s_.1;
          v
        };
    let split123: (&[u8], &[u8]) = (s10,s20);
    let input23: &[u8] =
        {
          let v: &[u8] = split123.1;
          v
        };
    let consumed0: usize = off.wrapping_sub(offset1);
    let s1s2: (&[u8], &[u8]) = input23.split_at(consumed0);
    let s11: &[u8] =
        {
          let v: &[u8] = s1s2.0;
          v
        };
    let s21: &[u8] =
        {
          let v: &[u8] = s1s2.1;
          v
        };
    let res: (&[u8], &[u8]) = (s11,s21);
    let left0: &[u8] =
        {
          let v: &[u8] = res.0;
          v
        };
    let right0: &[u8] =
        {
          let v: &[u8] = res.1;
          v
        };
    let split23: (&[u8], &[u8]) = (left0,right0);
    let input_: &[u8] =
        {
          let v: &[u8] = split23.0;
          v
        };
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
          let s_0: (&[u8], &[u8]) = input.split_at(offset20);
          let s100: &[u8] =
              {
                let v: &[u8] = s_0.0;
                v
              };
          let s200: &[u8] =
              {
                let v: &[u8] = s_0.1;
                v
              };
          let split1230: (&[u8], &[u8]) = (s100,s200);
          let input230: &[u8] =
              {
                let v: &[u8] = split1230.1;
                v
              };
          let consumed: usize = off1.wrapping_sub(offset20);
          let s1s20: (&[u8], &[u8]) = input230.split_at(consumed);
          let s1: &[u8] =
              {
                let v: &[u8] = s1s20.0;
                v
              };
          let s2: &[u8] =
              {
                let v: &[u8] = s1s20.1;
                v
              };
          let res2: (&[u8], &[u8]) = (s1,s2);
          let left: &[u8] =
              {
                let v: &[u8] = res2.0;
                v
              };
          let right: &[u8] =
              {
                let v: &[u8] = res2.1;
                v
              };
          let split230: (&[u8], &[u8]) = (left,right);
          let input_0: &[u8] =
              {
                let v: &[u8] = split230.0;
                v
              };
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
        let s_: (&[u8], &[u8]) = input.split_at(offset1);
        let s10: &[u8] =
            {
              let v: &[u8] = s_.0;
              v
            };
        let s20: &[u8] =
            {
              let v: &[u8] = s_.1;
              v
            };
        let split123: (&[u8], &[u8]) = (s10,s20);
        let input23: &[u8] =
            {
              let v: &[u8] = split123.1;
              v
            };
        let consumed: usize = off1.wrapping_sub(offset1);
        let s1s2: (&[u8], &[u8]) = input23.split_at(consumed);
        let s1: &[u8] =
            {
              let v: &[u8] = s1s2.0;
              v
            };
        let s2: &[u8] =
            {
              let v: &[u8] = s1s2.1;
              v
            };
        let res0: (&[u8], &[u8]) = (s1,s2);
        let left: &[u8] =
            {
              let v: &[u8] = res0.0;
              v
            };
        let right: &[u8] =
            {
              let v: &[u8] = res0.1;
              v
            };
        let split23: (&[u8], &[u8]) = (left,right);
        let input_: &[u8] =
            {
              let v: &[u8] = split23.0;
              v
            };
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
        let s_: (&[u8], &[u8]) = input.split_at(off);
        let s10: &[u8] =
            {
              let v: &[u8] = s_.0;
              v
            };
        let s20: &[u8] =
            {
              let v: &[u8] = s_.1;
              v
            };
        let split123: (&[u8], &[u8]) = (s10,s20);
        let input23: &[u8] =
            {
              let v: &[u8] = split123.1;
              v
            };
        let consumed: usize = offset10.wrapping_sub(off);
        let s1s2: (&[u8], &[u8]) = input23.split_at(consumed);
        let s1: &[u8] =
            {
              let v: &[u8] = s1s2.0;
              v
            };
        let s2: &[u8] =
            {
              let v: &[u8] = s1s2.1;
              v
            };
        let res0: (&[u8], &[u8]) = (s1,s2);
        let left: &[u8] =
            {
              let v: &[u8] = res0.0;
              v
            };
        let right: &[u8] =
            {
              let v: &[u8] = res0.1;
              v
            };
        let split23: (&[u8], &[u8]) = (left,right);
        let input1: &[u8] =
            {
              let v: &[u8] = split23.0;
              v
            };
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
  let s: (&[u8], &[u8]) = a.split_at(i);
  let s1: &[u8] =
      {
        let v: &[u8] = s.0;
        v
      };
  let s2: &[u8] =
      {
        let v: &[u8] = s.1;
        v
      };
  let res: (&[u8], &[u8]) = (s1,s2);
  let input1: &[u8] =
      {
        let v: &[u8] = res.0;
        v
      };
  let input2: &[u8] =
      {
        let v: &[u8] = res.1;
        v
      };
  let spl: (&[u8], &[u8]) = (input1,input2);
  let input10: &[u8] =
      {
        let v: &[u8] = spl.0;
        v
      };
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
