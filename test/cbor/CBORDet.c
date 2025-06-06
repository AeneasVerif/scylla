

#include "CBORDet.h"

#define CBOR_STRING_DEFAULT { .cbor_string_type = 0, .cbor_string_size = 0, .cbor_string_ptr = {.elt = NULL, .len = 0}}
#define CBOR_TAGGED_DEFAULT { .cbor_tagged_tag = {.size = 0, .value = 0}, .cbor_tagged_ptr = NULL }
#define CBOR_SERIALIZED_DEFAULT { .cbor_serialized_header = {.size = 0, .value = 0}, .cbor_serialized_payload = NULL }
#define CBOR_RAW_DEFAULT { .tag = 1, .case_CBOR_Case_Simple = 0 }
#define CBOR_MAP_ENTRY_DEFAULT { .cbor_map_entry_key = CBOR_RAW_DEFAULT, .cbor_map_entry_value = CBOR_RAW_DEFAULT }
#define OPTION_DEFAULT { .tag = 1 }
#define LONG_ARGUMENT_DEFAULT { .tag = LongArgumentOther }
#define split__uint8_t scylla_split

static uint8_t get_bitfield_gen8(uint8_t x, uint32_t lo, uint32_t hi)
{
  uint8_t op1 = (uint32_t)x << 8U - hi;
  return (uint32_t)op1 >> 8U - hi + lo;
}

static uint8_t set_bitfield_gen8(uint8_t x, uint32_t lo, uint32_t hi, uint8_t v)
{
  uint8_t op0 = 255U;
  uint8_t op1 = (uint32_t)op0 >> 8U - (hi - lo);
  uint8_t op2 = (uint32_t)op1 << lo;
  uint8_t op3 = ~op2;
  uint8_t op4 = (uint32_t)x & (uint32_t)op3;
  uint8_t op5 = (uint32_t)v << lo;
  return (uint32_t)op4 | (uint32_t)op5;
}

static CBOR_Spec_Raw_Base_raw_uint64 mk_raw_uint64(uint64_t x)
{
  uint8_t size;
  if (x <= (uint64_t)MAX_SIMPLE_VALUE_ADDITIONAL_INFO)
    size = 0U;
  else if (x < 256ULL)
    size = 1U;
  else if (x < 65536ULL)
    size = 2U;
  else if (x < 4294967296ULL)
    size = 3U;
  else
    size = 4U;
  return ((CBOR_Spec_Raw_Base_raw_uint64){ .size = size, .value = x });
}

#define ADDITIONAL_INFO_LONG_ARGUMENT_8_BITS (24U)

#define ADDITIONAL_INFO_UNASSIGNED_MIN (28U)

typedef struct initial_byte_t_s
{
  uint8_t major_type;
  uint8_t additional_info;
}
initial_byte_t;

#define ADDITIONAL_INFO_LONG_ARGUMENT_16_BITS (25U)

#define ADDITIONAL_INFO_LONG_ARGUMENT_32_BITS (26U)

#define ADDITIONAL_INFO_LONG_ARGUMENT_64_BITS (27U)

#define LongArgumentSimpleValue 0
#define LongArgumentU8 1
#define LongArgumentU16 2
#define LongArgumentU32 3
#define LongArgumentU64 4
#define LongArgumentOther 5

typedef uint8_t long_argument_tags;

typedef struct
__attribute__((annotate("scylla_adt")))
__attribute__((annotate("scylla_empty_variant(case_LongArgumentOther)")))
long_argument_s
{
  long_argument_tags tag;
  union {
    uint8_t case_LongArgumentSimpleValue;
    uint8_t case_LongArgumentU8;
    uint16_t case_LongArgumentU16;
    uint32_t case_LongArgumentU32;
    uint64_t case_LongArgumentU64;
  }
  ;
}
long_argument;

typedef struct header_s
{
  initial_byte_t fst;
  long_argument snd;
}
header;

static uint64_t argument_as_uint64(initial_byte_t b, long_argument x)
{
  CBOR_Spec_Raw_Base_raw_uint64 ite;
  if (x.tag == LongArgumentU8)
  {
    uint8_t v = x.case_LongArgumentU8;
    ite = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 1U, .value = (uint64_t)v });
  }
  else if (x.tag == LongArgumentU16)
  {
    uint16_t v = x.case_LongArgumentU16;
    ite = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 2U, .value = (uint64_t)v });
  }
  else if (x.tag == LongArgumentU32)
  {
    uint32_t v = x.case_LongArgumentU32;
    ite = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 3U, .value = (uint64_t)v });
  }
  else if (x.tag == LongArgumentU64)
  {
    uint64_t v = x.case_LongArgumentU64;
    ite = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 4U, .value = v });
  }
  else if (x.tag == LongArgumentOther)
    ite = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 0U, .value = (uint64_t)b.additional_info });
  else
    exit(253);
    // ite =
    //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
    //     "unreachable (pattern matches are exhaustive in F*)");
  return ite.value;
}

static header raw_uint64_as_argument(uint8_t t, CBOR_Spec_Raw_Base_raw_uint64 x)
{
  if (x.size == 0U)
    return
      (
        (header){
          .fst = { .major_type = t, .additional_info = (uint8_t)x.value },
          .snd = { .tag = LongArgumentOther }
        }
      );
  else if (x.size == 1U)
    return
      (
        (header){
          .fst = { .major_type = t, .additional_info = ADDITIONAL_INFO_LONG_ARGUMENT_8_BITS },
          .snd = { .tag = LongArgumentU8, { .case_LongArgumentU8 = (uint8_t)x.value } }
        }
      );
  else if (x.size == 2U)
    return
      (
        (header){
          .fst = { .major_type = t, .additional_info = ADDITIONAL_INFO_LONG_ARGUMENT_16_BITS },
          .snd = { .tag = LongArgumentU16, { .case_LongArgumentU16 = (uint16_t)x.value } }
        }
      );
  else if (x.size == 3U)
    return
      (
        (header){
          .fst = { .major_type = t, .additional_info = ADDITIONAL_INFO_LONG_ARGUMENT_32_BITS },
          .snd = { .tag = LongArgumentU32, { .case_LongArgumentU32 = (uint32_t)x.value } }
        }
      );
  else
    return
      (
        (header){
          .fst = { .major_type = t, .additional_info = ADDITIONAL_INFO_LONG_ARGUMENT_64_BITS },
          .snd = { .tag = LongArgumentU64, { .case_LongArgumentU64 = x.value } }
        }
      );
}

static header simple_value_as_argument(uint8_t x)
{
  if (x <= MAX_SIMPLE_VALUE_ADDITIONAL_INFO)
    return
      (
        (header){
          .fst = { .major_type = CBOR_MAJOR_TYPE_SIMPLE_VALUE, .additional_info = x },
          .snd = { .tag = LongArgumentOther }
        }
      );
  else
    return
      (
        (header){
          .fst = {
            .major_type = CBOR_MAJOR_TYPE_SIMPLE_VALUE,
            .additional_info = ADDITIONAL_INFO_LONG_ARGUMENT_8_BITS
          },
          .snd = { .tag = LongArgumentSimpleValue, { .case_LongArgumentSimpleValue = x } }
        }
      );
}

static uint8_t get_header_major_type(header h)
{
  initial_byte_t b = h.fst;
  return b.major_type;
}

typedef Pulse_Lib_Slice_slice__uint8_t cbor_raw_serialized_iterator;

static initial_byte_t
__proj__Mkdtuple2__item___1__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(
  header pair
)
{
  return pair.fst;
}

static initial_byte_t
dfst__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(header t)
{
  return
    __proj__Mkdtuple2__item___1__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(t);
}

static void op_Array_Assignment__uint8_t(Pulse_Lib_Slice_slice__uint8_t a, size_t i, uint8_t v)
{
  a.elt[i] = v;
}

static long_argument
__proj__Mkdtuple2__item___2__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(
  header pair
)
{
  return pair.snd;
}

static long_argument
dsnd__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(header t)
{
  return
    __proj__Mkdtuple2__item___2__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(t);
}

static size_t write_header(header x, Pulse_Lib_Slice_slice__uint8_t out, size_t offset)
{
  initial_byte_t
  xh1 = dfst__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(x);
  size_t pos_ = offset + (size_t)1U;
  uint8_t
  n_ =
    set_bitfield_gen8(set_bitfield_gen8(0U, 0U, 5U, xh1.additional_info),
      5U,
      8U,
      xh1.major_type);
  out.elt[pos_ - (size_t)1U] = n_;
  // op_Array_Assignment__uint8_t(out, pos_ - (size_t)1U, n_);
  size_t res1 = pos_;
  long_argument
  x2_ = dsnd__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(x);
  size_t res;
  if (xh1.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_8_BITS)
    if (xh1.major_type == CBOR_MAJOR_TYPE_SIMPLE_VALUE)
    {
      size_t pos_ = res1 + (size_t)1U;
      uint8_t n_;
      if (x2_.tag == LongArgumentSimpleValue)
        n_ = x2_.case_LongArgumentSimpleValue;
      else
        exit(253);
        // n_ = KRML_EABORT(uint8_t, "unreachable (pattern matches are exhaustive in F*)");
      out.elt[pos_ - (size_t)1U] = n_;
      // op_Array_Assignment__uint8_t(out, pos_ - (size_t)1U, n_);
      res = pos_;
    }
    else
    {
      size_t pos_ = res1 + (size_t)1U;
      uint8_t n_;
      if (x2_.tag == LongArgumentU8)
        n_ = x2_.case_LongArgumentU8;
      else
        exit(253);
        // n_ = KRML_EABORT(uint8_t, "unreachable (pattern matches are exhaustive in F*)");
      out.elt[pos_ - (size_t)1U] = n_;
      // op_Array_Assignment__uint8_t(out, pos_ - (size_t)1U, n_);
      res = pos_;
    }
  else if (xh1.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_16_BITS)
  {
    size_t pos_ = res1 + (size_t)2U;
    uint16_t ite0;
    if (x2_.tag == LongArgumentU16)
      ite0 = x2_.case_LongArgumentU16;
    else
      exit(253);
      // ite0 = KRML_EABORT(uint16_t, "unreachable (pattern matches are exhaustive in F*)");
    uint8_t lo = (uint8_t)ite0;
    uint16_t ite;
    if (x2_.tag == LongArgumentU16)
      ite = x2_.case_LongArgumentU16;
    else
      exit(253);
      // ite = KRML_EABORT(uint16_t, "unreachable (pattern matches are exhaustive in F*)");
    uint16_t hi = (uint32_t)ite / 256U;
    size_t pos_1 = pos_ - (size_t)1U;
    uint8_t n_ = (uint8_t)hi;
    out.elt[pos_1 - (size_t)1U] = n_;
    out.elt[pos_1] = lo;
    // op_Array_Assignment__uint8_t(out, pos_1 - (size_t)1U, n_);
    // op_Array_Assignment__uint8_t(out, pos_1, lo);
    res = pos_;
  }
  else if (xh1.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_32_BITS)
  {
    size_t pos_ = res1 + (size_t)4U;
    uint32_t ite0;
    if (x2_.tag == LongArgumentU32)
      ite0 = x2_.case_LongArgumentU32;
    else
      exit(253);
      // ite0 = KRML_EABORT(uint32_t, "unreachable (pattern matches are exhaustive in F*)");
    uint8_t lo = (uint8_t)ite0;
    uint32_t ite;
    if (x2_.tag == LongArgumentU32)
      ite = x2_.case_LongArgumentU32;
    else
      exit(253);
      // ite = KRML_EABORT(uint32_t, "unreachable (pattern matches are exhaustive in F*)");
    uint32_t hi = ite / 256U;
    size_t pos_1 = pos_ - (size_t)1U;
    uint8_t lo1 = (uint8_t)hi;
    uint32_t hi1 = hi / 256U;
    size_t pos_2 = pos_1 - (size_t)1U;
    uint8_t lo2 = (uint8_t)hi1;
    uint32_t hi2 = hi1 / 256U;
    size_t pos_3 = pos_2 - (size_t)1U;
    uint8_t n_ = (uint8_t)hi2;
    out.elt[pos_3 - (size_t)1U] = n_;
    out.elt[pos_3] = lo2;
    out.elt[pos_2] = lo1;
    out.elt[pos_1] = lo;
    // op_Array_Assignment__uint8_t(out, pos_3 - (size_t)1U, n_);
    // op_Array_Assignment__uint8_t(out, pos_3, lo2);
    // op_Array_Assignment__uint8_t(out, pos_2, lo1);
    // op_Array_Assignment__uint8_t(out, pos_1, lo);
    res = pos_;
  }
  else if (xh1.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_64_BITS)
  {
    size_t pos_ = res1 + (size_t)8U;
    uint64_t ite0;
    if (x2_.tag == LongArgumentU64)
      ite0 = x2_.case_LongArgumentU64;
    else
      exit(253);
      // ite0 = KRML_EABORT(uint64_t, "unreachable (pattern matches are exhaustive in F*)");
    uint8_t lo = (uint8_t)ite0;
    uint64_t ite;
    if (x2_.tag == LongArgumentU64)
      ite = x2_.case_LongArgumentU64;
    else
      exit(253);
      // ite = KRML_EABORT(uint64_t, "unreachable (pattern matches are exhaustive in F*)");
    uint64_t hi = ite / 256ULL;
    size_t pos_1 = pos_ - (size_t)1U;
    uint8_t lo1 = (uint8_t)hi;
    uint64_t hi1 = hi / 256ULL;
    size_t pos_2 = pos_1 - (size_t)1U;
    uint8_t lo2 = (uint8_t)hi1;
    uint64_t hi2 = hi1 / 256ULL;
    size_t pos_3 = pos_2 - (size_t)1U;
    uint8_t lo3 = (uint8_t)hi2;
    uint64_t hi3 = hi2 / 256ULL;
    size_t pos_4 = pos_3 - (size_t)1U;
    uint8_t lo4 = (uint8_t)hi3;
    uint64_t hi4 = hi3 / 256ULL;
    size_t pos_5 = pos_4 - (size_t)1U;
    uint8_t lo5 = (uint8_t)hi4;
    uint64_t hi5 = hi4 / 256ULL;
    size_t pos_6 = pos_5 - (size_t)1U;
    uint8_t lo6 = (uint8_t)hi5;
    uint64_t hi6 = hi5 / 256ULL;
    size_t pos_7 = pos_6 - (size_t)1U;
    uint8_t n_ = (uint8_t)hi6;
    out.elt[pos_7 - (size_t)1u] = n_;
    out.elt[pos_7] = lo6;
    out.elt[pos_6] = lo5;
    out.elt[pos_5] = lo4;
    out.elt[pos_4] = lo3;
    out.elt[pos_3] = lo2;
    out.elt[pos_2] = lo1;
    out.elt[pos_1] = lo;
    // op_Array_Assignment__uint8_t(out, pos_7 - (size_t)1U, n_);
    // op_Array_Assignment__uint8_t(out, pos_7, lo6);
    // op_Array_Assignment__uint8_t(out, pos_6, lo5);
    // op_Array_Assignment__uint8_t(out, pos_5, lo4);
    // op_Array_Assignment__uint8_t(out, pos_4, lo3);
    // op_Array_Assignment__uint8_t(out, pos_3, lo2);
    // op_Array_Assignment__uint8_t(out, pos_2, lo1);
    // op_Array_Assignment__uint8_t(out, pos_1, lo);
    res = pos_;
  }
  else
    res = res1;
  size_t res2 = res;
  size_t res0 = res2;
  return res0;
}

static bool size_header(header x, size_t *out)
{
  initial_byte_t
  xh1 = dfst__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(x);
  size_t capacity0 = *out;
  bool res0;
  if (capacity0 < (size_t)1U)
    res0 = false;
  else
  {
    *out = capacity0 - (size_t)1U;
    res0 = true;
  }
  bool res1 = res0;
  if (res1)
  {
    long_argument
    x2_ = dsnd__CBOR_Spec_Raw_EverParse_initial_byte_t_CBOR_Spec_Raw_EverParse_long_argument(x);
    scylla_reset(x2_); // KRML_MAYBE_UNUSED_VAR(x2_);
    bool res0;
    if (xh1.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_8_BITS)
    {
      size_t capacity = *out;
      bool res;
      if (capacity < (size_t)1U)
        res = false;
      else
      {
        *out = capacity - (size_t)1U;
        res = true;
      }
      res0 = res;
    }
    else if (xh1.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_16_BITS)
    {
      size_t capacity = *out;
      bool res;
      if (capacity < (size_t)2U)
        res = false;
      else
      {
        *out = capacity - (size_t)2U;
        res = true;
      }
      res0 = res;
    }
    else if (xh1.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_32_BITS)
    {
      size_t capacity = *out;
      bool res;
      if (capacity < (size_t)4U)
        res = false;
      else
      {
        *out = capacity - (size_t)4U;
        res = true;
      }
      res0 = res;
    }
    else if (xh1.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_64_BITS)
    {
      size_t capacity = *out;
      bool res;
      if (capacity < (size_t)8U)
        res = false;
      else
      {
        *out = capacity - (size_t)8U;
        res = true;
      }
      res0 = res;
    }
    else
      res0 = true;
    bool res2 = res0;
    return res2;
  }
  else
    return false;
}

static size_t len__uint8_t(Pulse_Lib_Slice_slice__uint8_t s)
{
  return s.len;
}

static header cbor_raw_get_header(cbor_raw xl)
{
  bool ite0;
  if (xl.tag == CBOR_Case_Int)
    ite0 = true;
  else
    ite0 = false;
  if (ite0)
  {
    cbor_int c_;
    if (xl.tag == CBOR_Case_Int)
      c_ = xl.case_CBOR_Case_Int;
    else
      exit(253);
      // c_ = KRML_EABORT(cbor_int, "unreachable (pattern matches are exhaustive in F*)");
    uint8_t ty = c_.cbor_int_type;
    cbor_int c_0;
    if (xl.tag == CBOR_Case_Int)
      c_0 = xl.case_CBOR_Case_Int;
    else
      exit(253);
      // c_0 = KRML_EABORT(cbor_int, "unreachable (pattern matches are exhaustive in F*)");
    CBOR_Spec_Raw_Base_raw_uint64 v = { .size = c_0.cbor_int_size, .value = c_0.cbor_int_value };
    return raw_uint64_as_argument(ty, v);
  }
  else
  {
    bool ite0;
    if (xl.tag == CBOR_Case_String)
      ite0 = true;
    else
      ite0 = false;
    if (ite0)
    {
      cbor_string c_ = CBOR_STRING_DEFAULT;
      if (xl.tag == CBOR_Case_String)
        c_ = xl.case_CBOR_Case_String;
      else
        exit(253);
        // c_ = KRML_EABORT(cbor_string, "unreachable (pattern matches are exhaustive in F*)");
      uint8_t ty = c_.cbor_string_type;
      cbor_string c_0 = CBOR_STRING_DEFAULT;
      if (xl.tag == CBOR_Case_String)
        c_0 = xl.case_CBOR_Case_String;
      else
        exit(253);
        // c_0 = KRML_EABORT(cbor_string, "unreachable (pattern matches are exhaustive in F*)");
      CBOR_Spec_Raw_Base_raw_uint64
      res = { .size = c_0.cbor_string_size, .value = (uint64_t)c_0.cbor_string_ptr.len };
      CBOR_Spec_Raw_Base_raw_uint64 len = res;
      return raw_uint64_as_argument(ty, len);
    }
    else
    {
      bool a;
      if (xl.tag == CBOR_Case_Tagged)
        a = true;
      else
        a = false;
      bool ite0;
      if (a)
        ite0 = true;
      else if (xl.tag == CBOR_Case_Serialized_Tagged)
        ite0 = true;
      else
        ite0 = false;
      if (ite0)
      {
        CBOR_Spec_Raw_Base_raw_uint64 tag;
        if (xl.tag == CBOR_Case_Tagged)
        {
          cbor_tagged c_ = xl.case_CBOR_Case_Tagged;
          tag = c_.cbor_tagged_tag;
        }
        else if (xl.tag == CBOR_Case_Serialized_Tagged)
        {
          cbor_serialized c_ = xl.case_CBOR_Case_Serialized_Tagged;
          tag = c_.cbor_serialized_header;
        }
        else
          exit(253);
          // tag =
          //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
          //     "unreachable (pattern matches are exhaustive in F*)");
        return raw_uint64_as_argument(CBOR_MAJOR_TYPE_TAGGED, tag);
      }
      else
      {
        bool a;
        if (xl.tag == CBOR_Case_Array)
          a = true;
        else
          a = false;
        bool ite0;
        if (a)
          ite0 = true;
        else if (xl.tag == CBOR_Case_Serialized_Array)
          ite0 = true;
        else
          ite0 = false;
        if (ite0)
        {
          CBOR_Spec_Raw_Base_raw_uint64 len;
          if (xl.tag == CBOR_Case_Array)
          {
            cbor_array c_ = xl.case_CBOR_Case_Array;
            len = c_.cbor_array_length;
          }
          else if (xl.tag == CBOR_Case_Serialized_Array)
          {
            cbor_serialized c_ = xl.case_CBOR_Case_Serialized_Array;
            len = c_.cbor_serialized_header;
          }
          else
            exit(253);
            // len =
            //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
            //     "unreachable (pattern matches are exhaustive in F*)");
          return raw_uint64_as_argument(CBOR_MAJOR_TYPE_ARRAY, len);
        }
        else
        {
          bool a;
          if (xl.tag == CBOR_Case_Map)
            a = true;
          else
            a = false;
          bool ite;
          if (a)
            ite = true;
          else if (xl.tag == CBOR_Case_Serialized_Map)
            ite = true;
          else
            ite = false;
          if (ite)
          {
            CBOR_Spec_Raw_Base_raw_uint64 len;
            if (xl.tag == CBOR_Case_Map)
            {
              cbor_map c_ = xl.case_CBOR_Case_Map;
              len = c_.cbor_map_length;
            }
            else if (xl.tag == CBOR_Case_Serialized_Map)
            {
              cbor_serialized c_ = xl.case_CBOR_Case_Serialized_Map;
              len = c_.cbor_serialized_header;
            }
            else
              exit(253);
              // len =
              //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
              //     "unreachable (pattern matches are exhaustive in F*)");
            return raw_uint64_as_argument(CBOR_MAJOR_TYPE_MAP, len);
          }
          else
          {
            uint8_t v;
            if (xl.tag == CBOR_Case_Simple)
              v = xl.case_CBOR_Case_Simple;
            else
              exit(253);
              // v = KRML_EABORT(uint8_t, "unreachable (pattern matches are exhaustive in F*)");
            return simple_value_as_argument(v);
          }
        }
      }
    }
  }
}

static header cbor_raw_with_perm_get_header(cbor_raw xl)
{
  header res = cbor_raw_get_header(xl);
  return res;
}

typedef struct
__attribute__((annotate("scylla_tuple")))
__Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t_s
{
  Pulse_Lib_Slice_slice__uint8_t fst;
  Pulse_Lib_Slice_slice__uint8_t snd;
}
__Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t;

static __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
scylla_split(Pulse_Lib_Slice_slice__uint8_t s, size_t i)
{
  uint8_t *elt_ = s.elt + i;
  Pulse_Lib_Slice_slice__uint8_t s1 = { .elt = s.elt, .len = i };
  Pulse_Lib_Slice_slice__uint8_t s2 = { .elt = elt_, .len = s.len - i };
  return
    ((__Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t){ .fst = s1, .snd = s2 });
}

static void
copy__uint8_t(Pulse_Lib_Slice_slice__uint8_t dst, Pulse_Lib_Slice_slice__uint8_t src)
{
  memcpy(dst.elt, src.elt, src.len * sizeof (uint8_t));
}

typedef struct
__attribute__((annotate("scylla_adt")))
__attribute__((annotate("scylla_empty_variant(cbor_map_None)")))
option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw__s
{
  FStar_Pervasives_Native_option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw__tags
  tag;
  union { cbor_raw *v };
}
option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_;

typedef struct
__attribute__((annotate("scylla_adt")))
__attribute__((annotate("scylla_empty_variant(cbor_map_entry_None)")))
option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry__s
{
  FStar_Pervasives_Native_option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw__tags
  tag;
  union { cbor_map_entry *v; };
}
option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_;

size_t
CBOR_Pulse_Raw_Format_Serialize_ser_(
  cbor_raw x_,
  Pulse_Lib_Slice_slice__uint8_t out,
  size_t offset
)
{
  header res0 = cbor_raw_with_perm_get_header(x_);
  header xh1 = res0;
  size_t res1 = write_header(xh1, out, offset);
  initial_byte_t b0 = xh1.fst;
  size_t res2;
  if
  (b0.major_type == CBOR_MAJOR_TYPE_BYTE_STRING || b0.major_type == CBOR_MAJOR_TYPE_TEXT_STRING)
  {
    cbor_raw scrut = x_;
    cbor_string c_ = CBOR_STRING_DEFAULT;
    if (scrut.tag == CBOR_Case_String)
      c_ = scrut.case_CBOR_Case_String;
    else
      exit(253);
      // c_ = KRML_EABORT(cbor_string, "unreachable (pattern matches are exhaustive in F*)");
    Pulse_Lib_Slice_slice__uint8_t s = c_.cbor_string_ptr;
    Pulse_Lib_Slice_slice__uint8_t x2_ = s;
    size_t length = len__uint8_t(x2_);
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t sp1 = split__uint8_t(out, res1);
    Pulse_Lib_Slice_slice__uint8_t sp12 = sp1.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    sp2 = split__uint8_t(sp12, length);
    Pulse_Lib_Slice_slice__uint8_t sp21 = sp2.fst;
    copy__uint8_t(sp21, x2_);
    size_t res = res1 + length;
    size_t res0 = res;
    res2 = res0;
  }
  else
  {
    initial_byte_t b = xh1.fst;
    if (b.major_type == CBOR_MAJOR_TYPE_ARRAY)
    {
      bool ite;
      if (x_.tag == CBOR_Case_Array)
        ite = true;
      else
        ite = false;
      if (ite)
      {
        cbor_raw x2_ = x_;
        cbor_raw scrut0 = x2_;
        option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_ scrut = OPTION_DEFAULT;
        if (scrut0.tag == CBOR_Case_Array)
        {
          cbor_array a = scrut0.case_CBOR_Case_Array;
          scrut =
            (
              (option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_){
                .tag = FStar_Pervasives_Native_Some,
                .v = a.cbor_array_ptr
              }
            );
        }
        else
          scrut =
            (
              (option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_){
                .tag = FStar_Pervasives_Native_None
              }
            );
        cbor_raw *a = NULL;
        if (scrut.tag == FStar_Pervasives_Native_Some)
          a = scrut.v;
        else
          exit(253);
          // a = KRML_EABORT(cbor_raw *, "unreachable (pattern matches are exhaustive in F*)");
        size_t pres = res1;
        size_t pi = (size_t)0U;
        size_t i0 = pi;
        bool cond = i0 < (size_t)argument_as_uint64(xh1.fst, xh1.snd);
        while (cond)
        {
          size_t i = pi;
          size_t off = pres;
          cbor_raw e = a[i];
          size_t i_ = i + (size_t)1U;
          cbor_raw x2_1 = e;
          size_t res = CBOR_Pulse_Raw_Format_Serialize_ser_(x2_1, out, off);
          size_t res0 = res;
          size_t res1 = res0;
          pi = i_;
          pres = res1;
          size_t i0 = pi;
          cond = i0 < (size_t)argument_as_uint64(xh1.fst, xh1.snd);
        }
        size_t res = pres;
        size_t res0 = res;
        size_t res1 = res0;
        res2 = res1;
      }
      else
      {
        cbor_raw scrut = x_;
        cbor_serialized xs = CBOR_SERIALIZED_DEFAULT;
        if (scrut.tag == CBOR_Case_Serialized_Array)
          xs = scrut.case_CBOR_Case_Serialized_Array;
        else
          exit(253);
          // xs = KRML_EABORT(cbor_serialized, "unreachable (pattern matches are exhaustive in F*)");
        Pulse_Lib_Slice_slice__uint8_t x2_ = xs.cbor_serialized_payload;
        size_t length = len__uint8_t(x2_);
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        sp1 = split__uint8_t(out, res1);
        Pulse_Lib_Slice_slice__uint8_t sp12 = sp1.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        sp2 = split__uint8_t(sp12, length);
        Pulse_Lib_Slice_slice__uint8_t sp21 = sp2.fst;
        copy__uint8_t(sp21, x2_);
        size_t res = res1 + length;
        size_t res0 = res;
        size_t res1 = res0;
        res2 = res1;
      }
    }
    else
    {
      initial_byte_t b = xh1.fst;
      if (b.major_type == CBOR_MAJOR_TYPE_MAP)
      {
        bool ite;
        if (x_.tag == CBOR_Case_Map)
          ite = true;
        else
          ite = false;
        if (ite)
        {
          cbor_raw x2_ = x_;
          cbor_raw scrut0 = x2_;
          option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_ scrut = OPTION_DEFAULT;
          if (scrut0.tag == CBOR_Case_Map)
          {
            cbor_map a = scrut0.case_CBOR_Case_Map;
            scrut =
              (
                (option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_){
                  .tag = FStar_Pervasives_Native_Some,
                  .v = a.cbor_map_ptr
                }
              );
          }
          else
            scrut =
              (
                (option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_){
                  .tag = FStar_Pervasives_Native_None
                }
              );
          cbor_map_entry *a = NULL;
          if (scrut.tag == FStar_Pervasives_Native_Some)
            a = scrut.v;
          else
            exit(253);
            // a = KRML_EABORT(cbor_map_entry *, "unreachable (pattern matches are exhaustive in F*)");
          size_t pres = res1;
          size_t pi = (size_t)0U;
          size_t i0 = pi;
          bool cond = i0 < (size_t)argument_as_uint64(xh1.fst, xh1.snd);
          while (cond)
          {
            size_t i = pi;
            size_t off = pres;
            cbor_map_entry e = a[i];
            size_t i_ = i + (size_t)1U;
            cbor_raw x11 = e.cbor_map_entry_key;
            size_t res = CBOR_Pulse_Raw_Format_Serialize_ser_(x11, out, off);
            size_t res11 = res;
            cbor_raw x2 = e.cbor_map_entry_value;
            size_t res0 = CBOR_Pulse_Raw_Format_Serialize_ser_(x2, out, res11);
            size_t res2 = res0;
            size_t res1 = res2;
            pi = i_;
            pres = res1;
            size_t i0 = pi;
            cond = i0 < (size_t)argument_as_uint64(xh1.fst, xh1.snd);
          }
          size_t res = pres;
          size_t res0 = res;
          size_t res1 = res0;
          res2 = res1;
        }
        else
        {
          cbor_raw scrut = x_;
          cbor_serialized xs = CBOR_SERIALIZED_DEFAULT;
          if (scrut.tag == CBOR_Case_Serialized_Map)
            xs = scrut.case_CBOR_Case_Serialized_Map;
          else
            exit(253);
            // xs = KRML_EABORT(cbor_serialized, "unreachable (pattern matches are exhaustive in F*)");
          Pulse_Lib_Slice_slice__uint8_t x2_ = xs.cbor_serialized_payload;
          size_t length = len__uint8_t(x2_);
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          sp1 = split__uint8_t(out, res1);
          Pulse_Lib_Slice_slice__uint8_t sp12 = sp1.snd;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          sp2 = split__uint8_t(sp12, length);
          Pulse_Lib_Slice_slice__uint8_t sp21 = sp2.fst;
          copy__uint8_t(sp21, x2_);
          size_t res = res1 + length;
          size_t res0 = res;
          size_t res1 = res0;
          res2 = res1;
        }
      }
      else
      {
        initial_byte_t b = xh1.fst;
        if (b.major_type == CBOR_MAJOR_TYPE_TAGGED)
        {
          bool ite;
          if (x_.tag == CBOR_Case_Tagged)
            ite = true;
          else
            ite = false;
          size_t res;
          if (ite)
          {
            cbor_raw scrut = x_;
            cbor_tagged tg = CBOR_TAGGED_DEFAULT;
            if (scrut.tag == CBOR_Case_Tagged)
              tg = scrut.case_CBOR_Case_Tagged;
            else
              exit(253);
              // tg = KRML_EABORT(cbor_tagged, "unreachable (pattern matches are exhaustive in F*)");
            cbor_raw x2_ = *tg.cbor_tagged_ptr;
            size_t res0 = CBOR_Pulse_Raw_Format_Serialize_ser_(x2_, out, res1);
            size_t res1 = res0;
            size_t res2 = res1;
            res = res2;
          }
          else
          {
            cbor_raw scrut = x_;
            cbor_serialized ser = CBOR_SERIALIZED_DEFAULT;
            if (scrut.tag == CBOR_Case_Serialized_Tagged)
              ser = scrut.case_CBOR_Case_Serialized_Tagged;
            else
              exit(253);
              // ser =
              //   KRML_EABORT(cbor_serialized,
              //     "unreachable (pattern matches are exhaustive in F*)");
            Pulse_Lib_Slice_slice__uint8_t x2_ = ser.cbor_serialized_payload;
            size_t length = len__uint8_t(x2_);
            __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
            sp1 = split__uint8_t(out, res1);
            Pulse_Lib_Slice_slice__uint8_t sp12 = sp1.snd;
            __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
            sp2 = split__uint8_t(sp12, length);
            Pulse_Lib_Slice_slice__uint8_t sp21 = sp2.fst;
            copy__uint8_t(sp21, x2_);
            size_t res0 = res1 + length;
            size_t res1 = res0;
            res = res1;
          }
          res2 = res;
        }
        else
          res2 = res1;
      }
    }
  }
  size_t res = res2;
  size_t res3 = res;
  return res3;
}

static size_t ser(cbor_raw x1_, Pulse_Lib_Slice_slice__uint8_t out, size_t offset)
{
  cbor_raw x2_ = x1_;
  size_t res = CBOR_Pulse_Raw_Format_Serialize_ser_(x2_, out, offset);
  size_t res0 = res;
  return res0;
}

static size_t cbor_serialize(cbor_raw x, Pulse_Lib_Slice_slice__uint8_t output)
{
  size_t res = ser(x, output, (size_t)0U);
  return res;
}

bool CBOR_Pulse_Raw_Format_Serialize_siz_(cbor_raw x_, size_t *out)
{
  header res0 = cbor_raw_with_perm_get_header(x_);
  header xh1 = res0;
  bool res1 = size_header(xh1, out);
  bool res3;
  if (res1)
  {
    initial_byte_t b0 = xh1.fst;
    bool res2;
    if
    (b0.major_type == CBOR_MAJOR_TYPE_BYTE_STRING || b0.major_type == CBOR_MAJOR_TYPE_TEXT_STRING)
    {
      cbor_raw scrut = x_;
      cbor_string c_ = CBOR_STRING_DEFAULT;
      if (scrut.tag == CBOR_Case_String)
        c_ = scrut.case_CBOR_Case_String;
      else
        exit(253);
        // c_ = KRML_EABORT(cbor_string, "unreachable (pattern matches are exhaustive in F*)");
      Pulse_Lib_Slice_slice__uint8_t s = c_.cbor_string_ptr;
      Pulse_Lib_Slice_slice__uint8_t x2_ = s;
      size_t length = len__uint8_t(x2_);
      size_t cur = *out;
      bool res;
      if (cur < length)
        res = false;
      else
      {
        *out = cur - length;
        res = true;
      }
      bool res0 = res;
      res2 = res0;
    }
    else
    {
      initial_byte_t b = xh1.fst;
      if (b.major_type == CBOR_MAJOR_TYPE_ARRAY)
      {
        bool ite;
        if (x_.tag == CBOR_Case_Array)
          ite = true;
        else
          ite = false;
        if (ite)
        {
          cbor_raw x2_ = x_;
          cbor_raw scrut0 = x2_;
          option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_ scrut = OPTION_DEFAULT;
          if (scrut0.tag == CBOR_Case_Array)
          {
            cbor_array a = scrut0.case_CBOR_Case_Array;
            scrut =
              (
                (option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_){
                  .tag = FStar_Pervasives_Native_Some,
                  .v = a.cbor_array_ptr
                }
              );
          }
          else
            scrut =
              (
                (option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_raw_){
                  .tag = FStar_Pervasives_Native_None
                }
              );
          cbor_raw *a;
          if (scrut.tag == FStar_Pervasives_Native_Some)
            a = scrut.v;
          else
            exit(253);
            // a = KRML_EABORT(cbor_raw *, "unreachable (pattern matches are exhaustive in F*)");
          bool pres = true;
          size_t pi = (size_t)0U;
          bool res = pres;
          size_t i0 = pi;
          bool cond = res && i0 < (size_t)argument_as_uint64(xh1.fst, xh1.snd);
          while (cond)
          {
            size_t i0 = pi;
            cbor_raw e = a[i0];
            cbor_raw x2_1 = e;
            bool res = CBOR_Pulse_Raw_Format_Serialize_siz_(x2_1, out);
            bool res0 = res;
            bool res1 = res0;
            if (res1)
            {
              size_t i_ = i0 + (size_t)1U;
              pi = i_;
            }
            else
              pres = false;
            bool res2 = pres;
            size_t i = pi;
            cond = res2 && i < (size_t)argument_as_uint64(xh1.fst, xh1.snd);
          }
          bool res0 = pres;
          bool res1 = res0;
          bool res3 = res1;
          res2 = res3;
        }
        else
        {
          cbor_raw scrut = x_;
          cbor_serialized xs = CBOR_SERIALIZED_DEFAULT;
          if (scrut.tag == CBOR_Case_Serialized_Array)
            xs = scrut.case_CBOR_Case_Serialized_Array;
          else
            exit(253);
            // xs = KRML_EABORT(cbor_serialized, "unreachable (pattern matches are exhaustive in F*)");
          Pulse_Lib_Slice_slice__uint8_t x2_ = xs.cbor_serialized_payload;
          size_t length = len__uint8_t(x2_);
          size_t cur = *out;
          bool res;
          if (cur < length)
            res = false;
          else
          {
            *out = cur - length;
            res = true;
          }
          bool res0 = res;
          bool res1 = res0;
          res2 = res1;
        }
      }
      else
      {
        initial_byte_t b = xh1.fst;
        if (b.major_type == CBOR_MAJOR_TYPE_MAP)
        {
          bool ite;
          if (x_.tag == CBOR_Case_Map)
            ite = true;
          else
            ite = false;
          if (ite)
          {
            cbor_raw x2_ = x_;
            cbor_raw scrut0 = x2_;
            option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_ scrut = OPTION_DEFAULT;
            if (scrut0.tag == CBOR_Case_Map)
            {
              cbor_map a = scrut0.case_CBOR_Case_Map;
              scrut =
                (
                  (option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_){
                    .tag = FStar_Pervasives_Native_Some,
                    .v = a.cbor_map_ptr
                  }
                );
            }
            else
              scrut =
                (
                  (option__LowParse_Pulse_Base_with_perm__CBOR_Pulse_Raw_Type_cbor_map_entry_){
                    .tag = FStar_Pervasives_Native_None
                  }
                );
            cbor_map_entry *a;
            if (scrut.tag == FStar_Pervasives_Native_Some)
              a = scrut.v;
            else
              exit(253);
              // a =
              //   KRML_EABORT(cbor_map_entry *,
              //     "unreachable (pattern matches are exhaustive in F*)");
            bool pres = true;
            size_t pi = (size_t)0U;
            bool res0 = pres;
            size_t i0 = pi;
            bool cond = res0 && i0 < (size_t)argument_as_uint64(xh1.fst, xh1.snd);
            while (cond)
            {
              size_t i0 = pi;
              cbor_map_entry e = a[i0];
              cbor_raw x11 = e.cbor_map_entry_key;
              bool res0 = CBOR_Pulse_Raw_Format_Serialize_siz_(x11, out);
              bool res11 = res0;
              bool res;
              if (res11)
              {
                cbor_raw x2 = e.cbor_map_entry_value;
                bool res0 = CBOR_Pulse_Raw_Format_Serialize_siz_(x2, out);
                bool res2 = res0;
                res = res2;
              }
              else
                res = false;
              if (res)
              {
                size_t i_ = i0 + (size_t)1U;
                pi = i_;
              }
              else
                pres = false;
              bool res1 = pres;
              size_t i = pi;
              cond = res1 && i < (size_t)argument_as_uint64(xh1.fst, xh1.snd);
            }
            bool res = pres;
            bool res1 = res;
            bool res3 = res1;
            res2 = res3;
          }
          else
          {
            cbor_raw scrut = x_;
            cbor_serialized xs = CBOR_SERIALIZED_DEFAULT;
            if (scrut.tag == CBOR_Case_Serialized_Map)
              xs = scrut.case_CBOR_Case_Serialized_Map;
            else
              exit(253);
              // xs =
              //   KRML_EABORT(cbor_serialized,
              //     "unreachable (pattern matches are exhaustive in F*)");
            Pulse_Lib_Slice_slice__uint8_t x2_ = xs.cbor_serialized_payload;
            size_t length = len__uint8_t(x2_);
            size_t cur = *out;
            bool res;
            if (cur < length)
              res = false;
            else
            {
              *out = cur - length;
              res = true;
            }
            bool res0 = res;
            bool res1 = res0;
            res2 = res1;
          }
        }
        else
        {
          initial_byte_t b = xh1.fst;
          if (b.major_type == CBOR_MAJOR_TYPE_TAGGED)
          {
            bool ite;
            if (x_.tag == CBOR_Case_Tagged)
              ite = true;
            else
              ite = false;
            bool res0;
            if (ite)
            {
              cbor_raw scrut = x_;
              cbor_tagged tg = CBOR_TAGGED_DEFAULT;
              if (scrut.tag == CBOR_Case_Tagged)
                tg = scrut.case_CBOR_Case_Tagged;
              else
                exit(253);
                // tg = KRML_EABORT(cbor_tagged, "unreachable (pattern matches are exhaustive in F*)");
              cbor_raw x2_ = *tg.cbor_tagged_ptr;
              bool res = CBOR_Pulse_Raw_Format_Serialize_siz_(x2_, out);
              bool res1 = res;
              bool res2 = res1;
              res0 = res2;
            }
            else
            {
              cbor_raw scrut = x_;
              cbor_serialized ser1 = CBOR_SERIALIZED_DEFAULT;
              if (scrut.tag == CBOR_Case_Serialized_Tagged)
                ser1 = scrut.case_CBOR_Case_Serialized_Tagged;
              else
                exit(253);
                // ser1 =
                //   KRML_EABORT(cbor_serialized,
                //     "unreachable (pattern matches are exhaustive in F*)");
              Pulse_Lib_Slice_slice__uint8_t x2_ = ser1.cbor_serialized_payload;
              size_t length = len__uint8_t(x2_);
              size_t cur = *out;
              bool res;
              if (cur < length)
                res = false;
              else
              {
                *out = cur - length;
                res = true;
              }
              bool res1 = res;
              res0 = res1;
            }
            res2 = res0;
          }
          else
            res2 = true;
        }
      }
    }
    res3 = res2;
  }
  else
    res3 = false;
  bool res = res3;
  return res;
}

static bool siz(cbor_raw x1_, size_t *out)
{
  cbor_raw x2_ = x1_;
  bool res = CBOR_Pulse_Raw_Format_Serialize_siz_(x2_, out);
  bool res0 = res;
  return res0;
}

static size_t cbor_size(cbor_raw x, size_t bound)
{
  size_t output = bound;
  bool res = siz(x, &output);
  if (res)
  {
    size_t rem = output;
    return bound - rem;
  }
  else
    return (size_t)0U;
}

static uint8_t op_Array_Access__uint8_t(Pulse_Lib_Slice_slice__uint8_t a, size_t i)
{
  return a.elt[i];
}

static initial_byte_t read_initial_byte_t(Pulse_Lib_Slice_slice__uint8_t input)
{
  uint8_t last = op_Array_Access__uint8_t(input, (size_t)0U);
  uint8_t res = last;
  uint8_t x = res;
  initial_byte_t
  res0 =
    { .major_type = get_bitfield_gen8(x, 5U, 8U), .additional_info = get_bitfield_gen8(x, 0U, 5U) };
  initial_byte_t res1 = res0;
  initial_byte_t res2 = res1;
  initial_byte_t res3 = res2;
  return res3;
}

static header read_header(Pulse_Lib_Slice_slice__uint8_t input)
{
  size_t i = (size_t)1U;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(input, i);
  Pulse_Lib_Slice_slice__uint8_t s1 = s.fst;
  Pulse_Lib_Slice_slice__uint8_t s2 = s.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res = { .fst = s1, .snd = s2 };
  Pulse_Lib_Slice_slice__uint8_t input10 = res.fst;
  Pulse_Lib_Slice_slice__uint8_t input20 = res.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  split12 = { .fst = input10, .snd = input20 };
  Pulse_Lib_Slice_slice__uint8_t input1 = split12.fst;
  Pulse_Lib_Slice_slice__uint8_t input2 = split12.snd;
  initial_byte_t x = read_initial_byte_t(input1);
  initial_byte_t res0 = x;
  initial_byte_t x1 = res0;
  long_argument x2 = LONG_ARGUMENT_DEFAULT;
  if (x1.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_8_BITS)
    if (x1.major_type == CBOR_MAJOR_TYPE_SIMPLE_VALUE)
    {
      uint8_t last = op_Array_Access__uint8_t(input2, (size_t)0U);
      uint8_t res = last;
      uint8_t x = res;
      long_argument
      res0 = { .tag = LongArgumentSimpleValue, { .case_LongArgumentSimpleValue = x } };
      long_argument res1 = res0;
      long_argument res2 = res1;
      x2 = res2;
    }
    else
    {
      uint8_t last = op_Array_Access__uint8_t(input2, (size_t)0U);
      uint8_t res = last;
      uint8_t x = res;
      long_argument res0 = { .tag = LongArgumentU8, { .case_LongArgumentU8 = x } };
      long_argument res1 = res0;
      x2 = res1;
    }
  else if (x1.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_16_BITS)
  {
    size_t pos_ = (size_t)1U;
    uint8_t last = op_Array_Access__uint8_t(input2, pos_);
    uint8_t last1 = op_Array_Access__uint8_t(input2, (size_t)0U);
    uint16_t n = (uint16_t)last1;
    uint16_t blast = (uint16_t)last;
    uint16_t res = (uint32_t)blast + (uint32_t)n * 256U;
    uint16_t x = res;
    long_argument res0 = { .tag = LongArgumentU16, { .case_LongArgumentU16 = x } };
    long_argument res1 = res0;
    x2 = res1;
  }
  else if (x1.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_32_BITS)
  {
    size_t pos_ = (size_t)3U;
    uint8_t last = op_Array_Access__uint8_t(input2, pos_);
    size_t pos_1 = pos_ - (size_t)1U;
    uint8_t last1 = op_Array_Access__uint8_t(input2, pos_1);
    size_t pos_2 = pos_1 - (size_t)1U;
    uint8_t last2 = op_Array_Access__uint8_t(input2, pos_2);
    uint8_t last3 = op_Array_Access__uint8_t(input2, (size_t)0U);
    uint32_t n = (uint32_t)last3;
    uint32_t blast0 = (uint32_t)last2;
    uint32_t n0 = blast0 + n * 256U;
    uint32_t blast1 = (uint32_t)last1;
    uint32_t n1 = blast1 + n0 * 256U;
    uint32_t blast = (uint32_t)last;
    uint32_t res = blast + n1 * 256U;
    uint32_t x = res;
    long_argument res0 = { .tag = LongArgumentU32, { .case_LongArgumentU32 = x } };
    long_argument res1 = res0;
    x2 = res1;
  }
  else if (x1.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_64_BITS)
  {
    size_t pos_ = (size_t)7U;
    uint8_t last = op_Array_Access__uint8_t(input2, pos_);
    size_t pos_1 = pos_ - (size_t)1U;
    uint8_t last1 = op_Array_Access__uint8_t(input2, pos_1);
    size_t pos_2 = pos_1 - (size_t)1U;
    uint8_t last2 = op_Array_Access__uint8_t(input2, pos_2);
    size_t pos_3 = pos_2 - (size_t)1U;
    uint8_t last3 = op_Array_Access__uint8_t(input2, pos_3);
    size_t pos_4 = pos_3 - (size_t)1U;
    uint8_t last4 = op_Array_Access__uint8_t(input2, pos_4);
    size_t pos_5 = pos_4 - (size_t)1U;
    uint8_t last5 = op_Array_Access__uint8_t(input2, pos_5);
    size_t pos_6 = pos_5 - (size_t)1U;
    uint8_t last6 = op_Array_Access__uint8_t(input2, pos_6);
    uint8_t last7 = op_Array_Access__uint8_t(input2, (size_t)0U);
    uint64_t n = (uint64_t)last7;
    uint64_t blast0 = (uint64_t)last6;
    uint64_t n0 = blast0 + n * 256ULL;
    uint64_t blast1 = (uint64_t)last5;
    uint64_t n1 = blast1 + n0 * 256ULL;
    uint64_t blast2 = (uint64_t)last4;
    uint64_t n2 = blast2 + n1 * 256ULL;
    uint64_t blast3 = (uint64_t)last3;
    uint64_t n3 = blast3 + n2 * 256ULL;
    uint64_t blast4 = (uint64_t)last2;
    uint64_t n4 = blast4 + n3 * 256ULL;
    uint64_t blast5 = (uint64_t)last1;
    uint64_t n5 = blast5 + n4 * 256ULL;
    uint64_t blast = (uint64_t)last;
    uint64_t res = blast + n5 * 256ULL;
    uint64_t x = res;
    long_argument res0 = { .tag = LongArgumentU64, { .case_LongArgumentU64 = x } };
    long_argument res1 = res0;
    x2 = res1;
  }
  else
    x2 = ((long_argument){ .tag = LongArgumentOther });
  header res1 = { .fst = x1, .snd = x2 };
  return res1;
}

static bool validate_header(Pulse_Lib_Slice_slice__uint8_t input, size_t *poffset)
{
  size_t offset1 = *poffset;
  size_t offset2 = *poffset;
  size_t offset30 = *poffset;
  bool is_valid0;
  if (len__uint8_t(input) - offset30 < (size_t)1U)
    is_valid0 = false;
  else
  {
    *poffset = offset30 + (size_t)1U;
    is_valid0 = true;
  }
  bool is_valid1;
  if (is_valid0)
  {
    size_t off = *poffset;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    s_ = split__uint8_t(input, offset2);
    Pulse_Lib_Slice_slice__uint8_t s10 = s_.fst;
    Pulse_Lib_Slice_slice__uint8_t s20 = s_.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    split123 = { .fst = s10, .snd = s20 };
    Pulse_Lib_Slice_slice__uint8_t input23 = split123.snd;
    size_t consumed = off - offset2;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    s1s2 = split__uint8_t(input23, consumed);
    Pulse_Lib_Slice_slice__uint8_t s1 = s1s2.fst;
    Pulse_Lib_Slice_slice__uint8_t s2 = s1s2.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res = { .fst = s1, .snd = s2 };
    Pulse_Lib_Slice_slice__uint8_t left = res.fst;
    Pulse_Lib_Slice_slice__uint8_t right = res.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    split23 = { .fst = left, .snd = right };
    Pulse_Lib_Slice_slice__uint8_t input_ = split23.fst;
    initial_byte_t res0 = read_initial_byte_t(input_);
    initial_byte_t x = res0;
    bool ite;
    if (x.major_type == CBOR_MAJOR_TYPE_SIMPLE_VALUE)
      ite = x.additional_info <= ADDITIONAL_INFO_LONG_ARGUMENT_8_BITS;
    else
      ite = true;
    is_valid1 = ite && x.additional_info < ADDITIONAL_INFO_UNASSIGNED_MIN;
  }
  else
    is_valid1 = false;
  if (is_valid1)
  {
    size_t off = *poffset;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    s_ = split__uint8_t(input, offset1);
    Pulse_Lib_Slice_slice__uint8_t s10 = s_.fst;
    Pulse_Lib_Slice_slice__uint8_t s20 = s_.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    split123 = { .fst = s10, .snd = s20 };
    Pulse_Lib_Slice_slice__uint8_t input23 = split123.snd;
    size_t consumed0 = off - offset1;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    s1s2 = split__uint8_t(input23, consumed0);
    Pulse_Lib_Slice_slice__uint8_t s11 = s1s2.fst;
    Pulse_Lib_Slice_slice__uint8_t s21 = s1s2.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res = { .fst = s11, .snd = s21 };
    Pulse_Lib_Slice_slice__uint8_t left0 = res.fst;
    Pulse_Lib_Slice_slice__uint8_t right0 = res.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    split23 = { .fst = left0, .snd = right0 };
    Pulse_Lib_Slice_slice__uint8_t input_ = split23.fst;
    initial_byte_t x = read_initial_byte_t(input_);
    initial_byte_t res0 = x;
    initial_byte_t res1 = res0;
    initial_byte_t x0 = res1;
    if (x0.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_8_BITS)
      if (x0.major_type == CBOR_MAJOR_TYPE_SIMPLE_VALUE)
      {
        size_t offset2 = *poffset;
        size_t offset3 = *poffset;
        bool is_valid;
        if (len__uint8_t(input) - offset3 < (size_t)1U)
          is_valid = false;
        else
        {
          *poffset = offset3 + (size_t)1U;
          is_valid = true;
        }
        if (is_valid)
        {
          size_t off1 = *poffset;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          s_ = split__uint8_t(input, offset2);
          Pulse_Lib_Slice_slice__uint8_t s10 = s_.fst;
          Pulse_Lib_Slice_slice__uint8_t s20 = s_.snd;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          split123 = { .fst = s10, .snd = s20 };
          Pulse_Lib_Slice_slice__uint8_t input23 = split123.snd;
          size_t consumed = off1 - offset2;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          s1s2 = split__uint8_t(input23, consumed);
          Pulse_Lib_Slice_slice__uint8_t s1 = s1s2.fst;
          Pulse_Lib_Slice_slice__uint8_t s2 = s1s2.snd;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          res = { .fst = s1, .snd = s2 };
          Pulse_Lib_Slice_slice__uint8_t left = res.fst;
          Pulse_Lib_Slice_slice__uint8_t right = res.snd;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          split23 = { .fst = left, .snd = right };
          Pulse_Lib_Slice_slice__uint8_t input_ = split23.fst;
          uint8_t last = op_Array_Access__uint8_t(input_, (size_t)0U);
          uint8_t res0 = last;
          uint8_t res1 = res0;
          uint8_t x1 = res1;
          return MIN_SIMPLE_VALUE_LONG_ARGUMENT <= x1;
        }
        else
          return false;
      }
      else
      {
        size_t offset2 = *poffset;
        if (len__uint8_t(input) - offset2 < (size_t)1U)
          return false;
        else
        {
          *poffset = offset2 + (size_t)1U;
          return true;
        }
      }
    else if (x0.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_16_BITS)
    {
      size_t offset2 = *poffset;
      if (len__uint8_t(input) - offset2 < (size_t)2U)
        return false;
      else
      {
        *poffset = offset2 + (size_t)2U;
        return true;
      }
    }
    else if (x0.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_32_BITS)
    {
      size_t offset2 = *poffset;
      if (len__uint8_t(input) - offset2 < (size_t)4U)
        return false;
      else
      {
        *poffset = offset2 + (size_t)4U;
        return true;
      }
    }
    else if (x0.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_64_BITS)
    {
      size_t offset2 = *poffset;
      if (len__uint8_t(input) - offset2 < (size_t)8U)
        return false;
      else
      {
        *poffset = offset2 + (size_t)8U;
        return true;
      }
    }
    else
      return true;
  }
  else
    return false;
}

static size_t jump_header(Pulse_Lib_Slice_slice__uint8_t input, size_t offset)
{
  size_t off1 = offset + (size_t)1U;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  s_ = split__uint8_t(input, offset);
  Pulse_Lib_Slice_slice__uint8_t s10 = s_.fst;
  Pulse_Lib_Slice_slice__uint8_t s20 = s_.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  split123 = { .fst = s10, .snd = s20 };
  Pulse_Lib_Slice_slice__uint8_t input23 = split123.snd;
  size_t consumed = off1 - offset;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  s1s2 = split__uint8_t(input23, consumed);
  Pulse_Lib_Slice_slice__uint8_t s1 = s1s2.fst;
  Pulse_Lib_Slice_slice__uint8_t s2 = s1s2.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res = { .fst = s1, .snd = s2 };
  Pulse_Lib_Slice_slice__uint8_t left = res.fst;
  Pulse_Lib_Slice_slice__uint8_t right = res.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  split23 = { .fst = left, .snd = right };
  Pulse_Lib_Slice_slice__uint8_t input_ = split23.fst;
  initial_byte_t x = read_initial_byte_t(input_);
  initial_byte_t res0 = x;
  initial_byte_t res1 = res0;
  initial_byte_t x0 = res1;
  if (x0.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_8_BITS)
    return off1 + (size_t)1U;
  else if (x0.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_16_BITS)
    return off1 + (size_t)2U;
  else if (x0.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_32_BITS)
    return off1 + (size_t)4U;
  else if (x0.additional_info == ADDITIONAL_INFO_LONG_ARGUMENT_64_BITS)
    return off1 + (size_t)8U;
  else
    return off1 + (size_t)0U;
}

static bool
validate_recursive_step_count_leaf(
  Pulse_Lib_Slice_slice__uint8_t a,
  size_t bound,
  size_t *prem
)
{
  size_t i = jump_header(a, (size_t)0U);
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(a, i);
  Pulse_Lib_Slice_slice__uint8_t s1 = s.fst;
  Pulse_Lib_Slice_slice__uint8_t s2 = s.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res = { .fst = s1, .snd = s2 };
  Pulse_Lib_Slice_slice__uint8_t input1 = res.fst;
  Pulse_Lib_Slice_slice__uint8_t input2 = res.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  spl = { .fst = input1, .snd = input2 };
  Pulse_Lib_Slice_slice__uint8_t input10 = spl.fst;
  header h = read_header(input10);
  uint8_t typ = get_header_major_type(h);
  if (typ == CBOR_MAJOR_TYPE_ARRAY)
  {
    initial_byte_t b = h.fst;
    long_argument l = h.snd;
    uint64_t arg64 = argument_as_uint64(b, l);
    *prem = (size_t)arg64;
    return false;
  }
  else if (typ == CBOR_MAJOR_TYPE_MAP)
  {
    initial_byte_t b = h.fst;
    long_argument l = h.snd;
    uint64_t arg64 = argument_as_uint64(b, l);
    size_t arg = (size_t)arg64;
    if (arg > bound)
      return true;
    else if (bound - arg < arg)
      return true;
    else
    {
      *prem = arg + arg;
      return false;
    }
  }
  else if (typ == CBOR_MAJOR_TYPE_TAGGED)
  {
    *prem = (size_t)1U;
    return false;
  }
  else
  {
    *prem = (size_t)0U;
    return false;
  }
}

static size_t jump_recursive_step_count_leaf(Pulse_Lib_Slice_slice__uint8_t a)
{
  size_t i = jump_header(a, (size_t)0U);
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(a, i);
  Pulse_Lib_Slice_slice__uint8_t s1 = s.fst;
  Pulse_Lib_Slice_slice__uint8_t s2 = s.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res = { .fst = s1, .snd = s2 };
  Pulse_Lib_Slice_slice__uint8_t input1 = res.fst;
  Pulse_Lib_Slice_slice__uint8_t input2 = res.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  spl = { .fst = input1, .snd = input2 };
  Pulse_Lib_Slice_slice__uint8_t input10 = spl.fst;
  header h = read_header(input10);
  uint8_t typ = get_header_major_type(h);
  if (typ == CBOR_MAJOR_TYPE_ARRAY)
  {
    initial_byte_t b = h.fst;
    long_argument l = h.snd;
    uint64_t arg64 = argument_as_uint64(b, l);
    return (size_t)arg64;
  }
  else if (typ == CBOR_MAJOR_TYPE_MAP)
  {
    initial_byte_t b = h.fst;
    long_argument l = h.snd;
    uint64_t arg64 = argument_as_uint64(b, l);
    size_t arg = (size_t)arg64;
    return arg + arg;
  }
  else if (typ == CBOR_MAJOR_TYPE_TAGGED)
    return (size_t)1U;
  else
    return (size_t)0U;
}

static bool validate_raw_data_item(Pulse_Lib_Slice_slice__uint8_t input, size_t *poffset)
{
  size_t pn = (size_t)1U;
  bool pres = true;
  bool res = pres;
  size_t n0 = pn;
  bool cond = res && n0 > (size_t)0U;
  while (cond)
  {
    size_t off = *poffset;
    size_t n0 = pn;
    if (n0 > len__uint8_t(input) - off)
      pres = false;
    else
    {
      size_t offset1 = *poffset;
      bool is_valid1 = validate_header(input, poffset);
      bool res1;
      if (is_valid1)
      {
        size_t off1 = *poffset;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        s_ = split__uint8_t(input, offset1);
        Pulse_Lib_Slice_slice__uint8_t s10 = s_.fst;
        Pulse_Lib_Slice_slice__uint8_t s20 = s_.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        split123 = { .fst = s10, .snd = s20 };
        Pulse_Lib_Slice_slice__uint8_t input23 = split123.snd;
        size_t consumed = off1 - offset1;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        s1s2 = split__uint8_t(input23, consumed);
        Pulse_Lib_Slice_slice__uint8_t s1 = s1s2.fst;
        Pulse_Lib_Slice_slice__uint8_t s2 = s1s2.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        res = { .fst = s1, .snd = s2 };
        Pulse_Lib_Slice_slice__uint8_t left = res.fst;
        Pulse_Lib_Slice_slice__uint8_t right = res.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        split23 = { .fst = left, .snd = right };
        Pulse_Lib_Slice_slice__uint8_t input_ = split23.fst;
        header res0 = read_header(input_);
        header x = res0;
        initial_byte_t b0 = x.fst;
        if
        (
          b0.major_type
          == CBOR_MAJOR_TYPE_BYTE_STRING
          || b0.major_type == CBOR_MAJOR_TYPE_TEXT_STRING
        )
        {
          size_t offset2 = *poffset;
          initial_byte_t b = x.fst;
          long_argument l0 = x.snd;
          if (len__uint8_t(input) - offset2 < (size_t)argument_as_uint64(b, l0))
            res1 = false;
          else
          {
            initial_byte_t b = x.fst;
            long_argument l = x.snd;
            *poffset = offset2 + (size_t)argument_as_uint64(b, l);
            res1 = true;
          }
        }
        else
          res1 = true;
      }
      else
        res1 = false;
      if (!res1)
        pres = false;
      else
      {
        size_t offset1 = *poffset;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        s_ = split__uint8_t(input, off);
        Pulse_Lib_Slice_slice__uint8_t s10 = s_.fst;
        Pulse_Lib_Slice_slice__uint8_t s20 = s_.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        split123 = { .fst = s10, .snd = s20 };
        Pulse_Lib_Slice_slice__uint8_t input23 = split123.snd;
        size_t consumed = offset1 - off;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        s1s2 = split__uint8_t(input23, consumed);
        Pulse_Lib_Slice_slice__uint8_t s1 = s1s2.fst;
        Pulse_Lib_Slice_slice__uint8_t s2 = s1s2.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        res = { .fst = s1, .snd = s2 };
        Pulse_Lib_Slice_slice__uint8_t left = res.fst;
        Pulse_Lib_Slice_slice__uint8_t right = res.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        split23 = { .fst = left, .snd = right };
        Pulse_Lib_Slice_slice__uint8_t input1 = split23.fst;
        size_t bound = len__uint8_t(input) - off - n0;
        bool res2 = validate_recursive_step_count_leaf(input1, bound, &pn);
        size_t count = pn;
        if (res2 || count > bound)
          pres = false;
        else
        {
          size_t n_ = n0 - (size_t)1U + count;
          pn = n_;
        }
      }
    }
    bool res = pres;
    size_t n = pn;
    cond = res && n > (size_t)0U;
  }
  return pres;
}

static size_t jump_raw_data_item(Pulse_Lib_Slice_slice__uint8_t input, size_t offset)
{
  size_t poffset = offset;
  size_t pn = (size_t)1U;
  size_t n0 = pn;
  bool cond = n0 > (size_t)0U;
  while (cond)
  {
    size_t off = poffset;
    size_t off10 = jump_header(input, off);
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s_ = split__uint8_t(input, off);
    Pulse_Lib_Slice_slice__uint8_t s10 = s_.fst;
    Pulse_Lib_Slice_slice__uint8_t s20 = s_.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    split123 = { .fst = s10, .snd = s20 };
    Pulse_Lib_Slice_slice__uint8_t input23 = split123.snd;
    size_t consumed0 = off10 - off;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    s1s2 = split__uint8_t(input23, consumed0);
    Pulse_Lib_Slice_slice__uint8_t s11 = s1s2.fst;
    Pulse_Lib_Slice_slice__uint8_t s21 = s1s2.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res = { .fst = s11, .snd = s21 };
    Pulse_Lib_Slice_slice__uint8_t left0 = res.fst;
    Pulse_Lib_Slice_slice__uint8_t right0 = res.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    split23 = { .fst = left0, .snd = right0 };
    Pulse_Lib_Slice_slice__uint8_t input_ = split23.fst;
    header res0 = read_header(input_);
    header x = res0;
    initial_byte_t b0 = x.fst;
    size_t off1;
    if
    (b0.major_type == CBOR_MAJOR_TYPE_BYTE_STRING || b0.major_type == CBOR_MAJOR_TYPE_TEXT_STRING)
    {
      initial_byte_t b = x.fst;
      long_argument l = x.snd;
      off1 = off10 + (size_t)argument_as_uint64(b, l);
    }
    else
      off1 = off10 + (size_t)0U;
    poffset = off1;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s_0 = split__uint8_t(input, off);
    Pulse_Lib_Slice_slice__uint8_t s12 = s_0.fst;
    Pulse_Lib_Slice_slice__uint8_t s22 = s_0.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    split1230 = { .fst = s12, .snd = s22 };
    Pulse_Lib_Slice_slice__uint8_t input230 = split1230.snd;
    size_t consumed = off1 - off;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    s1s20 = split__uint8_t(input230, consumed);
    Pulse_Lib_Slice_slice__uint8_t s1 = s1s20.fst;
    Pulse_Lib_Slice_slice__uint8_t s2 = s1s20.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res1 = { .fst = s1, .snd = s2 };
    Pulse_Lib_Slice_slice__uint8_t left = res1.fst;
    Pulse_Lib_Slice_slice__uint8_t right = res1.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    split230 = { .fst = left, .snd = right };
    Pulse_Lib_Slice_slice__uint8_t input1 = split230.fst;
    size_t n = pn;
    size_t unused = len__uint8_t(input) - off1;
    scylla_reset(unused); // KRML_MAYBE_UNUSED_VAR(unused);
    size_t count = jump_recursive_step_count_leaf(input1);
    pn = n - (size_t)1U + count;
    size_t n0 = pn;
    cond = n0 > (size_t)0U;
  }
  return poffset;
}

static cbor_raw cbor_read(Pulse_Lib_Slice_slice__uint8_t input)
{
  header
  ph =
    {
      .fst = { .major_type = CBOR_MAJOR_TYPE_SIMPLE_VALUE, .additional_info = 0U },
      .snd = { .tag = LongArgumentOther }
    };
  size_t i0 = jump_header(input, (size_t)0U);
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(input, i0);
  Pulse_Lib_Slice_slice__uint8_t s1 = s.fst;
  Pulse_Lib_Slice_slice__uint8_t s2 = s.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res = { .fst = s1, .snd = s2 };
  Pulse_Lib_Slice_slice__uint8_t input1 = res.fst;
  Pulse_Lib_Slice_slice__uint8_t input2 = res.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  spl = { .fst = input1, .snd = input2 };
  Pulse_Lib_Slice_slice__uint8_t ph1 = spl.fst;
  Pulse_Lib_Slice_slice__uint8_t outc = spl.snd;
  header h0 = read_header(ph1);
  ph = h0;
  Pulse_Lib_Slice_slice__uint8_t pc = outc;
  header h = ph;
  uint8_t typ = h.fst.major_type;
  if (typ == CBOR_MAJOR_TYPE_UINT64 || typ == CBOR_MAJOR_TYPE_NEG_INT64)
  {
    initial_byte_t b = h.fst;
    long_argument l = h.snd;
    CBOR_Spec_Raw_Base_raw_uint64 i;
    if (l.tag == LongArgumentU8)
    {
      uint8_t v1 = l.case_LongArgumentU8;
      i = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 1U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU16)
    {
      uint16_t v1 = l.case_LongArgumentU16;
      i = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 2U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU32)
    {
      uint32_t v1 = l.case_LongArgumentU32;
      i = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 3U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU64)
    {
      uint64_t v1 = l.case_LongArgumentU64;
      i = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 4U, .value = v1 });
    }
    else if (l.tag == LongArgumentOther)
      i = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 0U, .value = (uint64_t)b.additional_info });
    else
      exit(253);
      // i =
      //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
      //     "unreachable (pattern matches are exhaustive in F*)");
    cbor_int resi = { .cbor_int_type = typ, .cbor_int_size = i.size, .cbor_int_value = i.value };
    return ((cbor_raw){ .tag = CBOR_Case_Int, { .case_CBOR_Case_Int = resi } });
  }
  else if (typ == CBOR_MAJOR_TYPE_TEXT_STRING || typ == CBOR_MAJOR_TYPE_BYTE_STRING)
  {
    initial_byte_t b = h.fst;
    long_argument l = h.snd;
    CBOR_Spec_Raw_Base_raw_uint64 i;
    if (l.tag == LongArgumentU8)
    {
      uint8_t v1 = l.case_LongArgumentU8;
      i = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 1U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU16)
    {
      uint16_t v1 = l.case_LongArgumentU16;
      i = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 2U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU32)
    {
      uint32_t v1 = l.case_LongArgumentU32;
      i = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 3U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU64)
    {
      uint64_t v1 = l.case_LongArgumentU64;
      i = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 4U, .value = v1 });
    }
    else if (l.tag == LongArgumentOther)
      i = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 0U, .value = (uint64_t)b.additional_info });
    else
      exit(253);
      // i =
      //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
      //     "unreachable (pattern matches are exhaustive in F*)");
    cbor_string
    ress = { .cbor_string_type = typ, .cbor_string_size = i.size, .cbor_string_ptr = pc };
    return ((cbor_raw){ .tag = CBOR_Case_String, { .case_CBOR_Case_String = ress } });
  }
  else if (typ == CBOR_MAJOR_TYPE_TAGGED)
  {
    initial_byte_t b = h.fst;
    long_argument l = h.snd;
    CBOR_Spec_Raw_Base_raw_uint64 tag;
    if (l.tag == LongArgumentU8)
    {
      uint8_t v1 = l.case_LongArgumentU8;
      tag = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 1U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU16)
    {
      uint16_t v1 = l.case_LongArgumentU16;
      tag = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 2U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU32)
    {
      uint32_t v1 = l.case_LongArgumentU32;
      tag = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 3U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU64)
    {
      uint64_t v1 = l.case_LongArgumentU64;
      tag = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 4U, .value = v1 });
    }
    else if (l.tag == LongArgumentOther)
      tag = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 0U, .value = (uint64_t)b.additional_info });
    else
      exit(253);
      // tag =
      //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
      //     "unreachable (pattern matches are exhaustive in F*)");
    cbor_serialized rest = { .cbor_serialized_header = tag, .cbor_serialized_payload = pc };
    return
      (
        (cbor_raw){
          .tag = CBOR_Case_Serialized_Tagged,
          { .case_CBOR_Case_Serialized_Tagged = rest }
        }
      );
  }
  else if (typ == CBOR_MAJOR_TYPE_ARRAY)
  {
    initial_byte_t b = h.fst;
    long_argument l = h.snd;
    CBOR_Spec_Raw_Base_raw_uint64 len;
    if (l.tag == LongArgumentU8)
    {
      uint8_t v1 = l.case_LongArgumentU8;
      len = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 1U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU16)
    {
      uint16_t v1 = l.case_LongArgumentU16;
      len = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 2U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU32)
    {
      uint32_t v1 = l.case_LongArgumentU32;
      len = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 3U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU64)
    {
      uint64_t v1 = l.case_LongArgumentU64;
      len = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 4U, .value = v1 });
    }
    else if (l.tag == LongArgumentOther)
      len = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 0U, .value = (uint64_t)b.additional_info });
    else
      exit(253);
      // len =
      //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
      //     "unreachable (pattern matches are exhaustive in F*)");
    cbor_serialized resa = { .cbor_serialized_header = len, .cbor_serialized_payload = pc };
    return
      ((cbor_raw){ .tag = CBOR_Case_Serialized_Array, { .case_CBOR_Case_Serialized_Array = resa } });
  }
  else if (typ == CBOR_MAJOR_TYPE_MAP)
  {
    initial_byte_t b = h.fst;
    long_argument l = h.snd;
    CBOR_Spec_Raw_Base_raw_uint64 len;
    if (l.tag == LongArgumentU8)
    {
      uint8_t v1 = l.case_LongArgumentU8;
      len = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 1U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU16)
    {
      uint16_t v1 = l.case_LongArgumentU16;
      len = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 2U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU32)
    {
      uint32_t v1 = l.case_LongArgumentU32;
      len = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 3U, .value = (uint64_t)v1 });
    }
    else if (l.tag == LongArgumentU64)
    {
      uint64_t v1 = l.case_LongArgumentU64;
      len = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 4U, .value = v1 });
    }
    else if (l.tag == LongArgumentOther)
      len = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 0U, .value = (uint64_t)b.additional_info });
    else
      exit(253);
      // len =
      //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
      //     "unreachable (pattern matches are exhaustive in F*)");
    cbor_serialized resa = { .cbor_serialized_header = len, .cbor_serialized_payload = pc };
    return
      ((cbor_raw){ .tag = CBOR_Case_Serialized_Map, { .case_CBOR_Case_Serialized_Map = resa } });
  }
  else
  {
    initial_byte_t b = h.fst;
    long_argument l = h.snd;
    uint8_t i;
    if (l.tag == LongArgumentOther)
      i = b.additional_info;
    else if (l.tag == LongArgumentSimpleValue)
      i = l.case_LongArgumentSimpleValue;
    else
      exit(253);
      // i = KRML_EABORT(uint8_t, "unreachable (pattern matches are exhaustive in F*)");
    return ((cbor_raw){ .tag = CBOR_Case_Simple, { .case_CBOR_Case_Simple = i } });
  }
}

static cbor_raw cbor_match_serialized_tagged_get_payload(cbor_serialized c)
{
  cbor_raw res = cbor_read(c.cbor_serialized_payload);
  return res;
}

static cbor_raw cbor_serialized_array_item(cbor_serialized c, uint64_t i)
{
  size_t j = (size_t)i;
  size_t pi = (size_t)0U;
  Pulse_Lib_Slice_slice__uint8_t pres = c.cbor_serialized_payload;
  size_t i10 = pi;
  bool cond = i10 < j;
  while (cond)
  {
    Pulse_Lib_Slice_slice__uint8_t res = pres;
    size_t i1 = pi;
    size_t i2 = jump_raw_data_item(res, (size_t)0U);
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(res, i2);
    Pulse_Lib_Slice_slice__uint8_t s1 = s.fst;
    Pulse_Lib_Slice_slice__uint8_t s2 = s.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res1 = { .fst = s1, .snd = s2 };
    Pulse_Lib_Slice_slice__uint8_t input10 = res1.fst;
    Pulse_Lib_Slice_slice__uint8_t input20 = res1.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    res10 = { .fst = input10, .snd = input20 };
    Pulse_Lib_Slice_slice__uint8_t input1 = res10.fst;
    Pulse_Lib_Slice_slice__uint8_t input2 = res10.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    spl = { .fst = input1, .snd = input2 };
    Pulse_Lib_Slice_slice__uint8_t res11 = spl.snd;
    Pulse_Lib_Slice_slice__uint8_t res2 = res11;
    pi = i1 + (size_t)1U;
    pres = res2;
    size_t i10 = pi;
    cond = i10 < j;
  }
  Pulse_Lib_Slice_slice__uint8_t res = pres;
  size_t i1 = jump_raw_data_item(res, (size_t)0U);
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(res, i1);
  Pulse_Lib_Slice_slice__uint8_t s1 = s.fst;
  Pulse_Lib_Slice_slice__uint8_t s2 = s.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res1 = { .fst = s1, .snd = s2 };
  Pulse_Lib_Slice_slice__uint8_t input10 = res1.fst;
  Pulse_Lib_Slice_slice__uint8_t input20 = res1.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  res10 = { .fst = input10, .snd = input20 };
  Pulse_Lib_Slice_slice__uint8_t input1 = res10.fst;
  Pulse_Lib_Slice_slice__uint8_t input2 = res10.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  spl = { .fst = input1, .snd = input2 };
  Pulse_Lib_Slice_slice__uint8_t res11 = spl.fst;
  Pulse_Lib_Slice_slice__uint8_t res2 = res11;
  Pulse_Lib_Slice_slice__uint8_t elt = res2;
  cbor_raw res0 = cbor_read(elt);
  return res0;
}

static Pulse_Lib_Slice_slice__uint8_t cbor_serialized_array_iterator_init(cbor_serialized c)
{
  return c.cbor_serialized_payload;
}

static bool cbor_serialized_array_iterator_is_empty(Pulse_Lib_Slice_slice__uint8_t c)
{
  return len__uint8_t(c) == (size_t)0U;
}

static cbor_raw
cbor_serialized_array_iterator_next(
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw *pi,
  Pulse_Lib_Slice_slice__uint8_t i
)
{
  size_t i1 = jump_raw_data_item(i, (size_t)0U);
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(i, i1);
  Pulse_Lib_Slice_slice__uint8_t s10 = s.fst;
  Pulse_Lib_Slice_slice__uint8_t s20 = s.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  res0 = { .fst = s10, .snd = s20 };
  Pulse_Lib_Slice_slice__uint8_t input10 = res0.fst;
  Pulse_Lib_Slice_slice__uint8_t input20 = res0.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  res1 = { .fst = input10, .snd = input20 };
  Pulse_Lib_Slice_slice__uint8_t input1 = res1.fst;
  Pulse_Lib_Slice_slice__uint8_t input2 = res1.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  sp = { .fst = input1, .snd = input2 };
  Pulse_Lib_Slice_slice__uint8_t s1 = sp.fst;
  Pulse_Lib_Slice_slice__uint8_t s2 = sp.snd;
  cbor_raw res = cbor_read(s1);
  Pulse_Lib_Slice_slice__uint8_t i_ = s2;
  *pi =
    (
      (CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw){
        .tag = CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized,
        { .case_CBOR_Raw_Iterator_Serialized = i_ }
      }
    );
  return res;
}

static Pulse_Lib_Slice_slice__uint8_t cbor_serialized_map_iterator_init(cbor_serialized c)
{
  return c.cbor_serialized_payload;
}

static bool cbor_serialized_map_iterator_is_empty(Pulse_Lib_Slice_slice__uint8_t c)
{
  return len__uint8_t(c) == (size_t)0U;
}

static cbor_map_entry
cbor_serialized_map_iterator_next(
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry *pi,
  Pulse_Lib_Slice_slice__uint8_t i
)
{
  size_t off1 = jump_raw_data_item(i, (size_t)0U);
  size_t i10 = jump_raw_data_item(i, off1);
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(i, i10);
  Pulse_Lib_Slice_slice__uint8_t s10 = s.fst;
  Pulse_Lib_Slice_slice__uint8_t s20 = s.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res = { .fst = s10, .snd = s20 };
  Pulse_Lib_Slice_slice__uint8_t input10 = res.fst;
  Pulse_Lib_Slice_slice__uint8_t input20 = res.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  res0 = { .fst = input10, .snd = input20 };
  Pulse_Lib_Slice_slice__uint8_t input11 = res0.fst;
  Pulse_Lib_Slice_slice__uint8_t input21 = res0.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  sp = { .fst = input11, .snd = input21 };
  Pulse_Lib_Slice_slice__uint8_t s1 = sp.fst;
  Pulse_Lib_Slice_slice__uint8_t s2 = sp.snd;
  size_t i1 = jump_raw_data_item(s1, (size_t)0U);
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s0 = split__uint8_t(s1, i1);
  Pulse_Lib_Slice_slice__uint8_t s110 = s0.fst;
  Pulse_Lib_Slice_slice__uint8_t s210 = s0.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  res3 = { .fst = s110, .snd = s210 };
  Pulse_Lib_Slice_slice__uint8_t input12 = res3.fst;
  Pulse_Lib_Slice_slice__uint8_t input22 = res3.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  res4 = { .fst = input12, .snd = input22 };
  Pulse_Lib_Slice_slice__uint8_t input1 = res4.fst;
  Pulse_Lib_Slice_slice__uint8_t input2 = res4.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  sp1 = { .fst = input1, .snd = input2 };
  Pulse_Lib_Slice_slice__uint8_t s11 = sp1.fst;
  Pulse_Lib_Slice_slice__uint8_t s21 = sp1.snd;
  cbor_raw res1 = cbor_read(s11);
  cbor_raw res2 = cbor_read(s21);
  cbor_map_entry res5 = { .cbor_map_entry_key = res1, .cbor_map_entry_value = res2 };
  Pulse_Lib_Slice_slice__uint8_t i_ = s2;
  *pi =
    (
      (CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry){
        .tag = CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized,
        { .case_CBOR_Raw_Iterator_Serialized = i_ }
      }
    );
  return res5;
}

static int16_t impl_uint8_compare(uint8_t x1, uint8_t x2)
{
  if (x1 < x2)
    return (int16_t)-1;
  else if (x1 > x2)
    return (int16_t)1;
  else
    return (int16_t)0;
}

static int16_t
lex_compare_bytes(Pulse_Lib_Slice_slice__uint8_t s1, Pulse_Lib_Slice_slice__uint8_t s2)
{
  Pulse_Lib_Slice_slice__uint8_t sp1 = s1;
  Pulse_Lib_Slice_slice__uint8_t sp2 = s2;
  size_t pi1 = (size_t)0U;
  size_t pi2 = (size_t)0U;
  size_t n1 = len__uint8_t(sp1);
  size_t n2 = len__uint8_t(sp2);
  int16_t ite;
  if ((size_t)0U < n1)
    if ((size_t)0U < n2)
      ite = (int16_t)0;
    else
      ite = (int16_t)1;
  else if ((size_t)0U < n2)
    ite = (int16_t)-1;
  else
    ite = (int16_t)0;
  int16_t pres = ite;
  int16_t res = pres;
  size_t i10 = pi1;
  bool cond = res == (int16_t)0 && i10 < n1;
  while (cond)
  {
    size_t i10 = pi1;
    uint8_t x1 = op_Array_Access__uint8_t(sp1, i10);
    size_t i2 = pi2;
    uint8_t x2 = op_Array_Access__uint8_t(sp2, i2);
    int16_t res = impl_uint8_compare(x1, x2);
    int16_t c = res;
    if (c == (int16_t)0)
    {
      size_t i1_ = i10 + (size_t)1U;
      size_t i2_ = i2 + (size_t)1U;
      bool ci1_ = i1_ < n1;
      bool ci2_ = i2_ < n2;
      if (ci2_ && !ci1_)
        pres = (int16_t)-1;
      else if (ci1_ && !ci2_)
        pres = (int16_t)1;
      else
      {
        pi1 = i1_;
        pi2 = i2_;
      }
    }
    else
      pres = c;
    int16_t res0 = pres;
    size_t i1 = pi1;
    cond = res0 == (int16_t)0 && i1 < n1;
  }
  int16_t res0 = pres;
  int16_t res1 = res0;
  return res1;
}

static cbor_raw cbor_match_tagged_get_payload(cbor_raw c)
{
  bool ite;
  if (c.tag == CBOR_Case_Serialized_Tagged)
    ite = true;
  else
    ite = false;
  if (ite)
  {
    cbor_serialized cs = CBOR_SERIALIZED_DEFAULT;
    if (c.tag == CBOR_Case_Serialized_Tagged)
      cs = c.case_CBOR_Case_Serialized_Tagged;
    else
      exit(253);
      // cs = KRML_EABORT(cbor_serialized, "unreachable (pattern matches are exhaustive in F*)");
    cbor_raw res = cbor_match_serialized_tagged_get_payload(cs);
    return res;
  }
  else
  {
    cbor_tagged ct = CBOR_TAGGED_DEFAULT;
    if (c.tag == CBOR_Case_Tagged)
      ct = c.case_CBOR_Case_Tagged;
    else
      exit(253);
      // ct = KRML_EABORT(cbor_tagged, "unreachable (pattern matches are exhaustive in F*)");
    return *ct.cbor_tagged_ptr;
  }
}

static cbor_raw cbor_array_item(cbor_raw c, uint64_t i)
{
  if (c.tag == CBOR_Case_Serialized_Array)
  {
    cbor_serialized c_ = c.case_CBOR_Case_Serialized_Array;
    cbor_raw res = cbor_serialized_array_item(c_, i);
    return res;
  }
  else if (c.tag == CBOR_Case_Array)
  {
    cbor_array c_ = c.case_CBOR_Case_Array;
    return c_.cbor_array_ptr[(size_t)i];
  }
  else
  {
    KRML_HOST_EPRINTF("KaRaMeL abort at %s:%d\n%s\n",
      __FILE__,
      __LINE__,
      "unreachable (pattern matches are exhaustive in F*)");
    KRML_HOST_EXIT(255U);
  }
}

static Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw
from_array__CBOR_Pulse_Raw_Type_cbor_raw(cbor_raw *a, size_t alen)
{
  cbor_raw *ptr = a;
  return ((Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw){ .elt = ptr, .len = alen });
}

static CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw
cbor_array_iterator_init(cbor_raw c)
{
  if (c.tag == CBOR_Case_Serialized_Array)
  {
    cbor_serialized c_ = c.case_CBOR_Case_Serialized_Array;
    Pulse_Lib_Slice_slice__uint8_t i_ = cbor_serialized_array_iterator_init(c_);
    return
      (
        (CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw){
          .tag = CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized,
          { .case_CBOR_Raw_Iterator_Serialized = i_ }
        }
      );
  }
  else if (c.tag == CBOR_Case_Array)
  {
    cbor_array c_ = c.case_CBOR_Case_Array;
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw
    s =
      from_array__CBOR_Pulse_Raw_Type_cbor_raw(c_.cbor_array_ptr,
        (size_t)c_.cbor_array_length.value);
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw s0 = s;
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw i = s0;
    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw
    res =
      {
        .tag = CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice,
        { .case_CBOR_Raw_Iterator_Slice = i }
      };
    return res;
  }
  else
  {
    KRML_HOST_EPRINTF("KaRaMeL abort at %s:%d\n%s\n",
      __FILE__,
      __LINE__,
      "unreachable (pattern matches are exhaustive in F*)");
    KRML_HOST_EXIT(255U);
  }
}

static size_t
len__CBOR_Pulse_Raw_Type_cbor_raw(Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw s)
{
  return s.len;
}

static bool
cbor_array_iterator_is_empty(
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw c
)
{
  if (c.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
  {
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw c_ = c.case_CBOR_Raw_Iterator_Slice;
    bool res = len__CBOR_Pulse_Raw_Type_cbor_raw(c_) == (size_t)0U;
    bool res0 = res;
    return res0;
  }
  else if (c.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
  {
    Pulse_Lib_Slice_slice__uint8_t c_ = c.case_CBOR_Raw_Iterator_Serialized;
    bool res = cbor_serialized_array_iterator_is_empty(c_);
    return res;
  }
  else
  {
    KRML_HOST_EPRINTF("KaRaMeL abort at %s:%d\n%s\n",
      __FILE__,
      __LINE__,
      "unreachable (pattern matches are exhaustive in F*)");
    KRML_HOST_EXIT(255U);
  }
}

static cbor_raw
op_Array_Access__CBOR_Pulse_Raw_Type_cbor_raw(
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw a,
  size_t i
)
{
  return a.elt[i];
}

typedef struct
__Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_s
{
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw fst;
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw snd;
}
__Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw;

static __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw
split__CBOR_Pulse_Raw_Type_cbor_raw(
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw s,
  size_t i
)
{
  cbor_raw *elt_ = s.elt + i;
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw s1 = { .elt = s.elt, .len = i };
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw s2 = { .elt = elt_, .len = s.len - i };
  return
    (
      (__Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw){
        .fst = s1,
        .snd = s2
      }
    );
}

static cbor_raw
cbor_array_iterator_next(
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw *pi
)
{
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw i0 = *pi;
  if (i0.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
  {
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw i1 = i0.case_CBOR_Raw_Iterator_Slice;
    cbor_raw res = op_Array_Access__CBOR_Pulse_Raw_Type_cbor_raw(i1, (size_t)0U);
    __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw
    sp = split__CBOR_Pulse_Raw_Type_cbor_raw(i1, (size_t)1U);
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw s_ = sp.snd;
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw i11 = s_;
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw i_ = i11;
    *pi =
      (
        (CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw){
          .tag = CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice,
          { .case_CBOR_Raw_Iterator_Slice = i_ }
        }
      );
    cbor_raw res0 = res;
    return res0;
  }
  else if (i0.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
  {
    Pulse_Lib_Slice_slice__uint8_t i1 = i0.case_CBOR_Raw_Iterator_Serialized;
    cbor_raw res = cbor_serialized_array_iterator_next(pi, i1);
    return res;
  }
  else
  {
    KRML_HOST_EPRINTF("KaRaMeL abort at %s:%d\n%s\n",
      __FILE__,
      __LINE__,
      "unreachable (pattern matches are exhaustive in F*)");
    KRML_HOST_EXIT(255U);
  }
}

static Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry
from_array__CBOR_Pulse_Raw_Type_cbor_map_entry(cbor_map_entry *a, size_t alen)
{
  cbor_map_entry *ptr = a;
  return
    ((Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry){ .elt = ptr, .len = alen });
}

static CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
cbor_map_iterator_init(cbor_raw c)
{
  if (c.tag == CBOR_Case_Serialized_Map)
  {
    cbor_serialized c_ = c.case_CBOR_Case_Serialized_Map;
    Pulse_Lib_Slice_slice__uint8_t i_ = cbor_serialized_map_iterator_init(c_);
    return
      (
        (CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry){
          .tag = CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized,
          { .case_CBOR_Raw_Iterator_Serialized = i_ }
        }
      );
  }
  else if (c.tag == CBOR_Case_Map)
  {
    cbor_map c_ = c.case_CBOR_Case_Map;
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry
    s =
      from_array__CBOR_Pulse_Raw_Type_cbor_map_entry(c_.cbor_map_ptr,
        (size_t)c_.cbor_map_length.value);
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry s0 = s;
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry i = s0;
    CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
    res =
      {
        .tag = CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice,
        { .case_CBOR_Raw_Iterator_Slice = i }
      };
    return res;
  }
  else
  {
    KRML_HOST_EPRINTF("KaRaMeL abort at %s:%d\n%s\n",
      __FILE__,
      __LINE__,
      "unreachable (pattern matches are exhaustive in F*)");
    KRML_HOST_EXIT(255U);
  }
}

static size_t
len__CBOR_Pulse_Raw_Type_cbor_map_entry(
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry s
)
{
  return s.len;
}

static bool
cbor_map_iterator_is_empty(
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry c
)
{
  if (c.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
  {
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry c_ = c.case_CBOR_Raw_Iterator_Slice;
    bool res = len__CBOR_Pulse_Raw_Type_cbor_map_entry(c_) == (size_t)0U;
    bool res0 = res;
    return res0;
  }
  else if (c.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
  {
    Pulse_Lib_Slice_slice__uint8_t c_ = c.case_CBOR_Raw_Iterator_Serialized;
    bool res = cbor_serialized_map_iterator_is_empty(c_);
    return res;
  }
  else
  {
    KRML_HOST_EPRINTF("KaRaMeL abort at %s:%d\n%s\n",
      __FILE__,
      __LINE__,
      "unreachable (pattern matches are exhaustive in F*)");
    KRML_HOST_EXIT(255U);
  }
}

static cbor_map_entry
op_Array_Access__CBOR_Pulse_Raw_Type_cbor_map_entry(
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry a,
  size_t i
)
{
  return a.elt[i];
}

typedef struct
__Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_s
{
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry fst;
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry snd;
}
__Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry;

static __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry
split__CBOR_Pulse_Raw_Type_cbor_map_entry(
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry s,
  size_t i
)
{
  cbor_map_entry *elt_ = s.elt + i;
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry s1 = { .elt = s.elt, .len = i };
  Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry
  s2 = { .elt = elt_, .len = s.len - i };
  return
    (
      (__Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry){
        .fst = s1,
        .snd = s2
      }
    );
}

static cbor_map_entry
cbor_map_iterator_next(
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry *pi
)
{
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry i0 = *pi;
  if (i0.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
  {
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry i1 = i0.case_CBOR_Raw_Iterator_Slice;
    cbor_map_entry res = op_Array_Access__CBOR_Pulse_Raw_Type_cbor_map_entry(i1, (size_t)0U);
    __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry
    sp = split__CBOR_Pulse_Raw_Type_cbor_map_entry(i1, (size_t)1U);
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry s_ = sp.snd;
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry i11 = s_;
    Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry i_ = i11;
    *pi =
      (
        (CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry){
          .tag = CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice,
          { .case_CBOR_Raw_Iterator_Slice = i_ }
        }
      );
    cbor_map_entry res0 = res;
    return res0;
  }
  else if (i0.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
  {
    Pulse_Lib_Slice_slice__uint8_t i1 = i0.case_CBOR_Raw_Iterator_Serialized;
    cbor_map_entry res = cbor_serialized_map_iterator_next(pi, i1);
    return res;
  }
  else
  {
    KRML_HOST_EPRINTF("KaRaMeL abort at %s:%d\n%s\n",
      __FILE__,
      __LINE__,
      "unreachable (pattern matches are exhaustive in F*)");
    KRML_HOST_EXIT(255U);
  }
}

static uint8_t impl_major_type(cbor_raw x)
{
  if (x.tag == CBOR_Case_Simple)
    return CBOR_MAJOR_TYPE_SIMPLE_VALUE;
  else if (x.tag == CBOR_Case_Int)
  {
    cbor_int c_;
    if (x.tag == CBOR_Case_Int)
      c_ = x.case_CBOR_Case_Int;
    else
      exit(253);
      // c_ = KRML_EABORT(cbor_int, "unreachable (pattern matches are exhaustive in F*)");
    return c_.cbor_int_type;
  }
  else if (x.tag == CBOR_Case_String)
  {
    cbor_string c_ = CBOR_STRING_DEFAULT;
    if (x.tag == CBOR_Case_String)
      c_ = x.case_CBOR_Case_String;
    else
      exit(253);
      // c_ = KRML_EABORT(cbor_string, "unreachable (pattern matches are exhaustive in F*)");
    return c_.cbor_string_type;
  }
  else if (x.tag == CBOR_Case_Tagged)
    return CBOR_MAJOR_TYPE_TAGGED;
  else if (x.tag == CBOR_Case_Serialized_Tagged)
    return CBOR_MAJOR_TYPE_TAGGED;
  else if (x.tag == CBOR_Case_Array)
    return CBOR_MAJOR_TYPE_ARRAY;
  else if (x.tag == CBOR_Case_Serialized_Array)
    return CBOR_MAJOR_TYPE_ARRAY;
  else if (x.tag == CBOR_Case_Map)
    return CBOR_MAJOR_TYPE_MAP;
  else if (x.tag == CBOR_Case_Serialized_Map)
    return CBOR_MAJOR_TYPE_MAP;
  else
  {
    KRML_HOST_EPRINTF("KaRaMeL abort at %s:%d\n%s\n",
      __FILE__,
      __LINE__,
      "unreachable (pattern matches are exhaustive in F*)");
    KRML_HOST_EXIT(255U);
  }
}

static int16_t uint64_compare(uint64_t x1, uint64_t x2)
{
  if (x1 < x2)
    return (int16_t)-1;
  else if (x1 > x2)
    return (int16_t)1;
  else
    return (int16_t)0;
}

static int16_t
impl_raw_uint64_compare(CBOR_Spec_Raw_Base_raw_uint64 x1, CBOR_Spec_Raw_Base_raw_uint64 x2)
{
  int16_t c = impl_uint8_compare(x1.size, x2.size);
  if (c == (int16_t)0)
    return uint64_compare(x1.value, x2.value);
  else
    return c;
}

int16_t CBOR_Pulse_Raw_Compare_impl_cbor_compare(cbor_raw x1, cbor_raw x2)
{
  uint8_t ty1 = impl_major_type(x1);
  uint8_t ty2 = impl_major_type(x2);
  int16_t c = impl_uint8_compare(ty1, ty2);
  if (c == (int16_t)0)
    if (ty1 == CBOR_MAJOR_TYPE_UINT64 || ty1 == CBOR_MAJOR_TYPE_NEG_INT64)
    {
      cbor_int c_;
      if (x1.tag == CBOR_Case_Int)
        c_ = x1.case_CBOR_Case_Int;
      else
        exit(253);
        // c_ = KRML_EABORT(cbor_int, "unreachable (pattern matches are exhaustive in F*)");
      CBOR_Spec_Raw_Base_raw_uint64 i1 = { .size = c_.cbor_int_size, .value = c_.cbor_int_value };
      cbor_int c_0;
      if (x2.tag == CBOR_Case_Int)
        c_0 = x2.case_CBOR_Case_Int;
      else
        exit(253);
        // c_0 = KRML_EABORT(cbor_int, "unreachable (pattern matches are exhaustive in F*)");
      CBOR_Spec_Raw_Base_raw_uint64 i2 = { .size = c_0.cbor_int_size, .value = c_0.cbor_int_value };
      return impl_raw_uint64_compare(i1, i2);
    }
    else if (ty1 == CBOR_MAJOR_TYPE_BYTE_STRING || ty1 == CBOR_MAJOR_TYPE_TEXT_STRING)
    {
      cbor_string c_ = CBOR_STRING_DEFAULT;
      if (x1.tag == CBOR_Case_String)
        c_ = x1.case_CBOR_Case_String;
      else
        exit(253);
        // c_ = KRML_EABORT(cbor_string, "unreachable (pattern matches are exhaustive in F*)");
      CBOR_Spec_Raw_Base_raw_uint64
      res = { .size = c_.cbor_string_size, .value = (uint64_t)len__uint8_t(c_.cbor_string_ptr) };
      CBOR_Spec_Raw_Base_raw_uint64 i1 = res;
      cbor_string c_0 = CBOR_STRING_DEFAULT;
      if (x2.tag == CBOR_Case_String)
        c_0 = x2.case_CBOR_Case_String;
      else
        exit(253);
        // c_0 = KRML_EABORT(cbor_string, "unreachable (pattern matches are exhaustive in F*)");
      CBOR_Spec_Raw_Base_raw_uint64
      res0 = { .size = c_0.cbor_string_size, .value = (uint64_t)len__uint8_t(c_0.cbor_string_ptr) };
      CBOR_Spec_Raw_Base_raw_uint64 i2 = res0;
      int16_t c1 = impl_raw_uint64_compare(i1, i2);
      if (c1 == (int16_t)0)
      {
        cbor_string c_ = CBOR_STRING_DEFAULT;
        if (x1.tag == CBOR_Case_String)
          c_ = x1.case_CBOR_Case_String;
        else
          exit(253);
          // c_ = KRML_EABORT(cbor_string, "unreachable (pattern matches are exhaustive in F*)");
        Pulse_Lib_Slice_slice__uint8_t pl1 = c_.cbor_string_ptr;
        cbor_string c_0 = CBOR_STRING_DEFAULT;
        if (x2.tag == CBOR_Case_String)
          c_0 = x2.case_CBOR_Case_String;
        else
          exit(253);
          // c_0 = KRML_EABORT(cbor_string, "unreachable (pattern matches are exhaustive in F*)");
        Pulse_Lib_Slice_slice__uint8_t pl2 = c_0.cbor_string_ptr;
        int16_t res = lex_compare_bytes(pl1, pl2);
        return res;
      }
      else
        return c1;
    }
    else if (ty1 == CBOR_MAJOR_TYPE_TAGGED)
    {
      CBOR_Spec_Raw_Base_raw_uint64 tag1;
      if (x1.tag == CBOR_Case_Tagged)
      {
        cbor_tagged c_ = x1.case_CBOR_Case_Tagged;
        tag1 = c_.cbor_tagged_tag;
      }
      else if (x1.tag == CBOR_Case_Serialized_Tagged)
      {
        cbor_serialized c_ = x1.case_CBOR_Case_Serialized_Tagged;
        tag1 = c_.cbor_serialized_header;
      }
      else
        exit(253);
        // tag1 =
        //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
        //     "unreachable (pattern matches are exhaustive in F*)");
      CBOR_Spec_Raw_Base_raw_uint64 tag2;
      if (x2.tag == CBOR_Case_Tagged)
      {
        cbor_tagged c_ = x2.case_CBOR_Case_Tagged;
        tag2 = c_.cbor_tagged_tag;
      }
      else if (x2.tag == CBOR_Case_Serialized_Tagged)
      {
        cbor_serialized c_ = x2.case_CBOR_Case_Serialized_Tagged;
        tag2 = c_.cbor_serialized_header;
      }
      else
        exit(253);
        // tag2 =
        //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
        //     "unreachable (pattern matches are exhaustive in F*)");
      int16_t c1 = impl_raw_uint64_compare(tag1, tag2);
      if (c1 == (int16_t)0)
      {
        cbor_raw pl1 = cbor_match_tagged_get_payload(x1);
        cbor_raw pl2 = cbor_match_tagged_get_payload(x2);
        int16_t res = CBOR_Pulse_Raw_Compare_impl_cbor_compare(pl1, pl2);
        return res;
      }
      else
        return c1;
    }
    else if (ty1 == CBOR_MAJOR_TYPE_ARRAY)
    {
      CBOR_Spec_Raw_Base_raw_uint64 len1;
      if (x1.tag == CBOR_Case_Array)
      {
        cbor_array c_ = x1.case_CBOR_Case_Array;
        len1 = c_.cbor_array_length;
      }
      else if (x1.tag == CBOR_Case_Serialized_Array)
      {
        cbor_serialized c_ = x1.case_CBOR_Case_Serialized_Array;
        len1 = c_.cbor_serialized_header;
      }
      else
        exit(253);
        // len1 =
        //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
        //     "unreachable (pattern matches are exhaustive in F*)");
      CBOR_Spec_Raw_Base_raw_uint64 len2;
      if (x2.tag == CBOR_Case_Array)
      {
        cbor_array c_ = x2.case_CBOR_Case_Array;
        len2 = c_.cbor_array_length;
      }
      else if (x2.tag == CBOR_Case_Serialized_Array)
      {
        cbor_serialized c_ = x2.case_CBOR_Case_Serialized_Array;
        len2 = c_.cbor_serialized_header;
      }
      else
        exit(253);
        // len2 =
        //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
        //     "unreachable (pattern matches are exhaustive in F*)");
      int16_t c1 = impl_raw_uint64_compare(len1, len2);
      if (c1 == (int16_t)0)
      {
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw
        i1 = cbor_array_iterator_init(x1);
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw
        i2 = cbor_array_iterator_init(x2);
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw pl1 = i1;
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw pl2 = i2;
        bool fin1;
        if (pl1.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
        {
          Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw c_ = pl1.case_CBOR_Raw_Iterator_Slice;
          bool res = len__CBOR_Pulse_Raw_Type_cbor_raw(c_) == (size_t)0U;
          bool res0 = res;
          fin1 = res0;
        }
        else if (pl1.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
        {
          Pulse_Lib_Slice_slice__uint8_t c_ = pl1.case_CBOR_Raw_Iterator_Serialized;
          bool res = cbor_serialized_array_iterator_is_empty(c_);
          fin1 = res;
        }
        else
          exit(253);
          // fin1 = KRML_EABORT(bool, "unreachable (pattern matches are exhaustive in F*)");
        bool fin2;
        if (pl2.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
        {
          Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw c_ = pl2.case_CBOR_Raw_Iterator_Slice;
          bool res = len__CBOR_Pulse_Raw_Type_cbor_raw(c_) == (size_t)0U;
          bool res0 = res;
          fin2 = res0;
        }
        else if (pl2.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
        {
          Pulse_Lib_Slice_slice__uint8_t c_ = pl2.case_CBOR_Raw_Iterator_Serialized;
          bool res = cbor_serialized_array_iterator_is_empty(c_);
          fin2 = res;
        }
        else
          exit(253);
          // fin2 = KRML_EABORT(bool, "unreachable (pattern matches are exhaustive in F*)");
        int16_t res0;
        if (fin1)
          if (fin2)
            res0 = (int16_t)0;
          else
            res0 = (int16_t)-1;
        else if (fin2)
          res0 = (int16_t)1;
        else
        {
          CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw pi1 = pl1;
          CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw pi2 = pl2;
          int16_t pres = (int16_t)0;
          bool pfin1 = false;
          int16_t res1 = pres;
          bool fin110 = pfin1;
          bool cond = res1 == (int16_t)0 && !fin110;
          while (cond)
          {
            CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw i00 = pi1;
            cbor_raw elt1 = CBOR_RAW_DEFAULT;
            if (i00.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
            {
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw
              i = i00.case_CBOR_Raw_Iterator_Slice;
              cbor_raw res = op_Array_Access__CBOR_Pulse_Raw_Type_cbor_raw(i, (size_t)0U);
              __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw
              sp = split__CBOR_Pulse_Raw_Type_cbor_raw(i, (size_t)1U);
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw s_ = sp.snd;
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw i11 = s_;
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw i_ = i11;
              pi1 =
                (
                  (CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw){
                    .tag = CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice,
                    { .case_CBOR_Raw_Iterator_Slice = i_ }
                  }
                );
              cbor_raw res0 = res;
              elt1 = res0;
            }
            else if (i00.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
            {
              Pulse_Lib_Slice_slice__uint8_t i = i00.case_CBOR_Raw_Iterator_Serialized;
              cbor_raw res = cbor_serialized_array_iterator_next(&pi1, i);
              elt1 = res;
            }
            else
              exit(253);
              // elt1 = KRML_EABORT(cbor_raw, "unreachable (pattern matches are exhaustive in F*)");
            CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw i0 = pi2;
            cbor_raw elt2 = CBOR_RAW_DEFAULT;
            if (i0.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
            {
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw
              i = i0.case_CBOR_Raw_Iterator_Slice;
              cbor_raw res = op_Array_Access__CBOR_Pulse_Raw_Type_cbor_raw(i, (size_t)0U);
              __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_raw
              sp = split__CBOR_Pulse_Raw_Type_cbor_raw(i, (size_t)1U);
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw s_ = sp.snd;
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw i11 = s_;
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw i_ = i11;
              pi2 =
                (
                  (CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw){
                    .tag = CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice,
                    { .case_CBOR_Raw_Iterator_Slice = i_ }
                  }
                );
              cbor_raw res0 = res;
              elt2 = res0;
            }
            else if (i0.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
            {
              Pulse_Lib_Slice_slice__uint8_t i = i0.case_CBOR_Raw_Iterator_Serialized;
              cbor_raw res = cbor_serialized_array_iterator_next(&pi2, i);
              elt2 = res;
            }
            else
              exit(253);
              // elt2 = KRML_EABORT(cbor_raw, "unreachable (pattern matches are exhaustive in F*)");
            cbor_raw pelt1 = elt1;
            cbor_raw pelt2 = elt2;
            int16_t res = CBOR_Pulse_Raw_Compare_impl_cbor_compare(pelt1, pelt2);
            int16_t c2 = res;
            if (c2 == (int16_t)0)
            {
              CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw i11 = pi1;
              bool fin11;
              if (i11.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
              {
                Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw
                c_ = i11.case_CBOR_Raw_Iterator_Slice;
                bool res = len__CBOR_Pulse_Raw_Type_cbor_raw(c_) == (size_t)0U;
                bool res0 = res;
                fin11 = res0;
              }
              else if (i11.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
              {
                Pulse_Lib_Slice_slice__uint8_t c_ = i11.case_CBOR_Raw_Iterator_Serialized;
                bool res = cbor_serialized_array_iterator_is_empty(c_);
                fin11 = res;
              }
              else
                exit(253);
                // fin11 = KRML_EABORT(bool, "unreachable (pattern matches are exhaustive in F*)");
              CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw i21 = pi2;
              bool fin21;
              if (i21.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
              {
                Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_raw
                c_ = i21.case_CBOR_Raw_Iterator_Slice;
                bool res = len__CBOR_Pulse_Raw_Type_cbor_raw(c_) == (size_t)0U;
                bool res0 = res;
                fin21 = res0;
              }
              else if (i21.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
              {
                Pulse_Lib_Slice_slice__uint8_t c_ = i21.case_CBOR_Raw_Iterator_Serialized;
                bool res = cbor_serialized_array_iterator_is_empty(c_);
                fin21 = res;
              }
              else
                exit(253);
                // fin21 = KRML_EABORT(bool, "unreachable (pattern matches are exhaustive in F*)");
              if (fin11 == fin21)
                pfin1 = fin11;
              else if (fin11)
                pres = (int16_t)-1;
              else
                pres = (int16_t)1;
            }
            else
              pres = c2;
            int16_t res0 = pres;
            bool fin11 = pfin1;
            cond = res0 == (int16_t)0 && !fin11;
          }
          res0 = pres;
        }
        int16_t res = res0;
        return res;
      }
      else
        return c1;
    }
    else if (ty1 == CBOR_MAJOR_TYPE_MAP)
    {
      CBOR_Spec_Raw_Base_raw_uint64 len1;
      if (x1.tag == CBOR_Case_Map)
      {
        cbor_map c_ = x1.case_CBOR_Case_Map;
        len1 = c_.cbor_map_length;
      }
      else if (x1.tag == CBOR_Case_Serialized_Map)
      {
        cbor_serialized c_ = x1.case_CBOR_Case_Serialized_Map;
        len1 = c_.cbor_serialized_header;
      }
      else
        exit(253);
        // len1 =
        //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
        //     "unreachable (pattern matches are exhaustive in F*)");
      CBOR_Spec_Raw_Base_raw_uint64 len2;
      if (x2.tag == CBOR_Case_Map)
      {
        cbor_map c_ = x2.case_CBOR_Case_Map;
        len2 = c_.cbor_map_length;
      }
      else if (x2.tag == CBOR_Case_Serialized_Map)
      {
        cbor_serialized c_ = x2.case_CBOR_Case_Serialized_Map;
        len2 = c_.cbor_serialized_header;
      }
      else
        exit(253);
        // len2 =
        //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
        //     "unreachable (pattern matches are exhaustive in F*)");
      int16_t c1 = impl_raw_uint64_compare(len1, len2);
      if (c1 == (int16_t)0)
      {
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
        i1 = cbor_map_iterator_init(x1);
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
        i2 = cbor_map_iterator_init(x2);
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry pl1 = i1;
        CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry pl2 = i2;
        bool fin1;
        if (pl1.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
        {
          Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry
          c_ = pl1.case_CBOR_Raw_Iterator_Slice;
          bool res = len__CBOR_Pulse_Raw_Type_cbor_map_entry(c_) == (size_t)0U;
          bool res0 = res;
          fin1 = res0;
        }
        else if (pl1.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
        {
          Pulse_Lib_Slice_slice__uint8_t c_ = pl1.case_CBOR_Raw_Iterator_Serialized;
          bool res = cbor_serialized_map_iterator_is_empty(c_);
          fin1 = res;
        }
        else
          exit(253);
          // fin1 = KRML_EABORT(bool, "unreachable (pattern matches are exhaustive in F*)");
        bool fin2;
        if (pl2.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
        {
          Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry
          c_ = pl2.case_CBOR_Raw_Iterator_Slice;
          bool res = len__CBOR_Pulse_Raw_Type_cbor_map_entry(c_) == (size_t)0U;
          bool res0 = res;
          fin2 = res0;
        }
        else if (pl2.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
        {
          Pulse_Lib_Slice_slice__uint8_t c_ = pl2.case_CBOR_Raw_Iterator_Serialized;
          bool res = cbor_serialized_map_iterator_is_empty(c_);
          fin2 = res;
        }
        else
          exit(253);
          // fin2 = KRML_EABORT(bool, "unreachable (pattern matches are exhaustive in F*)");
        int16_t res0;
        if (fin1)
          if (fin2)
            res0 = (int16_t)0;
          else
            res0 = (int16_t)-1;
        else if (fin2)
          res0 = (int16_t)1;
        else
        {
          CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry pi1 = pl1;
          CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry pi2 = pl2;
          int16_t pres = (int16_t)0;
          bool pfin1 = false;
          int16_t res1 = pres;
          bool fin110 = pfin1;
          bool cond = res1 == (int16_t)0 && !fin110;
          while (cond)
          {
            CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry i00 = pi1;
            cbor_map_entry elt1 = CBOR_MAP_ENTRY_DEFAULT;
            if (i00.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
            {
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry
              i = i00.case_CBOR_Raw_Iterator_Slice;
              cbor_map_entry
              res = op_Array_Access__CBOR_Pulse_Raw_Type_cbor_map_entry(i, (size_t)0U);
              __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry
              sp = split__CBOR_Pulse_Raw_Type_cbor_map_entry(i, (size_t)1U);
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry s_ = sp.snd;
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry i11 = s_;
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry i_ = i11;
              pi1 =
                (
                  (CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry){
                    .tag = CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice,
                    { .case_CBOR_Raw_Iterator_Slice = i_ }
                  }
                );
              cbor_map_entry res0 = res;
              elt1 = res0;
            }
            else if (i00.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
            {
              Pulse_Lib_Slice_slice__uint8_t i = i00.case_CBOR_Raw_Iterator_Serialized;
              cbor_map_entry res = cbor_serialized_map_iterator_next(&pi1, i);
              elt1 = res;
            }
            else
              exit(253);
              // elt1 =
              //   KRML_EABORT(cbor_map_entry,
              //     "unreachable (pattern matches are exhaustive in F*)");
            CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry i0 = pi2;
            cbor_map_entry elt2 = CBOR_MAP_ENTRY_DEFAULT;
            if (i0.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
            {
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry
              i = i0.case_CBOR_Raw_Iterator_Slice;
              cbor_map_entry
              res = op_Array_Access__CBOR_Pulse_Raw_Type_cbor_map_entry(i, (size_t)0U);
              __Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry_Pulse_Lib_Slice_slice_CBOR_Pulse_Raw_Type_cbor_map_entry
              sp = split__CBOR_Pulse_Raw_Type_cbor_map_entry(i, (size_t)1U);
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry s_ = sp.snd;
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry i11 = s_;
              Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry i_ = i11;
              pi2 =
                (
                  (CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry){
                    .tag = CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice,
                    { .case_CBOR_Raw_Iterator_Slice = i_ }
                  }
                );
              cbor_map_entry res0 = res;
              elt2 = res0;
            }
            else if (i0.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
            {
              Pulse_Lib_Slice_slice__uint8_t i = i0.case_CBOR_Raw_Iterator_Serialized;
              cbor_map_entry res = cbor_serialized_map_iterator_next(&pi2, i);
              elt2 = res;
            }
            else
              exit(253);
              // elt2 =
              //   KRML_EABORT(cbor_map_entry,
              //     "unreachable (pattern matches are exhaustive in F*)");
            cbor_map_entry pelt1 = elt1;
            cbor_map_entry pelt2 = elt2;
            int16_t
            c20 =
              CBOR_Pulse_Raw_Compare_impl_cbor_compare(pelt1.cbor_map_entry_key,
                pelt2.cbor_map_entry_key);
            int16_t c2;
            if (c20 == (int16_t)0)
            {
              int16_t
              c3 =
                CBOR_Pulse_Raw_Compare_impl_cbor_compare(pelt1.cbor_map_entry_value,
                  pelt2.cbor_map_entry_value);
              c2 = c3;
            }
            else
              c2 = c20;
            if (c2 == (int16_t)0)
            {
              CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
              i11 = pi1;
              bool fin11;
              if (i11.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
              {
                Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry
                c_ = i11.case_CBOR_Raw_Iterator_Slice;
                bool res = len__CBOR_Pulse_Raw_Type_cbor_map_entry(c_) == (size_t)0U;
                bool res0 = res;
                fin11 = res0;
              }
              else if (i11.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
              {
                Pulse_Lib_Slice_slice__uint8_t c_ = i11.case_CBOR_Raw_Iterator_Serialized;
                bool res = cbor_serialized_map_iterator_is_empty(c_);
                fin11 = res;
              }
              else
                exit(253);
                // fin11 = KRML_EABORT(bool, "unreachable (pattern matches are exhaustive in F*)");
              CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
              i21 = pi2;
              bool fin21;
              if (i21.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Slice)
              {
                Pulse_Lib_Slice_slice__CBOR_Pulse_Raw_Type_cbor_map_entry
                c_ = i21.case_CBOR_Raw_Iterator_Slice;
                bool res = len__CBOR_Pulse_Raw_Type_cbor_map_entry(c_) == (size_t)0U;
                bool res0 = res;
                fin21 = res0;
              }
              else if (i21.tag == CBOR_Pulse_Raw_Iterator_CBOR_Raw_Iterator_Serialized)
              {
                Pulse_Lib_Slice_slice__uint8_t c_ = i21.case_CBOR_Raw_Iterator_Serialized;
                bool res = cbor_serialized_map_iterator_is_empty(c_);
                fin21 = res;
              }
              else
                exit(253);
                // fin21 = KRML_EABORT(bool, "unreachable (pattern matches are exhaustive in F*)");
              if (fin11 == fin21)
                pfin1 = fin11;
              else if (fin11)
                pres = (int16_t)-1;
              else
                pres = (int16_t)1;
            }
            else
              pres = c2;
            int16_t res = pres;
            bool fin11 = pfin1;
            cond = res == (int16_t)0 && !fin11;
          }
          res0 = pres;
        }
        int16_t res = res0;
        return res;
      }
      else
        return c1;
    }
    else
    {
      uint8_t val1;
      if (x1.tag == CBOR_Case_Simple)
        val1 = x1.case_CBOR_Case_Simple;
      else
        exit(253);
        // val1 = KRML_EABORT(uint8_t, "unreachable (pattern matches are exhaustive in F*)");
      uint8_t val2;
      if (x2.tag == CBOR_Case_Simple)
        val2 = x2.case_CBOR_Case_Simple;
      else
        exit(253);
        // val2 = KRML_EABORT(uint8_t, "unreachable (pattern matches are exhaustive in F*)");
      return impl_uint8_compare(val1, val2);
    }
  else
    return c;
}

static size_t cbor_validate(Pulse_Lib_Slice_slice__uint8_t input)
{
  size_t poffset = (size_t)0U;
  bool is_valid = validate_raw_data_item(input, &poffset);
  if (is_valid)
    return poffset;
  else
    return (size_t)0U;
}

static bool impl_raw_uint64_optimal(CBOR_Spec_Raw_Base_raw_uint64 x)
{
  if (x.value <= (uint64_t)MAX_SIMPLE_VALUE_ADDITIONAL_INFO == (x.size == 0U))
    if (x.size <= 1U)
      return true;
    else if (x.size == 2U)
      return 256ULL <= x.value;
    else if (x.size == 3U)
      return 65536ULL <= x.value;
    else
      return 4294967296ULL <= x.value;
  else
    return false;
}

static bool cbor_raw_ints_optimal(Pulse_Lib_Slice_slice__uint8_t a)
{
  size_t i = jump_header(a, (size_t)0U);
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(a, i);
  Pulse_Lib_Slice_slice__uint8_t s1 = s.fst;
  Pulse_Lib_Slice_slice__uint8_t s2 = s.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res = { .fst = s1, .snd = s2 };
  Pulse_Lib_Slice_slice__uint8_t input1 = res.fst;
  Pulse_Lib_Slice_slice__uint8_t input2 = res.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  spl = { .fst = input1, .snd = input2 };
  Pulse_Lib_Slice_slice__uint8_t input10 = spl.fst;
  header h = read_header(input10);
  if (get_header_major_type(h) == CBOR_MAJOR_TYPE_SIMPLE_VALUE)
    return true;
  else
  {
    long_argument scrut = h.snd;
    CBOR_Spec_Raw_Base_raw_uint64 ite;
    if (scrut.tag == LongArgumentU8)
    {
      uint8_t v = scrut.case_LongArgumentU8;
      ite = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 1U, .value = (uint64_t)v });
    }
    else if (scrut.tag == LongArgumentU16)
    {
      uint16_t v = scrut.case_LongArgumentU16;
      ite = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 2U, .value = (uint64_t)v });
    }
    else if (scrut.tag == LongArgumentU32)
    {
      uint32_t v = scrut.case_LongArgumentU32;
      ite = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 3U, .value = (uint64_t)v });
    }
    else if (scrut.tag == LongArgumentU64)
    {
      uint64_t v = scrut.case_LongArgumentU64;
      ite = ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 4U, .value = v });
    }
    else if (scrut.tag == LongArgumentOther)
      ite =
        ((CBOR_Spec_Raw_Base_raw_uint64){ .size = 0U, .value = (uint64_t)h.fst.additional_info });
    else
      exit(253);
      // ite =
      //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
      //     "unreachable (pattern matches are exhaustive in F*)");
    return impl_raw_uint64_optimal(ite);
  }
}

static bool
impl_deterministically_encoded_cbor_map_key_order(
  Pulse_Lib_Slice_slice__uint8_t a1,
  Pulse_Lib_Slice_slice__uint8_t a2
)
{
  size_t i0 = jump_raw_data_item(a1, (size_t)0U);
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(a1, i0);
  Pulse_Lib_Slice_slice__uint8_t s10 = s.fst;
  Pulse_Lib_Slice_slice__uint8_t s20 = s.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res = { .fst = s10, .snd = s20 };
  Pulse_Lib_Slice_slice__uint8_t input10 = res.fst;
  Pulse_Lib_Slice_slice__uint8_t input20 = res.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  res0 = { .fst = input10, .snd = input20 };
  Pulse_Lib_Slice_slice__uint8_t input11 = res0.fst;
  Pulse_Lib_Slice_slice__uint8_t input21 = res0.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  spl = { .fst = input11, .snd = input21 };
  Pulse_Lib_Slice_slice__uint8_t k1 = spl.fst;
  size_t i = jump_raw_data_item(a2, (size_t)0U);
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s0 = split__uint8_t(a2, i);
  Pulse_Lib_Slice_slice__uint8_t s1 = s0.fst;
  Pulse_Lib_Slice_slice__uint8_t s2 = s0.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res1 = { .fst = s1, .snd = s2 };
  Pulse_Lib_Slice_slice__uint8_t input12 = res1.fst;
  Pulse_Lib_Slice_slice__uint8_t input22 = res1.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  res2 = { .fst = input12, .snd = input22 };
  Pulse_Lib_Slice_slice__uint8_t input1 = res2.fst;
  Pulse_Lib_Slice_slice__uint8_t input2 = res2.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  spl0 = { .fst = input1, .snd = input2 };
  Pulse_Lib_Slice_slice__uint8_t k2 = spl0.fst;
  int16_t res3 = lex_compare_bytes(k1, k2);
  return res3 < (int16_t)0;
}

static bool cbor_raw_sorted(Pulse_Lib_Slice_slice__uint8_t a)
{
  size_t i0 = jump_header(a, (size_t)0U);
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(a, i0);
  Pulse_Lib_Slice_slice__uint8_t s10 = s.fst;
  Pulse_Lib_Slice_slice__uint8_t s20 = s.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  res0 = { .fst = s10, .snd = s20 };
  Pulse_Lib_Slice_slice__uint8_t input10 = res0.fst;
  Pulse_Lib_Slice_slice__uint8_t input20 = res0.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  spl = { .fst = input10, .snd = input20 };
  Pulse_Lib_Slice_slice__uint8_t ah = spl.fst;
  Pulse_Lib_Slice_slice__uint8_t ap = spl.snd;
  header h = read_header(ah);
  if (get_header_major_type(h) == CBOR_MAJOR_TYPE_MAP)
  {
    initial_byte_t b = h.fst;
    long_argument l = h.snd;
    uint64_t n = argument_as_uint64(b, l);
    if ((size_t)n == (size_t)0U)
      return true;
    else
    {
      size_t off10 = jump_raw_data_item(ap, (size_t)0U);
      size_t i0 = jump_raw_data_item(ap, off10);
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s10 = split__uint8_t(ap, i0);
      Pulse_Lib_Slice_slice__uint8_t s110 = s10.fst;
      Pulse_Lib_Slice_slice__uint8_t s20 = s10.snd;
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      res0 = { .fst = s110, .snd = s20 };
      Pulse_Lib_Slice_slice__uint8_t input10 = res0.fst;
      Pulse_Lib_Slice_slice__uint8_t input20 = res0.snd;
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      res1 = { .fst = input10, .snd = input20 };
      Pulse_Lib_Slice_slice__uint8_t input11 = res1.fst;
      Pulse_Lib_Slice_slice__uint8_t input21 = res1.snd;
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      res2 = { .fst = input11, .snd = input21 };
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t pl = res2;
      Pulse_Lib_Slice_slice__uint8_t s1 = pl.fst;
      Pulse_Lib_Slice_slice__uint8_t s2 = pl.snd;
      Pulse_Lib_Slice_slice__uint8_t phd = s1;
      Pulse_Lib_Slice_slice__uint8_t ptl = s2;
      size_t n_ = (size_t)n - (size_t)1U;
      size_t pi = n_;
      bool pres = true;
      size_t i = pi;
      bool res3 = pres;
      bool cond = res3 && i > (size_t)0U;
      while (cond)
      {
        Pulse_Lib_Slice_slice__uint8_t stl = ptl;
        size_t off1 = jump_raw_data_item(stl, (size_t)0U);
        size_t i = jump_raw_data_item(stl, off1);
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s3 = split__uint8_t(stl, i);
        Pulse_Lib_Slice_slice__uint8_t s110 = s3.fst;
        Pulse_Lib_Slice_slice__uint8_t s210 = s3.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        res0 = { .fst = s110, .snd = s210 };
        Pulse_Lib_Slice_slice__uint8_t input10 = res0.fst;
        Pulse_Lib_Slice_slice__uint8_t input20 = res0.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        res1 = { .fst = input10, .snd = input20 };
        Pulse_Lib_Slice_slice__uint8_t input1 = res1.fst;
        Pulse_Lib_Slice_slice__uint8_t input2 = res1.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        res2 = { .fst = input1, .snd = input2 };
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t pl1 = res2;
        Pulse_Lib_Slice_slice__uint8_t s11 = pl1.fst;
        Pulse_Lib_Slice_slice__uint8_t s21 = pl1.snd;
        Pulse_Lib_Slice_slice__uint8_t shd = phd;
        bool res3 = impl_deterministically_encoded_cbor_map_key_order(shd, s11);
        if (res3)
        {
          phd = s11;
          ptl = s21;
          size_t i = pi;
          size_t i_ = i - (size_t)1U;
          pi = i_;
        }
        else
          pres = false;
        size_t i0 = pi;
        bool res = pres;
        cond = res && i0 > (size_t)0U;
      }
      return pres;
    }
  }
  else
    return true;
}

static size_t cbor_validate_det_(Pulse_Lib_Slice_slice__uint8_t input)
{
  size_t len = cbor_validate(input);
  if (len == (size_t)0U)
    return len;
  else
  {
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    s_ = split__uint8_t(input, (size_t)0U);
    Pulse_Lib_Slice_slice__uint8_t s10 = s_.fst;
    Pulse_Lib_Slice_slice__uint8_t s20 = s_.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    split123 = { .fst = s10, .snd = s20 };
    Pulse_Lib_Slice_slice__uint8_t input23 = split123.snd;
    size_t consumed0 = len - (size_t)0U;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    s1s2 = split__uint8_t(input23, consumed0);
    Pulse_Lib_Slice_slice__uint8_t s11 = s1s2.fst;
    Pulse_Lib_Slice_slice__uint8_t s21 = s1s2.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    res0 = { .fst = s11, .snd = s21 };
    Pulse_Lib_Slice_slice__uint8_t left0 = res0.fst;
    Pulse_Lib_Slice_slice__uint8_t right0 = res0.snd;
    __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
    split23 = { .fst = left0, .snd = right0 };
    Pulse_Lib_Slice_slice__uint8_t input1 = split23.fst;
    bool check = false;
    KRML_HOST_IGNORE(&check);
    size_t pn = (size_t)1U;
    bool pres0 = true;
    Pulse_Lib_Slice_slice__uint8_t ppi0 = input1;
    bool res1 = pres0;
    size_t n0 = pn;
    bool cond = res1 && n0 > (size_t)0U;
    while (cond)
    {
      size_t n0 = pn;
      Pulse_Lib_Slice_slice__uint8_t pi = ppi0;
      size_t i0 = jump_raw_data_item(pi, (size_t)0U);
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(pi, i0);
      Pulse_Lib_Slice_slice__uint8_t s10 = s.fst;
      Pulse_Lib_Slice_slice__uint8_t s20 = s.snd;
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      res = { .fst = s10, .snd = s20 };
      Pulse_Lib_Slice_slice__uint8_t input110 = res.fst;
      Pulse_Lib_Slice_slice__uint8_t input20 = res.snd;
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      res0 = { .fst = input110, .snd = input20 };
      Pulse_Lib_Slice_slice__uint8_t input111 = res0.fst;
      Pulse_Lib_Slice_slice__uint8_t input21 = res0.snd;
      __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
      spl = { .fst = input111, .snd = input21 };
      Pulse_Lib_Slice_slice__uint8_t res1 = spl.fst;
      Pulse_Lib_Slice_slice__uint8_t px = res1;
      bool res2 = cbor_raw_ints_optimal(px);
      if (!res2)
        pres0 = false;
      else
      {
        size_t off1 = jump_header(pi, (size_t)0U);
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        s_ = split__uint8_t(pi, (size_t)0U);
        Pulse_Lib_Slice_slice__uint8_t s10 = s_.fst;
        Pulse_Lib_Slice_slice__uint8_t s20 = s_.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        split123 = { .fst = s10, .snd = s20 };
        Pulse_Lib_Slice_slice__uint8_t input23 = split123.snd;
        size_t consumed = off1 - (size_t)0U;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        s1s2 = split__uint8_t(input23, consumed);
        Pulse_Lib_Slice_slice__uint8_t s11 = s1s2.fst;
        Pulse_Lib_Slice_slice__uint8_t s21 = s1s2.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        res1 = { .fst = s11, .snd = s21 };
        Pulse_Lib_Slice_slice__uint8_t left = res1.fst;
        Pulse_Lib_Slice_slice__uint8_t right = res1.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        split23 = { .fst = left, .snd = right };
        Pulse_Lib_Slice_slice__uint8_t input_ = split23.fst;
        header res10 = read_header(input_);
        header x = res10;
        initial_byte_t b0 = x.fst;
        size_t i;
        if
        (
          b0.major_type
          == CBOR_MAJOR_TYPE_BYTE_STRING
          || b0.major_type == CBOR_MAJOR_TYPE_TEXT_STRING
        )
        {
          initial_byte_t b = x.fst;
          long_argument l = x.snd;
          i = off1 + (size_t)argument_as_uint64(b, l);
        }
        else
          i = off1 + (size_t)0U;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(pi, i);
        Pulse_Lib_Slice_slice__uint8_t s1 = s.fst;
        Pulse_Lib_Slice_slice__uint8_t s2 = s.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        res11 = { .fst = s1, .snd = s2 };
        Pulse_Lib_Slice_slice__uint8_t input11 = res11.fst;
        Pulse_Lib_Slice_slice__uint8_t input2 = res11.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        spl = { .fst = input11, .snd = input2 };
        Pulse_Lib_Slice_slice__uint8_t ph = spl.fst;
        Pulse_Lib_Slice_slice__uint8_t pc = spl.snd;
        size_t unused = len__uint8_t(pc);
        scylla_reset(unused); // KRML_MAYBE_UNUSED_VAR(unused);
        size_t count = jump_recursive_step_count_leaf(ph);
        pn = n0 - (size_t)1U + count;
        ppi0 = pc;
      }
      bool res3 = pres0;
      size_t n = pn;
      cond = res3 && n > (size_t)0U;
    }
    bool res2 = pres0;
    bool check1 = res2;
    if (!check1)
      return (size_t)0U;
    else
    {
      size_t pn = (size_t)1U;
      bool pres = true;
      Pulse_Lib_Slice_slice__uint8_t ppi = input1;
      bool res = pres;
      size_t n0 = pn;
      bool cond = res && n0 > (size_t)0U;
      while (cond)
      {
        size_t n0 = pn;
        Pulse_Lib_Slice_slice__uint8_t pi = ppi;
        size_t i0 = jump_raw_data_item(pi, (size_t)0U);
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(pi, i0);
        Pulse_Lib_Slice_slice__uint8_t s10 = s.fst;
        Pulse_Lib_Slice_slice__uint8_t s20 = s.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        res = { .fst = s10, .snd = s20 };
        Pulse_Lib_Slice_slice__uint8_t input110 = res.fst;
        Pulse_Lib_Slice_slice__uint8_t input20 = res.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        res0 = { .fst = input110, .snd = input20 };
        Pulse_Lib_Slice_slice__uint8_t input111 = res0.fst;
        Pulse_Lib_Slice_slice__uint8_t input21 = res0.snd;
        __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
        spl = { .fst = input111, .snd = input21 };
        Pulse_Lib_Slice_slice__uint8_t res1 = spl.fst;
        Pulse_Lib_Slice_slice__uint8_t px = res1;
        bool res2 = cbor_raw_sorted(px);
        if (!res2)
          pres = false;
        else
        {
          size_t off1 = jump_header(pi, (size_t)0U);
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          s_ = split__uint8_t(pi, (size_t)0U);
          Pulse_Lib_Slice_slice__uint8_t s10 = s_.fst;
          Pulse_Lib_Slice_slice__uint8_t s20 = s_.snd;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          split123 = { .fst = s10, .snd = s20 };
          Pulse_Lib_Slice_slice__uint8_t input23 = split123.snd;
          size_t consumed = off1 - (size_t)0U;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          s1s2 = split__uint8_t(input23, consumed);
          Pulse_Lib_Slice_slice__uint8_t s11 = s1s2.fst;
          Pulse_Lib_Slice_slice__uint8_t s21 = s1s2.snd;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          res1 = { .fst = s11, .snd = s21 };
          Pulse_Lib_Slice_slice__uint8_t left = res1.fst;
          Pulse_Lib_Slice_slice__uint8_t right = res1.snd;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          split23 = { .fst = left, .snd = right };
          Pulse_Lib_Slice_slice__uint8_t input_ = split23.fst;
          header res10 = read_header(input_);
          header x = res10;
          initial_byte_t b0 = x.fst;
          size_t i;
          if
          (
            b0.major_type
            == CBOR_MAJOR_TYPE_BYTE_STRING
            || b0.major_type == CBOR_MAJOR_TYPE_TEXT_STRING
          )
          {
            initial_byte_t b = x.fst;
            long_argument l = x.snd;
            i = off1 + (size_t)argument_as_uint64(b, l);
          }
          else
            i = off1 + (size_t)0U;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t s = split__uint8_t(pi, i);
          Pulse_Lib_Slice_slice__uint8_t s1 = s.fst;
          Pulse_Lib_Slice_slice__uint8_t s2 = s.snd;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          res11 = { .fst = s1, .snd = s2 };
          Pulse_Lib_Slice_slice__uint8_t input11 = res11.fst;
          Pulse_Lib_Slice_slice__uint8_t input2 = res11.snd;
          __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
          spl = { .fst = input11, .snd = input2 };
          Pulse_Lib_Slice_slice__uint8_t ph = spl.fst;
          Pulse_Lib_Slice_slice__uint8_t pc = spl.snd;
          size_t unused = len__uint8_t(pc);
          scylla_reset(unused); // KRML_MAYBE_UNUSED_VAR(unused);
          size_t count = jump_recursive_step_count_leaf(ph);
          pn = n0 - (size_t)1U + count;
          ppi = pc;
        }
        bool res3 = pres;
        size_t n = pn;
        cond = res3 && n > (size_t)0U;
      }
      bool res0 = pres;
      bool check2 = res0;
      if (!check2)
        return (size_t)0U;
      else
        return len;
    }
  }
}

static size_t cbor_validate_det(Pulse_Lib_Slice_slice__uint8_t input)
{
  size_t res = cbor_validate_det_(input);
  return res;
}

static cbor_raw cbor_parse(Pulse_Lib_Slice_slice__uint8_t input, size_t len)
{
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  s_ = split__uint8_t(input, (size_t)0U);
  Pulse_Lib_Slice_slice__uint8_t s10 = s_.fst;
  Pulse_Lib_Slice_slice__uint8_t s20 = s_.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  split123 = { .fst = s10, .snd = s20 };
  Pulse_Lib_Slice_slice__uint8_t input23 = split123.snd;
  size_t consumed = len - (size_t)0U;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  s1s2 = split__uint8_t(input23, consumed);
  Pulse_Lib_Slice_slice__uint8_t s1 = s1s2.fst;
  Pulse_Lib_Slice_slice__uint8_t s2 = s1s2.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t res = { .fst = s1, .snd = s2 };
  Pulse_Lib_Slice_slice__uint8_t left = res.fst;
  Pulse_Lib_Slice_slice__uint8_t right = res.snd;
  __Pulse_Lib_Slice_slice_uint8_t_Pulse_Lib_Slice_slice_uint8_t
  split23 = { .fst = left, .snd = right };
  Pulse_Lib_Slice_slice__uint8_t input1 = split23.fst;
  cbor_raw res0 = cbor_read(input1);
  return res0;
}

bool uu___is_CBOR_Case_Int(cbor_raw projectee)
{
  if (projectee.tag == CBOR_Case_Int)
    return true;
  else
    return false;
}

bool uu___is_CBOR_Case_Simple(cbor_raw projectee)
{
  if (projectee.tag == CBOR_Case_Simple)
    return true;
  else
    return false;
}

bool uu___is_CBOR_Case_String(cbor_raw projectee)
{
  if (projectee.tag == CBOR_Case_String)
    return true;
  else
    return false;
}

bool uu___is_CBOR_Case_Tagged(cbor_raw projectee)
{
  if (projectee.tag == CBOR_Case_Tagged)
    return true;
  else
    return false;
}

bool uu___is_CBOR_Case_Array(cbor_raw projectee)
{
  if (projectee.tag == CBOR_Case_Array)
    return true;
  else
    return false;
}

bool uu___is_CBOR_Case_Map(cbor_raw projectee)
{
  if (projectee.tag == CBOR_Case_Map)
    return true;
  else
    return false;
}

bool uu___is_CBOR_Case_Serialized_Tagged(cbor_raw projectee)
{
  if (projectee.tag == CBOR_Case_Serialized_Tagged)
    return true;
  else
    return false;
}

bool uu___is_CBOR_Case_Serialized_Array(cbor_raw projectee)
{
  if (projectee.tag == CBOR_Case_Serialized_Array)
    return true;
  else
    return false;
}

bool uu___is_CBOR_Case_Serialized_Map(cbor_raw projectee)
{
  if (projectee.tag == CBOR_Case_Serialized_Map)
    return true;
  else
    return false;
}

size_t cbor_det_validate(Pulse_Lib_Slice_slice__uint8_t input)
{
  size_t res = cbor_validate_det(input);
  return res;
}

cbor_raw cbor_det_parse(Pulse_Lib_Slice_slice__uint8_t input, size_t len)
{
  cbor_raw res = cbor_parse(input, len);
  return res;
}

size_t cbor_det_size(cbor_raw x, size_t bound)
{
  size_t res = cbor_size(x, bound);
  return res;
}

size_t cbor_det_serialize(cbor_raw x, Pulse_Lib_Slice_slice__uint8_t output)
{
  size_t res = cbor_serialize(x, output);
  return res;
}

cbor_raw cbor_det_mk_simple_value(uint8_t v)
{
  return ((cbor_raw){ .tag = CBOR_Case_Simple, { .case_CBOR_Case_Simple = v } });
}

cbor_raw cbor_det_mk_int64(uint8_t ty, uint64_t v)
{
  cbor_int
  res =
    {
      .cbor_int_type = ty,
      .cbor_int_size = mk_raw_uint64(v).size,
      .cbor_int_value = mk_raw_uint64(v).value
    };
  cbor_int resi = res;
  cbor_raw res0 = { .tag = CBOR_Case_Int, { .case_CBOR_Case_Int = resi } };
  return res0;
}

cbor_raw cbor_det_mk_string(uint8_t ty, Pulse_Lib_Slice_slice__uint8_t s)
{
  CBOR_Spec_Raw_Base_raw_uint64 len64 = mk_raw_uint64((uint64_t)len__uint8_t(s));
  cbor_string
  ress = { .cbor_string_type = ty, .cbor_string_size = len64.size, .cbor_string_ptr = s };
  return ((cbor_raw){ .tag = CBOR_Case_String, { .case_CBOR_Case_String = ress } });
}

cbor_raw cbor_det_mk_tagged(uint64_t tag, cbor_raw *r)
{
  CBOR_Spec_Raw_Base_raw_uint64 tag64 = mk_raw_uint64(tag);
  cbor_tagged res_ = { .cbor_tagged_tag = tag64, .cbor_tagged_ptr = r };
  return ((cbor_raw){ .tag = CBOR_Case_Tagged, { .case_CBOR_Case_Tagged = res_ } });
}

cbor_raw cbor_det_mk_array(cbor_raw *a, uint64_t len)
{
  CBOR_Spec_Raw_Base_raw_uint64 len64 = mk_raw_uint64(len);
  cbor_array res_ = { .cbor_array_length = len64, .cbor_array_ptr = a };
  return ((cbor_raw){ .tag = CBOR_Case_Array, { .case_CBOR_Case_Array = res_ } });
}

static int16_t cbor_raw_compare(cbor_raw x1, cbor_raw x2)
{
  return CBOR_Pulse_Raw_Compare_impl_cbor_compare(x1, x2);
}

static int16_t cbor_map_entry_raw_compare(cbor_map_entry x1, cbor_map_entry x2)
{
  int16_t res = cbor_raw_compare(x1.cbor_map_entry_key, x2.cbor_map_entry_key);
  return res;
}

bool cbor_raw_sort_aux(cbor_map_entry *a, size_t lo, size_t hi)
{
  size_t len = hi - lo;
  if (len < (size_t)2U)
    return true;
  else
  {
    size_t len_half = len / (size_t)2U;
    size_t mi = lo + len_half;
    bool res = cbor_raw_sort_aux(a, lo, mi);
    if (!res)
      return false;
    else
    {
      bool res1 = cbor_raw_sort_aux(a, mi, hi);
      if (!res1)
        return false;
      else
      {
        size_t pi1 = lo;
        size_t pi2 = mi;
        bool pres = true;
        size_t i10 = pi1;
        size_t i20 = pi2;
        bool res20 = pres;
        bool cond = res20 && !(i10 == i20 || i20 == hi);
        while (cond)
        {
          size_t i1 = pi1;
          cbor_map_entry x1 = a[i1];
          size_t i20 = pi2;
          cbor_map_entry x2 = a[i20];
          int16_t comp = cbor_map_entry_raw_compare(x1, x2);
          if (comp == (int16_t)0)
            pres = false;
          else if (comp < (int16_t)0)
          {
            size_t i1_ = i1 + (size_t)1U;
            pi1 = i1_;
          }
          else
          {
            size_t i2_ = i20 + (size_t)1U;
            size_t i1_;
            if (i1 == i20)
              i1_ = i2_;
            else if (i20 == i2_)
              i1_ = i1;
            else
            {
              size_t pn = i2_ - i1;
              size_t pl = i20 - i1;
              size_t l30 = pl;
              bool cond = l30 > (size_t)0U;
              while (cond)
              {
                size_t n = pn;
                size_t l3 = pl;
                size_t l_ = n % l3;
                pn = l3;
                pl = l_;
                size_t l30 = pl;
                cond = l30 > (size_t)0U;
              }
              size_t d = pn;
              size_t q = (i2_ - i1) / d;
              size_t pi = i1;
              size_t i0 = pi;
              bool cond0 = i0 - i1 < d;
              while (cond0)
              {
                size_t i = pi;
                cbor_map_entry save = a[i];
                size_t pj = (size_t)0U;
                size_t pidx = i;
                size_t j0 = pj;
                bool cond = j0 < q - (size_t)1U;
                while (cond)
                {
                  size_t j = pj;
                  size_t idx = pidx;
                  size_t idx_;
                  if (idx - i1 >= i2_ - i20)
                    idx_ = idx - (i2_ - i20);
                  else
                    idx_ = idx + i20 - i1;
                  cbor_map_entry x = a[idx_];
                  size_t j_ = j + (size_t)1U;
                  a[idx] = x;
                  pj = j_;
                  pidx = idx_;
                  size_t j0 = pj;
                  cond = j0 < q - (size_t)1U;
                }
                size_t idx = pidx;
                a[idx] = save;
                size_t i_ = i + (size_t)1U;
                pi = i_;
                size_t i0 = pi;
                cond0 = i0 - i1 < d;
              }
              i1_ = i1 + i2_ - i20;
            }
            pi1 = i1_;
            pi2 = i2_;
          }
          size_t i10 = pi1;
          size_t i2 = pi2;
          bool res2 = pres;
          cond = res2 && !(i10 == i2 || i2 == hi);
        }
        bool res2 = pres;
        return res2;
      }
    }
  }
}

static bool cbor_raw_sort(cbor_map_entry *a, size_t len)
{
  bool res = cbor_raw_sort_aux(a, (size_t)0U, len);
  return res;
}

cbor_raw cbor_det_mk_map(cbor_map_entry *a, uint64_t len)
{
  cbor_raw_sort(a, (size_t)len);
  CBOR_Spec_Raw_Base_raw_uint64 raw_len = mk_raw_uint64(len);
  cbor_map res_ = { .cbor_map_length = raw_len, .cbor_map_ptr = a };
  return ((cbor_raw){ .tag = CBOR_Case_Map, { .case_CBOR_Case_Map = res_ } });
}

bool cbor_det_equal(cbor_raw x1, cbor_raw x2)
{
  int16_t comp = CBOR_Pulse_Raw_Compare_impl_cbor_compare(x1, x2);
  return comp == (int16_t)0;
}

uint8_t cbor_det_major_type(cbor_raw x)
{
  uint8_t res = impl_major_type(x);
  return res;
}

uint8_t cbor_det_read_simple_value(cbor_raw x)
{
  if (x.tag == CBOR_Case_Simple)
    return x.case_CBOR_Case_Simple;
  else
  {
    KRML_HOST_EPRINTF("KaRaMeL abort at %s:%d\n%s\n",
      __FILE__,
      __LINE__,
      "unreachable (pattern matches are exhaustive in F*)");
    KRML_HOST_EXIT(255U);
  }
}

uint64_t cbor_det_read_uint64(cbor_raw x)
{
  cbor_int c_;
  if (x.tag == CBOR_Case_Int)
    c_ = x.case_CBOR_Case_Int;
  else
    exit(253);
    // c_ = KRML_EABORT(cbor_int, "unreachable (pattern matches are exhaustive in F*)");
  CBOR_Spec_Raw_Base_raw_uint64 res = { .size = c_.cbor_int_size, .value = c_.cbor_int_value };
  return res.value;
}

Pulse_Lib_Slice_slice__uint8_t cbor_det_get_string(cbor_raw x)
{
  cbor_string c_ = CBOR_STRING_DEFAULT;
  if (x.tag == CBOR_Case_String)
    c_ = x.case_CBOR_Case_String;
  else
    exit(253);
    // c_ = KRML_EABORT(cbor_string, "unreachable (pattern matches are exhaustive in F*)");
  return c_.cbor_string_ptr;
}

uint64_t cbor_det_get_tagged_tag(cbor_raw x)
{
  CBOR_Spec_Raw_Base_raw_uint64 res;
  if (x.tag == CBOR_Case_Tagged)
  {
    cbor_tagged c_ = x.case_CBOR_Case_Tagged;
    res = c_.cbor_tagged_tag;
  }
  else if (x.tag == CBOR_Case_Serialized_Tagged)
  {
    cbor_serialized c_ = x.case_CBOR_Case_Serialized_Tagged;
    res = c_.cbor_serialized_header;
  }
  else
    exit(253);
    // res =
    //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
    //     "unreachable (pattern matches are exhaustive in F*)");
  return res.value;
}

cbor_raw cbor_det_get_tagged_payload(cbor_raw x)
{
  cbor_raw res = cbor_match_tagged_get_payload(x);
  return res;
}

uint64_t cbor_det_get_array_length(cbor_raw x)
{
  CBOR_Spec_Raw_Base_raw_uint64 res;
  if (x.tag == CBOR_Case_Array)
  {
    cbor_array c_ = x.case_CBOR_Case_Array;
    res = c_.cbor_array_length;
  }
  else if (x.tag == CBOR_Case_Serialized_Array)
  {
    cbor_serialized c_ = x.case_CBOR_Case_Serialized_Array;
    res = c_.cbor_serialized_header;
  }
  else
    exit(253);
    // res =
    //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
    //     "unreachable (pattern matches are exhaustive in F*)");
  return res.value;
}

CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw
cbor_det_array_iterator_start(cbor_raw x)
{
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw
  res = cbor_array_iterator_init(x);
  return res;
}

bool
cbor_det_array_iterator_is_empty(
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw x
)
{
  bool res = cbor_array_iterator_is_empty(x);
  return res;
}

cbor_raw
cbor_det_array_iterator_next(
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_raw *x
)
{
  cbor_raw res = cbor_array_iterator_next(x);
  return res;
}

cbor_raw cbor_det_get_array_item(cbor_raw x, uint64_t i)
{
  cbor_raw res = cbor_array_item(x, i);
  return res;
}

uint64_t cbor_det_get_map_length(cbor_raw x)
{
  CBOR_Spec_Raw_Base_raw_uint64 res;
  if (x.tag == CBOR_Case_Map)
  {
    cbor_map c_ = x.case_CBOR_Case_Map;
    res = c_.cbor_map_length;
  }
  else if (x.tag == CBOR_Case_Serialized_Map)
  {
    cbor_serialized c_ = x.case_CBOR_Case_Serialized_Map;
    res = c_.cbor_serialized_header;
  }
  else
    exit(253);
    // res =
    //   KRML_EABORT(CBOR_Spec_Raw_Base_raw_uint64,
    //     "unreachable (pattern matches are exhaustive in F*)");
  return res.value;
}

static int16_t impl_cbor_det_compare(cbor_raw x1, cbor_raw x2)
{
  int16_t res = CBOR_Pulse_Raw_Compare_impl_cbor_compare(x1, x2);
  return res;
}

static CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
cbor_det_map_iterator_start_(cbor_raw x)
{
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
  res = cbor_map_iterator_init(x);
  return res;
}

CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
cbor_det_map_iterator_start(cbor_raw x)
{
  return cbor_det_map_iterator_start_(x);
}

bool
cbor_det_map_iterator_is_empty(
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry x
)
{
  bool res = cbor_map_iterator_is_empty(x);
  return res;
}

cbor_map_entry
cbor_det_map_iterator_next(
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry *x
)
{
  cbor_map_entry res = cbor_map_iterator_next(x);
  return res;
}

cbor_raw cbor_det_map_entry_key(cbor_map_entry x2)
{
  return x2.cbor_map_entry_key;
}

cbor_raw cbor_det_map_entry_value(cbor_map_entry x2)
{
  return x2.cbor_map_entry_value;
}

FStar_Pervasives_Native_option__CBOR_Pulse_Raw_Type_cbor_raw
cbor_det_map_get(cbor_raw x, cbor_raw k)
{
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry
  i = cbor_det_map_iterator_start_(x);
  CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry pi = i;
  FStar_Pervasives_Native_option__CBOR_Pulse_Raw_Type_cbor_raw
  pres = { .tag = FStar_Pervasives_Native_None };
  bool i_is_empty = cbor_det_map_iterator_is_empty(i);
  bool cont = !i_is_empty;
  bool pcont = cont;
  while (pcont)
  {
    cbor_map_entry entry = cbor_det_map_iterator_next(&pi);
    cbor_raw key = cbor_det_map_entry_key(entry);
    int16_t comp = impl_cbor_det_compare(key, k);
    if (comp == (int16_t)0)
    {
      cbor_raw value = cbor_det_map_entry_value(entry);
      pres =
        (
          (FStar_Pervasives_Native_option__CBOR_Pulse_Raw_Type_cbor_raw){
            .tag = FStar_Pervasives_Native_Some,
            .v = value
          }
        );
      pcont = false;
    }
    else if (comp > (int16_t)0)
      pcont = false;
    else
    {
      CBOR_Pulse_Raw_Iterator_cbor_raw_iterator__CBOR_Pulse_Raw_Type_cbor_map_entry i_ = pi;
      bool is_empty = cbor_det_map_iterator_is_empty(i_);
      bool cont1 = !is_empty;
      pcont = cont1;
    }
  }
  return pres;
}
