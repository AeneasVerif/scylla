/* MIT License
 *
 * Copyright (c) 2016-2022 INRIA, CMU and Microsoft Corporation
 * Copyright (c) 2022-2023 HACL* Contributors
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */


#ifndef __Hacl_Krmllib_H
#define __Hacl_Krmllib_H

#if defined(__cplusplus)
extern "C" {
#endif

#include <string.h>
#include "krml/internal/types.h"
#include "krml/lowstar_endianness.h"
#include "krml/internal/target.h"

__attribute__((annotate("scylla_opaque")))
static KRML_NOINLINE uint64_t FStar_UInt64_eq_mask(uint64_t a, uint64_t b);

__attribute__((annotate("scylla_opaque")))
static KRML_NOINLINE uint64_t FStar_UInt64_gte_mask(uint64_t a, uint64_t b);

__attribute__((annotate("scylla_opaque")))
static inline FStar_UInt128_uint128
FStar_UInt128_add_mod(FStar_UInt128_uint128 a, FStar_UInt128_uint128 b);

__attribute__((annotate("scylla_opaque")))
static inline FStar_UInt128_uint128
FStar_UInt128_sub_mod(FStar_UInt128_uint128 a, FStar_UInt128_uint128 b);

__attribute__((annotate("scylla_opaque")))
static inline FStar_UInt128_uint128
FStar_UInt128_shift_right(FStar_UInt128_uint128 a, uint32_t s);

__attribute__((annotate("scylla_opaque")))
static inline FStar_UInt128_uint128 FStar_UInt128_uint64_to_uint128(uint64_t a);

__attribute__((annotate("scylla_opaque")))
static inline uint64_t FStar_UInt128_uint128_to_uint64(FStar_UInt128_uint128 a);

#if defined(__cplusplus)
}
#endif

#define __Hacl_Krmllib_H_DEFINED
#endif
