#pragma once

#include <sys/types.h>

#if defined(__has_include)
#if __has_include("config.h")
#include "config.h"
#endif
#endif

/*
   GCC versions prior to 5.5 incorrectly optimize certain intrinsics.

   See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81300

   CLANG versions prior to 5 crash on certain intrinsics.

   See https://bugs.llvm.org/show_bug.cgi?id=24943
*/

#if !defined(HACL_CAN_COMPILE_INTRINSICS) || \
    (defined(__clang__) && (__clang_major__ < 5)) || \
    (defined(__GNUC__) && !defined(__clang__) &&  \
     (__GNUC__ < 5 || (__GNUC__ == 5 && (__GNUC_MINOR__ < 5))))

#include "Hacl_IntTypes_Intrinsics.h"

#if defined(HACL_CAN_COMPILE_UINT128)

#include "Hacl_IntTypes_Intrinsics_128.h"

#define Lib_IntTypes_Intrinsics_add_carry_u64(x1, x2, x3, x4) \
    (Hacl_IntTypes_Intrinsics_128_add_carry_u64(x1, x2, x3, x4))

#define Lib_IntTypes_Intrinsics_sub_borrow_u64(x1, x2, x3, x4) \
    (Hacl_IntTypes_Intrinsics_128_sub_borrow_u64(x1, x2, x3, x4))

#else

__attribute__((annotate("scylla_opaque")))
__attribute__((annotate("scylla_mutability(_, _, _, mut)")))
inline static uint64_t Lib_IntTypes_Intrinsics_add_carry_u64(uint64_t cin, uint64_t x, uint64_t y, uint64_t *r) {
    return Hacl_IntTypes_Intrinsics_add_carry_u64(cin, x, y, r);
}

__attribute__((annotate("scylla_opaque")))
__attribute__((annotate("scylla_mutability(_, _, _, mut)")))
inline static uint64_t Lib_IntTypes_Intrinsics_sub_borrow_u64(uint64_t cin, uint64_t x, uint64_t y, uint64_t *r) {
    return Hacl_IntTypes_Intrinsics_sub_borrow_u64(cin, x, y, r);
}

#endif // defined(HACL_CAN_COMPILE_UINT128)

__attribute__((annotate("scylla_opaque")))
__attribute__((annotate("scylla_mutability(_, _, _, mut)")))
inline static uint32_t Lib_IntTypes_Intrinsics_add_carry_u32(uint32_t cin, uint32_t x, uint32_t y, uint32_t *r) {
    return Hacl_IntTypes_Intrinsics_add_carry_u32(cin, x, y, r);
}

__attribute__((annotate("scylla_opaque")))
__attribute__((annotate("scylla_mutability(_, _, _, mut)")))
inline static uint32_t Lib_IntTypes_Intrinsics_sub_borrow_u32(uint32_t cin, uint32_t x, uint32_t y, uint32_t *r) {
    return Hacl_IntTypes_Intrinsics_sub_borrow_u32(cin, x, y, r);
}

#else // !defined(HACL_CAN_COMPILE_INTRINSICS)

#if defined(_MSC_VER)
#include <immintrin.h>
#else
#include <x86intrin.h>
#endif

#define Lib_IntTypes_Intrinsics_add_carry_u32(x1, x2, x3, x4) \
    (_addcarry_u32(x1, x2, x3, (unsigned int *)x4))

#define Lib_IntTypes_Intrinsics_add_carry_u64(x1, x2, x3, x4) \
    (_addcarry_u64(x1, x2, x3, (long long unsigned int *)x4))

/*
   GCC versions prior to 7.2 pass arguments to _subborrow_u{32,64}
   in an incorrect order.

   See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81294
*/
#if defined(__GNUC__) && !defined(__clang__) && \
    (__GNUC__ < 7 || (__GNUC__ == 7 && (__GNUC_MINOR__ < 2)))

#define Lib_IntTypes_Intrinsics_sub_borrow_u32(x1, x2, x3, x4) \
    (_subborrow_u32(x1, x3, x2, (unsigned int *)x4))

#define Lib_IntTypes_Intrinsics_sub_borrow_u64(x1, x2, x3, x4) \
    (_subborrow_u64(x1, x3, x2, (long long unsigned int *)x4))

#else

#define Lib_IntTypes_Intrinsics_sub_borrow_u32(x1, x2, x3, x4) \
    (_subborrow_u32(x1, x2, x3, (unsigned int *)x4))

#define Lib_IntTypes_Intrinsics_sub_borrow_u64(x1, x2, x3, x4) \
    (_subborrow_u64(x1, x2, x3, (long long unsigned int *)x4))

#endif // GCC < 7.2

#endif // !HACL_CAN_COMPILE_INTRINSICS
