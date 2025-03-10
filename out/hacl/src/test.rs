const KEY1: [u8; 32] = [
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e,
    0x1f
];

const NONCE1: [u8; 12 ] = [
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x4a, 0x00, 0x00, 0x00, 0x00
];

const COUNTER1: u32 = 1;

const PLAINTEXT1: [u8; 114 ] = [
    0x4c, 0x61, 0x64, 0x69, 0x65, 0x73, 0x20, 0x61, 0x6e, 0x64, 0x20, 0x47, 0x65, 0x6e, 0x74, 0x6c,
    0x65, 0x6d, 0x65, 0x6e, 0x20, 0x6f, 0x66, 0x20, 0x74, 0x68, 0x65, 0x20, 0x63, 0x6c, 0x61, 0x73,
    0x73, 0x20, 0x6f, 0x66, 0x20, 0x27, 0x39, 0x39, 0x3a, 0x20, 0x49, 0x66, 0x20, 0x49, 0x20, 0x63,
    0x6f, 0x75, 0x6c, 0x64, 0x20, 0x6f, 0x66, 0x66, 0x65, 0x72, 0x20, 0x79, 0x6f, 0x75, 0x20, 0x6f,
    0x6e, 0x6c, 0x79, 0x20, 0x6f, 0x6e, 0x65, 0x20, 0x74, 0x69, 0x70, 0x20, 0x66, 0x6f, 0x72, 0x20,
    0x74, 0x68, 0x65, 0x20, 0x66, 0x75, 0x74, 0x75, 0x72, 0x65, 0x2c, 0x20, 0x73, 0x75, 0x6e, 0x73,
    0x63, 0x72, 0x65, 0x65, 0x6e, 0x20, 0x77, 0x6f, 0x75, 0x6c, 0x64, 0x20, 0x62, 0x65, 0x20, 0x69,
    0x74, 0x2e
];

const CIPHER1: [u8; 114 ] = [
    0x6e, 0x2e, 0x35, 0x9a, 0x25, 0x68, 0xf9, 0x80, 0x41, 0xba, 0x07, 0x28, 0xdd, 0x0d, 0x69, 0x81,
    0xe9, 0x7e, 0x7a, 0xec, 0x1d, 0x43, 0x60, 0xc2, 0x0a, 0x27, 0xaf, 0xcc, 0xfd, 0x9f, 0xae, 0x0b,
    0xf9, 0x1b, 0x65, 0xc5, 0x52, 0x47, 0x33, 0xab, 0x8f, 0x59, 0x3d, 0xab, 0xcd, 0x62, 0xb3, 0x57,
    0x16, 0x39, 0xd6, 0x24, 0xe6, 0x51, 0x52, 0xab, 0x8f, 0x53, 0x0c, 0x35, 0x9f, 0x08, 0x61, 0xd8,
    0x07, 0xca, 0x0d, 0xbf, 0x50, 0x0d, 0x6a, 0x61, 0x56, 0xa3, 0x8e, 0x08, 0x8a, 0x22, 0xb6, 0x5e,
    0x52, 0xbc, 0x51, 0x4d, 0x16, 0xcc, 0xf8, 0x06, 0x81, 0x8c, 0xe9, 0x1a, 0xb7, 0x79, 0x37, 0x36,
    0x5a, 0xf9, 0x0b, 0xbf, 0x74, 0xa3, 0x5b, 0xe6, 0xb4, 0x0b, 0x8e, 0xed, 0xf2, 0x78, 0x5e, 0x42,
    0x87, 0x4d
];

#[test]
fn chacha20() {
    let mut out = [0u8; PLAINTEXT1.len()];
    crate::chacha::chacha20_encrypt(out.len().try_into().unwrap(), &mut out, &PLAINTEXT1, &KEY1, &NONCE1, COUNTER1);
    assert_eq!(out, CIPHER1);
    println!("chacha20: success");
}

#[test]
fn bignum_base() {
    // echo 'obase=16;123456789123456789' | bc
    let a = [ u32::from_be_bytes([ 0xAC, 0xD0, 0x5F, 0x15 ]), u32::from_be_bytes([0x01, 0xB6, 0x9B, 0x4B]), ];
    let b = [ u32::from_be_bytes([ 0xAC, 0xD0, 0x5F, 0x15 ]), u32::from_be_bytes([0x01, 0xB6, 0x9B, 0x4B]), ];
    let mut actual = [0u32; 4];
    crate::hacl::bignum_base::Hacl_Bignum_Multiplication_bn_mul_u32(2, &a, 2, &b, &mut actual);
    // echo 'obase=16;123456789123456789 * 123456789123456789' | bc
    let expected = [
        u32::from_be_bytes([ 0xDD, 0x70, 0x97, 0xB9 ]),  // 4
        u32::from_be_bytes([ 0x65, 0xBF, 0xCD, 0xAC ]), // 3
        u32::from_be_bytes([ 0xC5, 0x28, 0x12, 0xA8 ]), // 2
        u32::from_be_bytes([ 0x00, 0x02, 0xEF, 0x77 ]), // 1
    ];
    let actual_hex = actual.map(|x| hex::encode(x.to_be_bytes()));
    let expected_hex = expected.map(|x| hex::encode(x.to_be_bytes()));
    assert_eq!(actual_hex, expected_hex);
    println!("bignum_base: success");
}

const test1_nBytes: [u8; 512] = [
  0xf6, 0x1,  0xbe, 0xd,  0xcc, 0xd0, 0x4a, 0xa4, 0xb,  0x12, 0xf3, 0xf1, 0x91,
  0xae, 0x17, 0xc1, 0xf9, 0xc8, 0xc0, 0xb6, 0x8e, 0x7a, 0x77, 0xe1, 0x4b, 0xe2,
  0x5c, 0x3c, 0x79, 0x7,  0xcb, 0x1d, 0x33, 0xa6, 0xef, 0x41, 0x8e, 0xf4, 0x18,
  0x52, 0xf3, 0x2c, 0x98, 0x39, 0x2b, 0xc5, 0xc9, 0xae, 0xd9, 0x1c, 0x1a, 0x15,
  0x1,  0xc5, 0x3,  0xea, 0xb8, 0x9b, 0x3e, 0xe6, 0xf4, 0xf8, 0xeb, 0x2e, 0xf,
  0xcf, 0xc4, 0x1b, 0xd0, 0x36, 0x9,  0xcf, 0x6a, 0x8e, 0xb3, 0xaa, 0x6f, 0xf,
  0xbe, 0x23, 0x18, 0x7b, 0x33, 0xdb, 0x4d, 0x34, 0xb6, 0x6d, 0x12, 0x8a, 0x8a,
  0xba, 0xa,  0x2a, 0xbf, 0x40, 0xbb, 0x9d, 0x13, 0xd8, 0xe2, 0x55, 0x45, 0x69,
  0xa5, 0x7a, 0xb1, 0xd8, 0xc6, 0x1b, 0x8c, 0xad, 0x2d, 0xc8, 0x85, 0x99, 0xae,
  0xd,  0xa5, 0x34, 0x6e, 0x15, 0xda, 0xce, 0x1b, 0xac, 0x7b, 0xf6, 0x97, 0x37,
  0xc2, 0x2f, 0x8,  0x3b, 0xe9, 0xb4, 0x6b, 0xb8, 0xb1, 0xea, 0xb5, 0x95, 0x7b,
  0x2d, 0xa7, 0x40, 0x27, 0x5e, 0x96, 0xc8, 0x71, 0x95, 0xb9, 0x6f, 0xe1, 0x14,
  0x52, 0x15, 0x9d, 0xaf, 0xcf, 0xd9, 0x16, 0xce, 0xe5, 0xd7, 0x49, 0xa7, 0x7b,
  0xc3, 0x90, 0x5a, 0x5e, 0xbd, 0x38, 0x7a, 0xe4, 0x45, 0xe8, 0xfe, 0x70, 0xf1,
  0x6e, 0x9a, 0x8,  0x66, 0x39, 0x77, 0x9c, 0xef, 0xfb, 0xfd, 0x41, 0x55, 0x7b,
  0xd9, 0x9a, 0xea, 0x6a, 0x37, 0x1a, 0x6b, 0x4b, 0x16, 0x6,  0x15, 0xa1, 0xa1,
  0x2b, 0xc6, 0x95, 0x8d, 0x34, 0xbc, 0xe0, 0xc8, 0x5a, 0xdc, 0xbd, 0x83, 0x92,
  0xfa, 0x10, 0xce, 0xca, 0x52, 0x20, 0x9d, 0x56, 0x19, 0x6b, 0xa3, 0xd2, 0x73,
  0xce, 0x22, 0x8f, 0x1f, 0x11, 0x11, 0x92, 0xaa, 0x92, 0xde, 0x2a, 0x3,  0x97,
  0x98, 0xa1, 0x7b, 0xce, 0xcb, 0x4d, 0xc6, 0x10, 0xe,  0x6f, 0x8a, 0xe8, 0xc2,
  0x64, 0x3f, 0x2a, 0xe7, 0x68, 0xb2, 0x25, 0x5f, 0x8,  0x2c, 0x97, 0x8e, 0x95,
  0xca, 0x55, 0x15, 0x55, 0xf1, 0x6,  0x8,  0x23, 0x1c, 0xf8, 0x0,  0x3b, 0xbf,
  0x80, 0x79, 0x69, 0xff, 0xf1, 0xe5, 0x19, 0x14, 0xb9, 0xa8, 0xc9, 0xb8, 0xf4,
  0x56, 0x46, 0x45, 0xb9, 0xe5, 0xd7, 0x5,  0xff, 0xad, 0x29, 0x66, 0x3f, 0x5d,
  0xae, 0x3d, 0x76, 0x65, 0x2b, 0x42, 0x2e, 0x43, 0xf1, 0x3e, 0x6c, 0x14, 0x91,
  0x9,  0x8,  0x5,  0xc2, 0xd1, 0x26, 0x8a, 0x74, 0xa2, 0x51, 0x17, 0x74, 0x27,
  0xe3, 0x3a, 0x9a, 0x91, 0x17, 0x5c, 0x36, 0x70, 0xb9, 0x17, 0x46, 0x0,  0x8b,
  0xce, 0x1f, 0xd2, 0x31, 0xe6, 0xe4, 0xf2, 0xad, 0x70, 0xcb, 0x43, 0xac, 0xa5,
  0xf0, 0x76, 0x0,  0xa6, 0xd3, 0x1d, 0xd0, 0x29, 0x15, 0x24, 0x3d, 0xfd, 0xd9,
  0x43, 0xa0, 0x21, 0x65, 0xda, 0x36, 0x7a, 0x6b, 0x7e, 0x4d, 0xae, 0x1d, 0xd2,
  0xe8, 0xb8, 0x36, 0x90, 0x30, 0x80, 0x79, 0x5d, 0x25, 0x85, 0x7,  0x6c, 0xc1,
  0xc1, 0x5d, 0xd9, 0xe8, 0xd2, 0xe5, 0xe0, 0x47, 0x52, 0x65, 0x69, 0xb1, 0xbf,
  0xd3, 0x95, 0xd9, 0x57, 0xeb, 0x9f, 0xde, 0x32, 0x5d, 0x34, 0x2d, 0x14, 0x42,
  0x6e, 0x71, 0xef, 0xdc, 0x18, 0x87, 0x51, 0x5e, 0x53, 0xcd, 0xea, 0x58, 0x34,
  0x92, 0x1f, 0x92, 0x86, 0x29, 0xe7, 0x48, 0xee, 0xd0, 0x97, 0xac, 0x40, 0x24,
  0xe2, 0xbf, 0x25, 0x5d, 0x70, 0x41, 0x1f, 0x87, 0x37, 0x39, 0x48, 0xcf, 0x8e,
  0x8a, 0xa7, 0xef, 0xfa, 0x2b, 0xa,  0xb4, 0x7d, 0x51, 0x66, 0x9,  0x1e, 0x1a,
  0xed, 0xec, 0x60, 0x56, 0x8b, 0x15, 0x5b, 0xd9, 0xc2, 0x7b, 0xc5, 0x5f, 0x3e,
  0xce, 0x35, 0xf8, 0x3d, 0x63, 0x6d, 0xbc, 0xd5, 0xab, 0xf4, 0x85, 0x3a, 0x5,
  0x1d, 0xb9, 0x4d, 0x50, 0x45
];

const test1_bBytes: [u8; 512] = [
  0xd3, 0x51, 0x9b, 0xd8, 0x6e, 0xdf, 0x5d, 0xbe, 0x9c, 0x64, 0xa3, 0x78, 0x17,
  0x13, 0xdd, 0x4,  0x9b, 0x74, 0x7f, 0x56, 0x51, 0xfd, 0x91, 0x8c, 0xce, 0x8a,
  0x9b, 0x80, 0xb6, 0xd6, 0xa2, 0xfb, 0x6a, 0xab, 0x33, 0x4a, 0xb5, 0x69, 0xa8,
  0x9b, 0x58, 0x98, 0x2e, 0xa,  0xae, 0x37, 0x30, 0x90, 0x29, 0x9f, 0xa2, 0x27,
  0xf9, 0x65, 0x2b, 0x80, 0x2d, 0x23, 0xee, 0x5c, 0xa6, 0x82, 0x4c, 0x43, 0x3d,
  0x1,  0xd4, 0xb,  0xb0, 0x47, 0x3b, 0x16, 0x19, 0xa,  0x8b, 0xfb, 0x13, 0x7c,
  0xa,  0x70, 0x4b, 0x4f, 0x49, 0xb4, 0x50, 0x15, 0xbf, 0xbe, 0xe1, 0xf6, 0x70,
  0x44, 0x6f, 0x7,  0x59, 0x52, 0x59, 0xd7, 0xc,  0x7d, 0x79, 0xae, 0x95, 0xf9,
  0xb2, 0x54, 0x3b, 0x1,  0x62, 0xeb, 0xb7, 0x63, 0xeb, 0xb8, 0x1c, 0x4b, 0x6e,
  0xdf, 0xf9, 0xd1, 0x97, 0xde, 0xde, 0x1d, 0xbe, 0x57, 0xe4, 0x4,  0x66, 0xd1,
  0x56, 0xd4, 0xdf, 0xd5, 0xd7, 0x63, 0x4c, 0x45, 0x53, 0x4f, 0xb2, 0xbb, 0xbd,
  0x1b, 0xf2, 0x57, 0x81, 0x7a, 0x17, 0x3c, 0x7,  0x95, 0xfd, 0xbc, 0xc5, 0x33,
  0x97, 0x6f, 0x4a, 0xd,  0x4,  0x16, 0xf,  0x29, 0x9a, 0xf5, 0xdc, 0x27, 0x2a,
  0x81, 0x4b, 0x36, 0xd5, 0xba, 0xe5, 0x11, 0x6b, 0x95, 0x42, 0x17, 0x8f, 0x1d,
  0x55, 0x4c, 0xab, 0x7a, 0x64, 0x53, 0x3,  0x54, 0x82, 0xb9, 0x81, 0xad, 0xd4,
  0xbd, 0x8f, 0xfd, 0x5d, 0x50, 0x8,  0x39, 0xb2, 0x3e, 0x30, 0xeb, 0xd3, 0xf3,
  0x86, 0x8f, 0x7,  0x58, 0x4c, 0xbd, 0x12, 0xba, 0x8,  0x93, 0x25, 0x98, 0x28,
  0x60, 0xf6, 0x9f, 0x2b, 0xdc, 0x70, 0x77, 0xb6, 0xc6, 0xf,  0x58, 0xa2, 0x27,
  0x28, 0x23, 0x37, 0x9e, 0x10, 0x23, 0x70, 0x41, 0x65, 0xf7, 0xc1, 0x4f, 0x64,
  0xa1, 0x8f, 0xd5, 0xb2, 0x45, 0xa1, 0x81, 0x49, 0xb7, 0xed, 0xa3, 0x38, 0x5a,
  0x56, 0xba, 0x9e, 0x79, 0x50, 0x2a, 0x27, 0xbf, 0x13, 0x86, 0x5c, 0xde, 0x35,
  0xbe, 0x15, 0xde, 0x3,  0xd0, 0x6a, 0xa6, 0xf0, 0x8b, 0x17, 0x2b, 0x7e, 0xeb,
  0x4b, 0x73, 0xcb, 0xc1, 0x57, 0x1,  0x9c, 0x5d, 0x93, 0x35, 0x84, 0xa8, 0xd1,
  0xad, 0xd6, 0x1b, 0x7d, 0xbc, 0xa3, 0xb2, 0x53, 0xca, 0xc,  0xf9, 0x3d, 0xc9,
  0xa8, 0xa3, 0xaa, 0xc2, 0xf5, 0x2,  0x27, 0x2,  0x2e, 0x69, 0x2f, 0x7b, 0x47,
  0x67, 0x55, 0x66, 0x20, 0xcc, 0x92, 0x8d, 0x63, 0xe3, 0x10, 0x78, 0xc3, 0x60,
  0xe2, 0xab, 0x4b, 0x71, 0xa9, 0x17, 0xe1, 0x9e, 0x7e, 0xb0, 0x93, 0x80, 0x48,
  0x2e, 0x5b, 0x4e, 0xa8, 0x82, 0x2,  0x87, 0x1c, 0x2a, 0x29, 0xca, 0x6f, 0x66,
  0xb2, 0xfe, 0x30, 0x4a, 0xf6, 0x9,  0x52, 0xe,  0x4f, 0x81, 0xd6, 0x4c, 0x26,
  0x76, 0x8b, 0xb8, 0x12, 0xa8, 0x66, 0x79, 0x36, 0x66, 0x11, 0xf4, 0xcf, 0x6e,
  0x89, 0xe2, 0xbd, 0x1d, 0x9d, 0x7e, 0x28, 0x72, 0x9c, 0x7f, 0xe,  0x4e, 0x31,
  0x52, 0xd7, 0xad, 0x7f, 0x18, 0x6f, 0xa3, 0x2f, 0x1,  0xe1, 0x69, 0xff, 0x6,
  0xa1, 0x20, 0x1,  0xbc, 0x17, 0x9d, 0xfd, 0xd,  0xc9, 0x42, 0xcb, 0xab, 0xc5,
  0x55, 0xf6, 0x7f, 0x5f, 0xd0, 0x43, 0xe0, 0xa3, 0x35, 0x43, 0x40, 0xfa, 0x49,
  0xd,  0x2f, 0x12, 0x33, 0x67, 0xbe, 0x92, 0x6e, 0xb3, 0xe1, 0xdf, 0xfe, 0x70,
  0xc3, 0x15, 0x1c, 0x87, 0x45, 0xf,  0x32, 0x17, 0xdc, 0x2a, 0xa0, 0xb5, 0xa7,
  0x6,  0x9c, 0x5a, 0x17, 0xd5, 0x56, 0x14, 0xc9, 0x99, 0x41, 0x39, 0xf7, 0xb3,
  0x7b, 0x49, 0xe9, 0xfe, 0x78, 0xa,  0xf6, 0x5a, 0xe,  0x89, 0xe2, 0xe5, 0x8f,
  0xea, 0xaf, 0xfe, 0x16, 0x3b, 0xc9, 0xd1, 0x9e, 0xaf, 0xd9, 0xcd, 0x29, 0xb0,
  0xef, 0x3c, 0xef, 0x3d, 0x1
];

const test1_resBytes: [u8; 512] = [
  0xba, 0x8b, 0x73, 0x49, 0xf4, 0x8d, 0xa7, 0xe7, 0x5f, 0x26, 0xef, 0x37, 0x13,
  0x66, 0x0d, 0x8f, 0xd6, 0xcc, 0x51, 0x82, 0x2c, 0x5e, 0x68, 0xa0, 0xfe, 0x24,
  0xc5, 0xd6, 0x28, 0x40, 0xb0, 0x0d, 0xde, 0x5b, 0xa9, 0xb9, 0xd2, 0xa2, 0xe6,
  0xb1, 0x80, 0xaf, 0x6c, 0xf5, 0x52, 0x2c, 0xd7, 0x1a, 0x89, 0xf2, 0x0e, 0x0a,
  0x47, 0x10, 0xd1, 0xb7, 0x67, 0x5e, 0x34, 0x38, 0x0a, 0x34, 0xa3, 0xdf, 0x78,
  0x24, 0x7b, 0x1c, 0xc2, 0xb1, 0x60, 0x1b, 0x5d, 0xb1, 0x94, 0x6a, 0xed, 0x7e,
  0x38, 0x40, 0x2d, 0x9d, 0xa1, 0xcb, 0xfb, 0x66, 0x71, 0x5b, 0x7f, 0x17, 0x46,
  0x80, 0x0b, 0x6b, 0xc3, 0xed, 0x90, 0x0c, 0xe2, 0xc1, 0x70, 0x49, 0xf1, 0x66,
  0x0d, 0x96, 0x97, 0x63, 0xf8, 0xec, 0x38, 0x28, 0x6d, 0xda, 0xd8, 0x37, 0xf7,
  0x43, 0xb7, 0x76, 0x41, 0xe5, 0x19, 0x1b, 0x31, 0xfa, 0x19, 0x19, 0x27, 0xa4,
  0xb5, 0xe1, 0xed, 0x3e, 0x51, 0x26, 0x93, 0x90, 0x98, 0xd8, 0x32, 0xb0, 0xf2,
  0xda, 0x6b, 0x97, 0xc4, 0x7b, 0x3d, 0x8e, 0xcb, 0x87, 0x78, 0x56, 0xfe, 0x6d,
  0x86, 0x5a, 0x2c, 0x8b, 0x56, 0xf3, 0xfa, 0xe7, 0x66, 0xb6, 0xd6, 0x62, 0x86,
  0x65, 0xb8, 0x3f, 0xdf, 0x80, 0x63, 0xec, 0x25, 0x20, 0xaf, 0xf7, 0x45, 0xdc,
  0x6a, 0x4d, 0x5c, 0x68, 0x07, 0x4b, 0xb7, 0xa1, 0xce, 0xac, 0x17, 0x23, 0x0b,
  0x00, 0xc9, 0x87, 0x67, 0x41, 0x88, 0x34, 0x13, 0x58, 0x81, 0x9b, 0x26, 0x5e,
  0xee, 0xac, 0x6c, 0x79, 0x2c, 0xf7, 0x66, 0x3e, 0x4f, 0x3f, 0x53, 0xd7, 0x75,
  0x64, 0xaf, 0xee, 0x42, 0x38, 0xcd, 0x9d, 0x61, 0xcd, 0x34, 0x7c, 0xa3, 0xd4,
  0xa0, 0x95, 0xac, 0xac, 0x40, 0x5f, 0xbe, 0x27, 0x98, 0xc3, 0x1f, 0xcc, 0x62,
  0x2e, 0x43, 0x4c, 0xa9, 0xf8, 0x93, 0x86, 0xcc, 0x64, 0xac, 0xb6, 0x84, 0xa9,
  0xb8, 0x8f, 0xbd, 0xd2, 0x25, 0xa6, 0x86, 0x55, 0x79, 0xd7, 0x2d, 0x50, 0x3c,
  0x84, 0xdd, 0x95, 0xcd, 0x5f, 0x89, 0xad, 0xf9, 0xd8, 0x01, 0x8f, 0x5b, 0xac,
  0x89, 0xf5, 0xc3, 0xcc, 0x31, 0x79, 0x52, 0xf0, 0xfe, 0xc0, 0x7b, 0xd3, 0x65,
  0xb5, 0xe1, 0xe1, 0x44, 0xa7, 0xb0, 0xc6, 0xd1, 0x98, 0xae, 0x27, 0x26, 0x65,
  0x1e, 0x55, 0x9d, 0x1b, 0xa4, 0x5f, 0x75, 0x22, 0xba, 0xa6, 0xb4, 0xd1, 0xa1,
  0x99, 0x64, 0xae, 0x74, 0xfb, 0x41, 0x34, 0x53, 0x54, 0x4e, 0x09, 0xcd, 0xde,
  0x3f, 0x50, 0x8f, 0xa5, 0xf4, 0xba, 0x59, 0x01, 0x9a, 0xb9, 0xdb, 0x16, 0x37,
  0x6d, 0x35, 0x8d, 0x3d, 0x26, 0x5e, 0xa5, 0x42, 0x8c, 0x42, 0xa4, 0x61, 0x15,
  0x60, 0x6d, 0x50, 0x97, 0xe3, 0xc4, 0xc8, 0x29, 0x13, 0x3b, 0x8e, 0xf3, 0x74,
  0xfc, 0x79, 0xc5, 0x65, 0x40, 0xb3, 0xae, 0xec, 0x4a, 0x37, 0x0e, 0x52, 0x86,
  0x39, 0xdb, 0x69, 0x15, 0x39, 0x64, 0xea, 0x83, 0x14, 0x67, 0xe9, 0x7d, 0x51,
  0xd6, 0xa2, 0x5e, 0x17, 0x3b, 0xae, 0xec, 0x30, 0x23, 0xaa, 0x7b, 0x24, 0x13,
  0x53, 0xcd, 0xbc, 0x6c, 0xed, 0xe2, 0x34, 0xd1, 0xda, 0xd9, 0xd0, 0xe4, 0x1f,
  0xc5, 0x31, 0xb0, 0x0f, 0xdb, 0x23, 0x92, 0x88, 0xe7, 0x40, 0x2e, 0x42, 0xc3,
  0xd4, 0x31, 0x73, 0x51, 0xac, 0x1f, 0x11, 0xcc, 0x3e, 0x8f, 0x26, 0x4f, 0xed,
  0x4f, 0x3e, 0x7d, 0x90, 0xa2, 0x7a, 0x77, 0xb2, 0x0a, 0x6e, 0xc5, 0x2e, 0x39,
  0x8f, 0x37, 0x8e, 0xf4, 0x50, 0x62, 0xfc, 0x2d, 0x25, 0x70, 0x40, 0x65, 0x15,
  0x3f, 0x87, 0x9d, 0x4b, 0x49, 0x42, 0xc1, 0x85, 0x96, 0xfd, 0xd9, 0x06, 0x7b,
  0x83, 0xb9, 0x75, 0xbe, 0x1b, 0xd4, 0x02, 0xc2, 0xdb, 0x1d, 0x73, 0x2b, 0x76,
  0xc2, 0x8f, 0xa8, 0x9b, 0x1f
];

const test1_aBytes: [u8; 512] = [
  0x21, 0x6b, 0x8d, 0xa5, 0x2a, 0xa5, 0x72, 0x85, 0x7a, 0xf1, 0x2b, 0xaa, 0x25,
  0x56, 0x40, 0xf9, 0xb6, 0xaa, 0x0b, 0x3b, 0x22, 0x55, 0xe7, 0x30, 0xc1, 0x16,
  0x36, 0x1a, 0xde, 0xc8, 0xa5, 0xc5, 0xd0, 0xdc, 0xc5, 0x29, 0x16, 0x2e, 0x29,
  0x84, 0xd6, 0x9c, 0x9a, 0x40, 0x65, 0xb2, 0x1d, 0xb8, 0x3b, 0x79, 0xa8, 0x1a,
  0x8d, 0x4f, 0xd7, 0x0b, 0x2c, 0xdc, 0x3d, 0x3a, 0xe9, 0xc7, 0x83, 0x3a, 0xd4,
  0x9a, 0x26, 0x39, 0xa5, 0xb8, 0x2b, 0x43, 0x72, 0xd9, 0xd0, 0x39, 0xb6, 0xe6,
  0xfc, 0xad, 0xf7, 0x05, 0x3b, 0x5d, 0xad, 0xa4, 0x10, 0x4a, 0x23, 0xae, 0x08,
  0x16, 0xcd, 0x0a, 0x47, 0x01, 0xe2, 0x58, 0x17, 0x4a, 0x74, 0xc9, 0xb8, 0x43,
  0x21, 0xab, 0x5e, 0xcd, 0xb1, 0x9e, 0x0c, 0x5a, 0xb3, 0xa6, 0x25, 0xd7, 0x2c,
  0xb6, 0x6f, 0x29, 0xaa, 0x8a, 0xac, 0x3e, 0x43, 0x3e, 0xa4, 0xb9, 0x0d, 0x0d,
  0x19, 0x92, 0x57, 0x61, 0xc7, 0xc3, 0x09, 0x30, 0x9a, 0xa5, 0xb6, 0xb5, 0x12,
  0x6b, 0x83, 0xa7, 0xff, 0x0e, 0x5d, 0xf4, 0xd0, 0xfc, 0xba, 0xe9, 0x49, 0xaf,
  0xa8, 0x17, 0xa9, 0xb4, 0x34, 0x09, 0x88, 0x4d, 0x5e, 0xd9, 0x50, 0x32, 0x96,
  0xaa, 0x4d, 0xe9, 0xd7, 0x94, 0x60, 0x2a, 0xe4, 0x7d, 0xdb, 0x68, 0x34, 0x4b,
  0x81, 0xcd, 0x0e, 0x7b, 0xf8, 0xac, 0x7c, 0xee, 0xdd, 0xa2, 0x86, 0xa5, 0x67,
  0x3b, 0xa1, 0xba, 0x2d, 0xbe, 0x44, 0x61, 0xec, 0xbf, 0x67, 0xd0, 0x4b, 0xdb,
  0x0e, 0x5b, 0xea, 0x2e, 0x0a, 0xac, 0xe1, 0x35, 0x8d, 0x5a, 0xab, 0xce, 0x28,
  0xeb, 0xf3, 0x3a, 0x4f, 0x3a, 0x35, 0x47, 0xb6, 0xe2, 0xa6, 0xd9, 0x15, 0x62,
  0x40, 0xfb, 0x4f, 0x05, 0xa4, 0x05, 0x38, 0xd7, 0xb5, 0x82, 0xcb, 0x64, 0x97,
  0xad, 0xde, 0x17, 0x81, 0x76, 0xb5, 0x0a, 0xab, 0xee, 0x12, 0x07, 0x1a, 0xc7,
  0x80, 0xf8, 0xfe, 0x2f, 0x85, 0x8f, 0xf1, 0x75, 0x33, 0xce, 0xe5, 0x6e, 0x4f,
  0x46, 0x2b, 0xa4, 0x44, 0x6a, 0x3c, 0x20, 0x88, 0x89, 0x64, 0x04, 0xdd, 0x6f,
  0x45, 0xfe, 0xcf, 0xdf, 0x6b, 0xa4, 0x4d, 0x2b, 0x34, 0xde, 0x1a, 0x62, 0x89,
  0xc6, 0x36, 0x79, 0x4d, 0xd1, 0xd0, 0xa4, 0x9d, 0xb6, 0x3d, 0x1d, 0x75, 0x98,
  0xa9, 0x81, 0x72, 0xf6, 0x5e, 0x6d, 0x65, 0xf7, 0xcc, 0xfa, 0x07, 0x9e, 0x9f,
  0x66, 0x48, 0xd4, 0x11, 0x0d, 0x7d, 0xd2, 0xe0, 0xeb, 0x94, 0x84, 0x5e, 0x55,
  0x24, 0x4f, 0xbb, 0x71, 0xb2, 0x49, 0x14, 0x63, 0x7e, 0xa3, 0xbd, 0xa4, 0x10,
  0xb3, 0xc7, 0x56, 0xe1, 0x82, 0x5e, 0x21, 0xee, 0xb9, 0x9c, 0x3d, 0x6d, 0x70,
  0x67, 0xcd, 0x23, 0x79, 0xf1, 0x42, 0x34, 0xf9, 0x01, 0xe5, 0xb5, 0x1e, 0xa9,
  0x91, 0xda, 0x7f, 0xbe, 0x8d, 0xfb, 0x0b, 0x21, 0x99, 0x42, 0xae, 0xae, 0x7b,
  0xe8, 0xfc, 0x21, 0x6c, 0xaa, 0x4f, 0xc6, 0x73, 0xe2, 0x86, 0x72, 0xd6, 0xb7,
  0xc1, 0x0e, 0xac, 0x58, 0x74, 0x19, 0x3f, 0xcb, 0x20, 0x5f, 0xc7, 0xcd, 0x7e,
  0x47, 0x63, 0x55, 0xdc, 0x88, 0x2b, 0xa8, 0xac, 0x51, 0xe4, 0x12, 0x88, 0xdb,
  0xf7, 0x26, 0x00, 0x59, 0xda, 0x27, 0x6f, 0x5d, 0x90, 0x37, 0x46, 0x8b, 0x5f,
  0x51, 0xf6, 0x06, 0xb6, 0xbc, 0xf5, 0x47, 0xa0, 0xf5, 0x59, 0xe5, 0x49, 0xf0,
  0xad, 0xf4, 0x7c, 0x6f, 0x71, 0x05, 0xc1, 0x83, 0x9f, 0x10, 0x93, 0x51, 0x39,
  0x91, 0x93, 0x85, 0x18, 0x2f, 0xfb, 0x1e, 0x44, 0x11, 0x91, 0x5e, 0x82, 0x6c,
  0xbe, 0x45, 0xfe, 0xa0, 0x95, 0x11, 0x41, 0x82, 0x4c, 0x5a, 0x97, 0xcc, 0x73,
  0xef, 0xf1, 0xc1, 0x00, 0xb1, 0x20, 0x8f, 0x3d, 0xdc, 0xa9, 0x97, 0x34, 0x39,
  0x78, 0xf9, 0x16, 0x61, 0xbc
];

const test1_n: [u64; 64] = [
  9599991180914675781u64,  17887562617956183028u64, 15691240139348954677u64,
  1940466912979522907u64,  3101489743727560990u64,  4127777269603627002u64,
  13773517772092835639u64, 5255367165036471522u64,  6355865638426520039u64,
  15859574871329852906u64, 6716042509353382383u64,  15246331085171187250u64,
  16564317848683000255u64, 535015491997657298u64,   3931695803044537733u64,
  7745713819343841464u64,  15655532794973140602u64, 15212543991194336765u64,
  14646740241842831526u64, 2292950028038352240u64,  3922838785049529294u64,
  8369908273332164444u64,  14038043889594683671u64, 17383450447285127173u64,
  12555321513068342851u64, 15494071212227051357u64, 14535636448721615333u64,
  7638089459314244008u64,  2530169770561470585u64,  10793533007071348232u64,
  7544133415888263054u64,  8037492406599232231u64,  11636121434820317198u64,
  10568420958463563672u64, 15164691018111979793u64, 14578750803878177699u64,
  6547316331094085838u64,  3154373022727332040u64,  1903697825457611169u64,
  4707805160022895159u64,  605234385824775165u64,   16448809393554812570u64,
  8918130404934629498u64,  14977027113672460711u64, 13362146096824294831u64,
  12051675890053575061u64, 7762149579399985965u64,  10896391330703600052u64,
  7932486897445403638u64,  3299033622145508660u64,  11924038508011556013u64,
  13518983478667199849u64, 1336032771107897152u64,  1764060696031835757u64,
  7678271959302389283u64,  1139344854484388303u64,  13302295086042245934u64,
  15644407883104584682u64, 17522547621506697646u64, 3721925206918568018u64,
  5468034312379550493u64,  17998847801093158881u64, 797968302854772673u64,
  17726658574787037860u64
];

const test1_b: [u64; 64] = [
  14783541793386347777u64, 18308887073112633305u64, 6489275720694491823u64,
  12933012024269998838u64, 15372497234167937527u64, 3071654674841754135u64,
  1521239504821819356u64,  10551568759682658499u64, 18034960678601844670u64,
  6904092860848882496u64,  14502377688697009791u64, 11610281746722848013u64,
  8044325044537130758u64,  1030815596589973272u64,  13627221212663028863u64,
  3919840268822415842u64,  2771556243396388473u64,  17728791629537597004u64,
  3011341787730948170u64,  6579381381976497194u64,  16257571001708070958u64,
  14078501660493195543u64, 2363425040426537080u64,  3344256404824741222u64,
  12151743973886666498u64, 11795582501302910409u64, 9559120753645551036u64,
  14682111924634489653u64, 17332973066445867891u64, 3872556772876118694u64,
  5776473173675891934u64,  17123592168846040697u64, 10364386129595156919u64,
  8088858520508654753u64,  11684351889958113315u64, 3160524697977098072u64,
  617878909322589855u64,   9695976598577943226u64,  592700806154408947u64,
  9344358712493301072u64,  12356298835345310393u64, 7752175002540922188u64,
  3062811920474760465u64,  294439497007225895u64,   18283705412592028173u64,
  17462568441528190869u64, 5495890522041728283u64,  7408798334289565539u64,
  10943428363885732868u64, 16985357104936122833u64, 12849960513935488867u64,
  6473656722420307449u64,  16282324942027446610u64, 5426637114791280574u64,
  1804408511195253360u64,  4396027857957501718u64,  3252705437589589059u64,
  2999294169423620992u64,  6383903095205081232u64,  7686293585002080411u64,
  14882878895724864251u64, 11201718181896753548u64, 11269311903766404356u64,
  15227123168991731134u64
];

const test1_a: [u64; 64] = [
  10895396590253924796u64, 13907310402720357545u64, 9388978931177353201u64,
  7835777361589375297u64,  3457390416638336642u64,  1194387633880663320u64,
  17617078373585093535u64, 5161394939414311085u64,  10042835811175939317u64,
  6474530773563094854u64,  5900861790777452032u64,  5144049605052967084u64,
  1819396114387291518u64,  8275003405142612084u64,  2408487160344994438u64,
  2421039592707451132u64,  12218787506225543947u64, 17384515755686868254u64,
  11258274973041369977u64, 14363916011586055865u64, 1469157145179328691u64,
  6797379137451438665u64,  1228776618012152964u64,  14770126305775470804u64,
  12214170066877244919u64, 15034314862104507800u64, 1901233477114219985u64,
  14978809239071044830u64, 9838504833025263102u64,  5712301161888496672u64,
  9624176414396704110u64,  1299036461684293167u64,  16003402147727846382u64,
  4095941959479498669u64,  1540865470620869637u64,  5708934059772782297u64,
  10185642409189962554u64, 1034678022880158005u64,  4927479774144646107u64,
  9702274478053666238u64,  1043701158380494242u64,  16464557053336846797u64,
  10856575518469546026u64, 3749678030647808050u64,  13468377179416406452u64,
  9486831577250189564u64,  662199181749064299u64,   940436013512247235u64,
  12288823916649948345u64, 12945075785021419305u64, 2426137061810375770u64,
  16309811659076646979u64, 2571001331297437441u64,  17799698375782961226u64,
  8275874736570301613u64,  15319599111489465155u64, 3232525955784737594u64,
  4285641352413173515u64,  15464404821858196920u64, 15050120835021810052u64,
  13913367587842467269u64, 13162345209550726960u64, 8858909951785320697u64,
  2408174166284268165u64
];

const test1_res: [u64; 64] = [
  8298857316366195487u64,  8484249345405934365u64,  9626160362109502393u64,
  1531091507971965633u64,  5792469341583720549u64,  7981836739334868724u64,
  4502914229954130442u64,  1282468778437766479u64,  4810922135815040031u64,
  1142546042737606702u64,  15769865248418640304u64, 6038689851407217873u64,
  12604502386845361171u64, 16824693765214967611u64, 7572021454087328871u64,
  17026481875881179611u64, 8429746491244983214u64,  16412463020786814707u64,
  4802069847123775639u64,  3858807690732323468u64,  6413577467387590509u64,
  14834363837844944058u64, 8429402820546153993u64,  13449636149640914094u64,
  2185825936205640994u64,  12738099049751062117u64, 8922587118604207271u64,
  14108706129797250752u64, 18003141225643543029u64, 4360853974666938797u64,
  2713003527211789648u64,  12445280484765711826u64, 4849437783863250020u64,
  13702088355117556270u64, 11805237087952453727u64, 4771789881033438332u64,
  5710375037381685230u64,  17198240344722335294u64, 9814490758799631966u64,
  1667188385066215233u64,  6658580070852054700u64,  2675331655221930573u64,
  9684349158202303468u64,  6265627378370139746u64,  8671397878907415691u64,
  7752881317134388103u64,  10633166774552556250u64, 2856608045612290342u64,
  4748228986725341465u64,  7915877029940279158u64,  979136424633448488u64,
  10379920561622610278u64, 9157865883042563053u64,  3286961200518558043u64,
  6751340503377000512u64,  8657179747168641051u64,  7448448249011413983u64,
  9940022764864852407u64,  9272749958680598298u64,  16022586714868606641u64,
  18312979508384280589u64, 15477835638897535136u64, 6856430502545853839u64,
  13441964274290108391u64
];

fn mod_exp_bytes_be_precomp(n_bytes: &mut [u8], a_bytes: &mut [u8], b_bits: u32, b_bytes: &mut [u8], res_bytes: &mut [u8]) {
  let mut res = [ 0u64; 64 ];
  let b_bytes_len = (b_bits - 1u32) / 8u32 + 1u32;

  let a = crate::hacl::bignum4096::Hacl_Bignum4096_new_bn_from_bytes_be(512, a_bytes);
  let n = crate::hacl::bignum4096::Hacl_Bignum4096_new_bn_from_bytes_be(512, n_bytes);
  let b = crate::hacl::bignum4096::Hacl_Bignum4096_new_bn_from_bytes_be(b_bytes_len, b_bytes);
  let k = crate::hacl::bignum4096::Hacl_Bignum4096_mont_ctx_init(&n);

  crate::hacl::bignum4096::Hacl_Bignum4096_mod_exp_vartime_precomp(&k, &a, b_bits, &b, &mut res);
  crate::hacl::bignum4096::Hacl_Bignum4096_bn_to_bytes_be(&res, res_bytes);
}

fn mod_exp_bytes_be(n_bytes: &mut [u8], a_bytes: &mut [u8], b_bits: u32, b_bytes: &mut [u8], res_bytes: &mut [u8]) {
  let mut res = [ 0u64; 64 ];
  let b_bytes_len = (b_bits - 1u32) / 8u32 + 1u32;

  let a = crate::hacl::bignum4096::Hacl_Bignum4096_new_bn_from_bytes_be(512, a_bytes);
  let n = crate::hacl::bignum4096::Hacl_Bignum4096_new_bn_from_bytes_be(512, n_bytes);
  let b = crate::hacl::bignum4096::Hacl_Bignum4096_new_bn_from_bytes_be(b_bytes_len, b_bytes);

  crate::hacl::bignum4096::Hacl_Bignum4096_mod_exp_vartime(&n, &a, b_bits, &b, &mut res);
  crate::hacl::bignum4096::Hacl_Bignum4096_bn_to_bytes_be(&res, res_bytes);
}

#[test]
pub fn test_bignum () {

  // test bytes_be_precomp
  let mut res_bytes = [ 0u8; 512 ];
  mod_exp_bytes_be_precomp(&mut test1_nBytes, &mut test1_aBytes, 4096, &mut test1_bBytes, &mut res_bytes);
  assert_eq!(res_bytes, test1_resBytes);

  // test bytes_be
  let mut res_bytes = [ 0u8; 512 ];
  mod_exp_bytes_be(&mut test1_nBytes, &mut test1_aBytes, 4096, &mut test1_bBytes, &mut res_bytes);
  assert_eq!(res_bytes, test1_resBytes);

  // test mod_exp_vartime
  let mut res = [ 0u64; 64 ];
  crate::hacl::bignum4096::Hacl_Bignum4096_mod_exp_vartime(&mut test1_n, &mut test1_a, 4096, &mut test1_b, &mut res);
  assert_eq!(res, test1_res);
}

