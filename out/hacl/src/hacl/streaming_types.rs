#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(unused_assignments)]
#![allow(unreachable_patterns)]
#![allow(unused_mut)]

#[derive(PartialEq, Clone)]
#[repr(C)]
pub
struct Hacl_Streaming_MD_state_32
{ pub block_state: Box<[u32]>, pub buf: Box<[u8]>, pub total_len: u64 }

#[derive(PartialEq, Clone)]
#[repr(C)]
pub
struct Hacl_Streaming_MD_state_64
{ pub block_state: Box<[u64]>, pub buf: Box<[u8]>, pub total_len: u64 }

pub type Hacl_Streaming_Types_error_code = u8;

pub type Hacl_Streaming_Types_optional = u8;

#[derive(PartialEq)]
#[repr(C)]
pub
struct Hacl_Streaming_Types_optional_32 <'a>
{ pub tag: u8, pub v: &'a mut [u32] }

#[derive(PartialEq)]
#[repr(C)]
pub
struct Hacl_Streaming_Types_optional_64 <'a>
{ pub tag: u8, pub v: &'a mut [u64] }

pub type Spec_Hash_Definitions_hash_alg = u8;
