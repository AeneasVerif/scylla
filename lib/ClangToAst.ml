(* Copyright (c) INRIA and Microsoft Corporation. All rights reserved. *)
(* Licensed under the Apache 2.0 and MIT Licenses. *)

open Krml.Ast
open Krml.PrintAst.Ops
open Clang.Ast
module K = Krml.Constant
module Helpers = Krml.Helpers

let fatal_error fmt =
  Printf.kbprintf (fun b ->
    Buffer.output_buffer stderr b;
    exit 255
  ) (Buffer.create 256) fmt

module FileMap = Map.Make(String)
module StructMap = Map.Make(String)
module LidMap = Krml.AstToMiniRust.LidMap
module LidSet = Krml.AstToMiniRust.LidSet
module ElaboratedMap = Map.Make(struct
  type t = declaration_name * [ `Struct ]
  let compare = compare
end)

(* GLOBAL STATE *)

(* A map from function names to the string list used in their fully qualified
   name. It is filled at the beginning of the translation, when exploring the
   translation unit *)
(* FIXME this map is shared across all top-level declarations (typedefs, functions) but this is not
   right since they live in different namespaces in C. Also, we want the type of functions, but not
   the type of typedefs. *)
let name_map = ref FileMap.empty

(* A map from structure names to their corresponding KaRaMeL `type_def` declaration,
   as well as whether the struct type is annotated with the `scylla_box` attribute.
   Struct declarations are typically done through a typedef indirection in C, e.g.,
   `typedef struct name_s { ... } name;`
   This map is used to deconstruct the indirection in Rust, and directly define
   a struct type `name`
*)
let struct_map = ref StructMap.empty

(* A map from an elaborated type reference (e.g. `struct S`) to the lid it has been assigned in the
 translation -- we always eliminate elaborated types in favor of lids. *)
let elaborated_map = ref ElaboratedMap.empty

(* A map from type alias names to their underlying implementation.
   It is needed to retrieve the type of, e.g., constants
   when the expected type is an alias to an integer type *)
let abbrev_map = ref LidMap.empty

(* A map storing types that are annotated with `scylla_box`, indicating
   that internal pointers should be translated to Boxes instead of borrows *)
let boxed_types = ref LidSet.empty


(* ENVIRONMENTS *)

type env = {
  (* Variables in the context *)
  vars: (string * typ) list;
  (* Expected return typ of the function *)
  ret_t: typ;
}

let empty_env = {vars = []; ret_t = TAny}

let add_var env var = {env with vars = var :: env.vars }
let add_binders env binders = List.fold_left (fun env b ->
    let open Krml.Ast in
    add_var env (b.node.name, b.typ)
  ) env binders

(* TODO: Handle fully qualified names/namespaces/different files. *)
let find_var env name =
  let exception Found of int * typ in
  try
    List.iteri (fun i (name', t) ->
      if name = name' then
        raise (Found (i, t))
    ) env.vars;
    raise Not_found
  with
  | Found (i, t) ->
      with_type t (EBound i)
  | Not_found ->
      try
        let path, t = FileMap.find name !name_map in
        with_type t (EQualified (path, name))
      with
      | Not_found ->
          Printf.eprintf "Could not find variable %s\n" name;
          raise Not_found

(* TYPES *)

let get_id_name (dname: declaration_name) = match dname with
  | IdentifierName s -> s
  | ConstructorName _ -> failwith "constructor"
  | DestructorName _ -> failwith "destructor"
  | ConversionFunctionName _ -> failwith "conversion function"
  | DeductionGuideName _ -> failwith "deduction guide"
  | OperatorName _ -> failwith "operator name"
  | LiteralOperatorName _ -> failwith "literal operator name"
  | UsingDirectiveName -> failwith "using directive"

let translate_typ_name = function
  | "size_t" -> Helpers.usize
  | "uint8_t" -> Helpers.uint8
  | "uint16_t" -> Helpers.uint16
  | "uint32_t" -> Helpers.uint32
  | "uint64_t" -> Helpers.uint64

  | s ->
      (* We first try to find the type name in the environment *)
      match FileMap.find_opt s !name_map with
      | Some (path, _t) -> TQualified (path, s)
      | None ->
        (* If the type is not found in the environment, we assume
           it is an external type, and translate A_B_ty to a_b::ty *)
        let path = String.split_on_char '_' s in
        let name, path = match List.rev path with
        | [] -> failwith "Empty name"
        | hd :: tl -> hd, String.concat "_" (List.rev tl)
        in TQualified ([path], name)

(* We assume a modern system where sizeof int == 4, sizeof long long == 8, and sizeof long is
   determined at configure-time (see DataModel.ml). *)
let translate_builtin_typ (t: Clang.Ast.builtin_type) =
  match [@warnerror "-11"] t with
  | Void -> TUnit
  | UInt -> TInt UInt32
  | UShort -> failwith "translate_builtin_typ: ushort"
  | ULong ->
      begin match DataModel.size_long with
      | 4 -> TInt UInt32
      | 8 -> TInt UInt64
      | _ -> failwith "impossible"
      end
  | ULongLong -> TInt UInt64
  | UInt128 -> failwith "translate_builtin_typ: uint128"

  | Int -> TInt Int32

  | Short
  | Long
  | LongLong
  | Int128 -> failwith "translate_builtin_typ: signed int"
  | Bool -> TBool

  | Pointer -> failwith "translate_builtin_typ: pointer"

  | Invalid -> failwith "translate_builtin_typ: Invalid"
  | Unexposed -> failwith "translate_builtin_typ: Unexposed"
  | Char_U -> failwith "translate_builtin_typ: Char_U"
  | UChar -> failwith "translate_builtin_typ: UChar"
  | Char16 -> failwith "translate_builtin_typ: Char16"
  | Char32 -> failwith "translate_builtin_typ: Char32"
  | Char_S -> failwith "translate_builtin_typ: Char_S"
  | SChar -> failwith "translate_builtin_typ: SChar"
  | WChar -> failwith "translate_builtin_typ: WChar"
  | Float -> failwith "translate_builtin_typ: Float"
  | Double -> failwith "translate_builtin_typ: Double"
  | LongDouble -> failwith "translate_builtin_typ: LongDouble"
  | NullPtr -> failwith "translate_builtin_typ: NullPtr"
  | Overload -> failwith "translate_builtin_typ: Overload"
  | Dependent -> failwith "translate_builtin_typ: Dependent"
  | ObjCId -> failwith "translate_builtin_typ: ObjCId"
  | ObjCClass -> failwith "translate_builtin_typ: ObjCClass"
  | ObjCSel -> failwith "translate_builtin_typ: ObjCSel"
  | Float128 -> failwith "translate_builtin_typ: Float128"
  | Half -> failwith "translate_builtin_typ: Half"
  | Float16 -> failwith "translate_builtin_typ: Float16"
  | ShortAccum -> failwith "translate_builtin_typ: ShortAccum"
  | Accum -> failwith "translate_builtin_typ: Accum"
  | LongAccum -> failwith "translate_builtin_typ: LongAccum"
  | UShortAccum -> failwith "translate_builtin_typ: UShortAccum"
  | UAccum -> failwith "translate_builtin_typ: UAccum"
  | ULongAccum -> failwith "translate_builtin_typ: ULongAccum"
  | BFloat16 -> failwith "translate_builtin_typ: BFloat16"
  | Ibm128 -> failwith "translate_builtin_typ: Ibm128"
  | Complex -> failwith "translate_builtin_typ: Complex"
  | BlockPointer -> failwith "translate_builtin_typ: BlockPointer"
  | LValueReference -> failwith "translate_builtin_typ: LValueReference"
  | RValueReference -> failwith "translate_builtin_typ: RValueReference"
  | Record -> failwith "translate_builtin_typ: Record"
  | Enum -> failwith "translate_builtin_typ: Enum"
  | Typedef -> failwith "translate_builtin_typ: Typedef"
  | ObjCInterface -> failwith "translate_builtin_typ: ObjCInterface"
  | ObjCObjectPointer -> failwith "translate_builtin_typ: ObjCObjectPointer"
  | FunctionNoProto -> failwith "translate_builtin_typ: FunctionNoProto"
  | FunctionProto -> failwith "translate_builtin_typ: FunctionProto"
  | ConstantArray -> failwith "translate_builtin_typ: ConstantArray"
  | Vector -> failwith "translate_builtin_typ: Vector"
  | IncompleteArray -> failwith "translate_builtin_typ: IncompleteArray"
  | VariableArray -> failwith "translate_builtin_typ: VariableArray"
  | DependentSizedArray -> failwith "translate_builtin_typ: DependentSizedArray"
  | MemberPointer -> failwith "translate_builtin_typ: MemberPointer"
  | Auto -> failwith "translate_builtin_typ: Auto"
  | Elaborated -> failwith "translate_builtin_typ: Elaborated"
  | Pipe -> failwith "translate_builtin_typ: Pipe"
  | OCLImage1dRO -> failwith "translate_builtin_typ: OCLImage1dRO"
  | OCLImage1dArrayRO -> failwith "translate_builtin_typ: OCLImage1dArrayRO"
  | OCLImage1dBufferRO -> failwith "translate_builtin_typ: OCLImage1dBufferRO"
  | OCLImage2dRO -> failwith "translate_builtin_typ: OCLImage2dRO"
  | OCLImage2dArrayRO -> failwith "translate_builtin_typ: OCLImage2dArrayRO"
  | OCLImage2dDepthRO -> failwith "translate_builtin_typ: OCLImage2dDepthRO"
  | OCLImage2dArrayDepthRO -> failwith "translate_builtin_typ: OCLImage2dArrayDepthRO"
  | OCLImage2dMSAARO -> failwith "translate_builtin_typ: OCLImage2dMSAARO"
  | OCLImage2dArrayMSAARO -> failwith "translate_builtin_typ: OCLImage2dArrayMSAARO"
  | OCLImage2dMSAADepthRO -> failwith "translate_builtin_typ: OCLImage2dMSAADepthRO"
  | OCLImage2dArrayMSAADepthRO -> failwith "translate_builtin_typ: OCLImage2dArrayMSAADepthRO"
  | OCLImage3dRO -> failwith "translate_builtin_typ: OCLImage3dRO"
  | OCLImage1dWO -> failwith "translate_builtin_typ: OCLImage1dWO"
  | OCLImage1dArrayWO -> failwith "translate_builtin_typ: OCLImage1dArrayWO"
  | OCLImage1dBufferWO -> failwith "translate_builtin_typ: OCLImage1dBufferWO"
  | OCLImage2dWO -> failwith "translate_builtin_typ: OCLImage2dWO"
  | OCLImage2dArrayWO -> failwith "translate_builtin_typ: OCLImage2dArrayWO"
  | OCLImage2dDepthWO -> failwith "translate_builtin_typ: OCLImage2dDepthWO"
  | OCLImage2dArrayDepthWO -> failwith "translate_builtin_typ: OCLImage2dArrayDepthWO"
  | OCLImage2dMSAAWO -> failwith "translate_builtin_typ: OCLImage2dMSAAWO"
  | OCLImage2dArrayMSAAWO -> failwith "translate_builtin_typ: OCLImage2dArrayMSAAWO"
  | OCLImage2dMSAADepthWO -> failwith "translate_builtin_typ: OCLImage2dMSAADepthWO"
  | OCLImage2dArrayMSAADepthWO -> failwith "translate_builtin_typ: OCLImage2dArrayMSAADepthWO"
  | OCLImage3dWO -> failwith "translate_builtin_typ: OCLImage3dWO"
  | OCLImage1dRW -> failwith "translate_builtin_typ: OCLImage1dRW"
  | OCLImage1dArrayRW -> failwith "translate_builtin_typ: OCLImage1dArrayRW"
  | OCLImage1dBufferRW -> failwith "translate_builtin_typ: OCLImage1dBufferRW"
  | OCLImage2dRW -> failwith "translate_builtin_typ: OCLImage2dRW"
  | OCLImage2dArrayRW -> failwith "translate_builtin_typ: OCLImage2dArrayRW"
  | OCLImage2dDepthRW -> failwith "translate_builtin_typ: OCLImage2dDepthRW"
  | OCLImage2dArrayDepthRW -> failwith "translate_builtin_typ: OCLImage2dArrayDepthRW"
  | OCLImage2dMSAARW -> failwith "translate_builtin_typ: OCLImage2dMSAARW"
  | OCLImage2dArrayMSAARW -> failwith "translate_builtin_typ: OCLImage2dArrayMSAARW"
  | OCLImage2dMSAADepthRW -> failwith "translate_builtin_typ: OCLImage2dMSAADepthRW"
  | OCLImage2dArrayMSAADepthRW -> failwith "translate_builtin_typ: OCLImage2dArrayMSAADepthRW"
  | OCLImage3dRW -> failwith "translate_builtin_typ: OCLImage3dRW"
  | OCLSampler -> failwith "translate_builtin_typ: OCLSampler"
  | OCLEvent -> failwith "translate_builtin_typ: OCLEvent"
  | OCLQueue -> failwith "translate_builtin_typ: OCLQueue"
  | OCLReserveID -> failwith "translate_builtin_typ: OCLReserveID"
  | ObjCObject -> failwith "translate_builtin_typ: ObjCObject"
  | ObjCTypeParam -> failwith "translate_builtin_typ: ObjCTypeParam"
  | Attributed -> failwith "translate_builtin_typ: Attributed"
  | OCLIntelSubgroupAVCMcePayload -> failwith "translate_builtin_typ: OCLIntelSubgroupAVCMcePayload"
  | OCLIntelSubgroupAVCImePayload -> failwith "translate_builtin_typ: OCLIntelSubgroupAVCImePayload"
  | OCLIntelSubgroupAVCRefPayload -> failwith "translate_builtin_typ: OCLIntelSubgroupAVCRefPayload"
  | OCLIntelSubgroupAVCSicPayload -> failwith "translate_builtin_typ: OCLIntelSubgroupAVCSicPayload"
  | OCLIntelSubgroupAVCMceResult -> failwith "translate_builtin_typ: OCLIntelSubgroupAVCMceResult"
  | OCLIntelSubgroupAVCImeResult -> failwith "translate_builtin_typ: OCLIntelSubgroupAVCImeResult"
  | OCLIntelSubgroupAVCRefResult -> failwith "translate_builtin_typ: OCLIntelSubgroupAVCRefResult"
  | OCLIntelSubgroupAVCSicResult -> failwith "translate_builtin_typ: OCLIntelSubgroupAVCSicResult"
  | OCLIntelSubgroupAVCImeResultSingleRefStreamout -> failwith "translate_builtin_typ: OCLIntelSubgroupAVCImeResultSingleRefStreamout"
  | OCLIntelSubgroupAVCImeResultDualRefStreamout -> failwith "translate_builtin_typ: OCLIntelSubgroupAVCImeResultDualRefStreamout"
  | OCLIntelSubgroupAVCImeSingleRefStreamin -> failwith "translate_builtin_typ: OCLIntelSubgroupAVCImeSingleRefStreamin"
  | OCLIntelSubgroupAVCImeDualRefStreamin -> failwith "translate_builtin_typ: OCLIntelSubgroupAVCImeDualRefStreamin"
  | ExtVector -> failwith "translate_builtin_typ: ExtVector"
  | Atomic -> failwith "translate_builtin_typ: Atomic"
  | _ -> failwith "translate_builtin_typ: BTFTagAttributed"

let rec translate_typ (typ: qual_type) = match typ.desc with
  | Pointer typ -> TBuf (translate_typ typ, false)

  | LValueReference _ -> failwith "translate_typ: lvalue reference"
  | RValueReference _ -> failwith "translate_typ: rvalue reference"

  (* ConstantArray is a constant-size array. If we refine the AstToMiniRust analysis,
    we could extract array length information here *)
  | ConstantArray { element; _} -> TBuf (translate_typ element, false)

  | Enum _ -> failwith "translate_typ: enum"

  | FunctionType {result; parameters; _} ->
      let ret_typ = translate_typ result in
      begin match parameters with
      | None -> TArrow (TUnit, ret_typ)
      | Some params ->
          (* Not handling variadic parameters *)
          assert (not (params.variadic));
          let ts = List.map
            (fun (p: parameter) -> translate_typ p.desc.qual_type)
            params.non_variadic
          in
          Helpers.fold_arrow ts ret_typ
      end

  | Record  _ -> failwith "translate_typ: record"

  | Typedef {name; _} -> get_id_name name |> translate_typ_name

  | BuiltinType t -> translate_builtin_typ t

  | Elaborated { keyword = Struct; named_type = { desc = Record {name; _}; _}; _ } ->
      begin try
        TQualified (ElaboratedMap.find (name, `Struct) !elaborated_map)
      with Not_found ->
        Format.eprintf "Trying to translate type %a\n@." Clang.Type.pp typ;
        failwith "translate_typ: unsupported type"
      end

  | _ ->
      Format.eprintf "Trying to translate type %a\n@." Clang.Type.pp typ;
      failwith "translate_typ: unsupported type"

(* Takes a Clangml expression [e], and retrieves the corresponding karamel Ast type *)
let typ_of_expr (e: expr) : typ = Clang.Type.of_node e |> translate_typ

let rec normalize_type t =
  match t with
  | TQualified lid ->
      begin match LidMap.find lid !abbrev_map with
      | exception Not_found ->
          (* Krml.KPrint.bprintf "Not in the abbrev map: %a\n" Krml.PrintAst.Ops.plid lid; *)
          t
      | BuiltinType t -> translate_builtin_typ t
      | Typedef { name; _ } ->
          get_id_name name |> translate_typ_name
        (* We might have a chain of aliases, we recurse on the resulting type *)
        |> normalize_type
      | Pointer t -> TBuf (normalize_type (translate_typ t), false)
      | _ -> failwith "impossible"
      end
  | _ -> t

(* Indicate that we synthesize the type of an expression based on the information provided by
   Clang. We aim to do this only in a few select cases:
   - integer constants
   - variable declarations
   - function types (so, arguments and return types).
   Every other type should be able to be deduced from the context. *)
let typ_from_clang (e: Clang.Ast.expr): typ =
  normalize_type (typ_of_expr e)


(* HELPERS *)

(* Helpers to deal with the Clang AST, as opposed to Helpers which deals with the Krml AST. *)
module ClangHelpers = struct

  let is_known_name name' (e: expr) = match e.desc with
    | DeclRef { name; _ } ->
        let name = get_id_name name in
        name = name'
    | _ -> false

  (* Check whether a given Clang expression is a scylla_reset callee *)
  let is_scylla_reset = is_known_name "scylla_reset"

  (* Check whether a given Clang expression is a memcpy callee *)
  let is_memcpy e = is_known_name "__builtin___memcpy_chk" e || is_known_name "memcpy" e

  (* Check whether a given Clang expression is a memset callee *)
  let is_memset e = is_known_name "__builtin___memset_chk" e || is_known_name "memset" e

  (* Check whether a given Clang expression is a calloc callee *)
  let is_calloc = is_known_name "calloc"

  (* Check whether a given Clang expression is a malloc callee *)
  let is_malloc = is_known_name "malloc"

  (* Check whether a given Clang expression is a free callee *)
  let is_free = is_known_name "free"

  (* Check whether a given Clang expression is an exit callee *)
  let is_exit = is_known_name "exit"

  (* Check whether a variable declaration has a malloc initializer. If so,
     we will rewrite it based on the initializer that follows *)
  let is_malloc_vdecl (vdecl: var_decl_desc) = match vdecl.var_init with
    | Some {desc = Call {callee; _}; _}
    (* There commonly is a cast around malloc to the type of the variable. We omit it when translating it to Rust,
       as the allocation will be typed *)
    | Some {desc = Cast {operand = {desc = Call {callee; _}; _}; _}; _} when is_malloc callee ->
        true

    | _ -> false

  (* Check whether expression [e] is a pointer *)
  let has_pointer_type (e: expr) = match typ_of_expr e with
    | TBuf _ | TArray _ -> true
    | _ -> false

  (* Recognize several common patterns for the null pointer *)
  let rec is_null (e: expr) = match e.desc with
    | Cast { qual_type = {desc = Pointer { desc = BuiltinType Void; _}; _} ; operand = {desc = IntegerLiteral (Int 0); _}; _ } -> true
    | _ -> false

  let is_null_check var_name (e: expr) = match e.desc with
    | BinaryOperator {lhs = {desc = DeclRef { name; _}; _}; kind = NE; rhs } ->
        if get_id_name name = var_name && is_null rhs then true else false
    | _ -> false

  (* Check whether statement [s] corresponds to a malloc initializer for
     , which
      will therefore be rewritten in combination with malloc to generate
      a standard array or Vec declaration in Rust *)
  let is_malloc_initializer (vdecl: var_decl_desc) (s: stmt_desc) = match s with
    | If { cond; _ } when is_null_check vdecl.var_name cond -> true (* {cond; then_branch; else_branch; _} -> true *)
    | _ -> false

  (* Simple heuristics to detect whether a loop condition is always false, in this case we can omit the loop.
     TODO: Should probably check for absence of side-effects in condition evaluation *)
  let is_trivial_false (e: Krml.Ast.expr) = match e.node with
    (* e != e is always false *)
    | EApp ({node = EOp (Neq, _); _ }, [e1; e2]) when e1 = e2 -> true
    | EBool false -> true
    | _ -> false

  let extract_sizeof_ty = function
    | ArgumentExpr _ -> failwith "ArgumentExpr not supported"
    | ArgumentType ty -> translate_typ ty

  let extract_constarray_size (ty: qual_type) = match ty.desc with
    | ConstantArray {size; _} -> size, Helpers.mk_uint32 size
    | _ ->
        Format.eprintf "Expected ConstantArray, got type %a\n@." Clang.Type.pp ty;
        failwith "Type is not a ConstantArray"

  let is_constantarray (ty: qual_type) = match ty.desc with
    | ConstantArray _ -> true
    | _ -> false

  let is_assign_op (kind: Clang.Ast.binary_operator_kind) = match kind with
    | Assign | AddAssign | MulAssign | DivAssign | RemAssign
    | SubAssign | ShlAssign | ShrAssign | AndAssign
    | XorAssign | OrAssign -> true
    | _ -> false
end

open ClangHelpers


(* EXPRESSIONS *)

let assign_to_bop w (kind: Clang.Ast.binary_operator_kind) : Krml.Ast.expr =
  let op = match kind with
  (* TODO: Might need to disambiguate for pointer arithmetic *)
  | AddAssign -> K.Add
  | MulAssign -> Mult
  | DivAssign -> Div
  | RemAssign -> Mod
  | SubAssign -> Sub
  | ShlAssign -> BShiftL
  | ShrAssign -> BShiftR
  | AndAssign -> BAnd
  (* TODO: Disambiguate *)
  | XorAssign -> BXor
  | OrAssign -> BOr
  | _ -> failwith "not an assign op"
  in
  Helpers.mk_op op w

let translate_binop (kind: Clang.Ast.binary_operator_kind) : K.op = match kind with
  | PtrMemD | PtrMemI -> failwith "translate_binop: ptr mem"

  (* Disambiguation for pointer arithmetic must be done when calling translate_binop:
     This is a deeper rewriting than just disambiguating between two K.op *)
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mult
  | Div -> Div
  | Rem -> Mod

  | Shl -> BShiftL
  | Shr -> BShiftR

  | Cmp -> failwith "translate_binop: cmp"

  | LT -> Lt
  | GT -> Gt
  | LE -> Lte
  | GE -> Gte
  | EQ -> Eq
  | NE -> Neq

  | And -> BAnd
  (* TODO: How to distinguish between Xor and BXor? Likely need typing info from operands *)
  | Xor -> BXor
  | Or -> BOr

  | LAnd -> And
  | LOr -> Or

  | Assign | AddAssign | MulAssign | DivAssign | RemAssign
  | SubAssign | ShlAssign | ShrAssign | AndAssign
  | XorAssign | OrAssign ->
      failwith "Assign operators should have been previously rewritten"

  | Comma -> failwith "translate_binop: comma"
  | InvalidBinaryOperator -> failwith "translate_binop: invalid binop"

(* Adjust the type of expression `e` to be `t`. We synthesize types bottom-up, but sometimes, the
   context provides an expected type. So far, this happens in three situations:
   - condition expressions, which krml wants to be booleans, but which in C are integers
   - array indices, and operands of memory-related operations (e.g. memcpy), which in C might be ULL
     constants (i.e., synthesized as UInt64 bottom-up), but which need to be SizeT
   - enum tags, which are integers in C, but in krml need to be converted to constants. *)
let adjust e t =
  match e.node, t with
  (* Conversions to integers: we rewrite constants on the fly, or emit a cast. *)
  | EConstant (_, c), TInt w ->
      with_type t (EConstant (w, c))
  | _, TInt _ ->
      if e.typ <> t then
        with_type t (ECast (e, t))
      else
        e

  (* Conversions to booleans: we rewrite constants on the fly, or emit `e != 0` *)
  | EConstant (_, "0"), TBool ->
      with_type TBool (EBool false)
  | EConstant (_, "1"), TBool ->
      with_type TBool (EBool true)
  | _, TBool ->
      if e.typ <> t then
        let w = Helpers.assert_tint e.typ in
        Helpers.mk_neq e (Helpers.zero w)
      else
        e

  (* TODO: tag indices *)

  | _ ->
      if e.typ <> t then
        fatal_error "Could not convert expression %a to have type %a" pexpr e ptyp t;
      e

(* Translate expression [e].

 When adding a case to this function, two questions arise:
 - does the context provide enough information to insert a call to `adjust`? example: translating a
   While node, one must adjust the condition to be bool, because the typing rules of the krml ast
   are different from C
 - are we trusting the type from clang when we shouldn't? (i.e., is it ok to call typ_from_clang) --
   this should generally be avoided, because it is not true that `(translate_expr e).typ =
   typ_from_clang e`. *)
let rec translate_expr (env: env) (e: Clang.Ast.expr) : Krml.Ast.expr =
  if is_null e then
    with_type (TBuf (TAny, false)) EBufNull
  else match e.desc with
    | IntegerLiteral n ->
        begin match typ_from_clang e with
        | TInt w as t ->
            let signed = K.is_signed w in
            with_type t (EConstant (w, Clang.Ast.string_of_integer_literal ~signed n))
        | t ->
            fatal_error "integer literal does not have an int type, it has %a" ptyp t
        end

    | FloatingLiteral _ -> failwith "translate_expr: floating literal"
    | StringLiteral _ -> failwith "translate_expr: string literal"
    | CharacterLiteral _ -> failwith "translate_expr character literal"
    | ImaginaryLiteral _ -> failwith "translate_expr: imaginary literal"
    | BoolLiteral _ -> failwith "translate_expr: bool literal"
    | NullPtrLiteral -> failwith "translate_expr: null ptr literal"

    | CompoundLiteral {qual_type; init = {desc = InitList l; _}} when is_constantarray qual_type ->
        (* TODO: understand why this is repeated between here and translate_vardecl -- share! *)
        (* Also probably needs local downward propagation of the expected type (from the variable
           declaration), to adjust proper casting of the integer types, followed by a call to
           adjust. *)
        let size, size_e = extract_constarray_size qual_type in
        if List.length l = 1 then
          (* One element initializer, possibly repeated *)
          let e = translate_expr env (List.hd l) in
          (* TODO: Arrays are not on stack if at top-level *)
          with_type (TBuf (e.typ, false)) (EBufCreate (Krml.Common.Stack, e, size_e))
        else (
          assert (List.length l = size);
          let es = List.map (translate_expr env) l in
          with_type (TBuf ((List.hd es).typ, false)) (EBufCreateL (Krml.Common.Stack, es))
        )

    (* We handled above the case of array initialization, this should
       be a struct initialization *)
    | CompoundLiteral {init = {desc = InitList l; _}; _} ->
        let translate_field_expr (e : expr) = match e.desc with
          | DesignatedInit { designators; init }  ->
              begin match designators with
              | [FieldDesignator name] ->
                  (* FIXME -- adjust type against expected field type, obtained via a lookup in
                     struct_map *)
                  let e = translate_expr env init in
                  Some name, e
              | [_] -> failwith "expected a field designator"
              | _ -> failwith "assigning to several fields during struct initialization is not supported"
              end
        | _ -> failwith "a designated initializer was expected when initializing a struct"
        in
       with_type (typ_from_clang e) (EFlat (List.map translate_field_expr l))


    | UnaryOperator {kind = PostInc | PreInc; operand } ->
        (* This is a special case for loop increments. The current Karamel
           extraction pipeline only supports a specific case of loops *)
        let o = translate_expr env operand in
        begin match o.typ with
        | TInt w ->
            (* We rewrite `name++` into `name := name + 1` *)
            with_type TUnit @@ EAssign (
              o,
              Krml.Ast.with_type o.typ (EApp (Helpers.mk_op K.Add w, [o; Helpers.one w]))
            )
        | TBuf (_t, _) as t_buf ->
            (* We rewrite `name++` into `name := name + 1` *)
            with_type TUnit @@ EAssign (
              o,
              Krml.Ast.with_type t_buf (EBufSub (o, Helpers.one SizeT))
            )
        | _ ->
            failwith "cannot increment this type"
        end

    | UnaryOperator {kind = PostDec | PreDec; operand } ->
        (* This is a special case for loop increments. The current Karamel
           extraction pipeline only supports a specific case of loops *)
        let o = translate_expr env operand in
        let w = Helpers.assert_tint o.typ in
        (* We rewrite `name++` into `name := name + 1` *)
        with_type TUnit @@ EAssign (
          o,
          Krml.Ast.with_type o.typ (EApp (Helpers.mk_op K.Sub w, [o; Helpers.one w]))
        )

    | UnaryOperator {kind = Not; operand } ->
        (* Bitwise not: ~ syntax, operates on integers *)
        let o = translate_expr env operand in
        with_type o.typ @@ EApp (Helpers.mk_op K.Not (Helpers.assert_tint o.typ), [o])

    | UnaryOperator {kind = LNot; operand } ->
        (* Logical not: The operand should be a boolean *)
        let o = translate_expr env operand in
        Helpers.mk_not (adjust o TBool)

    | UnaryOperator {kind = Deref; operand } ->
        let o = translate_expr env operand in
        let t = Helpers.assert_tbuf_or_tarray o.typ in
        with_type t @@ EBufRead (o, Helpers.zero_usize)

    | UnaryOperator {kind = AddrOf; operand } ->
        let o = translate_expr env operand in
        with_type (TBuf (o.typ, false)) (EAddrOf o)

    | UnaryOperator _ ->
        Format.printf "Trying to translate unary operator %a@." Clang.Expr.pp e;
        failwith "translate_expr: unary operator"

    | BinaryOperator {lhs; kind = Assign; rhs} ->
        let lhs = translate_expr env lhs in
        let rhs = translate_expr env rhs in
        with_type TUnit begin match lhs.node with
        (* Special-case rewriting for buffer assignments *)
        | EBufRead (base, index) -> EBufWrite (base, index, rhs)
        | _ -> EAssign (lhs, rhs)
        end

    | BinaryOperator {lhs; kind; rhs} when is_assign_op kind ->
        (* Interpreting operations as homogenous *)
        let lhs = translate_expr env lhs in
        let rhs = translate_expr env rhs in
        (* TODO: looks like this is not catching the case of pointer arithmetic -- can this be
           redirected to the case below? *)
        let w = Helpers.assert_tint rhs.typ in
        (* Rewrite the rhs into the compound expression, using the underlying operator *)
        let rhs = Krml.Ast.with_type lhs.typ (EApp (assign_to_bop w kind, [lhs; rhs])) in
        with_type TUnit begin match lhs.node with
        (* Special-case rewriting for buffer assignments *)
        | EBufRead (base, index) -> EBufWrite (base, index, rhs)
        | _ -> EAssign (lhs, rhs)
        end

    | BinaryOperator {lhs; kind; rhs} ->
        let lhs = translate_expr env lhs in
        let rhs = translate_expr env rhs in
        let kind = translate_binop kind in

        let apply_op kind lhs rhs =
          let w = Helpers.assert_tint lhs.typ in
          let op = Helpers.mk_op kind w in
          with_type (fst (Helpers.flatten_arrow op.typ)) (EApp (op, [lhs; rhs]))
        in

        (* In case of pointer arithmetic, we need to perform a rewriting into EBufSub/Diff *)
        begin match lhs.typ, kind with
        | TBuf _, Add ->
            with_type lhs.typ begin match lhs.node with
            (* Successive pointer arithmetic operations are likely due to operator precedence, e.g.,
               ptr + n - m parsed as (ptr + n) - m, when ptr + (n - m) might be intended.
               We recognize these cases, and normalize them to perform pointer arithmetic only once
            *)
            | EBufSub (lhs', rhs') ->
                (* (lhs' + rhs') + rhs --> lhs' + (rhs' + rhs) *)
                EBufSub (lhs', apply_op Add rhs' rhs)
            | EBufDiff (lhs', rhs') ->
                (* (lhs' - rhs') + rhs --> lhs' + (rhs - rhs') *)
                EBufSub (lhs', apply_op Sub rhs rhs')
            | _ ->
                EBufSub (lhs, rhs)
            end
        | TBuf _, Sub ->
            with_type lhs.typ begin match lhs.node with
            | EBufSub (lhs', rhs') ->
                (* (lhs' + rhs') - rhs --> lhs' + (rhs' - rhs) *)
                EBufSub (lhs', apply_op Sub rhs' rhs)
            | EBufDiff (lhs', rhs') ->
                (* (lhs' - rhs') - rhs --> lhs' - (rhs' + rhs) *)
                EBufDiff (lhs', apply_op Add rhs' rhs)
            | _ ->
                EBufDiff (lhs, rhs)
            end
        | _ ->
            apply_op kind lhs rhs
        end

    | DeclRef {name; _} ->
        let e = get_id_name name |> find_var env in
        Krml.KPrint.bprintf "non-normalized type: %a\n" ptyp e.typ;
        (* TODO: should this be done more generally? *)
        { e with typ = normalize_type e.typ }

    | Call {callee; args} when is_scylla_reset callee ->
        begin match args with
        | [e] -> Helpers.push_ignore (translate_expr env e)
        | _ -> failwith "wrong number of arguments for scylla_reset"
        end

    | Call {callee; args} when is_memcpy callee ->
        (* Format.printf "Trying to translate memcpy %a@." Clang.Expr.pp e; *)
        begin match args with
        (* We are assuming here that this is __builtin___memcpy_chk.
           This function has a fourth argument, corresponding to the number of bytes
           remaining in dst. We omit it during the translation *)
        | dst :: src :: len :: _ ->
            (* TODO: The type returned by clangml for the arguments is void*.
               However, clang-analyzer is able to find the correct type, so it should be possible to get the correct type through clangml

               In the meantime, we extract it from the sizeof call
            *)
            let len, _ty = match len.desc with
            (* We recognize the case `len = lhs * sizeof (_)` *)
              | BinaryOperator {lhs; kind = Mul; rhs = { desc = UnaryExpr {kind = SizeOf; argument}; _}} ->
                  let len = adjust (translate_expr env lhs) (TInt SizeT) in
                  let ty = extract_sizeof_ty argument in
                  len, ty
              | _ -> failwith "ill-formed memcpy"
            in
            let dst = translate_expr env dst in
            let src = translate_expr env src in
            with_type TUnit @@ EBufBlit (src, Helpers.zerou32, dst, Helpers.zerou32, len)

        | _ -> failwith "memcpy does not have the right number of arguments"
        end

    | Call {callee; args} when is_memset callee ->
        (* Format.printf "Trying to translate memset %a@." Clang.Expr.pp e; *)
        begin match args with
        | dst :: v :: len :: _ ->
            let len, _ty = match len.desc with
            (* We recognize the case `len = lhs * sizeof (_)` *)
              | BinaryOperator {lhs; kind = Mul; rhs = { desc = UnaryExpr {kind = SizeOf; argument}; _}} ->
                  let len = adjust (translate_expr env lhs) (TInt SizeT) in
                  let ty = extract_sizeof_ty argument in
                  len, ty
              | _ -> failwith "ill-formed memcpy"
            in
            let dst = translate_expr env dst in
            let elt = translate_expr env v in
            with_type TUnit @@ EBufFill (dst, elt, len)
        | _ -> failwith "memset does not have the right number of arguments"
        end

    | Call {callee; args} when is_free callee ->
        begin match args with
        | [ptr] -> with_type TUnit @@ EBufFree (translate_expr env ptr)
        | _ -> failwith "ill-formed free: too many arguments"
        end

    | Call {callee; _} when is_exit callee ->
        (* TODO: We should likely check the exit code, and possibly translate this to
           std::process::exit.
           However, std::process:exit immediately terminates the process and does not
           run destructors. As it is likely used as an abort in our usecases, we instead
           translate it to EAbort, which will become a `panic` *)
        with_type TAny (EAbort (None, Some ""))

    | Call {callee; args} ->
        (* Format.printf "Trying to translate function call %a@." Clang.Expr.pp callee; *)
        let callee = translate_expr env callee in
        (* TODO: adjust the type of the callee to strip the pointer *)
        (* NOTE: should not be necessary since the map is constructed properly without the pointer *)
        let args = List.map (fun x -> translate_expr env x) args in
        with_type (fst (Helpers.flatten_arrow callee.typ)) (EApp (callee, args))

    | Cast {qual_type; operand; _} ->
        (* Format.printf "Cast %a@."  Clang.Expr.pp e; *)
        let typ = translate_typ qual_type in
        let e = translate_expr env operand in
        with_type typ (ECast (e, typ))

    | ArraySubscript {base; index} ->
        let base = translate_expr env base in
        let index = adjust (translate_expr env index) (TInt SizeT) in
        (* Is this only called on rvalues? Otherwise, might need EBufWrite *)
        with_type (Helpers.assert_tbuf_or_tarray base.typ) (EBufRead (base, index))

    | ConditionalOperator _ -> failwith "translate_expr: conditional operator"
    | Paren _ -> failwith "translate_expr: paren"

    | Member {base; arrow; field} ->
        let base = match base with
        | None -> failwith "field accesses without a base expression are not supported"
        | Some b -> b
        in
        let base = translate_expr env base in

        let f = match field with
        | FieldName {desc; _} -> get_id_name desc.name
        | _ -> failwith "member node: only field accesses supported"
        in

        if not arrow then
          (* base.f *)
          (* FIXME deduce this properly *)
          with_type (typ_from_clang e) (EField (base, f))
        else
          (* base->f *)
          let deref_base = Helpers.(with_type (assert_tbuf base.typ) (EBufRead (base, Helpers.zero_usize))) in
          (* FIXME deduce this properly *)
          with_type (typ_from_clang e) (EField (deref_base, f))

    | UnaryExpr {kind = SizeOf; argument = ArgumentType t; _ } ->
        begin match normalize_type (translate_typ t) with
        | TInt w -> (Helpers.mk_sizet (Krml.Constant.bytes_of_width w))
        | _ ->
            Format.printf "Trying to translate unary expr %a@." Clang.Expr.pp e;
            failwith "translate_expr: unary expr"
        end

    | _ ->
      Format.eprintf "Trying to translate expression %a@." Clang.Expr.pp e;
      failwith "translate_expr: unsupported expression"

(* Create a default value associated to a given type [typ] *)
let create_default_value typ = match typ with
  | _ -> Krml.Ast.with_type typ EAny

let translate_vardecl (env: env) (vdecl: var_decl_desc) : env * binder * Krml.Ast.expr =
  let vname = vdecl.var_name in
  let typ = translate_typ vdecl.var_type in
  match vdecl.var_init with
  | None ->
        (* If there is no associated definition, we attempt to craft
           a default initialization value *)
        add_var env (vname, typ), Helpers.fresh_binder vname typ, create_default_value typ

  (* Intializing a constant array with a list of elements.
     For instance, uint32[2] = { 0 };
  *)
  (* TODO understand why this case is needed *)
  | Some {desc = InitList l; _} when is_constantarray vdecl.var_type ->
        let size, size_e = extract_constarray_size vdecl.var_type in
        if List.length l = 1 then
          (* One element initializer, possibly repeated *)
          let e = translate_expr env (List.hd l) in
          (* TODO: Arrays are not on stack if at top-level *)
          add_var env (vname, typ), Helpers.fresh_binder vname typ, Krml.Ast.with_type typ (EBufCreate (Krml.Common.Stack, e, size_e))
        else (
          assert (List.length l = size);
          let _ty = Helpers.assert_tbuf typ in
          let es = List.map (translate_expr env) l in
          add_var env (vname, typ), Helpers.fresh_binder vname typ, Krml.Ast.with_type typ (EBufCreateL (Krml.Common.Stack, es))
        )

  (* Initializing a struct value.
     TODO: We should check that the declaration type indeed corresponds to a struct type *)
  (* TODO understand why this case is needed *)
  | Some {desc = InitList l; _} ->
      let translate_field_expr (e : expr) = match e.desc with
        | DesignatedInit { designators; init }  ->
            begin match designators with
            | [FieldDesignator name] ->
                let e = translate_expr env init in
                (Some name, e)
            | [_] -> failwith "expected a field designator"
            | _ -> failwith "assigning to several fields during struct initialization is not supported"
            end
      | _ -> failwith "a designated initializer was expected when initializing a struct"
      in
      add_var env (vname, typ), Helpers.fresh_binder vname typ, Krml.Ast.with_type typ (EFlat (List.map translate_field_expr l))


  | Some {desc = Call {callee; args}; _}
  (* There commonly is a cast around calloc to the type of the variable. We omit it when translating it to Rust,
     as the allocation will be typed *)
  | Some {desc = Cast {operand = {desc = Call {callee; args}; _}; _}; _} when is_calloc callee ->
      begin match args with
      | [len; {desc = UnaryExpr {kind = SizeOf; argument}; _}] ->
          let len = adjust (translate_expr env len) (TInt SizeT) in
          (* Sanity check: calloc is of the right type *)
          let ty = Helpers.assert_tbuf typ in
          assert (extract_sizeof_ty argument = ty);
          let w = Helpers.assert_tint ty in
          add_var env (vname, typ), Helpers.fresh_binder vname typ,
            Krml.Ast.with_type typ (EBufCreate (Krml.Common.Heap, Helpers.zero w, len))
      | _ -> failwith "calloc is expected to have two arguments"
      end

  | Some {desc = DeclRef { name; _ }; _} ->
      let var = get_id_name name |> find_var env in
      let e = match typ with
      (* If we have a statement of the shape `let x = y` where y is a pointer,
         this likely corresponds to taking a slice of y, starting at index 0.
         We need to explicitly insert the EBufSub node to create a split tree *)
      | TBuf _ | TArray _ -> with_type typ (EBufSub (var, Helpers.zero_usize))
      | _ -> var
      in
      add_var env (vname, typ), Helpers.fresh_binder vname typ, e

  | Some e -> add_var env (vname, typ), Helpers.fresh_binder vname typ, translate_expr env e

(* Translation of a variable declaration, followed by a memset of [args] *)
let translate_vardecl_with_memset (env: env) (vdecl: var_decl_desc) (args: expr list)
  : env * binder * Krml.Ast.expr =
  (* TODO: We should not hard-fail when this does not correspond to an array decl initialized
     by the following memset.
     Instead, we should just translate the vardecl, and let translate_stmt translate the
     second statement *)
  let vname = vdecl.var_name in
  let typ, size = match vdecl.var_type.desc with
    | VariableArray { element; size } ->
        TBuf (translate_typ element, false), translate_expr env size
    | ConstantArray {element; size_as_expr; _} ->
        let size = match size_as_expr with
        | None -> failwith "Length of constant array is not an expr"
        | Some size -> adjust (translate_expr env size) (TInt SizeT)
        in
        TBuf (translate_typ element, false), size
    | _ ->
        failwith "The variable being memset is not a constantArray or variableArray"
  in
  match args with
  | dst :: v :: len :: _ ->
      (* Check that the destination is the variable declared just before *)
      begin match dst.desc with
      | DeclRef {name; _} when get_id_name name = vname -> ()
      | _ -> failwith "not calling memset on the variable that was just declared"
      end;
      (* Checking that we are initializing the entire array *)
      let len = match len.desc with
      | BinaryOperator {lhs; kind = Mul;
                        rhs = { desc = UnaryExpr {kind = SizeOf; argument}; _}}
          when extract_sizeof_ty argument = Helpers.assert_tbuf typ ->
          lhs
      | _ -> failwith "memset length is not of the shape `N * sizeof(ty)`"
      in
      let v = translate_expr env v in
      let len = adjust (translate_expr env len) (TInt SizeT) in
      (* Types might have been inferred differently, we only compare the expressions *)
      if len.node = size.node then
        add_var env (vname, typ),
        Helpers.fresh_binder vname typ,
        Krml.Ast.with_type typ (EBufCreate (Krml.Common.Stack, v, len))
      else
        fatal_error "length of memset does not match declared length of array"

  | _ -> failwith "memset does not have the right number of arguments"

  (* Translation of a variable declaration through malloc, followed by an initialization through [s] *)

let translate_vardecl_malloc (env: env) (vdecl: var_decl_desc) (s: stmt_desc)
  : env * binder * Krml.Ast.expr =
  let vname = vdecl.var_name in
  let typ = match vdecl.var_type.desc with
  | Pointer ty -> TBuf (translate_typ ty, false)
  | _ -> failwith "The variable being malloc'ed is not a pointer"
  in

  let args = match vdecl.var_init with
  | Some {desc = Call {args; _}; _}
  (* There commonly is a cast around malloc to the type of the variable. We omit it when translating it to Rust,
     as the allocation will be typed *)
  | Some {desc = Cast {operand = {desc = Call {args; _}; _}; _}; _} -> args
  | _ -> failwith "impossible: calling translate_vardecl_malloc on a non-malloc initializer"
  in

  begin match args with
  | [{desc = UnaryExpr {kind = SizeOf; argument}; _}] ->
      (* Sanity-check: The sizeof argument correponds to the type of the pointer being malloc'ed *)
      assert (extract_sizeof_ty argument = Helpers.assert_tbuf typ)
  | [_] -> failwith "argument of malloc if not of the shape `sizeof(type)`"
  | _ -> failwith "Too many arguments for malloc"
  end;

  (* Check if expression [e] corresponds to accessing the 0-th element of array [var_name] *)
  let is_zero_access (e: expr) var_name = match e.desc with
  | ArraySubscript {base = {desc = DeclRef {name; _}; _}; index = {desc = IntegerLiteral (Int 0); _}} ->
      get_id_name name = var_name
  | _ -> false
  in

  let init_val = match s with
  (* We previously checked that this had shape 'if ptr != NULL { ... }`. *)
  | If {then_branch; _} -> begin match then_branch.desc with
      | Compound [{desc = Expr {desc = BinaryOperator {lhs; kind = Assign; rhs}; _}; _}] when is_zero_access lhs vname ->
          translate_expr env rhs
      | _ -> failwith "ill-formed malloc initializer"
      end
  | _ -> failwith "ill-formed malloc initializer"
  in

  add_var env (vname, typ), Helpers.fresh_binder vname typ, Krml.Ast.with_type typ (EBufCreate (Krml.Common.Heap, init_val, Helpers.oneu32))



(* Same as translate_expr: we try to avoid relying on Clang-provided type information as much as
   possible *)
let rec translate_stmt (env: env) (s: Clang.Ast.stmt_desc) : Krml.Ast.expr =
  match s with
  (* This is a null statement, not a null pointer. It corresponds to a no-op *)
  | Null -> Helpers.eunit

  | Compound l -> begin match l with
    | [] -> Helpers.eunit

    | [{desc = Decl [{desc = Var vdecl; _ }]; _}] ->
        let _, b, e = translate_vardecl env vdecl in
        with_type TUnit (ELet (b, e, Helpers.eunit))

    | [stmt] -> translate_stmt env stmt.desc

    | hd :: tl -> match hd.desc, (List.hd tl).desc with
      (* Special case when we have a variable declaration followed by a
         memset: this likely corresponds to an array initialization *)
      | Decl [{desc = Var vdecl; _}],
        Expr {desc = Call {callee; args}; _} when is_memset callee ->
          let env', b, e = translate_vardecl_with_memset env vdecl args in
          let e2 = translate_stmt env' (Compound (List.tl tl)) in
          with_type e2.typ (ELet (b, e, e2))

      (* Special case when we have a malloc followed by an initializer
         for the corresponding pointer: we rewrite this into a heap array
         initialization *)
      | Decl [{desc = Var vdecl; _}], stmt when is_malloc_vdecl vdecl && is_malloc_initializer vdecl stmt ->
          let env', b, e = translate_vardecl_malloc env vdecl stmt in
          let e2 = translate_stmt env' (Compound (List.tl tl)) in
          with_type e2.typ (ELet (b, e, e2))

      | Decl [{desc = Var vdecl; _ }], _ ->
          let env', b, e = translate_vardecl env vdecl in
          let e2 = translate_stmt env' (Compound tl) in
          with_type e2.typ (ELet (b, e, e2))

      | Decl [_], _ -> failwith "This decl is not a var declaration"
      | Decl _, _ -> failwith "multiple decls"
      | stmt, _ ->
          let e2 = translate_stmt (add_var env ("_", TUnit)) (Compound tl) in
          with_type e2.typ (ELet (
            Helpers.sequence_binding (),
            translate_stmt env stmt,
            e2))
   end

  | For {init; condition_variable; cond; inc; body} ->
      assert (condition_variable = None);
      begin match init, cond, inc with
      | Some init, Some cond, Some inc ->
          begin match init.desc with
          | Decl [{desc = Var vdecl; _}] ->
            let env, b, init = translate_vardecl env vdecl in
            let b = Helpers.mark_mut b in
            (* Cannot use type_of_expr cond here since C uses `int` but we want bool *)
            let cond = adjust (translate_expr env cond) TBool in
            let inc = translate_stmt env inc.desc in
            let body = translate_stmt env body.desc in
            with_type TUnit (EFor (b, init, cond, inc, body))
          | _ -> failwith "loop variable must be declared in for loop initializer"
          end
      | _ -> failwith "translation of for loops requires initialize, condition, and increment"
      end

  | ForRange _ -> failwith "translate_stmt: for range"

  (* There is no null pointer in Rust. We remove branching based on null-pointer
     comparisons *)
  | If {cond = {desc = BinaryOperator {lhs; kind = EQ; rhs}; _}; else_branch; _} when has_pointer_type lhs && is_null rhs ->
      begin match else_branch with
      | None -> Helpers.eunit
      | Some s -> translate_stmt env s.desc
      end

  | If {cond = {desc = BinaryOperator {lhs; kind = NE; rhs}; _}; then_branch; _} when has_pointer_type lhs && is_null rhs ->
      translate_stmt env then_branch.desc

  | If {init; condition_variable; cond; then_branch; else_branch} ->
      (* These two fields should be specific to C++ *)
      assert (init = None);
      assert (condition_variable = None);
      let cond = adjust (translate_expr env cond) TBool in
      let then_b = translate_stmt env then_branch.desc in
      let else_b = match else_branch with
        | None -> Helpers.eunit
        | Some el -> translate_stmt env el.desc
      in
      with_type then_b.typ (EIfThenElse (cond, then_b, else_b))

  | Switch {init; condition_variable; cond; body} ->
      (* C++ constructs *)
      assert (init = None);
      assert (condition_variable = None);

      let cond = translate_expr env cond in
      (* TODO most likely adjust *)
      let branches = translate_branches env body.desc in
      with_type (thd3 (List.hd branches)).typ (EMatch (Unchecked, cond, branches))

  | Case _ -> failwith "case not encapsulated in a switch"
  | Default _ -> failwith "default not encapsulated in a switch"

  | While { condition_variable = _; cond; body } ->
      let cond = adjust (translate_expr env cond) TBool in
      if is_trivial_false cond then
        Helpers.eunit
      else
        let body = translate_stmt env body.desc in
        with_type TUnit (EWhile (cond, body))

  | Do { body; cond } ->
    (* The do statements first executes the body before behaving as a while loop.
       We thus translate it as a sequence of the body and the corresponding while loop *)
    let body = translate_stmt env body.desc in
    let cond = adjust (translate_expr env cond) TBool in
    if is_trivial_false cond then body else
      with_type TUnit (ESequence [
        body;
        Krml.Ast.with_type TUnit (EWhile (cond, body))
      ])

  | Label _ -> failwith "translate_stmt: label"
  | Goto _ -> failwith "translate_stmt: goto"
  | IndirectGoto _ -> failwith "translate_stmt: indirect goto"

  | Continue -> failwith "translate_stmt: continue"
  | Break -> failwith "translate_stmt: break"
  | Asm _ -> failwith "translate_stmt: asm"

  | Return eo -> with_type TAny (match eo with
        | None -> EReturn Helpers.eunit
        | Some e ->
            (* Take expected return type; TODO adjust *)
            EReturn (translate_expr env e))

  | Decl _ -> failwith "translate_stmt: decl"
  | Expr e -> translate_expr env e

  | Try _ -> failwith "translate_stmt: try"
  | AttributedStmt _ -> failwith "translate_stmt: AttributedStmt"
  | UnknownStmt _ -> failwith "translate_stmt: UnknownStmt"

(* Translate case and default statements inside a switch to a list of branches for
   structured pattern-matching.
   The original C branches must consist of a list of `case` statements, terminated by
   a `default` statement *)
and translate_branches (env: env) (s: stmt_desc) : Krml.Ast.branches = match s with
  | Compound [{desc = Default body; _}] ->
      let body = translate_stmt env body.desc in
      (* The last case is a fallback, the pattern corresponds to a wildcard *)
      [([], Krml.Ast.with_type TAny PWild, body)]
  | Compound ({desc = Case {lhs; rhs; body}; _} :: tl) ->
      (* Unsupported GCC extension *)
      assert (rhs = None);
      let pat = translate_expr env lhs in
      let body = translate_stmt env body.desc in
      (* We only support pattern-matching on constants here.
         This allows to translate switches corresponding to pattern
         matching on a tagged union *)
      begin match pat.node with
      | EConstant n -> ([], Krml.Ast.with_type pat.typ (PConstant n), body)
      | _ -> failwith "Only constant patterns supported"
      end :: translate_branches env (Compound tl)
  | _ -> failwith "Ill-formed switch branches: Expected a case or a default"


let translate_param (p: parameter) : binder =
  let p = p.desc in
  let typ = translate_typ p.qual_type in
  (* Not handling default expressions for function parameters *)
  assert (p.default = None);
  Helpers.fresh_binder p.name typ

let translate_params (fdecl: function_decl) =
  match fdecl.function_type.parameters with
      | None -> []
      | Some params ->
          (* Not handling variadic parameters *)
          assert (not (params.variadic));
          List.map translate_param params.non_variadic

let translate_fundecl (fdecl: function_decl) =
  let name = get_id_name fdecl.name in
  let ret_type = translate_typ fdecl.function_type.result in
  let binders = translate_params fdecl in
  (* To adopt a DeBruijn representation, the list must be reversed to
   have the last binder as the first element of the environment *)
  let env = add_binders empty_env binders in
  let env = { env with ret_t = ret_type } in
  match fdecl.body with
  (* If the function body is empty, this is likely a prototype. We
     do not extract it *)
  | None -> None
  | Some s ->
    let body = translate_stmt env s.desc in
    let flags = if fdecl.inline_specified then [Krml.Common.Inline] else [] in
    let lid = fst (FileMap.find name !name_map), name in
    let decl = Krml.Ast.(DFunction (None, flags, 0, 0, ret_type, lid, binders, body)) in
    (* Krml.KPrint.bprintf "Resulting decl %a\n" Krml.PrintAst.pdecl decl; *)
    Some decl

(* Translate a field declaration inside a struct type declaration *)
let translate_field (decl: decl) =
  match decl.desc with
  | Field {name; qual_type; bitwidth; init; attributes} ->
      (* Sanity-checks for unsupported features *)
      assert (bitwidth = None);
      assert (init = None);
      assert (attributes = []);
      (* TODO: What is the boolean used for in krml fields_t_opt type? *)
      (Some name, (translate_typ qual_type, false))
  | _ -> failwith "Struct declarations should only contain fields"

let name_of_decl (decl: decl): string =
  match decl.desc with
  | Function { name; _ } -> get_id_name name
  | EnumDecl { name; _ } -> name
  | RecordDecl { name; _ } -> name
  | TypedefDecl { name; _ } -> name
  | Field { name; _ } -> name
  | Var desc -> desc.var_name
  | _ -> "unknown"

(* When writing `typedef struct S { ... } T;` in C, we actually see two declarations:
  - first, one for the `struct S { ... }` part (case RecordDecl)
  - second, one for the the `typedef struct S T;` part (case TypedefDecl).
  When visiting the first case, we generate the krml `type_decl` body, and store it in `struct_map`.
  When visiting the second case, we retrieve the `type_decl` and construct a `DType` node.

  This function performs the second task. *)
let elaborate_typ (typ: qual_type) = match typ.desc with
  | Elaborated { keyword = Struct; named_type = { desc = Record {name; _}; _}; _ } ->
      let name = get_id_name name in
      StructMap.find name !struct_map
  (* TODO: Similar workflow as structs *)
  | Elaborated { keyword = Enum; _} -> failwith "elaborated enums not supported"
  | Elaborated _ -> failwith "elaborated types that are not enums or structs are not supported"
  | _ -> failwith "The underlying type of a typedef is not an elaborated type"

(* Returning an option is only a hack to make progress.
   TODO: Proper handling of  decls *)
let translate_decl (decl: decl) =
  let exception Unsupported in
  (* Format.printf "visiting decl %s\n%a\n@." (name_of_decl decl) Clang.Decl.pp decl; *)
  try
    match decl.desc with
    | Function fdecl ->
      (* TODO: How to handle libc? *)
      (* TODO: Support multiple files *)
      translate_fundecl fdecl
    | Var vdecl ->
        if vdecl.var_init = None then
          (* Prototype, e.g. extern int x; *)
          None
        else
          let _, _, e = translate_vardecl empty_env vdecl in
          let lid = fst (FileMap.find vdecl.var_name !name_map), vdecl.var_name in
          let typ = translate_typ vdecl.var_type in
          (* TODO: Flags *)
          let flags = [] in
          (* TODO: What is the int for? *)
          Some (DGlobal (flags, lid, 0, typ, e))

    | RecordDecl {name; fields; attributes; _} ->
        let is_box = Attributes.has_box_attr attributes in
        let fields = List.map translate_field fields in
        struct_map := StructMap.update name (function
          | None -> Some (Flat fields, is_box)
          | Some _ -> Printf.eprintf "A type declaration already exists for struct %s\n" name; failwith "redefining a structure type")
        !struct_map;
        None

    | TypedefDecl {name; underlying_type} ->
        let lid = fst (FileMap.find name !name_map), name in
        begin match underlying_type.desc with
        | BuiltinType t ->
            let ty = translate_builtin_typ t in
            Some (DType (lid, [], 0, 0, Abbrev ty))
        | Typedef {name; _} ->
            let name = get_id_name name in
            let ty = translate_typ_name name in
            Some (DType (lid, [], 0, 0, Abbrev ty))
        | _ ->
            let ty, is_box = elaborate_typ underlying_type in
            if is_box then boxed_types := LidSet.add lid !boxed_types;
            Some (DType (lid, [], 0, 0, ty))
        end

    | _ ->
        raise Unsupported
  with e ->
    Format.printf "Declaration %a not supported@." Clang.Decl.pp decl;
    raise e

(* Computes the argument and return types of a function potentially marked as [[scylla_opaque]],
   taking into account attributes to adjust const/non-const pointers. *)
let compute_external_type (fdecl: function_decl): binder list * typ =
  let ret_type = translate_typ fdecl.function_type.result in
  let binders = translate_params fdecl in
  let args_mut = Attributes.retrieve_mutability fdecl.attributes in
  let binders = match args_mut with
    | None ->
        (* No mutability was specified, but we are in an opaque definition:
           All arguments must be considered as read-only *)
        List.map (fun arg -> match arg.typ with
          | TBuf (t, _) -> {arg with typ = TBuf (t, true)}
          | _ -> arg
        ) binders
    | Some muts -> List.map2 (fun mut arg -> match arg.typ, mut with
        (* In Ast, the flag set to true represents a constant, immutable array.
           The mutability flag is the converse, so we need to take the negation *)
        | TBuf (t, _), b -> {arg with typ = TBuf (t, not b)}
        (* For all other types, we do not modify the mutability *)
        | _ -> arg
        ) muts binders
  in
  binders, ret_type

(* We are traversing an external module. We filter it to only preserve
   declarations annotated with the [opaque_attr] attribute, which
   we translate as external.
   TODO: We should probably try to translate all declarations as external,
   and use bundling to remove unneeded ones *)
let translate_external_decl (decl: decl) = match decl.desc with
  | Function fdecl ->
      (* let name = get_id_name fdecl.name in *)
      if Attributes.has_opaque_attr fdecl.attributes then (
        let name = get_id_name fdecl.name in
        let binders, ret_type = compute_external_type fdecl in
        let fn_type = Helpers.fold_arrow (List.map (fun x -> x.typ) binders) ret_type in
        (* TODO: translate_lid *)
        let lid = fst (FileMap.find name !name_map), name in

        let decl = Krml.Ast.(DExternal (None, [], 0, 0, lid, fn_type, List.map (fun x -> Krml.Ast.(x.node.name)) binders)) in
        Some decl
      ) else None
  | _ -> None

let translate_file wanted_c_file file =
  let (name, decls) = file in
  (* We extract both the .c and the .h together. However, we will not
     extract function prototypes without a body, avoiding duplicated definitions *)
  let basename = Filename.remove_extension (Filename.basename wanted_c_file) in
  (* TODO: Multifile support *)
  if name = basename then
    Some (name, List.filter_map translate_decl decls)
  else
  (* translate_external_decl will only translate declarations annotated with the
     `scylla_opaque` attribute.
     Furthermore, a file that does not contain any definitions will be filtered
     out in krml during the Rust translation.
     Hence, we can apply translate_external_decl on any file in the tree *)
    Some (name, List.filter_map translate_external_decl decls)

(* add_to_list is only available starting from OCaml 5.1 *)
let add_to_list x data m =
  let add = function None -> Some [data] | Some l -> Some (data :: l) in
  FileMap.update x add m

(* This attempts to read the attributes since typedef attributes are not exposed in the
   ClangMl high-level AST. This is painful. *)
let decl_is_opaque (decl: decl) =
  let is_opaque = ref false in
  begin match decl.decoration with
  | Cursor cx ->
      Clang__.Clang__utils.iter_decl_attributes (fun cx ->
        match Clang.ext_attr_get_kind cx with
        | Annotate when Clang.ext_attrs_get_annotation cx = Attributes.opaque_attr ->
            is_opaque := true;
        | _ ->
            ()
      ) cx
  | Custom _ ->
      failwith "no cursor"
  end;
  !is_opaque

let add_lident_mapping (decl: decl) (filename: string) =
  Format.printf "add_lident_mapping %s\n%a\n@." (name_of_decl decl) Clang.Decl.pp decl;
  let path = [ Filename.(remove_extension (basename filename)) ] in
  match decl.desc with
  | Function fdecl ->
      let name = get_id_name fdecl.name in
      let t =
        try
          let binders, ret_type = compute_external_type fdecl in
          Helpers.fold_arrow (List.map (fun x -> x.typ) binders) ret_type
        with _ ->
          Format.printf "FIXME: could not represent the type of function declaration %s\n@." name;
          TAny
      in
      let path = path, t in
      (* Krml.KPrint.bprintf "%s --> %s\n" name (String.concat "::" path); *)
      name_map := FileMap.update name
        (function | None -> Some path | Some _ ->
          Format.printf "Declaration %s appears twice in translation unit, found again in %s\n@." name filename;
          Some path)
        !name_map

  | Var vdecl ->
      let t =
        try
          translate_typ vdecl.var_type
        with _ ->
          Format.printf "FIXME: could not represent the type of global declaration %s\n@." vdecl.var_name;
          TAny
      in
      let path = path, t in
      name_map := FileMap.update vdecl.var_name
        (function | None -> Some path | Some _ ->
          Format.printf "Variable declaration %s appears twice in translation unit\n@." vdecl.var_name;
          Some path)
        !name_map

  | RecordDecl rdecl ->
      (* FIXME dummy *)
      let path = path, TAny in
      name_map := FileMap.update rdecl.name
        (function | None -> Some path | Some _ ->
          Format.printf "Record Type declaration %s appears twice in translation unit\n@." rdecl.name;
          Some path)
        !name_map

  | TypedefDecl tdecl ->
      (* FIXME dummy *)
      name_map := FileMap.update tdecl.name
        (function | None -> Some (path, TAny) | Some _ ->
          Format.printf "Typedef declaration %s appears twice in translation unit\n@." tdecl.name;
          Some (path, TAny))
        !name_map;
      (* To normalize correctly, we might need to retrieve types beyond the file currently
         being translated. We thus construct this map here rather than during type declaration
         translation.

         We substitute type abbreviations on the fly, via normalize_type. This allows us to match
         synthesized type against expected type accurately during the translation, which in turn
         allows us to insert casts in suitable places. *)
      let lid = path, tdecl.name in
      begin match tdecl.underlying_type.desc with
      | BuiltinType _ | Typedef _ | Pointer _ as t ->
          if not (decl_is_opaque decl) then begin
            (* Krml.KPrint.bprintf "adding %a in the abbreviation map\n" Krml.PrintAst.Ops.plid lid; *)
            abbrev_map := LidMap.update lid (function
              | None -> Some t
              | Some t' when true || t = t' ->
                  (* Invalid_argument("compare: abstract value")
                     Values of type `type_desc` are probably not intended to be compared directly. I
                     cannot find in the Clang API where the would be an equality predicate for
                     `type_desc`s -- FIXME.

                     With the `true ||` above, we override any previous typedef (but will the
                     frontend allow this to happen?). *)
                  Some t
              | _ -> Printf.eprintf "A type alias already exists for type %s\n" tdecl.name; failwith "redefining a type alias")
            !abbrev_map
          end

      | Elaborated { keyword = Struct; named_type = { desc = Record {name; _}; _}; _ } ->
          (* When writing `typedef Struct S { ... } T;` in C, we see a RecordDecl (`struct S { ... };`)
             *and* a TypedefDecl (`typedef struct S T;`). This is the latter. We record a mapping
             `struct S` ~~> `T` in our map, so that occurrence of the type `struct S` in the Clang
             AST become nominal types `T` in the krml Ast. *)
          Krml.KPrint.bprintf "struct %s maps to %a\n" (get_id_name name) Krml.PrintAst.Ops.plid lid;
          elaborated_map := ElaboratedMap.add (name, `Struct) lid !elaborated_map

      (* (1* TODO: Similar workflow as structs *1) *)
      (* | Elaborated { keyword = Enum; _} -> failwith "elaborated enums not supported" *)
      (* | Elaborated _ -> failwith "elaborated types that are not enums or structs are not supported" *)

      | Elaborated _ -> Format.printf "TypedefDecl: skipping a Elaborated\n@."

      | LValueReference _ -> Format.printf "TypedefDecl: skipping a LValueReference\n@."
      | RValueReference _ -> Format.printf "TypedefDecl: skipping a RValueReference\n@."
      | ConstantArray _ -> Format.printf "TypedefDecl: skipping a ConstantArray\n@."
      | Vector _ -> Format.printf "TypedefDecl: skipping a Vector\n@."
      | IncompleteArray _ -> Format.printf "TypedefDecl: skipping a IncompleteArray\n@."
      | VariableArray _ -> Format.printf "TypedefDecl: skipping a VariableArray\n@."
      | Enum _ -> Format.printf "TypedefDecl: skipping a Enum\n@."
      | FunctionType _ -> Format.printf "TypedefDecl: skipping a FunctionType\n@."
      | Record _ -> Format.printf "TypedefDecl: skipping a Record\n@."
      | Complex _ -> Format.printf "TypedefDecl: skipping a Complex\n@."
      | Attributed _ -> Format.printf "TypedefDecl: skipping a Attributed\n@."
      | ParenType _ -> Format.printf "TypedefDecl: skipping a ParenType\n@."
      | TemplateTypeParm _ -> Format.printf "TypedefDecl: skipping a TemplateTypeParm\n@."
      | SubstTemplateTypeParm _ -> Format.printf "TypedefDecl: skipping a SubstTemplateTypeParm\n@."
      | TemplateSpecialization _ -> Format.printf "TypedefDecl: skipping a TemplateSpecialization\n@."
      | Auto -> Format.printf "TypedefDecl: skipping a Auto\n@."
      | PackExpansion _ -> Format.printf "TypedefDecl: skipping a PackExpansion\n@."
      | MemberPointer _ -> Format.printf "TypedefDecl: skipping a MemberPointer\n@."
      | Decltype _ -> Format.printf "TypedefDecl: skipping a Decltype\n@."
      | InjectedClassName _ -> Format.printf "TypedefDecl: skipping a InjectedClassName\n@."
      | Using _ -> Format.printf "TypedefDecl: skipping a Using\n@."
      | Atomic _ -> Format.printf "TypedefDecl: skipping a Atomic\n@."
      | TypeOf _ -> Format.printf "TypedefDecl: skipping a TypeOf\n@."
      | UnexposedType _ -> Format.printf "TypedefDecl: skipping a UnexposedType\n@."
      | InvalidType -> Format.printf "TypedefDecl: skipping a InvalidType\n@."
      end

  (* TODO: Do we need to support this mapping for more decls *)
  | _ ->
      (* Format.printf "add_lident_mapping: ignoring %a\n" Clang.Decl.pp decl *)
      ()

let split_into_files (lib_dirs: string list) (ast: translation_unit) =
  let add_decl acc decl =
    let loc = Clang.Ast.location_of_node decl |> Clang.Ast.concrete_of_source_location File in
    (* If this belongs to the C library, do not extract it *)
    (* TODO: This could be done more efficiently by filtering after splitting into files,
       to avoid repeated traversals of lib_dirs *)
    if List.exists (fun x -> String.starts_with ~prefix:x loc.filename) lib_dirs then (
      acc
    ) else (
      add_lident_mapping decl loc.filename;
      (* We merge .h and .c files here. Duplicated declarations (e.g., prototypes in the
         .h file, and definitions in the .c file) will be filtered during the translation
         of declaration *)
      let filename = loc.filename |> Filename.basename |> Filename.remove_extension in
      add_to_list filename decl acc
    )
  in
  let decl_map = List.fold_left add_decl FileMap.empty ast.desc.items in
  FileMap.bindings decl_map |> List.map (fun (k, l) -> (k, List.rev l))

(* On MacOS, C compilation often relies on a SDK, where parts of the stdlib
    is located *)
let get_sdkroot () =
  (* TODO: Is there something similar on Linux, or is the stdlib included in
     the Clang default include directories? *)
  try
    Unix.getenv "SDKROOT" |> String.split_on_char ':'
  with
    | Not_found -> []

let translate_compil_unit (ast: translation_unit) (wanted_c_file: string) =
  let lib_dirs = get_sdkroot () @ Clang.default_include_directories () in
  let files = split_into_files lib_dirs ast in
  let files = List.filter_map (translate_file wanted_c_file) files in
  !boxed_types, files

let read_file (filename: string) : translation_unit =
  Format.printf "Clang version is %s\n@." (Clang.get_clang_version());
  let command_line_args = !Scylla__Options.ccopts @
    List.map Clang.Command_line.include_directory (Clang.default_include_directories ()) in
  Format.printf "Arguments passed to clang are: %s\n@." (String.concat " " command_line_args);
  parse_file ~command_line_args filename
