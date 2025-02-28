(* Copyright (c) INRIA and Microsoft Corporation. All rights reserved. *)
(* Licensed under the Apache 2.0 and MIT Licenses. *)

open Krml.Ast
open Clang.Ast
module K = Krml.Constant
module Helpers = Krml.Helpers

module FileMap = Map.Make(String)

(* A map from function names to the string list used in their fully qualified
   name. It is filled at the beginning of the translation, when exploring the
   translation unit *)
let name_map = ref FileMap.empty

type env = {
  (* Variables in the context *)
  vars: string list
}

let empty_env = {vars = []}

let add_var env var = {vars = var :: env.vars }

(* TODO: Handle fully qualified names/namespaces/different files. *)
let find_var env var =
  try EBound (Krml.KList.index (fun x -> x = var) env.vars) with
  (* This variable is not a local var *)
  (* TODO: More robust check, likely need env for top-level decls *)
  | Not_found ->
      try
        let path = FileMap.find var !name_map in
        EQualified (path, var)
      with
      | Not_found ->
          Printf.eprintf "Could not find variable %s\n" var;
          raise Not_found

let get_id_name (dname: declaration_name) = match dname with
  | IdentifierName s -> s
  | ConstructorName _ -> failwith "constructor"
  | DestructorName _ -> failwith "destructor"
  | ConversionFunctionName _ -> failwith "conversion function"
  | DeductionGuideName _ -> failwith "deduction guide"
  | OperatorName _ -> failwith "operator name"
  | LiteralOperatorName _ -> failwith "literal operator name"
  | UsingDirectiveName -> failwith "using directive"

let is_assign_op (kind: Clang.Ast.binary_operator_kind) = match kind with
  | Assign | AddAssign | MulAssign | DivAssign | RemAssign
  | SubAssign | ShlAssign | ShrAssign | AndAssign
  | XorAssign | OrAssign -> true
  | _ -> false

let assign_to_bop (kind: Clang.Ast.binary_operator_kind) : Krml.Ast.expr =
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
  (* TODO: Infer width and type from types of operands *)
  Krml.Ast.with_type Helpers.uint32 (EOp (op, UInt32))

let translate_unop (kind: Clang.Ast.unary_operator_kind) : K.op = match kind with
  | PostInc -> PostIncr
  | PostDec -> PostDecr
  | PreInc -> PreIncr
  | PreDec -> PreDecr
  | AddrOf -> failwith "translate_unop: addrof"
  | Deref -> failwith "translate_unop: deref"
  | Plus -> failwith "translate_unop: plus"
  | Minus -> failwith "translate_unop: minus"
  | Not -> failwith "translate_unop: not"
  | LNot -> failwith "translate_unop: lnot"
  | Real -> failwith "translate_unop: real"
  | Imag -> failwith "translate_unop: imag"
  | Extension -> failwith "translate_unop: extension"
  | Coawait -> failwith "translate_unop: coawait"
  | InvalidUnaryOperator -> failwith "translate_unop: invalid unop"

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

let translate_typ_name = function
  | "size_t" -> Helpers.usize
  | "uint8_t" -> Helpers.uint8
  | "uint16_t" -> Helpers.uint16
  | "uint32_t" -> Helpers.uint32
  | "uint64_t" -> Helpers.uint64
  | s ->
      Printf.eprintf "type name %s is unsupported\n" s;
      failwith "unsupported name"


let translate_builtin_typ (t: Clang.Ast.builtin_type) = match [@warnerror "-11"] t with
  | Void -> TUnit
  | UInt -> TInt UInt32 (* TODO: How to retrieve exact width? *)
  | UShort -> failwith "translate_builtin_typ: ushort"
  | ULong ->
      begin match DataModel.size_long with
      | 4 -> TInt UInt32
      | 8 -> TInt UInt64
      | _ -> failwith "impossible"
      end
  | ULongLong -> TInt UInt64
  | UInt128 -> failwith "translate_builtin_typ: uint128"

  | Int -> TInt Int32 (* TODO: Retrieve exact width *)
  (* JP: this depends on the *data model* -- int is always 4 bytes, long long is always 8
     bytes, and the size of long depends on windows vs the rest of the world (we assume no PDP-11)
     *)

  | Short
  | Long
  | LongLong
  | Int128 -> failwith "translate_builtin_typ: signed int"

  | Pointer -> failwith "translate_builtin_typ: pointer"

  | Invalid -> failwith "translate_builtin_typ: Invalid"
  | Unexposed -> failwith "translate_builtin_typ: Unexposed"
  | Bool -> failwith "translate_builtin_typ: Bool"
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

  | _ ->
      Format.printf "Trying to translate type %a" Clang.Type.pp typ;
      failwith "translate_typ: unsupported type"

(* Takes a Clangml expression [e], and retrieves the corresponding karamel Ast type *)
let typ_of_expr (e: expr) : typ = Clang.Type.of_node e |> translate_typ

(* Check whether a given Clang expression is a memcpy callee *)
let is_memcpy (e: expr) = match e.desc with
  | DeclRef { name; _ } ->
      let name = get_id_name name in
      name = "__builtin___memcpy_chk" || name = "memcpy"
  | _ -> false

(* Check whether a given Clang expression is a memset callee *)
let is_memset (e: expr) = match e.desc with
  | DeclRef { name; _ } ->
      let name = get_id_name name in
      name = "__builtin___memset_chk" || name = "memset"
  | _ -> false

(* Simple heuristics to detect whether a loop condition is always false, in this case we can omit the loop.
   TODO: Should probably check for absence of side-effects in condition evaluation *)
let is_trivial_false (e: Krml.Ast.expr) = match e.node with
  (* e != e is always false *)
  | EApp ({node = EOp (Neq, _); _ }, [e1; e2]) when e1 = e2 -> true
  | _ -> false

let extract_sizeof_ty = function
  | ArgumentExpr _ -> failwith "ArgumentExpr not supported"
  | ArgumentType ty -> translate_typ ty

(* Translate expression [e], with expected type [t] *)
let rec translate_expr' (env: env) (t: typ) (e: expr) : expr' = match e.desc with
  | IntegerLiteral (Int n) -> EConstant (Helpers.assert_tint t, string_of_int n)

  | FloatingLiteral _ -> failwith "translate_expr: floating literal"
  | StringLiteral _ -> failwith "translate_expr: string literal"
  | CharacterLiteral _ -> failwith "translate_expr character literal"
  | ImaginaryLiteral _ -> failwith "translate_expr: imaginary literal"
  | BoolLiteral _ -> failwith "translate_expr: bool literal"
  | NullPtrLiteral -> failwith "translate_expr: null ptr literal"

  | UnaryOperator {kind = PostInc; operand = { desc = DeclRef {name; _}; _ }} ->
      (* This is a special case for loop increments. The current Karamel
         extraction pipeline only supports a specific case of loops *)
      let var_name = get_id_name name in
      (* TODO: Retrieve correct width *)
      let w = K.UInt32 in
      let t = TInt w in
      let v = find_var env var_name in
      (* We rewrite `name++` into `name := name + 1` *)
      EAssign (
        Krml.Ast.with_type t v,
        Krml.Ast.with_type t (EApp (Helpers.mk_op K.Add w, [Krml.Ast.with_type t v; Helpers.one w]))
      )

  | UnaryOperator {kind = Not; operand } ->
      let ty = typ_of_expr operand in
      let o = translate_expr env ty operand in
      (* TODO: Retrieve type *)
      EApp (Helpers.mk_op K.Not UInt32, [o])

  | UnaryOperator _ ->
      Format.printf "Trying to translate unary operator %a@." Clang.Expr.pp e;
      failwith "translate_expr: unary operator"

  | BinaryOperator {lhs; kind = Assign; rhs} ->
      let lhs = translate_expr env (typ_of_expr lhs) lhs in
      let rhs = translate_expr env (typ_of_expr rhs) rhs in
      begin match lhs.node with
      (* Special-case rewriting for buffer assignments *)
      | EBufRead (base, index) -> EBufWrite (base, index, rhs)
      | _ -> EAssign (lhs, rhs)
      end

  | BinaryOperator {lhs; kind; rhs} when is_assign_op kind ->
      let lhs_ty = typ_of_expr lhs in
      let lhs = translate_expr env (typ_of_expr lhs) lhs in
      let rhs = translate_expr env (typ_of_expr rhs) rhs in
      (* Rewrite the rhs into the compound expression, using the underlying operator *)
      let rhs = Krml.Ast.with_type lhs_ty (EApp (assign_to_bop kind, [lhs; rhs])) in
      begin match lhs.node with
      (* Special-case rewriting for buffer assignments *)
      | EBufRead (base, index) -> EBufWrite (base, index, rhs)
      | _ -> EAssign (lhs, rhs)
      end

  | BinaryOperator {lhs; kind; rhs} ->
      let lhs_ty = typ_of_expr lhs in
      let lhs = translate_expr env lhs_ty lhs in
      let rhs_ty = typ_of_expr rhs in
      let rhs = translate_expr env rhs_ty rhs in
      let kind = translate_binop kind in

      let combine_arith kind lhs rhs =
        let w = Helpers.assert_tint rhs_ty in
        let op_type = Helpers.type_of_op kind w in
        let op = with_type op_type (EOp (kind, w)) in
        with_type rhs_ty (EApp (op, [lhs; rhs]))
      in

      (* In case of pointer arithmetic, we need to perform a rewriting into EBufSub/Diff *)
      begin match lhs_ty, kind with
      | TBuf _, Add ->
          begin match lhs.node with
          (* Successive pointer arithmetic operations are likely due to operator precedence, e.g.,
             ptr + n - m parsed as (ptr + n) - m, when ptr + (n - m) might be intended.
             We recognize these cases, and normalize them to perform pointer arithmetic only once
          *)
          | EBufSub (lhs', rhs') ->
              (* (lhs' + rhs') + rhs --> lhs' + (rhs' + rhs) *)
              EBufSub (lhs', combine_arith Add rhs' rhs)
          | EBufDiff (lhs', rhs') ->
              (* (lhs' - rhs') + rhs --> lhs' + (rhs - rhs') *)
              EBufSub (lhs', combine_arith Sub rhs rhs')
          | _ -> EBufSub (lhs, rhs)
          end
      | TBuf _, Sub ->
          begin match lhs.node with
          | EBufSub (lhs', rhs') ->
              (* (lhs' + rhs') - rhs --> lhs' + (rhs' - rhs) *)
              EBufSub (lhs', combine_arith Sub rhs' rhs)
          | EBufDiff (lhs', rhs') ->
              (* (lhs' - rhs') - rhs --> lhs' - (rhs' + rhs) *)
              EBufDiff (lhs', combine_arith Add rhs' rhs)
          | _ -> EBufDiff (lhs, rhs)
          end
      | _ ->
        (* TODO: Likely need a "assert_tint_or_tbool" *)
        let lhs_w = Helpers.assert_tint lhs_ty in
        let op_type = Helpers.type_of_op kind (Helpers.assert_tint lhs_ty) in
        let op : Krml.Ast.expr = with_type op_type (EOp (kind, lhs_w)) in
        EApp (op, [lhs; rhs])
      end

  | DeclRef {name; _} -> get_id_name name |> find_var env

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
          let len, ty = match len.desc with
          (* We recognize the case `len = lhs * sizeof (_)` *)
            | BinaryOperator {lhs; kind = Mul; rhs = { desc = UnaryExpr {kind = SizeOf; argument}; _}} ->
                let len = translate_expr env Helpers.usize lhs in
                let ty = extract_sizeof_ty argument in
                len, ty
            | _ -> failwith "ill-formed memcpy"
          in
          let dst = translate_expr env (TBuf (ty, false)) dst in
          let src = translate_expr env (TBuf (ty, false)) src in
          EBufBlit (src, Helpers.zerou32, dst, Helpers.zerou32, len)

      | _ -> failwith "memcpy does not have the right number of arguments"
      end

  | Call {callee; args} when is_memset callee ->
      (* Format.printf "Trying to translate memset %a@." Clang.Expr.pp e; *)
      begin match args with
      | dst :: v :: len :: _ ->
          let len, ty = match len.desc with
          (* We recognize the case `len = lhs * sizeof (_)` *)
            | BinaryOperator {lhs; kind = Mul; rhs = { desc = UnaryExpr {kind = SizeOf; argument}; _}} ->
                let len = translate_expr env Helpers.usize lhs in
                let ty = extract_sizeof_ty argument in
                len, ty
            | _ -> failwith "ill-formed memcpy"
          in
          let dst = translate_expr env (TBuf (ty, false)) dst in
          let elt = translate_expr env ty v in
          EBufFill (dst, elt, len)
      | _ -> failwith "memset does not have the right number of arguments"
      end

  | Call {callee; args} ->
      (* In C, a function type is a pointer. We need to strip it to retrieve
         the standard arrow abstraction *)
      let fun_typ = Helpers.assert_tbuf (typ_of_expr callee) in
      (* Format.printf "Trying to translate function call %a@." Clang.Expr.pp callee; *)
      let callee = translate_expr env fun_typ callee in
      let args = List.map (fun x -> translate_expr env (typ_of_expr x) x) args in
      EApp (callee, args)

  | Cast {qual_type; operand; _} ->
      (* Format.printf "Cast %a@."  Clang.Expr.pp e; *)
      let typ = translate_typ qual_type in
      let e = translate_expr env (typ_of_expr operand) operand in
      ECast (e, typ)

  | ArraySubscript {base; index} ->
      let base = translate_expr env (TBuf (t, false)) base in
      let index = translate_expr env (TInt SizeT) index in
      (* Is this only called on rvalues? Otherwise, might need EBufWrite *)
      EBufRead (base, index)

  | ConditionalOperator _ -> failwith "translate_expr: conditional operator"
  | Paren _ -> failwith "translate_expr: paren"

  | _ ->
    Format.printf "Trying to translate expression %a@." Clang.Expr.pp e;
    failwith "translate_expr: unsupported expression"

and translate_expr (env: env) (t: typ) (e: expr) : Krml.Ast.expr =
  Krml.Ast.with_type t (translate_expr' env t e)

let extract_constarray_size (ty: qual_type) = match ty.desc with
  | ConstantArray {size; _} -> size, Helpers.mk_uint32 size
  | _ -> failwith "Type is not a ConstantArray"

(* Create a default value associated to a given type [typ] *)
let create_default_value typ = match typ with
  | TInt w -> Helpers.zero w
  | _ -> failwith "Creating a default value is only supported for integer types"

let translate_vardecl (env: env) (vdecl: var_decl_desc) : env * binder * Krml.Ast.expr =
  let name = vdecl.var_name in
  let typ = translate_typ vdecl.var_type in
  match vdecl.var_init with
  | None ->
        (* If there is no associated definition, we attempt to craft
           a default initialization value *)
        add_var env name, Helpers.fresh_binder name typ, create_default_value typ
  | Some {desc = InitList l; _} ->
        let size, size_e = extract_constarray_size vdecl.var_type in
        if List.length l = 1 then
          (* One element initializer, possibly repeated *)
          let e = translate_expr env (Helpers.assert_tbuf typ) (List.hd l) in
          (* TODO: Arrays are not on stack if at top-level *)
          add_var env name, Helpers.fresh_binder name typ, Krml.Ast.with_type typ (EBufCreate (Krml.Common.Stack, e, size_e))
        else (
          assert (List.length l = size);
          let ty = Helpers.assert_tbuf typ in
          let es = List.map (translate_expr env ty) l in
          add_var env name, Helpers.fresh_binder name typ, Krml.Ast.with_type typ (EBufCreateL (Krml.Common.Stack, es))
        )
  | Some e -> add_var env name, Helpers.fresh_binder name typ, translate_expr env typ e

(* Translation of a variable declaration, followed by a memset of [args] *)
let translate_vardecl_with_memset (env: env) (vdecl: var_decl_desc) (args: expr list)
  : env * binder * Krml.Ast.expr =
  (* TODO: We should not hard-fail when this does not correspond to an array decl initialized
     by the following memset.
     Instead, we should just translate the vardecl, and let translate_stmt translate the
     second statement *)
  let vname = vdecl.var_name in
  let typ, len_var, size = match vdecl.var_type.desc with
  | VariableArray { element; size = {desc = DeclRef {name; _}; _} as size } ->
      TBuf (translate_typ element, false), name, size
  | _ -> failwith "The variable being memset it not a variableArray"
  in
  match args with
  | dst :: v :: len :: _ ->
      (* Check that the destination is the variable declared just before *)
      begin match dst.desc with
      | DeclRef {name; _} when get_id_name name = vname -> ()
      | _ -> failwith "not calling memset on the variable that was just declared"
      end;
      (* Checking that we are initializing the entire array *)
      begin match len.desc with
      | BinaryOperator {lhs = { desc = DeclRef { name; _}; _} ; kind = Mul;
                        rhs = { desc = UnaryExpr {kind = SizeOf; argument}; _}}
          when name = len_var && extract_sizeof_ty argument = Helpers.assert_tbuf typ ->
          ()
      | _ -> failwith "length of memset does not match declared length of array"
      end;
      let v = translate_expr env (Helpers.assert_tbuf typ) v in
      let len = translate_expr env (typ_of_expr size) size in
      add_var env vname,
      Helpers.fresh_binder vname typ,
      Krml.Ast.with_type typ (EBufCreate (Krml.Common.Stack, v, len))

  | _ -> failwith "memset does not have the right number of arguments"


let rec translate_stmt' (env: env) (t: typ) (s: stmt_desc) : expr' = match s with
  | Null -> failwith "translate_stmt: null"

  | Compound l -> begin match l with
    | [] -> EUnit
    | [{desc = Decl [{desc = Var vdecl; _ }]; _}] ->
        let _, b, e = translate_vardecl env vdecl in
        ELet (b, e, Helpers.eunit)
    | [stmt] -> translate_stmt' env TUnit stmt.desc
    | hd :: tl -> match hd.desc, (List.hd tl).desc with
      (* Special case when we have a variable declaration followed by a
         memset: this likely corresponds to an array initialization *)
      | Decl [{desc = Var vdecl; _}],
        Expr {desc = Call {callee; args}; _} when is_memset callee ->
          let env', b, e = translate_vardecl_with_memset env vdecl args in
          ELet (b, e, translate_stmt env' t (Compound (List.tl tl)))

      | Decl [{desc = Var vdecl; _ }], _ ->
          let env', b, e = translate_vardecl env vdecl in
          ELet (b, e, translate_stmt env' t (Compound tl))
      | Decl [_], _ -> failwith "This decl is not a var declaration"
      | Decl _, _ -> failwith "multiple decls"
      | stmt, _ -> ELet (
        Helpers.sequence_binding (),
        translate_stmt env TUnit stmt,
        translate_stmt (add_var env "_") t (Compound tl))
   end

  | For {init; condition_variable; cond; inc; body} ->
      assert (condition_variable = None);
      begin match init, cond, inc with
      | Some init, Some cond, Some inc ->
          begin match init.desc with
          | Decl [{desc = Var vdecl; _}] ->
            let env, b, init = translate_vardecl env vdecl in
            let cond = translate_expr env (typ_of_expr cond) cond in
            let inc = translate_stmt env TUnit inc.desc in
            let body = translate_stmt env t body.desc in
            EFor (b, init, cond, inc, body)
          | _ -> failwith "loop variable must be declared in for loop initializer"
          end
      | _ -> failwith "translation of for loops requires initialize, condition, and increment"
      end

  | ForRange _ -> failwith "translate_stmt: for range"
  | If {init; condition_variable; cond; then_branch; else_branch} ->
      (* These two fields should be specific to C++ *)
      assert (init = None);
      assert (condition_variable = None);
      let cond = translate_expr env (typ_of_expr cond) cond in
      let then_b = translate_stmt env TUnit then_branch.desc in
      let else_b = match else_branch with
        | None -> Helpers.eunit
        | Some el -> translate_stmt env TUnit el.desc
      in
      EIfThenElse (cond, then_b, else_b)

  | Switch _ -> failwith "translate_stmt: switch"
  | Case _ -> failwith "translate_stmt: case"
  | Default _ -> failwith "translate_stmt: default"

  | While _ -> failwith "translate_stmt: while"
  | Do { body; cond } ->
    (* The do statements first executes the body before behaving as a while loop.
       We thus translate it as a sequence of the body and the corresponding while loop *)
    let body = translate_stmt env t body.desc in
    (* TODO: Likely need to translate int conditions to boolean expressions *)
    let cond_ty = typ_of_expr cond in
    let cond = translate_expr env cond_ty cond in
    let cond = match cond_ty with
      | TBool -> cond
      | TInt w ->
        (* If we have an integer expression [e], the condition is equivalent to `e != 0` *)
        Helpers.mk_neq cond (Helpers.zero w)
      | _ -> failwith "incorrect type for while condition"
    in
    if is_trivial_false cond then body.node else
      ESequence [
        body;
        Krml.Ast.with_type TUnit (EWhile (cond, body))
      ]

  | Label _ -> failwith "translate_stmt: label"
  | Goto _ -> failwith "translate_stmt: goto"
  | IndirectGoto _ -> failwith "translate_stmt: indirect goto"

  | Continue -> failwith "translate_stmt: continue"
  | Break -> failwith "translate_stmt: break"
  | Asm _ -> failwith "translate_stmt: asm"

  (* TODO: Should this be an EReturn ? If so, need to support Return constructs in MiniRust *)
  | Return eo -> begin match eo with
        | None -> EUnit
        | Some e -> translate_expr' env (typ_of_expr e) e
    end

  | Decl _ -> failwith "translate_stmt: decl"
  | Expr e -> translate_expr' env t e

  | Try _ -> failwith "translate_stmt: try"
  | AttributedStmt _ -> failwith "translate_stmt: AttributedStmt"
  | UnknownStmt _ -> failwith "translate_stmt: UnknownStmt"

and translate_stmt (env: env) (t: typ) (s: stmt_desc) : Krml.Ast.expr =
  Krml.Ast.with_type t (translate_stmt' env t s)

let translate_param (p: parameter) : binder * string =
  let p = p.desc in
  let typ = translate_typ p.qual_type in
  (* Not handling default expressions for function parameters *)
  assert (p.default = None);
  Helpers.fresh_binder p.name typ, p.name


let translate_fundecl (fdecl: function_decl) =
  let name = get_id_name fdecl.name in
  let ret_type = translate_typ fdecl.function_type.result in
  let args, vars = match fdecl.function_type.parameters with
    | None -> [], []
    | Some params ->
        (* Not handling variadic parameters *)
        assert (not (params.variadic));
        List.map translate_param params.non_variadic |> List.split
  in
  (* To adopt a DeBruijn representation, the list must be reversed to
   have the last binder as the first element of the environment *)
  let env = {vars = List.rev vars} in
  let body = match fdecl.body with
    | None -> Helpers.eunit
    | Some s -> translate_stmt env ret_type s.desc
  in
  let flags = if fdecl.inline_specified then [Krml.Common.Inline] else [] in
  let decl = Krml.Ast.(DFunction (None, flags, 0, 0, ret_type, (FileMap.find name !name_map, name), args, body)) in
  (* Krml.KPrint.bprintf "Resulting decl %a\n" Krml.PrintAst.pdecl decl; *)
  decl

(* Returning an option is only a hack to make progress.
   TODO: Proper handling of  decls *)
let translate_decl (decl: decl) =
  let exception Unsupported in
  try
    match decl.desc with
    | Function fdecl ->
      (* TODO: How to handle libc? *)
      (* TODO: Support multiple files *)
      Some (translate_fundecl fdecl)
    | Var vdecl ->
        if vdecl.var_init = None then
          (* Prototype, e.g. extern int x; *)
          None
        else
          let _, _, e = translate_vardecl empty_env vdecl in
          let lid = FileMap.find vdecl.var_name !name_map, vdecl.var_name in
          let typ = translate_typ vdecl.var_type in
          (* TODO: Flags *)
          let flags = [] in
          (* TODO: What is the int for? *)
          Some (DGlobal (flags, lid, 0, typ, e))
    | _ ->
        raise Unsupported
  with e ->
    Format.printf "Declaration %a not supported@." Clang.Decl.pp decl;
    raise e

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
        let ret_type = translate_typ fdecl.function_type.result in
        let args, vars = match fdecl.function_type.parameters with
          | None -> [], []
          | Some params ->
              (* Not handling variadic parameters *)
              assert (not (params.variadic));
              List.map translate_param params.non_variadic |> List.split
        in
        let args_mut = Attributes.retrieve_mutability fdecl.attributes in
        let args = match args_mut with
          | None ->
              (* No mutability was specified, but we are in an opaque definition:
                 All arguments must be considered as read-only *)
              List.map (fun arg -> match arg.typ with
                | TBuf (t, _) -> {arg with typ = TBuf (t, true)}
                | _ -> arg
              ) args
          | Some muts -> List.map2 (fun mut arg -> match arg.typ, mut with
              (* In Ast, the flag set to true represents a constant, immutable array.
                 The mutability flag is the converse, so we need to take the negation *)
              | TBuf (t, _), b -> {arg with typ = TBuf (t, not b)}
              (* For all other types, we do not modify the mutability *)
              | _ -> arg
              ) muts args
        in
        let fn_type = Helpers.fold_arrow (List.map (fun x -> x.typ) args) ret_type in

        let decl = Krml.Ast.(DExternal (None, [], 0, 0, (FileMap.find name !name_map, name), fn_type, vars)) in
        Some decl
      ) else None
  | _ -> None

let translate_file wanted_c_file file =
  let (name, decls) = file in
  let basename = Filename.basename wanted_c_file in
  (* TODO: Multifile support *)
  if name = Filename.basename wanted_c_file then
    Some (Filename.chop_suffix name ".c", List.filter_map translate_decl decls)
  else if Filename.remove_extension name = Filename.remove_extension basename then
    (* Special case for a header file corresponding to the C file we want to extract
       TODO: We should probably translate this file for type definitions, and to determine
       which functions should be public *)
    None
  else
  (* translate_external_decl will only translate declarations annotated with the
     `scylla_opaque` attribute.
     Furthermore, a file that does not contain any definitions will be filtered
     out in krml during the Rust translation.
     Hence, we can apply translate_external_decl on any file in the tree *)
    Some (Filename.chop_suffix name ".h", List.filter_map translate_external_decl decls)

(* add_to_list is only available starting from OCaml 5.1 *)
let add_to_list x data m =
  let add = function None -> Some [data] | Some l -> Some (data :: l) in
  FileMap.update x add m

let add_lident_mapping (decl: decl) (filename: string) =
  let sep =
    (* We need to translate the separator to a char.
       We assume we are on a system where it is one character (Unix or Windows) *)
    assert (String.length Filename.dir_sep = 1);
    String.get Filename.dir_sep 0
  in
  let path = [ Filename.remove_extension filename |> String.split_on_char sep |> Krml.KList.last ] in
  match decl.desc with
  | Function fdecl ->
      let name = get_id_name fdecl.name in
      (* Krml.KPrint.bprintf "%s --> %s\n" name (String.concat "::" path); *)
      name_map := FileMap.update name
        (function | None -> Some path | Some _ ->
          Format.printf "Declaration %s appears twice in translation unit, found again in %s\n" name filename;
          Some path)
        !name_map

  | Var vdecl ->
      name_map := FileMap.update vdecl.var_name
        (function | None -> Some path | Some _ ->
          Format.printf "Variable declaration %s appears twice in translation unit\n" vdecl.var_name;
          Some path)
        !name_map

  (* TODO: Do we need to support this mapping for more decls *)
  | _ -> ()

let split_into_files (lib_dirs: string list) (ast: translation_unit) =
  let add_decl acc decl =
    let loc = Clang.Ast.location_of_node decl |> Clang.Ast.concrete_of_source_location File in
    (* If this belongs to the C library, do not extract it *)
    (* TODO: This could be done more efficiently by filtering after splitting into files,
       to avoid repeated traversals of lib_dirs *)
    if List.exists (fun x -> String.starts_with ~prefix:x loc.filename) lib_dirs then acc
    else (
      add_lident_mapping decl loc.filename;
      let filename = loc.filename |> Filename.basename in
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
  (* Format.printf "@[%a@]@." (Refl.pp [%refl: Clang.Ast.translation_unit] []) ast; *)
  let files = split_into_files lib_dirs ast in
  let files = List.filter_map (translate_file wanted_c_file) files in
  files

let read_file (filename: string) : translation_unit =
  Format.printf "Clang version is %s\n" (Clang.get_clang_version());
  let command_line_args = !Scylla__Options.ccopts in
  Format.printf "Arguments passed to clang are: %s\n" (String.concat " " command_line_args);
  parse_file ~command_line_args filename
