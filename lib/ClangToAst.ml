(* Copyright (c) INRIA and Microsoft Corporation. All rights reserved. *)
(* Licensed under the Apache 2.0 and MIT Licenses. *)

open Krml.Ast
open Clang.Ast
module K = Krml.Constant
module Helpers = Krml.Helpers

module FileMap = Map.Make(String)
module StructMap = Map.Make(String)

(* A map from function names to the string list used in their fully qualified
   name. It is filled at the beginning of the translation, when exploring the
   translation unit *)
let name_map = ref FileMap.empty

(* A map from structure names to their corresponding KaRaMeL `type_def` declaration,
   as well as whether the struct type is annotated with the `scylla_box` attribute.
   Struct declarations are typically done through a typedef indirection in C, e.g.,
   `typedef struct name_s { ... } name;`
   This map is used to deconstruct the indirection in Rust, and directly define
   a struct type `name`
*)
let struct_map = ref StructMap.empty

(* A map storing types that are annotated with `scylla_box`, indicating
   that internal pointers should be translated to Boxes instead of borrows *)
let boxed_types = ref Krml.AstToMiniRust.LidSet.empty

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
      (* We first try to find the type name in the environment *)
      match FileMap.find_opt s !name_map with
      | Some t -> TQualified (t, s)
      | None ->
        (* If the type is not found in the environment, we assume
           it is an external type, and translate A_B_ty to a_b::ty *)
        let path = String.split_on_char '_' s in
        let name, path = match List.rev path with
        | [] -> failwith "Empty name"
        | hd :: tl -> hd, String.concat "_" (List.rev tl)
        in TQualified ([path], name)

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
  | Bool -> TBool

  | Pointer -> failwith "translate_builtin_typ: pointer"
  | Float -> failwith "translate_builtin_typ: Float"
  | Double -> failwith "translate_builtin_typ: Double"
  | LongDouble -> failwith "translate_builtin_typ: LongDouble"

  | _ -> failwith "translate_builtin_typ: unsupported type"

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
      Format.eprintf "Trying to translate type %a\n" Clang.Type.pp typ;
      failwith "translate_typ: unsupported type"

(* Elaborate a type during a typedef declaration. Also return whether the type should be boxed *)
let elaborate_typ (typ: qual_type) = match typ.desc with
  | Elaborated { keyword = Struct; named_type = { desc = Record {name; _}; _}; _ } ->
      let name = get_id_name name in
      StructMap.find name !struct_map
  (* TODO: Similar workflow as structs *)
  | Elaborated { keyword = Enum; _} -> failwith "elaborated enums not supported"
  | Elaborated _ -> failwith "elaborated types that are not enums or structs are not supported"
  | _ -> failwith "The underlying type of a typedef is not an elaborated type"

(* Takes a Clangml expression [e], and retrieves the corresponding karamel Ast type *)
let typ_of_expr (e: expr) : typ = Clang.Type.of_node e |> translate_typ

(* Check whether a given Clang expression is a scylla_reset callee *)
let is_scylla_reset (e: expr) = match e.desc with
  | DeclRef { name; _ } ->
      let name = get_id_name name in
      name = "scylla_reset"
  | _ -> false

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

(* Check whether a given Clang expression is a calloc callee *)
let is_calloc (e: expr) = match e.desc with
  | DeclRef { name; _ } ->
      let name = get_id_name name in
      name = "calloc"
  | _ -> false

(* Check whether a given Clang expression is a malloc callee *)
let is_malloc (e: expr) = match e.desc with
  | DeclRef { name; _ } ->
      let name = get_id_name name in
      name = "malloc"
  | _ -> false

(* Check whether a given Clang expression is a free callee *)
let is_free (e: expr) = match e.desc with
  | DeclRef { name; _ } ->
      let name = get_id_name name in
      name = "free"
  | _ -> false

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
  | _ -> false

let extract_sizeof_ty = function
  | ArgumentExpr _ -> failwith "ArgumentExpr not supported"
  | ArgumentType ty -> translate_typ ty

let extract_constarray_size (ty: qual_type) = match ty.desc with
  | ConstantArray {size; _} -> size, Helpers.mk_uint32 size
  | _ ->
      Format.eprintf "Expected ConstantArray, got type %a\n" Clang.Type.pp ty;
      failwith "Type is not a ConstantArray"

let is_constantarray (ty: qual_type) = match ty.desc with
  | ConstantArray _ -> true
  | _ -> false

(* Translate expression [e], with expected type [t] *)
let rec translate_expr' (env: env) (t: typ) (e: expr) : expr' =
  if is_null e then EBufNull
  else
  match e.desc with
  | IntegerLiteral n ->
      begin match t with
      | TBool ->
          begin match n with
          | Int 0 -> EBool false
          | Int 1 ->  EBool true
          | _ -> failwith "Not a boolean literal"
          end
      | TInt _ ->
        let ty = Helpers.assert_tint t in
        let signed = K.is_signed ty in
        EConstant (ty, Clang.Ast.string_of_integer_literal ~signed n)
      | _ ->
        (* TODO: Handle this better *)
        EConstant (UInt32, Clang.Ast.string_of_integer_literal n)
      end

  | FloatingLiteral _ -> failwith "translate_expr: floating literal"
  | StringLiteral _ -> failwith "translate_expr: string literal"
  | CharacterLiteral _ -> failwith "translate_expr character literal"
  | ImaginaryLiteral _ -> failwith "translate_expr: imaginary literal"
  | BoolLiteral _ -> failwith "translate_expr: bool literal"
  | NullPtrLiteral -> failwith "translate_expr: null ptr literal"

  | CompoundLiteral {qual_type; init = {desc = InitList l; _}} when is_constantarray qual_type ->
        let size, size_e = extract_constarray_size qual_type in
        if List.length l = 1 then
          (* One element initializer, possibly repeated *)
          let e = translate_expr env (Helpers.assert_tbuf t) (List.hd l) in
          (* TODO: Arrays are not on stack if at top-level *)
          EBufCreate (Krml.Common.Stack, e, size_e)
        else (
          assert (List.length l = size);
          let ty = Helpers.assert_tbuf t in
          let es = List.map (translate_expr env ty) l in
          EBufCreateL (Krml.Common.Stack, es)
        )

  (* We handled above the case of array initialization, this should
     be a struct initialization *)
  | CompoundLiteral {init = {desc = InitList l; _}; _} ->
      let translate_field_expr (e : expr) = match e.desc with
        | DesignatedInit { designators; init }  ->
            begin match designators with
            | [FieldDesignator name] ->
                let e = translate_expr env (typ_of_expr init) init in
                (Some name, e)
            | [_] -> failwith "expected a field designator"
            | _ -> failwith "assigning to several fields during struct initialization is not supported"
            end
      | _ -> failwith "a designated initializer was expected when initializing a struct"
      in
     EFlat (List.map translate_field_expr l)


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

  | UnaryOperator {kind = LNot; operand } ->
      (* Logical not: The operand should be a boolean *)
      let o = translate_expr env TBool operand in
      (Helpers.mk_not o).node

  | UnaryOperator {kind = Deref; operand } ->
      let ty = Helpers.assert_tbuf (typ_of_expr operand) in
      let o = translate_expr env (TBuf (ty, false)) operand in
      EBufRead (o, Helpers.zero_usize)

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

  | Call {callee; args} when is_scylla_reset callee ->
      begin match args with
      | [e] -> let e = translate_expr env (typ_of_expr e) e in (Helpers.push_ignore e).node
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

  | Call {callee; args} when is_free callee ->
      begin match args with
      | [ptr] -> EBufFree (translate_expr env (typ_of_expr ptr) ptr)
      | _ -> failwith "ill-formed free: too many arguments"
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

  | Member {base; arrow; field} ->
      (* TODO: Support for arrow access *)
      assert (not arrow);
      let base = match base with
      | None -> failwith "field accesses without a base expression are not supported"
      | Some b -> b
      in
      let base = translate_expr env (typ_of_expr base) base in

      let f = match field with
      | FieldName {desc; _} -> get_id_name desc.name
      | _ -> failwith "member node: only field accesses supported"
      in

      EField (base, f)

  | _ ->
    Format.eprintf "Trying to translate expression %a@." Clang.Expr.pp e;
    failwith "translate_expr: unsupported expression"

and translate_expr (env: env) (t: typ) (e: expr) : Krml.Ast.expr =
  Krml.Ast.with_type t (translate_expr' env t e)

(* Create a default value associated to a given type [typ] *)
let create_default_value typ = match typ with
  | TInt w -> Helpers.zero w
  | _ -> Helpers.any

let translate_vardecl (env: env) (vdecl: var_decl_desc) : env * binder * Krml.Ast.expr =
  let vname = vdecl.var_name in
  let typ = translate_typ vdecl.var_type in
  match vdecl.var_init with
  | None ->
        (* If there is no associated definition, we attempt to craft
           a default initialization value *)
        add_var env vname, Helpers.fresh_binder vname typ, create_default_value typ

  (* Intializing a constant array with a list of elements.
     For instance, uint32[2] = { 0 };
  *)
  | Some {desc = InitList l; _} when is_constantarray vdecl.var_type ->
        let size, size_e = extract_constarray_size vdecl.var_type in
        if List.length l = 1 then
          (* One element initializer, possibly repeated *)
          let e = translate_expr env (Helpers.assert_tbuf typ) (List.hd l) in
          (* TODO: Arrays are not on stack if at top-level *)
          add_var env vname, Helpers.fresh_binder vname typ, Krml.Ast.with_type typ (EBufCreate (Krml.Common.Stack, e, size_e))
        else (
          assert (List.length l = size);
          let ty = Helpers.assert_tbuf typ in
          let es = List.map (translate_expr env ty) l in
          add_var env vname, Helpers.fresh_binder vname typ, Krml.Ast.with_type typ (EBufCreateL (Krml.Common.Stack, es))
        )

  (* Initializing a struct value.
     TODO: We should check that the declaration type indeed corresponds to a struct type *)
  | Some {desc = InitList l; _} ->
      let translate_field_expr (e : expr) = match e.desc with
        | DesignatedInit { designators; init }  ->
            begin match designators with
            | [FieldDesignator name] ->
                let e = translate_expr env (typ_of_expr init) init in
                (Some name, e)
            | [_] -> failwith "expected a field designator"
            | _ -> failwith "assigning to several fields during struct initialization is not supported"
            end
      | _ -> failwith "a designated initializer was expected when initializing a struct"
      in
      add_var env vname, Helpers.fresh_binder vname typ, Krml.Ast.with_type typ (EFlat (List.map translate_field_expr l))


  | Some {desc = Call {callee; args}; _}
  (* There commonly is a cast around calloc to the type of the variable. We omit it when translating it to Rust,
     as the allocation will be typed *)
  | Some {desc = Cast {operand = {desc = Call {callee; args}; _}; _}; _} when is_calloc callee ->
      begin match args with
      | [len; {desc = UnaryExpr {kind = SizeOf; argument}; _}] ->
          let len = translate_expr env Helpers.usize len in
          (* Sanity check: calloc is of the right type *)
          let ty = Helpers.assert_tbuf typ in
          assert (extract_sizeof_ty argument = ty);
          let w = Helpers.assert_tint ty in
          add_var env vname, Helpers.fresh_binder vname typ,
            Krml.Ast.with_type typ (EBufCreate (Krml.Common.Heap, Helpers.zero w, len))
      | _ -> failwith "calloc is expected to have two arguments"
      end

  | Some {desc = DeclRef { name; _ }; _} ->
      let var = get_id_name name |> find_var env in
      let e = match typ with
      (* If we have a statement of the shape `let x = y` where y is a pointer,
         this likely corresponds to taking a slice of y, starting at index 0.
         We need to explicitly insert the EBufSub node to create a split tree *)
      | TBuf _ | TArray _ -> EBufSub (Krml.Ast.with_type typ var, Helpers.zero_usize)
      | _ -> var
      in
      add_var env vname, Helpers.fresh_binder vname typ, Krml.Ast.with_type typ e

  | Some e -> add_var env vname, Helpers.fresh_binder vname typ, translate_expr env typ e

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
      TBuf (translate_typ element, false), translate_expr env (typ_of_expr size) size
  | ConstantArray {element; size_as_expr; _} ->
      let size = match size_as_expr with
      | None -> failwith "Length of constant array is not an expr"
      | Some size -> translate_expr env Helpers.usize size
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
      let v = translate_expr env (Helpers.assert_tbuf typ) v in
      let len = translate_expr env Helpers.usize len in
      (* Types might have been inferred differently, we only compare the expressions *)
      if len.node = size.node then
        add_var env vname,
        Helpers.fresh_binder vname typ,
        Krml.Ast.with_type typ (EBufCreate (Krml.Common.Stack, v, len))
      else
        failwith "length of memset does not match declared length of array"

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
          translate_expr env (Helpers.assert_tbuf typ) rhs
      | _ -> failwith "ill-formed malloc initializer"
      end
  | _ -> failwith "ill-formed malloc initializer"
  in

  add_var env vname, Helpers.fresh_binder vname typ, Krml.Ast.with_type typ (EBufCreate (Krml.Common.Heap, init_val, Helpers.oneu32))



let rec translate_stmt' (env: env) (t: typ) (s: stmt_desc) : expr' = match s with
  (* This is a null statement, not a null pointer. It corresponds to a no-op *)
  | Null -> EUnit

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

      (* Special case when we have a malloc followed by an initializer
         for the corresponding pointer: we rewrite this into a heap array
         initialization *)
      | Decl [{desc = Var vdecl; _}], stmt when is_malloc_vdecl vdecl && is_malloc_initializer vdecl stmt ->
          let env', b, e = translate_vardecl_malloc env vdecl stmt in
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

  (* There is no null pointer in Rust. We remove branching based on null-pointer
     comparisons *)
  | If {cond = {desc = BinaryOperator {lhs; kind = EQ; rhs}; _}; else_branch; _} when has_pointer_type lhs && is_null rhs ->
      begin match else_branch with
      | None -> EUnit
      | Some s -> translate_stmt' env TUnit s.desc
      end
  | If {cond = {desc = BinaryOperator {lhs; kind = NE; rhs}; _}; then_branch; _} when has_pointer_type lhs && is_null rhs ->
      translate_stmt' env TUnit then_branch.desc

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

  | Switch {init; condition_variable; cond; body} ->
      (* C++ constructs *)
      assert (init = None);
      assert (condition_variable = None);

      let cond = translate_expr env (typ_of_expr cond) cond in
      let branches = translate_branches env t body.desc in
      EMatch (Unchecked, cond, branches)

  | Case _ -> failwith "case not encapsulated in a switch"
  | Default _ -> failwith "default not encapsulated in a switch"

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

  | Return eo -> begin match eo with
        | None -> EReturn Helpers.eunit
        | Some e -> EReturn (translate_expr env (typ_of_expr e) e)
    end

  | Decl _ -> failwith "translate_stmt: decl"
  | Expr e -> translate_expr' env t e

  | Try _ -> failwith "translate_stmt: try"
  | AttributedStmt _ -> failwith "translate_stmt: AttributedStmt"
  | UnknownStmt _ -> failwith "translate_stmt: UnknownStmt"

and translate_stmt (env: env) (t: typ) (s: stmt_desc) : Krml.Ast.expr =
  Krml.Ast.with_type t (translate_stmt' env t s)

(* Translate case and default statements inside a switch to a list of branches for
   structured pattern-matching.
   The original C branches must consist of a list of `case` statements, terminated by
   a `default` statement *)
and translate_branches (env: env) (t: typ) (s: stmt_desc) : Krml.Ast.branches = match s with
  | Compound [{desc = Default body; _}] ->
      let body = translate_stmt env t body.desc in
      (* The last case is a fallback, the pattern corresponds to a wildcard *)
      [([], Krml.Ast.with_type TAny PWild, body)]
  | Compound ({desc = Case {lhs; rhs; body}; _} :: tl) ->
      (* Unsupported GCC extension *)
      assert (rhs = None);
      let pat_ty = typ_of_expr lhs in
      let pat = translate_expr' env (typ_of_expr lhs) lhs in
      let body = translate_stmt env t body.desc in
      (* We only support pattern-matching on constants here.
         This allows to translate switches corresponding to pattern
         matching on a tagged union *)
      begin match pat with
      | EConstant n -> ([], Krml.Ast.with_type pat_ty (PConstant n), body)
      | _ -> failwith "Only constant patterns supported"
      end :: translate_branches env t (Compound tl)
  | _ -> failwith "Ill-formed switch branches: Expected a case or a default"


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
  match fdecl.body with
  (* If the function body is empty, this is likely a prototype. We
     do not extract it *)
  | None -> None
  | Some s ->
    let body = translate_stmt env ret_type s.desc in
    let flags = if fdecl.inline_specified then [Krml.Common.Inline] else [] in
    let decl = Krml.Ast.(DFunction (None, flags, 0, 0, ret_type, (FileMap.find name !name_map, name), args, body)) in
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

(* Returning an option is only a hack to make progress.
   TODO: Proper handling of  decls *)
let translate_decl (decl: decl) =
  let exception Unsupported in
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
          let lid = FileMap.find vdecl.var_name !name_map, vdecl.var_name in
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
        let lid = FileMap.find name !name_map, name in
        begin match underlying_type.desc with
        | Typedef {name; _} ->
            let name = get_id_name name in
            Some (DType (lid, [], 0, 0, Abbrev (translate_typ_name name)))
        | _ ->
          let ty, is_box = elaborate_typ underlying_type in
          if is_box then boxed_types := Krml.AstToMiniRust.LidSet.add lid !boxed_types;
          Some (DType (lid, [], 0, 0, ty))
        end

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

  | RecordDecl rdecl ->
      name_map := FileMap.update rdecl.name
        (function | None -> Some path | Some _ ->
          Format.printf "Record Type declaration %s appears twice in translation unit\n" rdecl.name;
          Some path)
        !name_map

  | TypedefDecl tdecl ->
      name_map := FileMap.update tdecl.name
        (function | None -> Some path | Some _ ->
          Format.printf "Typedef declaration %s appears twice in translation unit\n" tdecl.name;
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
  (* Format.printf "@[%a@]@." (Refl.pp [%refl: Clang.Ast.translation_unit] []) ast; *)
  let files = split_into_files lib_dirs ast in
  let files = List.filter_map (translate_file wanted_c_file) files in
  !boxed_types, files

let read_file (filename: string) : translation_unit =
  Format.printf "Clang version is %s\n" (Clang.get_clang_version());
  let command_line_args = !Scylla__Options.ccopts @
    List.map Clang.Command_line.include_directory (Clang.default_include_directories ()) in
  Format.printf "Arguments passed to clang are: %s\n" (String.concat " " command_line_args);
  parse_file ~command_line_args filename
