(* Copyright (c) INRIA and Microsoft Corporation. All rights reserved. *)
(* Licensed under the Apache 2.0 and MIT Licenses. *)

module ScyllaOptions = Options
open Krml.Ast
open Krml.PrintAst.Ops
open Clang.Ast
module K = Krml.Constant
module Helpers = Krml.Helpers

let fatal_error = Krml.Warn.fatal_error

module StringMap = Map.Make (String)
module LidMap = Krml.AstToMiniRust.LidMap
module LidSet = Krml.AstToMiniRust.LidSet

module ElaboratedMap = Map.Make (struct
  (* FIXME this should be a DeclName.t *)
  type t = declaration_name * [ `Struct ]

  let compare = compare
end)

(* GLOBAL STATE *)

let get_id_name (dname : declaration_name) =
  match dname with
  | IdentifierName s -> s
  | ConstructorName _ -> failwith "constructor"
  | DestructorName _ -> failwith "destructor"
  | ConversionFunctionName _ -> failwith "conversion function"
  | DeductionGuideName _ -> failwith "deduction guide"
  | OperatorName _ -> failwith "operator name"
  | LiteralOperatorName _ -> failwith "literal operator name"
  | UsingDirectiveName -> failwith "using directive"

module DeclName = struct
  (* https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf, 6.2.3 *)
  type namespace =
    | Tag (* struct, enum, union *)
    | Member (* a.k.a. field names *)
    | Ordinary (* everything else, including typedefs *)

  type t = namespace * string
  let compare = compare

  let of_decl (decl : decl) : t =
    match decl.desc with
    | Function { name; _ } -> Ordinary, get_id_name name
    | EnumDecl { name; _ } -> Tag, name
    | RecordDecl { name; _ } -> Tag, name
    | TypedefDecl { name; _ } -> Ordinary, name
    | Field { name; _ } -> Member, name
    | Var desc -> Ordinary, desc.var_name
    | _ -> Ordinary, "unknown"

  let of_enum_constant (constant: enum_constant): t =
    Ordinary, constant.desc.constant_name

  (* For use with Format *)
  let pp (fmt: Format.formatter) (ns, n) =
    match ns with
    | Tag -> Format.pp_print_string fmt "struct/union/enum "
    | Member -> Format.pp_print_string fmt "."
    | Ordinary -> ()
    ; ;
    Format.pp_print_string fmt n

  (* For use with Krml.KPrint.bprintf *)
  let p buf (ns, n) =
    match ns with
    | Tag -> Buffer.add_string buf "struct/union/enum "
    | Member -> Buffer.add_char buf '.'
    | Ordinary -> ()
    ; ;
    Buffer.add_string buf n
end

module DeclMap = Map.Make(DeclName)

(* A map from C names to the file stem (i.e. `foo` for `bar/foo.c`) they belong to. It is filled
   at the beginning of the translation, when exploring the translation unit. It allows converting a
   C name to a krml `lident`. *)
let name_map : string DeclMap.t ref = ref DeclMap.empty

(* This domain of this map is functions and global variables. *)
let global_type_map : (typ * [ `Enum | `GlobalOrFun ]) StringMap.t ref = ref StringMap.empty

(* A map from an elaborated type reference (e.g. `struct S`) to the lid it has been assigned in the
 translation -- we always eliminate elaborated types in favor of lids. *)
let elaborated_map = ref ElaboratedMap.empty

(* A map storing types that are annotated with `scylla_box`, indicating
   that internal pointers should be translated to Boxes instead of borrows *)
let boxed_types = ref LidSet.empty

(* A map storing types that are annotated with `scylla_container_type`,
   to be passed to karamel *)
let container_types = ref LidSet.empty

(* The values of type_def_map below used to be of type lazy AST.type_def.
    However, when pattern-matching on lazy values, OCaml will force the evaluation,
    even if the resulting value does not correspond the pattern.
    This leads to issues in type normalization when writing, e.g.,
    ```
    match find ... !type_def_map with
    | (lazy (Abbrev t)) -> normalize_type t
    | _ -> t
    ```

    If the stored value is instead, e.g., a `lazy (Flat fields)`, the pattern-matching
    will nevertheless force the evaluation, leading to cycles when translating mutually
    recursive structs.

   To avoid forcing the evaluation in unwanted cases during pattern-matching, we therefore
   redefine AST's `type_def` to contain lazy values: this allows us to pattern-match on
   the constructor, and to only force the execution of the payload when needed.
*)
type type_def_lazy =
  | CVariant of Krml.Ast.branches_t Lazy.t
  | CFlat of Krml.Ast.fields_t_opt Lazy.t
  (* For the translation, we preserve the list of fields as if the tuple
     were a struct: This allows us to replace field access by the correct
     tuple access, depending on the numbering of the branches *)
  | CTuple of Krml.Ast.fields_t_opt Lazy.t
  (* A slice of elements of type t. In C, this is a pointer type t* *)
  | CSlice of Krml.Ast.typ Lazy.t
  | CAbbrev of Krml.Ast.typ Lazy.t
  | CEnum of (lident * Krml.Ast.z option) list Lazy.t

let force_type_def_lazy (t : type_def_lazy) : Krml.Ast.type_def =
  match t with
  | CVariant branches -> Variant (Lazy.force branches)
  | CFlat fields -> Flat (Lazy.force fields)
  | CTuple fields ->
      let typ = TTuple (List.map (fun (_, (t, _)) -> t) (Lazy.force fields)) in
      Abbrev typ
  | CSlice t ->
      let typ = TBuf (Lazy.force t, false) in
      Abbrev typ
  | CAbbrev t -> Abbrev (Lazy.force t)
  | CEnum l -> Enum (Lazy.force l)

(* A map from type names to their underlying implementation (abbreviation, struct, etc.).
   It is needed to retrieve the type of, e.g., constants when the expected type is an alias to an
   integer type -- also allows not generating code that relies on type abbreviations being in scope
   in order to type-check -- every synthesized type goes through `normalize_type` which inlines
   abbreviations away. Finally, it also allows resolving proper type information for field
   operations. *)
let type_def_map : type_def_lazy LidMap.t ref = ref LidMap.empty

(* A map from top-level declaration to additional traits that they ought to derive *)
let deriving_traits : string list LidMap.t ref = ref LidMap.empty

(* A map from top-level declarations to additional attributes they may have *)
let attributes_map : string list LidMap.t ref = ref LidMap.empty
let exposed_globals : LidSet.t ref = ref LidSet.empty

(* add_to_list is only available starting from OCaml 5.1 *)
let add_to_list x data m =
  let add = function
    | None -> Some [ data ]
    | Some l -> Some (data :: l)
  in
  StringMap.update x add m

let add_to_list_lid x data m =
  let add = function
    | None -> Some [ data ]
    | Some l -> Some (data :: l)
  in
  LidMap.update x add m

(* ENVIRONMENTS *)

(* For a given tagged union variable, we store the case (variant name) it is currently
  in, as well as the variable corresponding to the constructor contents pattern *)
type tagged_case = { case : string; var : string }

(* A variable in the context. It contains its name, type, a reference to tell
   whether they end up being mutated at some point, and meta information about
   whether they are a tagged union, and if so their current state *)
type env_var = { name : string; t : typ; mut : bool ref; case : tagged_case option }

type env = {
  (* Variables in the context *)
  vars : env_var list;
  (* Expected return typ of the function *)
  ret_t : typ;
  (* In C, one can do `int x = sizeof(x)` -- but in krml, `x` is not in the scope of `e1` when doing
     `let x = e1 in e2`. However, we still want to resolve `x` in `e1`, but only for the limited
     use-case of doing `int *x = malloc(sizeof( *x))`. We use this dedicated field. *)
  self : (string * typ) option;
}

let empty_env = { vars = []; ret_t = TAny; self = None }

let add_var env (x, t) =
  { env with vars = { name = x; t; mut = ref false; case = None } :: env.vars }

let add_self env (x, t) = { env with self = Some (x, t) }

(* Refines the `case` field corresponding to the variable `x`.
   This updates the first variable `x` in the vars context, corresponding
   to the currently live `x` *)
let refine_var_case env x case =
  let rec aux = function
    | [] -> fatal_error "Did not find variable %s in list" x
    | hd :: tl when hd.name = x -> { hd with case } :: tl
    | hd :: tl -> hd :: aux tl
  in
  { env with vars = aux env.vars }

let add_binders env binders =
  List.fold_left
    (fun env b ->
      let open Krml.Ast in
      add_var env (b.node.name, b.typ))
    env binders

(* TODO: Handle fully qualified names/namespaces/different files. *)
let find_var env name =
  let exception Found of int * typ * bool ref * tagged_case option in
  try
    List.iteri
      (fun i { name = name'; t; mut; case } ->
        if name = name' then
          raise (Found (i, t, mut, case)))
      env.vars;
    raise Not_found
  with
  | Found (i, t, mut, case) -> with_type t (EBound i), mut, case
  | Not_found -> (
      try
        let path = DeclMap.find (Ordinary, name) !name_map in
        let t, kind = StringMap.find name !global_type_map in
        (* FIXME handle mutable globals *)
        match kind with
        | `GlobalOrFun -> with_type t (EQualified ([ path ], name)), ref false, None
        | `Enum -> with_type t (EEnum ([ path ], name)), ref false, None
      with Not_found ->
        Printf.eprintf "Could not find variable %s\n" name;
        raise Not_found)

(* TYPES *)

let lid_of_name name =
  match DeclMap.find_opt name !name_map with
  | Some path -> Some ([ path ], snd name)
  | None -> None

let lid_of_ordinary_name name =
  lid_of_name (Ordinary, name)

let translate_typ_name = function
  | "size_t" -> Helpers.usize
  | "int8_t" -> TInt K.Int8
  | "int16_t" -> TInt K.Int16
  | "int32_t" -> TInt K.Int32
  | "int64_t" -> TInt K.Int64
  | "uint8_t" -> Helpers.uint8
  | "uint16_t" -> Helpers.uint16
  | "uint32_t" -> Helpers.uint32
  | "uint64_t" -> Helpers.uint64
  | s -> (
      (* We first try to find the type name in the environment *)
      match lid_of_ordinary_name s with
      | Some lid -> TQualified lid
      | None ->
          (* If the type is not found in the environment, we assume
           it is an external type, and translate A_B_ty to a_b::ty *)
          let path = String.split_on_char '_' s in
          let name, path =
            match List.rev path with
            | [] -> failwith "Empty name"
            | hd :: tl -> hd, String.concat "_" (List.rev tl)
          in
          TQualified ([ path ], name))

(* We assume a modern system where sizeof int == 4, sizeof long long == 8, and sizeof long is
   determined at configure-time (see DataModel.ml). *)
let translate_builtin_typ (t : Clang.Ast.builtin_type) =
  match[@warnerror "-11"] t with
  | Void -> TUnit
  | UInt -> TInt UInt32
  | UShort -> TInt UInt16
  | ULong -> begin
      match DataModel.size_long with
      | 4 -> TInt UInt32
      | 8 -> TInt UInt64
      | _ -> failwith "impossible"
    end
  | ULongLong -> TInt UInt64
  | UInt128 -> failwith "translate_builtin_typ: uint128"
  | Int -> TInt Int32
  | Short -> TInt Int16
  | Long -> begin
      match DataModel.size_long with
      | 4 -> TInt Int32
      | 8 -> TInt Int64
      | _ -> failwith "impossible"
    end
  | LongLong -> TInt Int64
  | Int128 -> failwith "translate_builtin_typ: signed int 128"
  | Bool -> TBool
  | Pointer -> failwith "translate_builtin_typ: pointer"
  | Invalid -> failwith "translate_builtin_typ: Invalid"
  | Unexposed -> failwith "translate_builtin_typ: Unexposed"
  | Char_U -> failwith "translate_builtin_typ: Char_U"
  | UChar -> TInt UInt8
  | Char16 -> failwith "translate_builtin_typ: Char16"
  | Char32 -> failwith "translate_builtin_typ: Char32"
  | Char_S -> TInt Int8
  | SChar -> failwith "translate_builtin_typ: SChar"
  | WChar -> failwith "translate_builtin_typ: WChar"
  | Float -> TInt Float32
  | Double -> TInt Float64
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
  | OCLIntelSubgroupAVCImeResultSingleRefStreamout ->
      failwith "translate_builtin_typ: OCLIntelSubgroupAVCImeResultSingleRefStreamout"
  | OCLIntelSubgroupAVCImeResultDualRefStreamout ->
      failwith "translate_builtin_typ: OCLIntelSubgroupAVCImeResultDualRefStreamout"
  | OCLIntelSubgroupAVCImeSingleRefStreamin ->
      failwith "translate_builtin_typ: OCLIntelSubgroupAVCImeSingleRefStreamin"
  | OCLIntelSubgroupAVCImeDualRefStreamin ->
      failwith "translate_builtin_typ: OCLIntelSubgroupAVCImeDualRefStreamin"
  | ExtVector -> failwith "translate_builtin_typ: ExtVector"
  | Atomic -> failwith "translate_builtin_typ: Atomic"
  | _ -> failwith "translate_builtin_typ: BTFTagAttributed"

let rec translate_typ (typ : qual_type) =
  match typ.desc with
  | Pointer typ -> TBuf (translate_typ typ, typ.const)
  | LValueReference _ -> failwith "translate_typ: lvalue reference"
  | RValueReference _ -> failwith "translate_typ: rvalue reference"
  (* ConstantArray is a constant-size array. If we refine the AstToMiniRust analysis,
    we could extract array length information here *)
  | ConstantArray { element; size; _ } -> TArray (translate_typ element, (SizeT, string_of_int size))
  | Enum _ -> failwith "translate_typ: enum"
  | FunctionType { result; parameters; _ } ->
      let ret_typ = translate_typ result in
      begin
        match parameters with
        | None -> TArrow (TUnit, ret_typ)
        | Some params ->
            (* Not handling variadic parameters *)
            assert (not params.variadic);
            let ts =
              List.map (fun (p : parameter) -> translate_typ p.desc.qual_type) params.non_variadic
            in
            Helpers.fold_arrow ts ret_typ
      end
  | Record _ -> failwith "translate_typ: record"
  | Typedef { name; _ } -> get_id_name name |> translate_typ_name
  | BuiltinType t -> translate_builtin_typ t
  | Elaborated { keyword = Struct | Enum; named_type = { desc = (Record { name; _ } | Enum { name; _ }); _ }; _ } -> begin
      assert (get_id_name name <> "");
      try TQualified (ElaboratedMap.find (name, `Struct) !elaborated_map)
      with Not_found ->
        failwith
          (Format.asprintf "translate_typ: unsupported elaborated type %a\n@." Clang.Type.pp typ)
    end
  | _ -> failwith (Format.asprintf "translate_typ: unsupported type %a\n@." Clang.Type.pp typ)

let rec normalize_type t =
  match t with
  | TQualified lid -> begin
      match LidMap.find lid !type_def_map with
      | exception Not_found ->
          (* Krml.KPrint.bprintf "Not in the abbrev map: %a\n" Krml.PrintAst.Ops.plid lid; *)
          t
      | CAbbrev lazy_t -> normalize_type (Lazy.force lazy_t)
      | _ -> t
    end
  | TBuf (t, c) -> TBuf (normalize_type t, c)
  | TArray (t, c) -> TArray (normalize_type t, c)
  | TArrow (t1, t2) -> TArrow (normalize_type t1, normalize_type t2)
  | TTuple ts -> TTuple (List.map normalize_type ts)
  | _ -> t

let translate_typ t = normalize_type (translate_typ t)
let translate_typ_name t = normalize_type (translate_typ_name t)

let find_var env name =
  match find_var env name with
  | ({ node = EQualified _; _ } as e), mut, case -> { e with typ = normalize_type e.typ }, mut, case
  | e -> e

(* Indicate that we synthesize the type of an expression based on the information provided by
   Clang. We aim to do this only in a few select cases:
   - integer constants
   - variable declarations
   - function types (so, arguments and return types).
   Every other type should be able to be deduced from the context. *)
let typ_from_clang (e : Clang.Ast.expr) : typ = Clang.Type.of_node e |> translate_typ

(* Extension of karamel's assert_tbuf_or_tarray to handle slice types.
   Note, we cannot simply normalize slice types as we need to know
   a given type is a slice to rewrite "len" and "elt" field accesses
   accordingly *)
let assert_tbuf_or_tarray t =
  match t with
  | TQualified lid -> begin
      match LidMap.find lid !type_def_map with
      | CSlice (lazy t) -> t
      | _ -> fatal_error "Type %a is not a tbuf or tarray" ptyp t
    end
  | _ -> Helpers.assert_tbuf_or_tarray t

let is_tbuf_tarray_tslice t =
  match t with
  | TQualified lid -> begin
      match LidMap.find lid !type_def_map with
      | CSlice _ -> true
      | _ -> false
    end
  | TBuf _ | TArray _ -> true
  | _ -> false

(* HELPERS *)

(* Helpers to deal with the Clang AST, as opposed to Helpers which deals with the Krml AST. *)
module ClangHelpers = struct
  let is_known_name name' (e : expr) =
    match e.desc with
    | DeclRef { name; _ } ->
        let name = get_id_name name in
        name = name'
    | _ -> false

  (* Check whether a given Clang expression is a scylla_reset callee *)
  let is_scylla_reset = is_known_name "scylla_reset"

  (* Check whether a given Clang expression is a scylla_split callee *)
  let is_scylla_split = is_known_name "scylla_split"

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

  (* Check whether a variable is initialized with a call to `malloc`. *)
  let is_malloc_vdecl (vdecl : var_decl_desc) =
    (* There commonly is a cast around malloc to the type of the variable. We omit it when translating it to Rust,
       as the allocation will be typed *)
    match vdecl.var_init with
    | Some { desc = Call { callee; args; _ }; _ }
    | Some { desc = Cast { operand = { desc = Call { callee; args; _ }; _ }; _ }; _ }
      when is_malloc callee -> Some args
    | _ -> None

  (* Check whether expression [e] is a pointer *)
  let has_pointer_type (e : expr) =
    match typ_from_clang e with
    | TBuf _ | TArray _ -> true
    | _ -> false

  (* Recognize several common patterns for the null pointer *)
  let rec is_null (e : expr) =
    match e.desc with
    | Cast
        {
          qual_type = { desc = Pointer { desc = BuiltinType Void; _ }; _ };
          operand = { desc = IntegerLiteral (Int 0); _ };
          _;
        } -> true
    | _ -> false

  (* Matches `var_name != NULL` *)
  let is_null_check var_name (e : expr) =
    match e.desc with
    | BinaryOperator { lhs = { desc = DeclRef { name; _ }; _ }; kind = NE; rhs } ->
        if get_id_name name = var_name && is_null rhs then
          true
        else
          false
    | _ -> false

  let is_zero_access (e : expr) var_name =
    match e.desc with
    | ArraySubscript
        { base = { desc = DeclRef { name; _ }; _ }; index = { desc = IntegerLiteral (Int 0); _ } }
      -> get_id_name name = var_name
    | _ -> false

  (* Simple heuristics to detect whether a loop condition is always false, in this case we can omit the loop.
     TODO: Should probably check for absence of side-effects in condition evaluation *)
  let is_trivial_false (e : Krml.Ast.expr) =
    match e.node with
    (* e != e is always false *)
    | EApp ({ node = EOp (Neq, _); _ }, [ e1; e2 ]) when e1 = e2 -> true
    | EBool false -> true
    | _ -> false

  let extract_constarray_size (ty : qual_type) =
    match ty.desc with
    | ConstantArray { size; _ } -> size, Helpers.mk_uint32 size
    | _ ->
        Format.eprintf "Expected ConstantArray, got type %a\n@." Clang.Type.pp ty;
        failwith "Type is not a ConstantArray"

  let is_constantarray (ty : qual_type) =
    match ty.desc with
    | ConstantArray _ -> true
    | _ -> false

  let is_assign_op (kind : Clang.Ast.binary_operator_kind) =
    match kind with
    | Assign
    | AddAssign
    | MulAssign
    | DivAssign
    | RemAssign
    | SubAssign
    | ShlAssign
    | ShrAssign
    | AndAssign
    | XorAssign
    | OrAssign -> true
    | _ -> false
end

open ClangHelpers

(* EXPRESSIONS *)

let assign_to_bop (kind : Clang.Ast.binary_operator_kind) : Clang.Ast.binary_operator_kind =
  match kind with
  | AddAssign -> Add
  | MulAssign -> Mul
  | DivAssign -> Div
  | RemAssign -> Rem
  | SubAssign -> Sub
  | ShlAssign -> Shl
  | ShrAssign -> Shr
  | AndAssign -> And
  (* TODO: Disambiguate. JP: how so? *)
  | XorAssign -> Xor
  | OrAssign -> Or
  | _ -> failwith "not an assign op"

let translate_binop (kind : Clang.Ast.binary_operator_kind) : K.op =
  match kind with
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
  | Assign
  | AddAssign
  | MulAssign
  | DivAssign
  | RemAssign
  | SubAssign
  | ShlAssign
  | ShrAssign
  | AndAssign
  | XorAssign
  | OrAssign -> failwith "Assign operators should have been previously rewritten"
  | Comma -> failwith "translate_binop: comma"
  | InvalidBinaryOperator -> failwith "translate_binop: invalid binop"

(* Adjust the type of expression `e` to be `t`. We synthesize types bottom-up, but sometimes, the
   context provides an expected type. So far, this happens in three situations:
   - condition expressions, which krml wants to be booleans, but which in C are integers
   - array indices, and operands of memory-related operations (e.g. memcpy), which in C might be ULL
     constants (i.e., synthesized as UInt64 bottom-up), but which need to be SizeT
   - enum tags, which are integers in C, but in krml need to be converted to constants. *)
let adjust e t =
  match e.node, e.typ, t with
  (* Conversions to integers: we rewrite constants on the fly, or emit a cast. *)
  | EConstant (_, c), _, TInt w -> with_type t (EConstant (w, c))
  | _, _, TInt _ ->
      if e.typ <> t then
        with_type t (ECast (e, t))
      else
        e
  (* Conversions to booleans: we rewrite constants on the fly, or emit `e != 0` *)
  | EConstant (_, "0"), _, TBool -> with_type TBool (EBool false)
  | EConstant (_, "1"), _, TBool -> with_type TBool (EBool true)
  (* Pointer is not null *)
  | _, TBuf _, TBool -> Helpers.mk_neq e (with_type e.typ EBufNull)
  | _, _, TBool ->
      if e.typ <> t then
        match e.typ with
        | TInt w -> Helpers.mk_neq e (Helpers.zero w)
        | _ -> fatal_error "Cannot adjust %a: %a to have type bool" pexpr e ptyp e.typ
      else
        e
  (* Conversions via expected return type of the function (return NULL) *)
  | EBufNull, _, TBuf _ -> with_type t e.node
  (* Array decay in C -- it's ok *)
  | _, TArray (t, _), TBuf (t', _) when t = t' -> e
  (* Casting to const -- also ok *)
  | _, TBuf (t, false), TBuf (t', true) when t = t' -> e
  (* Special handling for slice types *)
  | _, TBuf (t, _), TQualified lid | _, TQualified lid, TBuf (t, _) -> begin
      match LidMap.find_opt lid !type_def_map with
      (* The second case of the when is to handle null pointers *)
      | Some (CSlice (lazy t')) when t = t' || t = TAny ->
          (* Nothing to do, this will be erased at a later phase *)
          e
      | _ ->
          fatal_error "Could not convert expression %a: %a to have type %a" pexpr e ptyp e.typ ptyp
            t
    end
  | _, TTuple [ t1; t2 ], TQualified lid | _, TQualified lid, TTuple [ t1; t2 ] -> begin
      match LidMap.find_opt lid !type_def_map with
      | Some (CTuple (lazy [ (_, (t1', _)); (_, (t2', _)) ])) when t1 = t1' && t2 = t2' ->
          (* Nothing to do, this will be erased at a later phase *)
          e
      | _ ->
          fatal_error "Could not convert expression %a: %a to have type %a" pexpr e ptyp e.typ ptyp
            t
    end
  (* TODO: tag indices *)
  | _ ->
      if e.typ <> t then
        fatal_error "Could not convert expression %a: %a to have type %a" pexpr e ptyp e.typ ptyp t;
      e

let mark_mut_if_variable env e =
  match e.node with
  | EBound i -> (List.nth env.vars i).mut := true
  | _ -> ()

(* A function that behaves like compare, but implements C's notion of rank
   See https://en.cppreference.com/w/c/language/conversion#Integer_promotions *)
let rank (w : Krml.Constant.width) =
  match w with
  | Bool -> 1
  | UInt8 | Int8 -> 8
  | UInt16 | Int16 -> 16
  | UInt32 | Int32 -> 32
  | UInt64 | Int64 -> 64
  | SizeT | PtrdiffT -> 8 * DataModel.size_size
  | _ -> invalid_arg "rank"

(* Deal with various discrepancies between C (arithmetic operations work for pointers, too) vs. krml
   AST (arithmetic operations are distinguished) *)
let mk_binop lhs kind rhs =
  if Krml.Options.debug "BinOp" then
    Krml.KPrint.bprintf "mk_binop: %a: %a { %a } %a: %a\n" pexpr lhs ptyp lhs.typ pop
      (translate_binop kind) pexpr rhs ptyp rhs.typ;

  (* This function first compiles pointer arithmetic *then* defers to this to compile integer
     arithmetic. *)
  let apply_op kind lhs rhs =
    let kind = translate_binop kind in

    (* "Note: integer promotions are applied only (...)

      to the operand of the unary arithmetic operators + and -, TODO
      to the operand of the unary bitwise operator ~, TODO
      to both operands of the shift operators << and >>. " *)
    let lhs, rhs =
      match kind with
      | BShiftL | BShiftR ->
          let integer_promotion w e =
            if rank (Helpers.assert_tint_or_tbool e.typ) < rank w then
              adjust e (TInt w)
            else
              e
          in
          integer_promotion Int32 lhs, integer_promotion UInt32 rhs (* krml wants u32 here *)
      | _ -> lhs, rhs
    in

    if Krml.Options.debug "BinOp" then
      Krml.KPrint.bprintf "After promotions: w=%a lhs=%a, rhs=%a\n" ptyp lhs.typ pexpr lhs pexpr rhs;

    let w, lhs, rhs =
      match kind with
      (* "The arguments of the following arithmetic operators undergo implicit conversions... " *)
      | Mult | Div | Mod | Add | Sub | Lt | Gt | Lte | Gte | Eq | Neq | BAnd | BXor | BOr ->
          let open Krml.Constant in
          let adjust x y = adjust y x in

          (* https://en.cppreference.com/w/c/language/conversion *)
          let wl = Helpers.assert_tint_or_tbool lhs.typ in
          let wr = Helpers.assert_tint_or_tbool rhs.typ in

          (* 3) Otherwise, if one operand is double, double complex, or double imaginary(since C99),
             the other operand is implicitly converted as follows:
             integer or real floating type to double  *)
          if wl = Float64 || wr = Float64 then
            wl, adjust (TInt Float64) lhs, adjust (TInt Float64) rhs
            (* 4) Otherwise, if one operand is float, float complex, or float imaginary(since C99),
             the other operand is implicitly converted as follows: integer type to float (the only
             real type possible is float, which remains as-is) *)
          else if wl = Float32 || wr = Float32 then
            wl, adjust (TInt Float32) lhs, adjust (TInt Float32) rhs
            (* 5) Otherwise, both operands are integers. Both operands undergo integer promotions;
             then, after integer promotion, one of the following cases applies:

             "If the types are the same, that type is the common type. " *)
          else if wl = wr then
            wl, lhs, rhs
            (*  "If the types have the same signedness (both signed or both unsigned), the operand
              whose type has the lesser conversion rank is implicitly converted to the
              other type." *)
          else if is_signed wl = is_signed wr then
            if rank wl < rank wr then
              wr, adjust (TInt wr) lhs, rhs
            else
              wl, lhs, adjust (TInt wl) rhs
          else if
            (* "If the unsigned type has conversion rank greater than or equal to the rank of the
               signed type, then the operand with the signed type is implicitly converted to the
               unsigned type." *)
            is_unsigned wl && rank wl >= rank wr
          then
            wl, lhs, adjust (TInt wl) rhs
          else if is_unsigned wr && rank wr >= rank wl then
            wr, adjust (TInt wr) lhs, rhs
          else
            (* "If the signed type can represent all values of the unsigned type, then the operand
                 with the unsigned type is implicitly converted to the signed type."
                 ^^^ This doesn't happen here -- I presume this is for the case where e.g. long and
                 long long have the same size. *)
            (* "Else, both operands undergo implicit conversion to the unsigned type counterpart of
                 the signed operand's type." *)
            let w =
              if is_signed wl then
                unsigned_of_signed wl
              else
                unsigned_of_signed wr
            in
            w, adjust (TInt w) lhs, adjust (TInt w) rhs
      | _ -> Helpers.assert_tint_or_tbool lhs.typ, lhs, rhs
    in

    (* Krml.KPrint.bprintf "After conversions: w=%a w=%a lhs=%a, rhs=%a\n" *)
    (*   pwidth w *)
    (*   pwidth (Helpers.assert_tint_or_tbool lhs.typ) pexpr lhs pexpr rhs; *)
    match kind with
    | And | Or | Xor | Not ->
        (* Monomorphic boolean operators *)
        let lhs = adjust lhs TBool in
        let rhs = adjust rhs TBool in
        with_type TBool (EApp (Helpers.mk_op kind Bool, [ lhs; rhs ]))
    | _ ->
        (* Width-polymorphic operators *)
        let op = Helpers.mk_op kind w in
        (* Krml.KPrint.bprintf "w=%a, op=%a\n" pwidth w pexpr op; *)
        let t_ret, t_args = Helpers.flatten_arrow op.typ in
        let rhs = adjust rhs (List.nth t_args 1) in

        (* Krml.KPrint.bprintf "Result: %a\n" pexpr (with_type t_ret (EApp (op, [ lhs; rhs ]))); *)
        with_type t_ret (EApp (op, [ lhs; rhs ]))
  in

  (* In case of pointer arithmetic, we need to perform a rewriting into EBufSub/Diff *)
  match lhs.typ, kind with
  | t, Clang.Add when is_tbuf_tarray_tslice t ->
      with_type lhs.typ
        begin
          match lhs.node with
          (* Successive pointer arithmetic operations are likely due to operator precedence, e.g.,
             ptr + n - m parsed as (ptr + n) - m, when ptr + (n - m) might be intended.
             We recognize these cases, and normalize them to perform pointer arithmetic only once
          *)
          | EBufSub (lhs', rhs') ->
              (* (lhs' + rhs') + rhs --> lhs' + (rhs' + rhs) *)
              EBufSub (lhs', adjust (apply_op Add rhs' rhs) (TInt SizeT))
          | EBufDiff (lhs', rhs') ->
              (* JP: I doubt this happens, and if it does, I doubt the code below is correct:
            EBufSub returns a t* but EBufDiff returns a ptrdiff_t. Also C does not allow
            comparing two pointers from different objects... puzzled. To be debugged if the
            assert below triggers. *)
              if true then
                failwith "is this really happening???";
              (* (lhs' - rhs') + rhs --> lhs' + (rhs - rhs') *)
              EBufSub (lhs', adjust (apply_op Sub rhs rhs') (TInt SizeT))
          | _ -> EBufSub (lhs, adjust rhs (TInt SizeT))
        end
  | t, Sub when is_tbuf_tarray_tslice t ->
      with_type lhs.typ
        begin
          match lhs.node with
          | EBufSub (lhs', rhs') ->
              (* (lhs' + rhs') - rhs --> lhs' + (rhs' - rhs) *)
              EBufSub (lhs', adjust (apply_op Sub rhs' rhs) (TInt SizeT))
          | EBufDiff (lhs', rhs') ->
              (* (lhs' - rhs') - rhs --> lhs' - (rhs' + rhs) *)
              EBufDiff (lhs', apply_op Add rhs' rhs)
          | _ -> EBufDiff (lhs, rhs)
        end
  | _, (EQ | NE) when lhs.typ = rhs.typ ->
      let applied_typ = Helpers.fold_arrow [ lhs.typ; rhs.typ ] TBool in
      let poly_op = match kind with EQ -> Krml.Constant.PEq | NE -> PNeq | _ -> assert false in
      let op = with_type applied_typ (EPolyComp (poly_op, lhs.typ)) in
      with_type TBool (EApp (op, [ lhs; rhs ]))
  | _ -> apply_op kind lhs rhs

(* Translate expression [e].

 When adding a case to this function, two questions arise:
 - does the context provide enough information to insert a call to `adjust`? example: translating a
   While node, one must adjust the condition to be bool, because the typing rules of the krml ast
   are different from C
 - are we trusting the type from clang when we shouldn't? (i.e., is it ok to call typ_from_clang) --
   this should generally be avoided, because it is not true that `(translate_expr e).typ =
   typ_from_clang e`. *)
let rec extract_sizeof_ty env = function
  | ArgumentExpr e -> (translate_expr env e).typ
  | ArgumentType ty -> translate_typ ty

and translate_expr (env : env) ?(must_return_value = false) (e : Clang.Ast.expr) : Krml.Ast.expr =
  if is_null e then
    with_type (TBuf (TAny, false)) EBufNull
  else
    match e.desc with
    | IntegerLiteral n -> begin
        match typ_from_clang e with
        | TInt w as t ->
            let signed = K.is_signed w in
            with_type t (EConstant (w, Clang.Ast.string_of_integer_literal ~signed n))
        | TBool ->
            let signed = false in
            with_type TBool (EConstant (Bool, Clang.Ast.string_of_integer_literal ~signed n))
        | t -> fatal_error "integer literal does not have an int type, it has %a" ptyp t
      end
    | FloatingLiteral f -> begin
        match typ_from_clang e with
        | TInt w as t -> with_type t (EConstant (w, Clang.Ast.string_of_floating_literal f))
        | t -> fatal_error "float literal does not have a float type, it has %a" ptyp t
      end
    | StringLiteral _ -> failwith "translate_expr: string literal"
    | CharacterLiteral _ -> failwith "translate_expr character literal"
    | ImaginaryLiteral _ -> failwith "translate_expr: imaginary literal"
    | BoolLiteral _ -> failwith "translate_expr: bool literal"
    | NullPtrLiteral -> failwith "translate_expr: null ptr literal"
    | CompoundLiteral { qual_type; init = { desc = InitList _l; _ } }
      when is_constantarray qual_type ->
        failwith "FIXME: reinstante this case and understand why it was needed"
    (* We handled above the case of array initialization, this should
       be a struct initialization *)
    | CompoundLiteral { init = { desc = InitList l; _ }; _ } | InitList l ->
        translate_fields env (typ_from_clang e) l
    | UnaryOperator { kind = (PostInc | PreInc) as kind; operand } ->
        (* This is a special case for loop increments. The current Karamel
           extraction pipeline only supports a specific case of loops *)
        let o = translate_expr env operand in
        mark_mut_if_variable env o;
        let assignment =
          match o.typ with
          | TInt w ->
              mark_mut_if_variable env o;
              (* We rewrite `name++` into `name := name + 1` *)
              with_type TUnit
              @@ EAssign
                   (o, Krml.Ast.with_type o.typ (EApp (Helpers.mk_op K.Add w, [ o; Helpers.one w ])))
          | TBuf (_t, _) as t_buf ->
              (* We rewrite `name++` into `name := name + 1` *)
              mark_mut_if_variable env o;
              with_type TUnit
              @@ EAssign (o, Krml.Ast.with_type t_buf (EBufSub (o, Helpers.one SizeT)))
          | _ -> failwith "cannot increment this type"
        in
        if not must_return_value then
          assignment
        else if kind = PreInc then
          with_type o.typ (ESequence [ assignment; o ])
        else
          with_type o.typ
            (ELet
               ( Helpers.fresh_binder "old_value" o.typ,
                 o,
                 with_type o.typ
                   (ESequence [ Krml.DeBruijn.lift 1 assignment; with_type o.typ (EBound 0) ]) ))
    | UnaryOperator { kind = (PostDec | PreDec) as kind; operand } ->
        (* This is a special case for loop increments. The current Karamel
           extraction pipeline only supports a specific case of loops *)
        let o = translate_expr env operand in
        mark_mut_if_variable env o;
        let w = Helpers.assert_tint o.typ in
        (* We rewrite `name++` into `name := name + 1` *)
        let assignment =
          with_type TUnit
          @@ EAssign
               (o, Krml.Ast.with_type o.typ (EApp (Helpers.mk_op K.Sub w, [ o; Helpers.one w ])))
        in
        if not must_return_value then
          assignment
        else if kind = PreDec then
          with_type o.typ (ESequence [ assignment; o ])
        else
          with_type o.typ
            (ELet
               ( Helpers.fresh_binder "old_value" o.typ,
                 o,
                 with_type o.typ
                   (ESequence [ Krml.DeBruijn.lift 1 assignment; with_type o.typ (EBound 0) ]) ))
    | UnaryOperator { kind = Not; operand } ->
        (* Bitwise not: ~ syntax, operates on integers *)
        let o = translate_expr env operand in
        with_type o.typ @@ EApp (Helpers.mk_op K.BNot (Helpers.assert_tint o.typ), [ o ])
    | UnaryOperator { kind = LNot; operand } ->
        (* Logical not: The operand should be a boolean *)
        let o = translate_expr env operand in
        Helpers.mk_not (adjust o TBool)
    | UnaryOperator { kind = Minus; operand } ->
        (* No unary minus in krml ast *)
        let o = translate_expr env operand in
        let w = Helpers.assert_tint o.typ in
        with_type o.typ (EApp (Helpers.mk_op Sub w, [ Helpers.zero w; o ]))
    | UnaryOperator { kind = Deref; operand } ->
        let o = translate_expr env operand in
        let t = Helpers.assert_tbuf_or_tarray o.typ in
        with_type t @@ EBufRead (o, Helpers.zero_usize)
    | UnaryOperator { kind = AddrOf; operand } ->
        let o = translate_expr env operand in
        with_type (TBuf (o.typ, false)) (EAddrOf o)
    | UnaryOperator _ ->
        Format.printf "Trying to translate unary operator %a@." Clang.Expr.pp e;
        failwith "translate_expr: unary operator"
    | BinaryOperator { lhs; kind = Assign; rhs } ->
        let rec find_extra_lhs lhss (rhs : expr) =
          match rhs.desc with
          | BinaryOperator { lhs; kind = Assign; rhs } -> find_extra_lhs (lhs :: lhss) rhs
          | _ -> List.rev lhss, rhs
        in
        let lhs, rhs = find_extra_lhs [ lhs ] rhs in
        let lhs = List.map (translate_expr env) lhs in
        let rhs = translate_expr env rhs in
        let assign_one lhs =
          with_type TUnit
            begin
              match lhs.node with
              (* Special-case rewriting for buffer assignments *)
              | EBufRead (base, index) ->
                  let t = assert_tbuf_or_tarray base.typ in
                  EBufWrite (base, index, adjust rhs t)
              | _ ->
                  mark_mut_if_variable env lhs;
                  EAssign (lhs, adjust rhs lhs.typ)
            end
        in
        let assignment =
          if List.length lhs > 1 then
            with_type TUnit (ESequence (List.map assign_one lhs))
          else
            assign_one (Krml.KList.one lhs)
        in
        if not must_return_value then
          assignment
        else
          with_type (List.hd lhs).typ (ESequence [ assignment; List.hd lhs ])
    | BinaryOperator { lhs; kind; rhs } when is_assign_op kind ->
        (* FIXME this is not correct if the lhs is not a value -- consider, for instance:
          int x;
          int *f() { return &x; }
          int main() {
            ( *(f()))++;
            return x;
          }
        *)
        (* Interpreting operations as homogenous *)
        let lhs = translate_expr env lhs in
        let rhs = translate_expr env rhs in
        let kind = assign_to_bop kind in
        let rhs = mk_binop lhs kind rhs in

        with_type TUnit
          begin
            match lhs.node with
            (* Special-case rewriting for buffer assignments *)
            | EBufRead (base, index) ->
                let t = assert_tbuf_or_tarray base.typ in
                EBufWrite (base, index, adjust rhs t)
            | _ ->
                mark_mut_if_variable env lhs;
                EAssign (lhs, adjust rhs lhs.typ)
          end
    | BinaryOperator { lhs; kind; rhs } ->
        let lhs = translate_expr ~must_return_value:true env lhs in
        let rhs = translate_expr ~must_return_value:true env rhs in
        mk_binop lhs kind rhs
    | DeclRef { name; _ } ->
        let name = get_id_name name in
        begin
          match env.self with
          | Some (name', t) when name = name' ->
              with_type t
                (EAbort (Some t, Some ("The definition of " ^ name ^ " refers to itself")))
          | _ ->
              let e, _, _ = find_var env name in
              (* Krml.KPrint.bprintf "%a: %a\n" pexpr e ptyp e.typ; *)
              e
        end
    | Call { callee; args } when is_scylla_reset callee -> begin
        match args with
        | [ e ] -> Helpers.push_ignore (translate_expr env e)
        | _ -> failwith "wrong number of arguments for scylla_reset"
      end
    | Call { callee; args } when is_scylla_split callee -> begin
        match args with
        | [ e; i ] ->
            let e = translate_expr env e in
            (* Sanity-check: The argument should be a pointer *)
            let _ = assert_tbuf_or_tarray e.typ in
            let i = translate_expr env i in
            let split_fn = with_type TAny (EQualified ([ "Pulse"; "Lib"; "Slice" ], "split")) in
            let split_call = with_type TAny (ETApp (split_fn, [], [], [ e.typ ])) in
            with_type (TTuple [ e.typ; e.typ ]) (EApp (split_call, [ e; i ]))
        | _ -> failwith "wrong number of arguments for scylla_split"
      end
    | Call { callee; args } when is_memcpy callee ->
        (* Format.printf "Trying to translate memcpy %a@." Clang.Expr.pp e; *)
        begin
          match args with
          (* We are assuming here that this is __builtin___memcpy_chk.
           This function has a fourth argument, corresponding to the number of bytes
           remaining in dst. We omit it during the translation *)
          | dst :: src :: len :: _ ->
              let dst = translate_expr env dst in
              let src = translate_expr env src in
              if assert_tbuf_or_tarray dst.typ <> assert_tbuf_or_tarray src.typ then
                fatal_error
                  "in this memcpy, source and destination types differ: memcpy(%a: %a, %a: %a, ...)"
                  pexpr dst ptyp dst.typ pexpr src ptyp src.typ;
              let len =
                match len.desc, src.typ with
                | ( BinaryOperator
                      { lhs; kind = Mul; rhs = { desc = UnaryExpr { kind = SizeOf; argument }; _ } },
                    _ ) ->
                    (* We recognize the case `len = lhs * sizeof (_)` *)
                    let len = adjust (translate_expr env lhs) (TInt SizeT) in
                    let ty = extract_sizeof_ty env argument in
                    assert (ty = assert_tbuf_or_tarray dst.typ);
                    len
                | _, TBuf (TInt UInt8, _) ->
                    (* Unless it's a UInt8 in which case we may omit the sizeof *)
                    adjust (translate_expr env len) (TInt SizeT)
                | _ -> fatal_error "ill-formed memcpy; type is %a" ptyp src.typ
              in
              with_type TUnit @@ EBufBlit (src, Helpers.zerou32, dst, Helpers.zerou32, len)
          | _ -> failwith "memcpy does not have the right number of arguments"
        end
    | Call { callee; args } when is_memset callee ->
        (* Format.printf "Trying to translate memset %a@." Clang.Expr.pp e; *)
        begin
          match args with
          | dst :: v :: len :: _ ->
              let dst = translate_expr env dst in
              let len =
                match len.desc, dst.typ with
                (* We recognize the case `len = lhs * sizeof (_)` *)
                | ( BinaryOperator
                      { lhs; kind = Mul; rhs = { desc = UnaryExpr { kind = SizeOf; argument }; _ } },
                    _ ) ->
                    let len = adjust (translate_expr env lhs) (TInt SizeT) in
                    let ty = extract_sizeof_ty env argument in
                    assert (ty = assert_tbuf_or_tarray dst.typ);
                    len
                | _, TBuf (TInt UInt8, _) ->
                    (* Unless it's a UInt8 in which case we may omit the sizeof *)
                    adjust (translate_expr env len) (TInt SizeT)
                | _ -> failwith "ill-formed memset"
              in
              let elt = adjust (translate_expr env v) (assert_tbuf_or_tarray dst.typ) in
              with_type TUnit @@ EBufFill (dst, elt, len)
          | _ -> failwith "memset does not have the right number of arguments"
        end
    | Call { callee; args } when is_free callee -> begin
        match args with
        | [ ptr ] -> with_type TUnit @@ EBufFree (translate_expr env ptr)
        | _ -> failwith "ill-formed free: too many arguments"
      end
    | Call { callee; _ } when is_exit callee ->
        (* TODO: We should likely check the exit code, and possibly translate this to
           std::process::exit.
           However, std::process:exit immediately terminates the process and does not
           run destructors. As it is likely used as an abort in our usecases, we instead
           translate it to EAbort, which will become a `panic` *)
        with_type TAny (EAbort (None, Some ""))
    | Call { callee; args } ->
        (* Format.printf "Trying to translate function call %a@." Clang.Expr.pp callee; *)
        let callee = translate_expr env callee in
        (* Krml.KPrint.bprintf "callee is %a and has type %a\n" pexpr callee ptyp callee.typ; *)
        let t, ts =
          Helpers.flatten_arrow
            (match callee.typ with
            | TBuf (t, _) -> t
            | t -> t)
        in
        let args = List.map2 (fun x t -> adjust (translate_expr env x) t) args ts in
        with_type t (EApp (callee, args))
    | Cast { qual_type; operand; _ } ->
        (* Format.printf "Cast %a@."  Clang.Expr.pp e; *)
        let typ = translate_typ qual_type in
        let e = translate_expr env operand in
        with_type typ (ECast (e, typ))
    | ArraySubscript { base; index } ->
        let base = translate_expr env base in
        let index = adjust (translate_expr env ~must_return_value:true index) (TInt SizeT) in
        (* Is this only called on rvalues? Otherwise, might need EBufWrite *)
        with_type (assert_tbuf_or_tarray base.typ) (EBufRead (base, index))
    | ConditionalOperator { cond; then_branch; else_branch } ->
        let cond = translate_expr env cond in
        let else_branch = translate_expr env else_branch in
        let then_branch =
          match then_branch with
          | None ->
              assert (else_branch.typ = TUnit);
              Helpers.eunit
          | Some e -> adjust (translate_expr env e) else_branch.typ
        in
        with_type else_branch.typ (EIfThenElse (cond, then_branch, else_branch))
    | Paren _ -> failwith "translate_expr: paren"
    | Member { base; arrow; field } ->
        let base =
          match base with
          | None -> failwith "field accesses without a base expression are not supported"
          | Some b -> b
        in
        let base = translate_expr env base in

        let lid =
          Helpers.assert_tlid
            (if arrow then
               Helpers.assert_tbuf base.typ
             else
               base.typ)
        in

        let f =
          match field with
          | FieldName { desc; _ } -> get_id_name desc.name
          | _ -> failwith "member node: only field accesses supported"
        in

        begin
          match LidMap.find_opt lid !type_def_map with
          | Some (CVariant lazy_branches) ->
              let branch =
                match List.find_opt (fun b -> fst b = f) (Lazy.force lazy_branches) with
                | Some b -> b
                | None -> fatal_error "Field %s of %a not found in tagged union" f plid lid
              in

              begin
                match snd branch with
                | [ _ ] ->
                    let var =
                      match base.node with
                      | EBound n -> List.nth env.vars n
                      | _ -> failwith "Tagged union access is only supported on a variable"
                    in
                    begin
                      match var.case with
                      | Some { case; var } when case = f ->
                          let e, _, _ = find_var env var in
                          e
                      | _ -> failwith "Tagged union variable is not in the correct case"
                    end
                | _ -> failwith "More than one field in tagged union case"
              end
          | Some (CFlat lazy_fields) ->
              let fields = Lazy.force lazy_fields in
              let field_t =
                if List.mem_assoc (Some f) fields then
                  fst (List.assoc (Some f) fields)
                else
                  fatal_error "Field %s of %a not found in struct def (available fields are: %s)" f
                    plid lid
                    (String.concat ", "
                       (List.map (fun (f, _) -> Option.value ~default:"<noname>" f) fields))
              in
              if not arrow then
                (* base.f *)
                with_type field_t (EField (base, f))
              else
                (* base->f *)
                let deref_base =
                  Helpers.(with_type (assert_tbuf base.typ) (EBufRead (base, Helpers.zero_usize)))
                in
                with_type field_t (EField (deref_base, f))
          | Some (CTuple lazy_fields) ->
              if arrow then
                fatal_error "Arrow access on tuple struct not supported";
              let fields = Lazy.force lazy_fields in
              let field_t =
                if List.mem_assoc (Some f) fields then
                  fst (List.assoc (Some f) fields)
                else
                  fatal_error "Field %s of %a not found in struct def (available fields are: %s)" f
                    plid lid
                    (String.concat ", "
                       (List.map (fun (f, _) -> Option.value ~default:"<noname>" f) fields))
              in
              (* Retrieve the position of the field in the tuple *)
              let idx = Krml.KList.index (fun x -> fst x = Some f) fields in

              let binder = Helpers.fresh_binder "v" field_t in
              let tuple_pat =
                PTuple
                  (List.mapi
                     (fun i (_, (t, _)) ->
                       Krml.Ast.with_type t
                         (if i = idx then
                            PBound 0
                          else
                            PWild))
                     fields)
              in
              let tuple_branch =
                ( [ binder ],
                  Krml.Ast.with_type field_t tuple_pat,
                  Krml.Ast.with_type field_t (EBound 0) )
              in
              Krml.Ast.with_type field_t (EMatch (Unchecked, base, [ tuple_branch ]))
          | Some (CSlice t) ->
              let t = Lazy.force t in
              if arrow then
                fatal_error "Arrow access on slice struct not supported";
              if f = "elt" then
                base
              else if f = "len" then
                let len_fn =
                  Krml.Ast.with_type TAny (EQualified ([ "Pulse"; "Lib"; "Slice" ], "len"))
                in
                let len_call =
                  Krml.Ast.with_type TAny (ETApp (len_fn, [], [], [ TBuf (t, false) ]))
                in
                Krml.Ast.with_type (TInt K.SizeT) (EApp (len_call, [ base ]))
              else
                fatal_error "Field %s of slice %a is not elt or len" f plid lid
          | Some _ ->
              fatal_error
                "Taking a field of %a which is not a struct, tuple, slice, nor a tagged union" plid
                lid
          | None -> fatal_error "Taking a field of %a which is not in the map" plid lid
        end
    | UnaryExpr { kind = SizeOf; argument; _ } -> begin
        match extract_sizeof_ty env argument with
        | TInt w -> Helpers.mk_sizet (Krml.Constant.bytes_of_width w)
        | _ ->
            Format.printf "Trying to translate unary expr %a@." Clang.Expr.pp e;
            failwith "translate_expr: unary expr"
      end
    | _ ->
        Format.eprintf "Trying to translate expression %a@." Clang.Expr.pp e;
        failwith "translate_expr: unsupported expression"

and translate_field_expr env (e : expr) field_name =
  match e.desc with
  | DesignatedInit { designators; init } -> begin
      match designators with
      | [ FieldDesignator name ] ->
          (* FIXME -- adjust type against expected field type, obtained via a lookup in
             struct_map *)
          let e = translate_expr env init in
          if name <> field_name then
            failwith "TODO: out-of-order fields in a designated initializer";
          Some name, e
      | [ _ ] -> failwith "expected a field designator"
      | _ -> failwith "assigning to several fields during struct initialization is not supported"
    end
  | _ -> Some field_name, translate_expr env e

and translate_variant env branches (tag : expr) (e : expr option) =
  let tag =
    match tag.desc with
    | DesignatedInit { init = { desc = IntegerLiteral n; _ }; _ } -> Clang.Ast.int_of_literal n
    | _ ->
        Format.eprintf "Expected integer literal for tagged union tag, got %a\n@." Clang.Expr.pp tag;
        failwith "Could not translate tagged union expression"
  in
  if tag >= List.length branches then
    fatal_error "tag is greater than number of variants";
  let name, _ = List.nth branches tag in
  match e with
  (* This is the case of an empty variant *)
  | None -> ECons (name, [])
  | Some e -> (
      match e.desc with
      | InitList [ { desc = DesignatedInit { designators = [ FieldDesignator f ]; init }; _ } ]
      (* Not sure why this case is needed, but there seems to occassionally be an empty FieldDesignator inserted
           during parsing, and the designatedInit is not encapsulated in an InitList *)
      | DesignatedInit { designators = [ FieldDesignator ""; FieldDesignator f ]; init } ->
          if f <> name then
            failwith "incorrect variant type for tagged union";
          let e = translate_expr env init in
          ECons (name, [ e ])
      | _ ->
          Format.eprintf "Expected initializer, got %a\n@." Clang.Expr.pp e;
          failwith "Incorrect expression for tagged union")

and translate_fields env t es =
  match LidMap.find (Helpers.assert_tlid t) !type_def_map with
  | CFlat lazy_fields ->
      let fields = Lazy.force lazy_fields in
      let field_names = List.map (fun x -> Option.get (fst x)) fields in
      if List.length field_names <> List.length es then
        fatal_error "TODO: partial initializers (%s but %d initializers)"
          (String.concat ", " field_names) (List.length es);
      Krml.Ast.with_type t (EFlat (List.map2 (translate_field_expr env) es field_names))
  | CVariant lazy_branches ->
      let branches = Lazy.force lazy_branches in
      begin
        match es with
        | [ tag ] -> Krml.Ast.with_type t (translate_variant env branches tag None)
        | [ tag; e ] -> Krml.Ast.with_type t (translate_variant env branches tag (Some e))
        | _ -> fatal_error "Expected two arguments for tagged union initializer"
      end
  | CTuple lazy_fields ->
      let fields = Lazy.force lazy_fields in
      let field_names = List.map (fun x -> Option.get (fst x)) fields in
      if List.length field_names <> List.length es then
        fatal_error "TODO: partial initializers (%s but %d initializers)"
          (String.concat ", " field_names) (List.length es);
      (* We go through translate_field_expr to ensure that the order of the
           fields matches the initializers *)
      let fields = List.map2 (translate_field_expr env) es field_names in
      Krml.Ast.with_type t (ETuple (List.map snd fields))
  | CSlice _ ->
      if List.length es <> 2 then
        fatal_error "Expected two initializers for slice type for fields elt and len";
      (* Ensuring the right order and names of initializers *)
      let fields = List.map2 (translate_field_expr env) es [ "elt"; "len" ] in
      snd (List.hd fields)
  | _ -> failwith "impossible"

let is_tag_check env (cond : expr) =
  match cond.desc with
  | BinaryOperator
      {
        lhs =
          {
            desc =
              Member
                {
                  base = Some { desc = DeclRef { name; _ }; _ };
                  arrow = false;
                  field = FieldName { desc; _ };
                };
            _;
          };
        kind = EQ;
        rhs = { desc = IntegerLiteral _; _ };
      } -> (
      (* We assume that the tag field will always be called "tag" *)
      get_id_name desc.name = "tag"
      &&
      (* And we check whether the variable has been registered as a tagged union *)
      let var, _, _ = get_id_name name |> find_var env in
      let lid = Helpers.assert_tlid var.typ in
      match LidMap.find_opt lid !type_def_map with
      | Some (CVariant _) -> true
      | _ -> false)
  | _ -> false

let deconstruct_tag_check env (cond : expr) =
  match cond.desc with
  | BinaryOperator
      {
        lhs = { desc = Member { base = Some { desc = DeclRef { name; _ }; _ }; _ }; _ };
        kind = EQ;
        rhs = { desc = IntegerLiteral (Int n); _ };
      } ->
      let name = get_id_name name in
      let e, _, _ = find_var env name in
      e, n, name
  | _ -> failwith "not a tag_check"

(* Assuming that [lid] corresponds to a tagged union type, which was
   therefore translated to a variant type, retrieves the branch
   corresponding to the [n]-th constructor (starting count at 0) *)
let lookup_nth_branch lid n =
  match LidMap.find_opt lid !type_def_map with
  | Some (CVariant lazy_branches) -> List.nth (Lazy.force lazy_branches) n
  | _ -> fatal_error "Expected a tagged union expression"

(* Create a default value associated to a given type [typ] *)
let create_default_value typ =
  match typ with
  | _ -> Krml.Ast.with_type typ EAny

(* Translate a variable declaration, returning an updated environment, the binder `b` and the body of
   the let `e1`, in order to support creating `ELet (b, e1, ...)` *)
let translate_vardecl (env : env) (vdecl : var_decl_desc) : env * binder * Krml.Ast.expr =
  let vname = vdecl.var_name in
  let typ = translate_typ vdecl.var_type in
  match vdecl.var_init with
  | None ->
      (* If there is no associated definition, we attempt to craft
           a default initialization value *)
      add_var env (vname, typ), Helpers.fresh_binder vname typ, create_default_value typ
  | Some { desc = InitList l; _ } when is_constantarray vdecl.var_type ->
      (* Intializing a constant array with a list of elements.
         For instance, uint32[2] = { 0 };
      *)
      let size, size_e = extract_constarray_size vdecl.var_type in
      if List.length l = 1 then
        (* One element initializer, possibly repeated *)
        let e = translate_expr env (List.hd l) in
        let e = adjust e (assert_tbuf_or_tarray typ) in
        (* TODO: Arrays are not on stack if at top-level *)
        ( add_var env (vname, typ),
          Helpers.fresh_binder vname typ,
          Krml.Ast.with_type typ (EBufCreate (Krml.Common.Stack, e, size_e)) )
      else (
        assert (List.length l = size);
        let ty = assert_tbuf_or_tarray typ in
        let es = List.map (fun e -> adjust (translate_expr env e) ty) l in
        ( add_var env (vname, typ),
          Helpers.fresh_binder vname typ,
          Krml.Ast.with_type typ (EBufCreateL (Krml.Common.Stack, es)) ))
  | Some { desc = InitList l; _ } ->
      (* Initializing a struct value.
         TODO: We should check that the declaration type indeed corresponds to a struct type *)
      add_var env (vname, typ), Helpers.fresh_binder vname typ, translate_fields env typ l
  | Some { desc = Call { callee; args }; _ }
  | Some { desc = Cast { operand = { desc = Call { callee; args }; _ }; _ }; _ }
    when is_calloc callee -> begin
      (* There commonly is a cast around calloc to the type of the variable. We omit it when translating it to Rust,
         as the allocation will be typed *)
      match args with
      | [ len; { desc = UnaryExpr { kind = SizeOf; argument }; _ } ] ->
          let len = adjust (translate_expr env len) (TInt SizeT) in
          (* Sanity check: calloc is of the right type *)
          let ty = Helpers.assert_tbuf typ in
          assert (extract_sizeof_ty env argument = ty);
          let w = Helpers.assert_tint ty in
          ( add_var env (vname, typ),
            Helpers.fresh_binder vname typ,
            Krml.Ast.with_type typ (EBufCreate (Krml.Common.Heap, Helpers.zero w, len)) )
      | _ -> failwith "calloc is expected to have two arguments"
    end
  | Some { desc = DeclRef { name; _ }; _ } ->
      let var, _, _ = get_id_name name |> find_var env in
      let e =
        match typ with
        (* If we have a statement of the shape `let x = y` where y is a pointer,
         this likely corresponds to taking a slice of y, starting at index 0.
         We need to explicitly insert the EBufSub node to create a split tree *)
        | TBuf _ | TArray _ -> with_type typ (EBufSub (var, Helpers.zero_usize))
        | _ -> var
      in
      add_var env (vname, typ), Helpers.fresh_binder vname typ, e
  | Some e ->
      (* TODO insert call to adjust here *)
      ( add_var env (vname, typ),
        Helpers.fresh_binder vname typ,
        translate_expr (add_self env (vname, typ)) e )

(* Translation of a variable declaration, followed by a memset of [args] *)
let translate_vardecl_with_memset (env : env) (vdecl : var_decl_desc) (args : expr list) :
    env * binder * Krml.Ast.expr =
  (* TODO: We should not hard-fail when this does not correspond to an array decl initialized
     by the following memset.
     Instead, we should just translate the vardecl, and let translate_stmt translate the
     second statement *)
  let vname = vdecl.var_name in
  let typ, size =
    match vdecl.var_type.desc with
    | VariableArray { element; size } ->
        TBuf (translate_typ element, false), translate_expr env size
    | ConstantArray { element; size_as_expr; _ } ->
        let size =
          match size_as_expr with
          | None -> failwith "Length of constant array is not an expr"
          | Some size -> adjust (translate_expr env size) (TInt SizeT)
        in
        TBuf (translate_typ element, false), size
    | _ -> failwith "The variable being memset is not a constantArray or variableArray"
  in
  match args with
  | dst :: v :: len :: _ ->
      (* Check that the destination is the variable declared just before *)
      begin
        match dst.desc with
        | DeclRef { name; _ } when get_id_name name = vname -> ()
        | _ -> failwith "not calling memset on the variable that was just declared"
      end;
      (* Checking that we are initializing the entire array *)
      let len =
        match len.desc with
        | BinaryOperator
            { lhs; kind = Mul; rhs = { desc = UnaryExpr { kind = SizeOf; argument }; _ } }
          when extract_sizeof_ty env argument = Helpers.assert_tbuf typ -> lhs
        | _ -> failwith "memset length is not of the shape `N * sizeof(ty)`"
      in
      let v = translate_expr env v in
      let len = translate_expr env len in
      (* Types might have been inferred differently, we only compare the expressions *)
      if
        match len.node, size.node with
        | EConstant (_, c1), EConstant (_, c2) -> c1 = c2
        | _ -> len.node = size.node
      then
        let len = adjust len (TInt SizeT) in
        let v = adjust v (Helpers.assert_tbuf typ) in
        ( add_var env (vname, typ),
          Helpers.fresh_binder vname typ,
          Krml.Ast.with_type typ (EBufCreate (Krml.Common.Stack, v, len)) )
      else
        fatal_error "length of memset %a does not match declared length of array %a" pexpr len pexpr
          size
  | _ -> failwith "memset does not have the right number of arguments"

(* This function translates `t *x = (t* ) malloc(...); stmts...` to krml's internal representation of heap
   allocations, which requires an initial value. This function thus assumes `is_malloc_vdecl vdecl <> None`.
   The cast `(t* )` is optional. 

   We recognize a few distinguished patterns:
   - if the first statement is `if (x != NULL) { x[0] = e_init } else { ... }`, then `e_init` is a
     suitable initial value; the user then will need to tweak the produced Rust code if there was
     some meaningful error handling in the `else` branch
   - for other cases, we try to come up with a default value, and error out otherwise

   The function returns the remainder of the statements to be translated. *)
let translate_vardecl_malloc (env : env) (vdecl : var_decl_desc) (s : stmt list) :
    env * binder * Krml.Ast.expr * stmt list =
  let vname = vdecl.var_name in

  (* Assert that the variable has a pointer type *)
  let typ =
    match vdecl.var_type.desc with
    | Pointer ty -> TBuf (translate_typ ty, false)
    | _ -> failwith ("The variable being malloc'ed is not a pointer: " ^ vname)
  in

  (* Analyze the argument to malloc.
     - `sizeof(t)`, `sizeof(x[0])`: size = 1
     - `e * sizeof(t)`, `e * sizeof(x[0])`: size = e 
     - `e` when `sizeof(t)` is known statically (e.g. `t = TInt w`): size = `e/sizeof(t)` (TODO)
  *)
  let n_elements =
    let env = add_self env (vname, typ) in

    let is_correct_sizeof (x: expr) =
      match x with
      | { desc = UnaryExpr { kind = SizeOf; argument }; _ } ->
          (* Sanity-check: The sizeof argument correponds to the type of the pointer being malloc'ed *)
          assert (extract_sizeof_ty env argument = Helpers.assert_tbuf typ);
          true
      | _ ->
          false
    in

    match Krml.KList.one (Option.get (is_malloc_vdecl vdecl)) with
    | e when is_correct_sizeof e ->
        Krml.Helpers.mk_sizet 1
    | { desc = BinaryOperator { lhs; kind = Mul; rhs }; _ } when is_correct_sizeof rhs ->
        translate_expr env lhs
    | { desc = BinaryOperator { lhs; kind = Mul; rhs }; _ } when is_correct_sizeof lhs ->
        translate_expr env rhs
    | _ ->
        failwith ("argument of malloc if not of the shape `sizeof(type)` or `e * sizeof(type)` for " ^ vname)
  in

  (* Try to find a default value; fallback to synthesizing one, if the type permits. *)
  let init_val, rest =
    match s, Helpers.assert_tbuf typ with
    | { desc = If { cond; then_branch = { desc =
      Compound [ { desc = Expr { desc = BinaryOperator { lhs; kind = Assign; rhs }; _ }; _ } ]; _
    }; _ }; _ } :: s, _
    when is_null_check vname cond && is_zero_access lhs vname ->
        (* if (vname != NULL) { vname[0] = init_val; } else { ... } *)
        translate_expr env rhs, s
    | _, TInt w ->
        with_type typ (EConstant (w, "0")), s
    | _ ->
        fatal_error "cannot find a default value of type %a for malloc, when initializing %s" ptyp typ vname
  in

  let init_val = adjust init_val (Helpers.assert_tbuf typ) in

  ( add_var env (vname, typ),
    Helpers.fresh_binder vname typ,
    Krml.Ast.with_type typ (EBufCreate (Krml.Common.Heap, init_val, n_elements)),
    rest )

let maybe_align attributes (b : binder) =
  match Attributes.retrieve_alignment attributes with
  | Some n -> { b with node = { b.node with meta = Align n :: b.node.meta } }
  | None -> b

(* Same as translate_expr: we try to avoid relying on Clang-provided type information as much as
   possible *)
let rec translate_stmt (env : env) (s : Clang.Ast.stmt_desc) : Krml.Ast.expr =
  match s with
  (* This is a null statement, not a null pointer. It corresponds to a no-op *)
  | Null -> Helpers.eunit
  | Compound l -> begin
      match l with
      | [] -> Helpers.eunit
      | [ { desc = Decl [ { desc = Var vdecl; _ } ]; _ } ] ->
          let _, b, e = translate_vardecl env vdecl in
          with_type TUnit (ELet (maybe_align vdecl.attributes b, e, Helpers.eunit))
      | [ stmt ] -> translate_stmt env stmt.desc
      | hd :: tl -> begin
          match hd.desc, (List.hd tl).desc with
          (* Special case when we have a variable declaration followed by a
             memset: this likely corresponds to an array initialization *)
          | Decl [ { desc = Var vdecl; _ } ], Expr { desc = Call { callee; args }; _ }
            when is_memset callee ->
              let env', b, e = translate_vardecl_with_memset env vdecl args in
              let e2 = translate_stmt env' (Compound (List.tl tl)) in
              with_type e2.typ (ELet (maybe_align vdecl.attributes b, e, e2))
          (* We have a few special cases for `malloc`, hoisted in a separate function. *)
          | Decl [ { desc = Var vdecl; _ } ], _ when is_malloc_vdecl vdecl <> None ->
              let env', b, e, rest = translate_vardecl_malloc env vdecl tl in
              let e2 = translate_stmt env' (Compound rest) in
              with_type e2.typ (ELet (maybe_align vdecl.attributes b, e, e2))
          (* Regular variable declaration case *)
          | Decl ds, _ ->
              let rec translate_one_decl env (decls : decl list) =
                match decls with
                | { desc = Var vdecl; _ } :: decls ->
                    let env, b, e = translate_vardecl env vdecl in
                    (* TODO: analysis that figures out what needs to be mut *)
                    let e2 = translate_one_decl env decls in
                    let b =
                      if !((List.hd env.vars).mut) then
                        Helpers.mark_mut b
                      else
                        b
                    in
                    with_type e2.typ (ELet (maybe_align vdecl.attributes b, adjust e b.typ, e2))
                | _ :: _ -> failwith "This decl is not a var declaration"
                | [] -> translate_stmt env (Compound tl)
              in
              translate_one_decl env ds
          | stmt, _ ->
              let s = translate_stmt env stmt in
              let e2 = translate_stmt (add_var env ("_", s.typ)) (Compound tl) in
              if s.typ = TUnit then
                with_type e2.typ (ELet (Helpers.sequence_binding (), s, e2))
              else
                with_type e2.typ (ELet (Helpers.fresh_binder "_ignored_stmt" s.typ, s, e2))
        end
    end
  | For { init; condition_variable; cond; inc; body } ->
      assert (condition_variable = None);
      begin
        match init, cond, inc with
        | Some { desc = Decl [ { desc = Var vdecl; _ } ]; _ }, Some cond, Some inc ->
            let env, b, init = translate_vardecl env vdecl in
            let b = Helpers.mark_mut b in
            (* Cannot use type_of_expr cond here since C uses `int` but we want bool *)
            let cond = adjust (translate_expr env cond) TBool in
            let inc = translate_stmt env inc.desc in
            let body = translate_stmt env body.desc in
            with_type TUnit (EFor (b, init, cond, inc, body))
        | _ ->
            let init =
              match init with
              | None -> Helpers.eunit
              | Some init -> translate_stmt env init.desc
            in
            let cond =
              match cond with
              | None -> Helpers.etrue
              | Some cond -> translate_expr env cond
            in
            let inc =
              match inc with
              | None -> Helpers.eunit
              | Some inc -> translate_stmt env inc.desc
            in
            let body = translate_stmt env body.desc in
            with_type TUnit
              (ESequence
                 [
                   init;
                   with_type TUnit
                     (EWhile (adjust cond TBool, with_type TUnit (ESequence [ body; inc ])));
                 ])
      end
  | ForRange _ -> failwith "translate_stmt: for range"
  (* There is no null pointer in Rust. We remove branching based on null-pointer
     comparisons *)
  | If { cond = { desc = BinaryOperator { lhs; kind = EQ; rhs }; _ }; else_branch; _ }
    when has_pointer_type lhs && is_null rhs -> begin
      match else_branch with
      | None -> Helpers.eunit
      | Some s -> translate_stmt env s.desc
    end
  | If { cond = { desc = BinaryOperator { lhs; kind = NE; rhs }; _ }; then_branch; _ }
    when has_pointer_type lhs && is_null rhs -> translate_stmt env then_branch.desc
  (* We recognize here patterns of the shape `if x.tag == i`, when x is
     a variable whose type `typ` was annotated with the scylla_adt attribute.
     This type was previously checked to be a tagged union, with shape
     ```
     { int tag;
       union {
        t0 case0;
        t1 case1;
        ...
        tn casen;
      }
    }
    ```

    and translated to the ADT
    ```
    case0 { v: t0 },
    case1 { v: t1 },
    ...
    casen { v : tn }
    ```

    We translate the if/then/else to
    `match x with | casei { v } -> then_branch | _ -> else_branch`,

    Inside the then_branch, we will track that the tagged union x is currently
    in the `casei` state, and will replace all occurences of x.casei
    by the variable v, which is the payload of the casei constructor.
    All occurences of x.casej where j is different from i will raise
    an error.

    Inside the else branch, we have no information about the state of the
    tagged union, and will therefore raise an error for any x.casej access.
  *)
  | If { cond; then_branch; else_branch; _ } when is_tag_check env cond ->
      let var, variant, varname = deconstruct_tag_check env cond in

      let lid = Helpers.assert_tlid var.typ in
      let case, fs = lookup_nth_branch lid variant in

      begin
        match fs with
        | [] ->
            (* This case corresponds to a constructor with an empty payload,
             specified through the [empty_variant_attr] attribute *)
            let then_e = translate_stmt env then_branch.desc in
            let else_e =
              match else_branch with
              | None -> Helpers.eunit
              (* We translate the else branch with the old environment, without
               adding a binder for the pattern *)
              | Some el -> translate_stmt env el.desc
            in
            let t =
              match then_e.typ, else_e.typ with
              | TAny, t -> t
              | t, TAny -> t
              | _ -> then_e.typ
            in

            let then_branch = [], Krml.Ast.with_type t (PCons (case, [])), then_e in
            let else_branch = [], Krml.Ast.with_type t PWild, else_e in
            Krml.Ast.with_type t (EMatch (Unchecked, var, [ then_branch; else_branch ]))
        | [ (name, (case_t, _)) ] ->
            let binder = Helpers.fresh_binder name case_t in
            let pat = Krml.Ast.with_type case_t (PBound 0) in
            (* We need to add the new binder to the environment before translating
             the branches.
             We also need to be able to later retrieve the content of the variant
             constructor when accessing the corresponding field.
             To do so, we store in the environment a variable called v!!{atom},
             where {atom} is replaced by the binder's atom.
             We then update the environment for `var` to store that it is a tagged
             union in case `case`, and that the corresponding content is ``v!!{atom}`.
             Importantly, `v!!{atom}` is not a valid variable name, and therefore does
             not conflict with existing variables.
          *)
            let env_binder_name = binder.node.name ^ "!!" ^ show_atom_t binder.node.atom in
            let new_env = add_var env (env_binder_name, case_t) in
            let new_env = refine_var_case new_env varname (Some { case; var = env_binder_name }) in

            (* We only change the state of the tagged union case to translate the if branch,
             which is the one where we checked the tag of the variable *)
            (* TODO: Should we sanity-check that old is None? As, if we are already in
             a tagged union case, there is no need for rechecking the tag? *)
            let then_e = translate_stmt new_env then_branch.desc in

            let else_e =
              match else_branch with
              | None -> Helpers.eunit
              (* We translate the else branch with the old environment, without
               adding a binder for the pattern *)
              | Some el -> translate_stmt env el.desc
            in
            let t =
              match then_e.typ, else_e.typ with
              | TAny, t -> t
              | t, TAny -> t
              | _ -> then_e.typ
            in

            let then_branch = [ binder ], Krml.Ast.with_type t (PCons (case, [ pat ])), then_e in
            let else_branch = [], Krml.Ast.with_type t PWild, else_e in
            Krml.Ast.with_type t (EMatch (Unchecked, var, [ then_branch; else_branch ]))
        | _ -> failwith "Tagged union variant has more than one value in payload"
      end
  | If { init; condition_variable; cond; then_branch; else_branch } ->
      (* These two fields should be specific to C++ *)
      assert (init = None);
      assert (condition_variable = None);
      let cond = adjust (translate_expr env cond) TBool in
      let then_b = translate_stmt env then_branch.desc in
      let else_b =
        match else_branch with
        | None -> Helpers.eunit
        | Some el -> translate_stmt env el.desc
      in
      let t =
        match then_b.typ, else_b.typ with
        | TAny, t -> t
        | t, TAny -> t
        | _ -> then_b.typ
      in
      with_type t (EIfThenElse (cond, then_b, else_b))
  | Switch { init; condition_variable; cond; body } ->
      (* C++ constructs *)
      assert (init = None);
      assert (condition_variable = None);

      let cond = translate_expr env cond in

      (* TODO most likely adjust *)
      let branches = translate_branches env cond.typ body.desc in
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
      if is_trivial_false cond then
        body
      else
        with_type TUnit (ESequence [ body; Krml.Ast.with_type TUnit (EWhile (cond, body)) ])
  | Label _ -> failwith "translate_stmt: label"
  | Goto _ -> failwith "translate_stmt: goto"
  | IndirectGoto _ -> failwith "translate_stmt: indirect goto"
  | Continue -> with_type TUnit EContinue
  | Break -> with_type TUnit EBreak
  | Asm _ -> failwith "translate_stmt: asm"
  | Return eo ->
      with_type TAny
        (match eo with
        | None -> EReturn Helpers.eunit
        | Some e -> EReturn (adjust (translate_expr env e) env.ret_t))
  | Decl _ -> failwith "translate_stmt: decl"
  | Expr e -> translate_expr env e
  | Try _ -> failwith "translate_stmt: try"
  | AttributedStmt _ -> failwith "translate_stmt: AttributedStmt"
  | UnknownStmt _ -> failwith "translate_stmt: UnknownStmt"

(* Translate case and default statements inside a switch to a list of branches for
   structured pattern-matching.
   The original C branches must consist of a list of `case` statements, terminated by
   a `default` statement.
   [t] corresponds to the type of the expression we are pattern-matching on, to
   direct the translation
   *)
and translate_branches (env : env) (t : typ) (s : stmt_desc) : Krml.Ast.branches =
  match s with
  | Compound [ { desc = Default body; _ } ] ->
      let body = translate_stmt env body.desc in
      (* The last case is a fallback, the pattern corresponds to a wildcard *)
      [ [], Krml.Ast.with_type TAny PWild, body ]
  | Compound ({ desc = Case { lhs; rhs; body }; _ } :: tl) ->
      (* Unsupported GCC extension *)
      assert (rhs = None);
      let pat = adjust (translate_expr env lhs) t in
      let body = translate_stmt env body.desc in
      (* We only support pattern-matching on constants here.
         This allows to translate switches corresponding to pattern
         matching on a tagged union *)
      begin
        match pat.node with
        | EConstant n -> [], Krml.Ast.with_type pat.typ (PConstant n), body
        | _ -> failwith "Only constant patterns supported"
      end
      :: translate_branches env t (Compound tl)
  | _ -> failwith "Ill-formed switch branches: Expected a case or a default"

let translate_param (p : parameter) : binder =
  let p = p.desc in
  let typ = translate_typ p.qual_type in
  (* Not handling default expressions for function parameters *)
  assert (p.default = None);
  Helpers.fresh_binder p.name typ

let translate_params (fdecl : function_decl) =
  match fdecl.function_type.parameters with
  | None -> []
  | Some params ->
      (* Not handling variadic parameters *)
      assert (not params.variadic);
      List.map translate_param params.non_variadic

let translate_fundecl (fdecl : function_decl) =
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
      let flags =
        if fdecl.inline_specified then
          [ Krml.Common.Inline ]
        else
          []
      in
      let lid = Option.get (lid_of_ordinary_name name) in
      let binders =
        List.map2
          (fun (b : binder) { mut; _ } ->
            let m = !mut in
            { b with node = { b.node with mut = m } })
          binders (List.rev env.vars)
      in

      if Attributes.has_always_inline fdecl.attributes then
        attributes_map := add_to_list_lid lid (Printf.sprintf "inline(always)") !attributes_map;

      let decl = Krml.Ast.(DFunction (None, flags, 0, 0, ret_type, lid, binders, body)) in
      (* Krml.KPrint.bprintf "Resulting decl %a\n" Krml.PrintAst.pdecl decl; *)
      Some decl

(* Translate a field declaration inside a struct type declaration *)
let translate_field (decl : decl) =
  match decl.desc with
  | Field { name; qual_type; bitwidth; init; attributes } ->
      (* Sanity-checks for unsupported features *)
      assert (bitwidth = None);
      assert (init = None);
      (* TODO: what do we want to do if there attributes, like alignment? *)
      assert (attributes = [] || true);
      (* TODO: do not mark all fields as mutable by default? *)
      Some name, (translate_typ qual_type, true)
  | _ -> failwith "Struct declarations should only contain fields"

(* Translate a union field to a variant *)
let translate_variant (decl : decl) : Krml.Ast.branch_t =
  let name, t_mut = translate_field decl in
  Option.get name, [ "v", t_mut ]

(* Translate a union field into variant branches.
   [empty_ctr_names] corresponds to a list of additional
   constructors with no payload, appended at the end of the
   datatype.
 *)
let translate_field_union (decl : decl) (empty_ctr_names : string list) =
  match decl.desc with
  | RecordDecl { keyword = Union; fields; _ } ->
      let branches = List.map translate_variant fields in
      let empty_ctrs = List.map (fun s -> s, []) empty_ctr_names in
      branches @ empty_ctrs
  | _ -> failwith "Second field in tagged union is not an union"


exception Unsupported

let filename_of_decl (decl : decl) =
  let loc = Clang.Ast.location_of_node decl |> Clang.Ast.concrete_of_source_location File in
  loc.filename

let has_prefix_in filename lib_dirs =
  List.exists (fun x -> String.starts_with ~prefix:x filename) lib_dirs

let decl_error_handler ?(ignored_dirs = []) (decl : decl) default f =
  if Krml.Options.debug "Verbose" then
    Format.printf "Visiting declaration %a\n%a@." DeclName.pp (DeclName.of_decl decl) Clang.Decl.pp decl;
  try f ()
  with e ->
    if not (has_prefix_in (filename_of_decl decl) ignored_dirs) then begin
      Format.eprintf "%!@.";
      Format.printf "%!@.";
      let loc = Clang.Ast.location_of_node decl |> Clang.Ast.concrete_of_source_location File in
      Format.printf "Declaration %a (in file %s) not supported\n@." DeclName.pp (DeclName.of_decl decl) loc.filename;
      if !ScyllaOptions.fatal_errors then
        raise e
      else begin
        Format.eprintf "Error: %s\n@." (Printexc.to_string e);
        Printexc.print_backtrace stderr;
        Format.eprintf "%s\n@." (String.make 80 '-');
        default
      end
    end
    else
      default

(* Computes the argument and return types of a function potentially marked as [[scylla_opaque]],
   taking into account attributes to adjust const/non-const pointers. *)
let compute_external_type (fdecl : function_decl) : binder list * typ =
  let ret_type = translate_typ fdecl.function_type.result in
  let binders = translate_params fdecl in
  let args_mut = Attributes.retrieve_mutability fdecl.attributes in
  let set_const t b =
    match t with
    | TBuf (t, _) -> TBuf (t, b)
    | _ -> t
  in
  let binders, ret_type =
    match args_mut with
    | None ->
        (* No mutability was specified, but we are in an opaque definition:
           All arguments must be considered as read-only *)
        List.map (fun arg -> { arg with typ = set_const arg.typ true }) binders, ret_type
    | Some (muts, mut_ret) ->
        (* In Ast, the flag set to true represents a constant, immutable array.
         The mutability flag is the converse, so we need to take the negation *)
        ( List.map2 (fun mut arg -> { arg with typ = set_const arg.typ (not mut) }) muts binders,
          set_const ret_type (not mut_ret) )
  in
  binders, ret_type

let translate_external_fundecl (fdecl : function_decl) =
  let name = get_id_name fdecl.name in
  let binders, ret_type = compute_external_type fdecl in
  let fn_type = Helpers.fold_arrow (List.map (fun x -> x.typ) binders) ret_type in
  let lid = Option.get (lid_of_ordinary_name name) in

  Krml.Ast.(
    DExternal (None, [], 0, 0, lid, fn_type, List.map (fun x -> Krml.Ast.(x.node.name)) binders))

let translate_decl (decl : decl) =
  (* Format.printf "visiting decl %s\n%a\n@." (name_of_decl decl) Clang.Decl.pp decl; *)
  decl_error_handler decl None @@ fun () ->
  match decl.desc with
  | Function fdecl ->
      if Attributes.has_opaque_attr fdecl.attributes then
        Some (translate_external_fundecl fdecl)
      else
        translate_fundecl fdecl
  | Var vdecl ->
      if vdecl.var_init = None then
        (* Prototype, e.g. extern int x; *)
        None
      else
        let _, _, e = translate_vardecl empty_env vdecl in
        let lid = Option.get (lid_of_ordinary_name vdecl.var_name) in
        let typ = translate_typ vdecl.var_type in
        (* TODO: Flags *)
        let flags = [] in
        if Attributes.has_expose_attr vdecl.attributes then
          exposed_globals := LidSet.add lid !exposed_globals;
        Some (DGlobal (flags, lid, 0 (* no polymorphic constant *), typ, e))
  | RecordDecl _ -> None
  | TypedefDecl { name; _ } ->
      let lid = Option.get (lid_of_ordinary_name name) in
      if Attributes.decl_is_container decl then
        container_types := LidSet.add lid !container_types;
      begin
        match LidMap.find_opt lid !type_def_map with
        | Some def -> Some (DType (lid, [], 0, 0, force_type_def_lazy def))
        | None -> None
      end
  | EnumDecl _ -> None
  | _ -> raise Unsupported

(* We are traversing an external module. We filter it to only preserve
   declarations annotated with the [opaque_attr] attribute, which
   we translate as external.
   TODO: We should probably try to translate all declarations as external,
   and use bundling to remove unneeded ones *)
let translate_external_decl (decl : decl) =
  match decl.desc with
  | Function fdecl ->
      (* let name = get_id_name fdecl.name in *)
      if Attributes.has_opaque_attr fdecl.attributes then
        Some (translate_external_fundecl fdecl)
      else
        None
  | _ -> None

let translate_file wanted_c_file file =
  (* Format.printf "Hitting file %s (wanting: %s)\n@." (fst file) wanted_c_file; *)
  let name, decls = file in
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

(* C guarantees very little in terms of ordering of declarations. To make our translation
   successful, we run a first pass that pre-allocates names and types of functions, and records type
   definitions so that we can have enough type information accessible to generate a well-typed krml
   AST. This phase does not produce any declarations -- it merely fills some maps. *)
let prepopulate_type_map ignored_dirs (decl : decl) =
  decl_error_handler ~ignored_dirs decl () @@ fun () ->
  (* Always in the ordinary namespace *)
  let name = snd (DeclName.of_decl decl) in
  let t =
    match decl.desc with
    | Function fdecl ->
        let binders, ret_type =
          if Attributes.has_opaque_attr fdecl.attributes then
            compute_external_type fdecl
          else
            translate_params fdecl, translate_typ fdecl.function_type.result
        in
        Some (Helpers.fold_arrow (List.map (fun x -> x.typ) binders) ret_type)
    | Var vdecl -> Some (translate_typ vdecl.var_type)
    | _ -> None
  in
  (* Krml.KPrint.bprintf "Adding into type map %s --> %a\n" name ptyp t; *)
  Option.iter (fun t ->
    global_type_map := StringMap.add name (t, `GlobalOrFun) !global_type_map
  ) t

type filename = string

(* A map from C identifier to its "best" declaration, along with the file the declaration belongs
   to. *)
type deduplicated_decls = (decl * filename) DeclMap.t

let prepopulate_type_maps (ignored_dirs : string list) (decls : deduplicated_decls) (decl : decl) =
  decl_error_handler ~ignored_dirs decl () @@ fun () ->
  let lid = Option.get (lid_of_name (DeclName.of_decl decl)) in

  (* declarations may be annotated with scylla_default *)
  if Attributes.decl_has_default decl then
    deriving_traits := add_to_list_lid lid "Default" !deriving_traits;

  match decl.desc with
  | TypedefDecl tdecl when not (Attributes.decl_is_opaque decl) ->
      (* To normalize correctly, we might need to retrieve types beyond the file currently
         being translated. We thus construct this map here rather than during type declaration
         translation.

         We substitute type abbreviations on the fly, via normalize_type. This allows us to match
         synthesized type against expected type accurately during the translation, which in turn
         allows us to insert casts in suitable places. *)
      let lid = Option.get (lid_of_ordinary_name tdecl.name) in
      (* Krml.KPrint.bprintf "typedef %s --> %a\n" tdecl.name plid lid; *)
      let def =
        match tdecl.underlying_type.desc with
        | Elaborated { keyword = (Struct | Enum); named_type = { cxtype; desc = (Record { name; _ } | Enum { name; _ }); _ }; _ }
          -> (
            (* When writing `typedef struct S { ... } T;` in C, we actually see two declarations:
              - first, one for the `struct S { ... }` part (case RecordDecl)
              - second, one for the the `typedef struct S T;` part (case TypedefDecl).
              We are now visiting the latter. Because our deduplicated map contains the most
              informative version of the former, we can look it up to get the definition.
            *)
            let def =
              if get_id_name name <> "" then begin
                (* `typedef struct foo_s { ... } foo;`: clang processes this as two separate
                   declarations, and references to `foo` later on appear as references to `struct
                   foo_s` -- thus, we record a mapping `struct S` ~~> `T` in our map, so that
                   occurrences of the type `struct S` in the Clang AST become nominal types `T` in
                   the krml Ast. *)
                elaborated_map := ElaboratedMap.add (name, `Struct) lid !elaborated_map;
                fst (DeclMap.(find (Tag, get_id_name name)) decls)
              end
              else
                (* `typedef struct { ... } foo;`: clang processes this as two separate declarations,
                   except now we can't refer to the struct by its name (meaning, we can't find it in
                   the map! instead, we use the cursor; further references to this type will appear as
                   `foo` *)
                let cx = Clang.get_type_declaration cxtype in
                Clang.Decl.of_cxcursor cx
            in

            (* This function executes as part of `fill_type_maps`, which is
                performed after declarations have been grouped and deduplicated.
                In particular, the `decls` map has been entirely filled, so we
                can safely perform a lookup outside of the lazy block; we only
                need the lazy for the translation of type definitions, e.g.,
                when a struct field refers to another type.
            *)
            match def with
            | { desc = RecordDecl { fields; attributes; _ }; _ }
              when Attributes.has_tuple_attr attributes ->
                Some (CTuple (lazy (List.map translate_field fields)))
            | { desc = RecordDecl { fields; attributes; _ }; _ }
              when Attributes.has_slice_attr attributes ->
                Some
                  (CSlice
                     (lazy
                       (let fields = List.map translate_field fields in
                        match fields with
                        | [ (Some "elt", (TBuf (t, _), _)); (Some "len", (TInt _, _)) ] -> t
                        | _ -> fatal_error "A slice type should have two fields called elt and len")))
            | { desc = RecordDecl { fields; attributes; _ }; _ }
              when Attributes.has_adt_attr attributes ->
                Some
                  (CVariant
                     (lazy
                       (match fields with
                       | [ tag; union ] ->
                           let name, (ty, _) = translate_field tag in
                           if name <> Some "tag" then
                             failwith "Tag of tagged union must be called tag";
                           begin
                             match ty with
                             | TInt _ -> ()
                             | _ -> failwith "tag must be an integer"
                           end;
                           let empty_ctr_names = Attributes.retrieve_empty_variants attributes in
                           let variant = translate_field_union union empty_ctr_names in
                           variant
                       | _ ->
                           failwith
                             "Tagged union translation to an ADT assumes that the structs contains \
                              two field: the tag, and the union")))
            | { desc = RecordDecl { fields; attributes; _ }; _ } ->
                Some
                  (CFlat
                     (lazy
                       (let fields = List.map translate_field fields in

                        if Attributes.has_box_attr attributes then
                          boxed_types := LidSet.add lid !boxed_types;

                        (* By default, we compile C structs to Rust structs with a C layout. This could
                         be changed, for instance, either with a command-line flag, or by defining a
                         new attribute __attribute__((annotate("scylla_c_layout"))) *)
                        attributes_map :=
                          add_to_list_lid lid (Printf.sprintf "repr(C)") !attributes_map;

                        (* Carry alignment down to Rust *)
                        begin
                          match Attributes.retrieve_alignment attributes with
                          | Some n ->
                              attributes_map :=
                                add_to_list_lid lid
                                  (Printf.sprintf "repr(align(%d))" n)
                                  !attributes_map
                          | None -> ()
                        end;

                        fields)))
            | { desc = EnumDecl { constants; attributes = _; _ }; _ } ->
                Some (CEnum (lazy (List.map (fun (constant: enum_constant)  ->
                  global_type_map := StringMap.add constant.desc.constant_name (TQualified lid, `Enum) !global_type_map;
                  Option.get (lid_of_ordinary_name constant.desc.constant_name), Option.map (fun (e: expr) ->
                    match e.desc with
                    | IntegerLiteral n -> Z.of_string (Clang.Ast.string_of_integer_literal n)
                    | _ -> fatal_error "unsupport default value for enum case"
                  ) constant.desc.constant_init
                ) constants)))
            | _ -> fatal_error "unknown struct definition: %s" (get_id_name name))
        | Pointer t ->
            Some
              (CAbbrev
                 (lazy
                   (let ty = translate_typ t in
                    TBuf (ty, t.const))))
        | BuiltinType t -> Some (CAbbrev (lazy (translate_builtin_typ t)))
        | Typedef { name; _ } -> Some (CAbbrev (lazy (get_id_name name |> translate_typ_name)))
        | _ ->
            (* Unsupported *)
            (* Krml.KPrint.bprintf "%a is unsupported\n" plid lid; *)
            None
      in
      Option.iter (fun def -> type_def_map := LidMap.add lid def !type_def_map) def
  | _ -> ()

let stem_of_file f = f |> Filename.basename |> Filename.remove_extension
let file_of_loc (loc : Clang.concrete_location) = loc.filename |> stem_of_file

let decl_is_better ~(old_decl : decl) (decl : decl) =
  (* TODO: we are assuming global consistency, i.e. that for all the translation units passed on the
     command-line, it is never the case that two definitions exist in two translation units, with
     identical names, but with different meanings. This is of course not true (static inline, extern
     vs inline visibilities, etc. -- especially the devious case for the latter where there can be
     two functions that have different definitions as long as they are equivalent, anyhow, I
     digress). *)
  match old_decl.desc, decl.desc with
  (* A definition is better than its prototype *)
  | Function { body = None; _ }, Function { body = Some _; _ } -> true
  | Var { var_init = None; _ }, Var { var_init = Some _; _ } -> true
  (* A full struct is better than its forward declaration *)
  | RecordDecl { fields = []; _ }, RecordDecl { fields = _ :: _; _ } -> true
  | _ -> false

let assign_file (file : translation_unit) (decl : decl) =
  match decl.desc with
  | Function fdecl when fdecl.body <> None && not fdecl.inline_specified -> file.desc.filename
  | _ -> filename_of_decl decl

(* A first pass that considers all possible declarations for a given name, then retains the most
   precise one. For instance, the prototype for `f` might be in header Foo.h but its
   definition might be in Bar.c -- this first pass considers all possible declarations for a given
   name then keeps the "best" one. *)
let pick_most_suitable (files : translation_unit list) : deduplicated_decls =
  List.fold_left
    (fun map (file : translation_unit) ->
      List.fold_left
        (fun map (decl : decl) ->
          let name = DeclName.of_decl decl in
          DeclMap.update name
            (fun old_entry ->
              match old_entry with
              | None -> Some (decl, assign_file file decl)
              | Some ((old_decl, _) as old_entry) ->
                  if decl_is_better ~old_decl decl then
                    Some (decl, assign_file file decl)
                  else
                    Some old_entry)
            map)
        map file.desc.items)
    DeclMap.empty files

(* Declarations, grouped by filename *)
type grouped_decls = (string * Clang.Ast.decl list) list

(* A second pass that preallocates names (since everything is potentially mutually recursive),
   relying on the fact that we now know the "best" location for a definition (first pass). This also
   groups declarations by file. Note that we have totally lost the order of declarations within a
   file here. *)
let split_into_files (lib_dirs : string list) (decls : deduplicated_decls) : grouped_decls =
  (* If this belongs to the C library, do not extract it. *)
  let decls =
    DeclMap.filter
      (fun _ (_, filename) ->
        if has_prefix_in filename lib_dirs then
          false
        else
          true)
      decls
  in

  let add_decl _ (decl, loc) (acc: _ StringMap.t) =
    (* Remember the file that this declaration is conceptually associated to *)
    (* Krml.KPrint.bprintf "Declaration %s goes into file %s\n" (name_of_decl decl) (stem_of_file loc); *)
    name_map := DeclMap.add (DeclName.of_decl decl) (stem_of_file loc) !name_map;
    (* Enum constants also get a name allocated, in the ordinary namespace, too *)
    match decl.desc with
    | EnumDecl { constants; _ } ->
        List.iter (fun c -> name_map := DeclMap.add (DeclName.of_enum_constant c) (stem_of_file loc) !name_map) constants
    | _ -> ()
    ; ;
    (* Group this declaration with others that also "belong" to this file *)
    add_to_list (stem_of_file loc) decl acc
  in
  let decl_map = DeclMap.fold add_decl decls StringMap.empty in
  StringMap.bindings decl_map |> List.map (fun (k, l) -> k, List.rev l)

(* Third pass. Now that names can be resolved properly, we fill various type maps, and precompute type
   definitions while we're at it -- this makes sure type aliases are known, since they need to be
   substituted away (normalized) prior to doing the type-directed expression translation. *)
let fill_type_maps (ignored_dirs : string list) (decls : deduplicated_decls) =
  DeclMap.iter (fun _ (decl, _) -> prepopulate_type_maps ignored_dirs decls decl) decls;
  (* This can only be done AFTER abbreviations are recorded, otherwise, the annotations cannot be
     applied properly. *)
  DeclMap.iter (fun _ (decl, _) -> prepopulate_type_map ignored_dirs decl) decls

(* Final pass. Actually emit definitions. *)
let translate_compil_units (ast : grouped_decls) (command_line_args : string list) =
  let file_args = List.map stem_of_file command_line_args in
  ( !boxed_types,
    !container_types,
    List.map
      (fun (file, decls) ->
        if List.mem file file_args then
          file, List.filter_map translate_decl decls
        else
          file, List.filter_map translate_external_decl decls)
      ast )

let read_file (filename : string) : translation_unit =
  Format.printf "Clang version is %s\n@." (Clang.get_clang_version ());
  let command_line_args =
    !Scylla__Options.ccopts
    @ List.map Clang.Command_line.include_directory (Clang.default_include_directories ())
  in
  Format.printf "Arguments passed to clang are: %s\n@." (String.concat " " command_line_args);
  parse_file ~command_line_args filename
