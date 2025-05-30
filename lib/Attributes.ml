open Clang.Ast

(* C definitions can be annotated with this attribute to be extracted
   opaquely as an external function *)
let opaque_attr = "scylla_opaque"

(* An attribute to specify the mutability of arguments of external definitions.
   By default, all arguments are assumed to be read-only. If this attribute is
   specified, it must be as, e.g., `scylla_mutability(mut, _, _)` Mutable
   arguments are specified using `mut`, read-only arguments are specified using
   an underscore. The number of annotations must match the number of arguments,
   which will be checked during the translation from Clang to Ast *)
let mut_attr = "scylla_mutability"

(* Generate #[deriving(Default)] *)
let default_attr = "scylla_default"

(* An attribute to specify that a given type (and its internal pointers) should
   be translated to `Box`es instead of borrows *)
let box_attr = "scylla_box"

(* An attribute to specify that a tagged union should be translated
   to a Rust algebraic data type. We assume that the corresponding struct
   consists of an integer field (the tag), followed by the union, and that
   the tag ranges from 0 to the number of constructor, and matches the
   order of the union cases. *)
let adt_attr = "scylla_adt"

(* When translating a tagged union into an ADT, we can also specify
   additional constructors with an empty payload (which are therefore
   not part of the C tagged union cases) using this attribute.
   The constructor(s) will be appended at the end of the translated
   tagged union cases *)
let empty_variant_attr = "scylla_empty_variant"

(* Translate a given type to a tuple instead of a struct *)
let tuple_attr = "scylla_tuple"

(* Translate a struct type to a slice. It requires the
   struct to have two fields, called `elt` and `len`,
   where `elt` is a pointer type, and `len` is an integer *)
let slice_attr = "scylla_slice"

(* Expose directly as a C FFI function or global, with #[no_mangle] and the like *)
let expose_attr = "scylla_expose"

(* Recognize container types, i.e., structs that contain pointers to other pointers,
   and that therefore need to be extracted with different lifetimes *)
let container_attr = "scylla_container_type"

let has a (attrs : attribute list) =
  List.exists (fun (attr: attribute) ->
    match attr.desc with
    | Clang__.Attributes.Annotate s -> String.equal s.annotation a
    | _ -> false
  ) attrs

(* We check for the presence of the [opaque_attr] attribute. We require it to
   be exactly the annotation *)
let has_opaque_attr = has opaque_attr

(* We check for the presence of the [box_attr] attribute. We require it to
   be exactly the annotation *)
let has_box_attr = has box_attr

(* We check for the presence of the [adt_attr] attribute. We require it
   to be exactly the annotation *)
let has_adt_attr = has adt_attr

let has_tuple_attr = has tuple_attr

let has_slice_attr = has slice_attr

let has_expose_attr = has expose_attr

let has_container_attr = has container_attr

(* If the [adt_attr] attribute is specified on a structure,
   we can also specify `scylla_empty_variant(name)`, which
   will append a variant with no payload with constructor
   name {name} at the end of the constructor list *)
let retrieve_empty_variant (attr: attribute) =
  match attr.desc with
  | Clang__.Attributes.Annotate s ->
      if String.starts_with ~prefix:empty_variant_attr s.annotation then
        (* Syntax: scylla_empty_variant (constructor_name) *)
        let after_open_paren = String.index s.annotation '(' + 1 in
        let close_paren = String.index s.annotation ')' in
        (* We extract the substring corresponding to the constructor name *)
        let ctr_name = String.sub s.annotation after_open_paren (close_paren - after_open_paren) in
        Some ctr_name
      else None
  | _ -> None

(* Collects all specified empty variants *)
let retrieve_empty_variants (attrs : attribute list) =
  List.filter_map retrieve_empty_variant attrs

let retrieve_mutability' (attr : attribute) =
  match attr.desc with
  | Clang__.Attributes.Annotate s ->
      let parse_mut x =
        match String.trim x with
        | "mut" -> true
        | "_" -> false
        | _ -> failwith "Ill-formed mutability annotation"
      in
      if String.starts_with ~prefix:mut_attr s.annotation then
        (* Syntax: scylla_mutability (mut, _, mut, ...) -> mut
           where the -> mut part is optional *)
        let after_open_paren = String.index s.annotation '(' + 1 in
        let close_paren = String.index s.annotation ')' in
        (* We extract the substring corresponding to the list of mut annotations *)
        let muts = String.sub s.annotation after_open_paren (close_paren - after_open_paren) in
        (* We split into a list of attributes, and trim whitespaces *)
        let muts = String.split_on_char ',' muts |> List.map parse_mut in
        (* Optional return annotation *)
        let ret = String.trim (String.sub s.annotation (close_paren + 1) (String.length s.annotation - (close_paren + 1))) in
        let ret =
          if ret <> "" then
            let gt = String.index s.annotation '>' in
            parse_mut (String.sub s.annotation (gt + 1) (String.length s.annotation - (gt + 1)))
          else
            false
        in
        Some (muts, ret)
      else
        None
  | _ -> None

let retrieve_mutability (attrs : attribute list) =
  List.fold_left
    (fun acc x ->
      match acc, retrieve_mutability' x with
      | None, m | m, None -> m
      | Some _, Some _ -> failwith "Mutability of opaque function is specified twice")
    None attrs

let retrieve_alignment (attrs: attribute list) =
  List.find_map (fun (x: attribute) ->
    match x.desc with
    | Clang__.Attributes.Aligned { alignment_expr; _ } ->
        begin match alignment_expr with
        | { desc = IntegerLiteral n; _ } ->
            Some (Clang.Ast.int_of_literal n)
        | _ ->
            Krml.KPrint.bprintf "Warning: alignment is not a constant, ignoring\n";
            None
        end
    | _ -> None
  ) attrs

let has_always_inline (attrs: attribute list) =
  List.exists (fun (x: attribute) -> match x.desc with Clang__.Attributes.AlwaysInline _ -> true | _ -> false) attrs

(* LOW-LEVEL API -- FOR THINGS THAT DO NOT HAVE AN ATTRIBUTES FIELD *)

(* This attempts to read the attributes since typedef attributes are not exposed in the
   ClangMl high-level AST. This is painful. *)
let decl_has_attr (decl : decl) attr =
  let has_attr = ref false in
  begin
    match decl.decoration with
    | Cursor cx ->
        Clang__.Clang__utils.iter_decl_attributes
          (fun cx ->
            match Clang.ext_attr_get_kind cx with
            | Annotate when Clang.ext_attrs_get_annotation cx = attr -> has_attr := true
            | _ -> ())
          cx
    | Custom _ -> failwith "no cursor"
  end;
  !has_attr

(* This attempts to read the attributes since typedef attributes are not exposed in the
   ClangMl high-level AST. This is painful. *)
let decl_is_opaque (decl : decl) =
  decl_has_attr decl opaque_attr

let decl_has_default decl =
  decl_has_attr decl default_attr

let decl_is_container decl =
  decl_has_attr decl container_attr
