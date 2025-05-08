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

(* We check for the presence of the [opaque_attr] attribute. We require it to
   be exactly the annotation *)
let has_opaque_attr' (attr : attribute) =
  match attr.desc with
  | Clang__.Attributes.Annotate s -> String.equal s.annotation opaque_attr
  | _ -> false

let has_opaque_attr (attrs : attribute list) = List.exists has_opaque_attr' attrs

(* We check for the presence of the [box_attr] attribute. We require it to
   be exactly the annotation *)
let has_box_attr' (attr : attribute) =
  match attr.desc with
  | Clang__.Attributes.Annotate s -> String.equal s.annotation box_attr
  | _ -> false

let has_box_attr (attrs : attribute list) = List.exists has_box_attr' attrs

(* We check for the presence of the [adt_attr] attribute. We require it
   to be exactly the annotation *)
let has_adt_attr' (attr : attribute) =
  match attr.desc with
  | Clang__.Attributes.Annotate s -> String.equal s.annotation adt_attr
  | _ -> false

let has_adt_attr (attrs : attribute list) = List.exists has_adt_attr' attrs

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
    | Clang__.Attributes.Aligned { alignment_expr; _ } -> Some alignment_expr
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
