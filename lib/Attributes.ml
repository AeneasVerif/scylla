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

(* An attribute to specify that a given type (and its internal pointers) should
   be translated to `Box`es instead of borrows *)
let box_attr = "scylla_box"

(* We check for the presence of the [opaque_attr] attribute. We require it to
   be exactly the annotation *)
let has_opaque_attr' (attr: attribute) = match attr.desc with
  | Clang__.Attributes.Annotate s -> String.equal s.annotation opaque_attr
  | _ -> false

let has_opaque_attr (attrs: attribute list) = List.exists has_opaque_attr' attrs

(* We check for the presence of the [box_attr] attribute. We require it to
   be exactly the annotation *)
let has_box_attr' (attr: attribute) = match attr.desc with
  | Clang__.Attributes.Annotate s -> String.equal s.annotation box_attr
  | _ -> false

let has_box_attr (attrs: attribute list) = List.exists has_box_attr' attrs

let retrieve_mutability' (attr: attribute) = match attr.desc with
  | Clang__.Attributes.Annotate s ->
      let prefix = mut_attr in
      if String.starts_with ~prefix s.annotation then
        (* We extract the substring corresponding to the list of mut annotations *)
        let muts = String.sub s.annotation
          (* We start after the mut_attr annotation and the opening parenthesis *)
          (String.length mut_attr + 1)
          (* The length is the length of the full annotation, minus the mut_attr
             annotation and the enclosing parentheses *)
          (String.length s.annotation - String.length mut_attr - 2) in
        (* We split into a list of attributes, and trim whitespaces *)
        let muts = String.split_on_char ',' muts |> List.map String.trim in
        let muts = List.map (fun x ->
          if String.equal x "mut" then true
          else if String.equal x "_" then false
          else failwith "Ill-formed mutability annotation"
        ) muts in
        Some muts
      else None
  | _ -> None

let retrieve_mutability (attrs: attribute list) = List.fold_left (fun acc x ->
    match acc, retrieve_mutability' x with
    | None, m | m, None -> m
    | Some _, Some _ -> failwith "Mutability of opaque function is specified twice"
) None attrs

