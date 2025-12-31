(* High-level logic for parsing / grouping / deduplicating Clang definitions before jumping into
   ClangToAst *)

open Clang.Ast
open ClangToAst

let read_file (filename : string) : translation_unit =
  Format.printf "Clang version is %s\n@." (Clang.get_clang_version ());
  let command_line_args =
    !Scylla__Options.ccopts
    @ List.map Clang.Command_line.include_directory (Clang.default_include_directories ())
  in
  Format.printf "Arguments passed to clang are: %s\n@." (String.concat " " command_line_args);
  parse_file ~command_line_args filename

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
  let add_decl _ (decl, loc) (acc: _ StringMap.t) =
    (* Remember the file that this declaration is conceptually associated to *)
    (* Krml.KPrint.bprintf "Declaration %a goes into file %s\n" DeclName.p (DeclName.of_decl decl) (stem_of_file loc); *)
    name_map := DeclMap.add (DeclName.of_decl decl) (stem_of_file loc) !name_map;
    if not (has_prefix_in loc lib_dirs) then begin
      (* Enum constants also get a name allocated, in the ordinary namespace, too *)
      match decl.desc with
      | EnumDecl { constants; _ } ->
          List.iter (fun c -> name_map := DeclMap.add (DeclName.of_enum_constant c) (stem_of_file loc) !name_map) constants
      | TypedefDecl { underlying_type = { desc = Elaborated { keyword = Enum; named_type = { cxtype; desc = Enum { name; _ }; _ }; _ }; _ }; _ } -> 
          if get_id_name name = "" then
            (* See prepopulate_type_maps; sometimes, `typedef enum { ... } t;` is represented this way
               (as opposed to seeing an EnumDecl above), but I don't know under which circumstances,
               and I have yet to exercise this codepath with a unit test. *)
            begin match Clang.(Decl.of_cxcursor (get_type_declaration cxtype)).desc with
            | EnumDecl { constants; _ } ->
                List.iter (fun c -> name_map := DeclMap.add (DeclName.of_enum_constant c) (stem_of_file loc) !name_map) constants
            | _ -> failwith "enum typedef is not an enum after all"
            end
      | _ ->
          ()
      ; ;
      (* Group this declaration with others that also "belong" to this file *)
      add_to_list (stem_of_file loc) decl acc
    end else
      add_to_list (stem_of_file loc) decl acc
  in
  let decl_map = DeclMap.fold add_decl decls StringMap.empty in
  StringMap.bindings decl_map |> List.map (fun (k, l) -> k, List.rev l)

(* Third pass. Now that names can be resolved properly, we fill various type maps, and precompute type
   definitions while we're at it -- this makes sure type aliases are known, since they need to be
   substituted away (normalized) prior to doing the type-directed expression translation. *)
let fill_type_maps ~lib_dirs (ignored_dirs : string list) (decls : deduplicated_decls) =
  DeclMap.iter (fun _ (decl, _) -> prepopulate_type_maps ignored_dirs decls decl) decls;
  (* This can only be done AFTER abbreviations are recorded, otherwise, the annotations cannot be
     applied properly. *)
  DeclMap.iter (fun _ (decl, _) -> prepopulate_type_map ~lib_dirs ignored_dirs decl) decls

(* Final pass. Actually emit definitions. *)
let translate_compil_units (ignored_dirs: string list) (ast : grouped_decls) (command_line_args : string list) =
  let file_args = List.map stem_of_file command_line_args in
  ( !boxed_types,
    !container_types,
    List.map
      (fun (file, decls) ->
        if List.mem file file_args then
          file, List.filter_map translate_decl decls
        else
          file, List.filter_map (translate_external_decl ignored_dirs) decls)
      ast )
