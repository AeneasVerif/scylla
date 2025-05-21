open Krml.Ast
open Krml

let remove_addrof_index =
  object (self)
    inherit [_] map

    (* &(e1[e2]) --> e1 + e2 *)
    method! visit_EAddrOf _ e =
      let e = self#visit_expr_w () e in
      match e.node with
      | EBufRead (e1, e2) -> EBufSub (e1, e2)
      | _ -> EAddrOf e
  end


let inline_immediate_vardef =
  object (self)
    inherit [_] map

    (* let x = any;
       x = e;
       ----->
       let x = e;
    *)
    method! visit_ELet _ b e1 e2 =
      let e1 = self#visit_expr_w () e1 in
      let e2 = self#visit_expr_w () e2 in
      match e1.node, e2.node with
      | EAny, ELet (_, { node = EAssign (var, e); _}, e3) when var.node = EBound 0 ->
          ELet (b, Krml.DeBruijn.lift (-1) e, Krml.DeBruijn.lift (-1) e3)
      (* More uncommon case, where the assignment is in terminal position *)
      | EAny, EAssign (var, e) when var.node = EBound 0 ->
          ELet (b, e, Krml.Helpers.eunit)
      | _ -> ELet (b, e1, e2)

    end

let materialize_casts =
  object (_self)
    inherit [_] map as super

    method! visit_ECast ((), _ as env) e t_to =
      match e.typ, t_to with
      | (TArray (TInt w_from, _) | TBuf (TInt w_from, _)),
        (TArray (TInt w_to, _) | TBuf (TInt w_to, _)) when w_from <> w_to ->
          let is_const = match t_to with TBuf (_, false) -> false | _ -> true in
          let is_mut = not is_const in
          let t_from = TBuf (Helpers.assert_tbuf_or_tarray e.typ, is_const) in
          let t_to = TBuf (Helpers.assert_tbuf_or_tarray t_to, is_const) in
          let name = Printf.sprintf "scylla_%s_of_%s%s" (PrintMiniRust.string_of_width w_to)
            (PrintMiniRust.string_of_width w_from) (if is_mut then "_mut" else "")
          in
          let scylla_cast = with_type (TArrow (t_from, t_to)) (EQualified (["scylla_glue"], name)) in
          EApp (scylla_cast, [ e ])
      | _ ->
          super#visit_ECast env e t_to
  end

let inline_tuple_types tuple_types =
  object (_self)
    inherit [_] map

    method! visit_TQualified _ t =
      match ClangToAst.LidMap.find_opt t tuple_types with
      | Some t -> t
      | None -> TQualified t
  end

let simplify files =
  let files = remove_addrof_index#visit_files () files in
  inline_immediate_vardef#visit_files () files
