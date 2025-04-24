open Krml.Ast

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
          ELet (b, e, Krml.DeBruijn.lift (-1) e3)
      (* More uncommon case, where the assignment is in terminal position *)
      | EAny, EAssign (var, e) when var.node = EBound 0 ->
          ELet (b, e, Krml.Helpers.eunit)
      | _ -> ELet (b, e1, e2)

    end


let simplify files =
  let files = remove_addrof_index#visit_files () files in
  inline_immediate_vardef#visit_files () files
