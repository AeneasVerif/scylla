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
