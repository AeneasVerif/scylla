open Krml.MiniRust

let map_funs = Krml.OptimizeMiniRust.map_funs

let default = Call (Name [ "Default"; "default" ], [], [])

let is_scalar (x: typ) =
  match x with
  | Constant _ | Unit -> true
  | _ -> false

let add_defaults = object
  inherit [_] map_expr as self

  method! visit_Let () b e1 e2 =
    let e1 =
      match e1 with
      | None ->
          if not (is_scalar b.typ) then
            match b.typ with
            | Array (Constant w, n) -> Some (Array (Repeat (Constant (w, "0"), Constant (SizeT, string_of_int n))))
            | _ -> Some default
          else
            None
      | Some e1 -> Some (self#visit_expr () e1)
    in
    Let (b, e1, self#visit_expr () e2)
end

let add_defaults files =
  map_funs add_defaults#visit_expr files
