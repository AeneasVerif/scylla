(* A set of helpers for transformations that are performed on-the-fly as part of ClangToAst *)

open Krml.Ast
(* open Krml.PrintAst.Ops *)

exception NotRegular

let rec assert_branch_is_regular (terminal: bool) (e: expr): expr =
  { e with node =
    match e.node with
    | EAbort (_, _)
    | EApp ({ node = EQualified ([], "exit"); _ }, _)
    | EReturn _
    | EContinue ->
       (* All of these nodes end control-flow, so they are ok in any position, including terminal. *)
        e.node
    | EBreak ->
        (* ClangToAst translates break to break, but `break` in the krml ast means "break to loop"
           -- there is no "break to switch", so we assert that the only `break`s found in a case
           body in an terminal position, and we remove them to avoid the semantic mismatch. *)
        if not terminal then
          raise NotRegular;
        EUnit
    | ELet (b, e1, e2) ->
        ELet (b, e1, assert_branch_is_regular terminal e2)
    | EIfThenElse (e1, e2, e3) ->
        EIfThenElse (e1, assert_branch_is_regular terminal e2, assert_branch_is_regular terminal e3)
    | ESequence es ->
        let es, e = Krml.KList.split_at_last es in
        ESequence (List.map (assert_branch_is_regular false) es @ [ assert_branch_is_regular terminal e ])
    | EMatch (f, c, bs) ->
        EMatch (f, c, List.map (fun (bs, p, e) -> bs, p, assert_branch_is_regular terminal e) bs)
    | ESwitch (c, bs) ->
        ESwitch (c, List.map (fun (p, e) -> p, assert_branch_is_regular terminal e) bs)
    | _ ->
        if terminal then
          raise NotRegular
        else
          e.node
  }

(* We start in terminal position *)
let assert_branch_is_regular = assert_branch_is_regular true
