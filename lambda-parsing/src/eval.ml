open Expression

type context = {
  mutable seq: int;
}
let create_context () : context = { seq = 0 }
let nextVar (ctx : context) : string =
  let seq = ctx.seq in
  ctx.seq <- ctx.seq + 1;
  Printf.sprintf "v%d" seq

let rec rename_vars_in_pattern (cntx : context) (p : pattern) : pattern * (string * string) list =
  match p with
  | Just name ->
      let new_name = nextVar cntx in
      (Just new_name, [(name, new_name)])
  | Tuple patterns ->
      let (new_patterns, mappings_list) =
        List.split (List.map (rename_vars_in_pattern cntx) patterns)
      in
      (Tuple new_patterns, List.concat mappings_list)

let rec rename_bounded (cntx : context) (e : expr) (ns : (string * string) list) : expr =
  match e with
  | Var x -> 
    (match List.assoc_opt x ns with
     | Some y -> Var y
     | None -> Var x)
  | App (f, a) -> App (rename_bounded cntx f ns, rename_bounded cntx a ns)
  | Lam (p, b) ->
      let (new_p, mappings) = rename_vars_in_pattern cntx p in
      let ns' = mappings @ ns in
      Lam (new_p, rename_bounded cntx b ns')
  | TupleE es -> TupleE (List.map (fun e -> rename_bounded cntx e ns) es)
  | Arith (e1, op, e2) ->
      Arith (rename_bounded cntx e1 ns, op, rename_bounded cntx e2 ns)
  | Int _ -> e
  | IfThenElse (c, t, e) -> IfThenElse (rename_bounded cntx c ns, rename_bounded cntx t ns, rename_bounded cntx e ns)

let rec bound_vars_in_pattern = function
  | Just s -> [s]
  | Tuple ps -> List.concat_map bound_vars_in_pattern ps

let rec subs_multi (cntx : context) (substs : (string * expr) list) (e : expr) : expr =
  match e with
  | Var y ->
    (match List.assoc_opt y substs with
     | Some v -> rename_bounded cntx v []
     | None -> e)
  | App (f, a) -> App (subs_multi cntx substs f, subs_multi cntx substs a)
  | Lam (p, b) ->
      let p_vars = bound_vars_in_pattern p in
      let substs' = List.filter (fun (x, _) -> not (List.mem x p_vars)) substs in
      if substs' = [] then e
      else Lam (p, subs_multi cntx substs' b)
  | TupleE es -> TupleE (List.map (subs_multi cntx substs) es)
  | Arith (e1, op, e2) -> Arith (subs_multi cntx substs e1, op, subs_multi cntx substs e2)
  | IfThenElse (c, t, e_else) -> IfThenElse (subs_multi cntx substs c, subs_multi cntx substs t, subs_multi cntx substs e_else)
  | Int _ -> e

let rec generate_substitutions (p : pattern) (arg : expr) : (string * expr) list option =
  match p, arg with
  | Just x, e -> Some [(x, e)]
  | Tuple ps, TupleE es ->
      if List.length ps <> List.length es then None else
      List.fold_left2 (fun acc_opt p_i e_i ->
        match acc_opt with
        | None -> None
        | Some acc ->
          match generate_substitutions p_i e_i with
          | None -> None
          | Some new_substs -> Some (new_substs @ acc)
      ) (Some []) ps es
  | Tuple _, _ -> None (* Match failure *)

let rec subs (cntx : context) (x : string) (v : expr) (e : expr) : expr =
  match e with
  | Var y when y = x -> rename_bounded cntx v []
  | App (f, a) -> App (subs cntx x v f, subs cntx x v a)
  | Lam (p, b) ->
      let p_vars = bound_vars_in_pattern p in
      if List.mem x p_vars then e
      else Lam (p, subs cntx x v b)
  | TupleE es -> TupleE (List.map (subs cntx x v) es)
  | Var _ -> e
  | Int _ -> e
  | Arith (e1, op, e2) -> Arith (subs cntx x v e1, op, subs cntx x v e2)
  | IfThenElse (c, t, e) -> IfThenElse (subs cntx x v c, subs cntx x v t, subs cntx x v e)

let rec eval (cntx : context) (expr : expr) : expr * bool =
  match expr with
  | App (Lam (p, b), t) ->
      (match p with
      | Just x -> (subs cntx x t b, true)
      | Tuple _ ->
        let t', changed_t = eval cntx t in
        (match t' with
         | TupleE _ as v ->
           (match generate_substitutions p v with
            | Some substs -> (subs_multi cntx substs b, true)
            | None -> (App (Lam (p, b), t'), changed_t) (* pattern match failure, stuck *)
           )
         | _ -> (App (Lam (p, b), t'), changed_t) (* not a tuple, maybe still evaluating *)
        )
      )
  | App (s, t) ->
    let (s', cs) = eval cntx s in
    let (t', ct) = eval cntx t in
    (App (s', t'), cs || ct)
  | Lam (p, b) -> 
    let (b', cb) = eval cntx b in
    (Lam (p, b'), cb)
  | Var _ -> (expr, false)
  | Int _ -> (expr, false)
  | TupleE es ->
      let (es', changed) = List.fold_right (fun e (acc, changed) -> 
          let (e', c) = eval cntx e in (e'::acc, c || changed)
      ) es ([], false) in
      (TupleE es', changed)
  | Arith (e1, op, e2) ->
      let e1', c1 = eval cntx e1 in
      let e2', c2 = eval cntx e2 in
      ( match (e1', e2') with
      | Int i1, Int i2 ->
          let res =
            match op with
            | Plus -> Int (i1 + i2)
            | Minus -> Int (i1 - i2)
            | Mul -> Int (i1 * i2)
            | Eq -> if i1 = i2 then Int 1 else Int 0
          in
          (res, true)
      | _ -> (Arith (e1', op, e2'), c1 || c2) )
  | IfThenElse (c, t, e) ->
      let (c', cc) = eval cntx c in
      match c' with
      | Int 0 -> (e, true)
      | Int _ -> (t, true)
      | _ -> (IfThenElse (c', t, e), cc)

let rec eval_all (expr : expr) : expr =
  let rec aux cntx e = 
    let (e', changed) = eval cntx e in
    Printf.printf "Evaluating: \n";
    Expression.short_print_expr e';
    Printf.printf "\n\n\n%!";
    Unix.sleepf 0.1;
    if changed then aux cntx e' else e'
  in
  aux (create_context ()) expr
