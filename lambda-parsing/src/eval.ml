open Expression

type context = {
  mutable seq: int;
}
let create_context () : context = { seq = 0 }
let nextVar (ctx : context) : string =
  let seq = ctx.seq in
  ctx.seq <- ctx.seq + 1;
  Printf.sprintf "v%d" seq

let rec rename_bounded (cntx : context) (e : expr) (ns : (string * string) list) : expr =
  match e with
  | Var x -> 
    (match List.assoc_opt x ns with
     | Some y -> Var y
     | None -> Var x)
  | App (f, a) -> App (rename_bounded cntx f ns, rename_bounded cntx a ns)
  | Lam (Just p, b) -> 
    let new_name = nextVar cntx in
    let ns' = (p, new_name) :: ns in
    Lam (Just new_name, rename_bounded cntx b ns')
  | _ -> assert false

let rec subs (cntx : context) (x : string) (v : expr) (e : expr) : expr =
  match e with
  | Var y when y = x -> rename_bounded cntx v []
  | Var _ -> e
  | App (f, a) -> App (subs cntx x v f, subs cntx x v a)
  | Lam (p, b) -> Lam (p, subs cntx x v b)
  | TupleE es -> TupleE (List.map (subs cntx x v) es)

let rec eval (cntx : context) (expr : expr) : expr * bool =
  match expr with
  | App (Lam (Just x, b), t) -> (subs cntx x t b, true)
  | App (s, t) -> 
    let (s', cs) = eval cntx s in
    let (t', ct) = eval cntx t in
    (App (s', t'), cs || ct)
  | Lam (p, b) -> 
    let (b', cb) = eval cntx b in
    (Lam (p, b'), cb)
  | Var _ -> (expr, false)
  | _ -> assert false 

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
