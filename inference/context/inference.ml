open Types
open Utils

let apply_subst_type (s: subst) (t : typee) : typee =
  let rec aux t = match t with
    | TVar x -> Option.value (Subst.find_opt x s) ~default:t
    | TFun (t1, t2) -> TFun (aux t1, aux t2)
    | TList t1 -> TList (aux t1)
    | _ -> t
  in aux t

let apply_subst_scheme (s: subst) (Scheme (vars, t): scheme) : scheme =
  Scheme (vars, apply_subst_type s t)

let apply_subst_context (s: subst) (ctx: context) : context =
  Context.map (apply_subst_scheme s) ctx

let empty_subst : subst = Subst.empty
let compose_subst s1 s2 =
  Subst.fold (fun s t acc -> Subst.add s (apply_subst_type s1 t) acc) s2 s1


let remove_binding (ctx: context) (x: string) : context =
  Context.remove x ctx

let free_vars_type (t: typee) : fv_set =
  let rec aux t acc = match t with
    | TVar x -> FVSet.add x acc
    | TFun (t1, t2) -> aux t1 (aux t2 acc)
    | TList t1 -> aux t1 acc
    | _ -> acc
  in aux t FVSet.empty

let free_vars_scheme (Scheme (_, t): scheme) : fv_set = free_vars_type t

let free_vars_context (ctx: context) : fv_set =
  Context.fold (fun _ v acc -> FVSet.union acc (free_vars_scheme v)) ctx FVSet.empty

let generalize (ctx : context) (t: typee) : scheme =
  let fv_ctx = free_vars_context ctx in
  let fv_t = free_vars_type t in
  let vars = FVSet.diff fv_t fv_ctx |> FVSet.elements in
  Scheme (vars, t)



let instantiate (fc : VarFactory.var_fac) (Scheme (vars, t): scheme) : typee =
  let aux_sub = Subst.of_seq @@ List.to_seq @@ List.map (fun v -> (v, fc ())) vars in
  apply_subst_type aux_sub t

(* This is really nice -- an analogue of Haskell's do-notation *)
let (let*) = Result.bind

let rec unify (t1: typee) (t2: typee) : (subst, string) result =
  match (t1, t2) with
    | (TFun (a1, r1), TFun (a2, r2)) ->
        let* s1 = unify a1 a2 in
        let* s2 = unify (apply_subst_type s1 r1) (apply_subst_type s1 r2) in
        Ok (compose_subst s2 s1)
    | (TVar x, t) | (t, TVar x) -> begin
        match t with
          | TVar y when x = y -> Ok empty_subst
          | _ when FVSet.mem x (free_vars_type t) -> 
            Error ("Cannot unify " ^ x ^ " with " ^ string_of_type t ^ ": " ^ "free variable occurs in type")
          | _ -> Ok (Subst.singleton x t)
        end
    | (TList t1, TList t2) -> unify t1 t2
    | (TInt, TInt) -> Ok empty_subst
    | (TBool, TBool) -> Ok empty_subst
    | _ -> Error ("Cannot unify " ^ string_of_type t1 ^ " with " ^ string_of_type t2)


(* 
Algorithm W for inference in the Hindley-Milner type system
Verbatim from https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W
*)
let type_inference (fc : VarFactory.var_fac) (ctx : context) (expr : expr) : (typee * subst, string) result =
  let rec infer ctx expr = match expr with
    | EVar x -> begin
        match Context.find_opt x ctx with
        | Some sigma -> Ok (instantiate fc sigma, empty_subst)
        | None -> Error ("Unbound variable: " ^ x)
      end
    | EAbs (x, e) ->
        let tau = fc () in
        let nctx = Context.add x (Scheme ([], tau)) ctx in
        let* (tau', s) = infer nctx e in
        Ok (TFun (apply_subst_type s tau, tau'), s)
    | EApp (e0, e1) ->
        let* (tau0, s0) = infer ctx e0 in
        let* (tau1, s1) = infer (apply_subst_context s0 ctx) e1 in
        let tau' = fc () in
        let s1tau0 = apply_subst_type s1 tau0 in
        let tau1_tau' = TFun (tau1, tau') in
        let* s2 = unify s1tau0 tau1_tau' in
        Ok (apply_subst_type s2 tau', compose_subst s2 (compose_subst s1 s0))
    | ELet (x, e0, e1) -> 
        let* (tau, s0) = infer ctx e0 in
        let nctx = apply_subst_context s0 ctx in
        let x_scheme = generalize nctx tau in
        let nnctx = Context.add x x_scheme nctx in
        let* (tau', s1) = infer nnctx e1 in
        Ok (tau', compose_subst s1 s0)
    | ELit (IntLit _) -> Ok (TInt, empty_subst)
    | ELit (BoolLit _) -> Ok (TBool, empty_subst)
    (* This could also be handled by adding Nil : ∀a.[a] to the default context *)
    | ELit ListNil -> Ok (TList (fc ()), empty_subst) 
    | ELetRec (f, e0, e1) ->
      let desugar = ELet (f, EApp (EVar "fix", (EAbs (f, e0))) , e1) in
      infer ctx desugar
    | EIfThenElse (e1, e2, e3) ->
      let desugar = EApp (EApp (EApp (EVar "ite", e1), e2), e3) in
      infer ctx desugar
    in infer ctx expr

let type_to_scheme (t : typee) : scheme = generalize Context.empty t

(* Context with 'builtins' *)
let default_context : context =
  Context.empty
  |> Context.add "Cons" @@ type_to_scheme (a => (l a => l a))
  |> Context.add "Plus" @@ type_to_scheme (TInt => (TInt => TInt))
  |> Context.add "fix"  @@ type_to_scheme ((a => a) => a)
  |> Context.add "ite"  @@ type_to_scheme (TBool => (a => (a => a)))
