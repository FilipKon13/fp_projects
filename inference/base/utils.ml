open Types

let rec string_of_expr = function
  | EVar x -> x
  | EApp (e1, e2) -> "(" ^ string_of_expr e1
                    ^ " " ^ string_of_expr e2 ^ ")"
  | EAbs (x, e) -> "(λ" ^ x ^ ". " ^ string_of_expr e ^ ")"
  | ELet (x, e1, e2) -> "(let " ^ x
                    ^ " = " ^ string_of_expr e1
                    ^ " in " ^ string_of_expr e2 ^ ")"

let rec string_of_type = function
  | TVar x -> x
  | TFun (t1, t2) -> "(" ^ string_of_type t1
                    ^ " -> " ^ string_of_type t2 ^ ")"

let string_of_scheme (Scheme (vars, ty)) =
  let vars_str = String.concat ", " vars in
  "∀" ^ vars_str ^ ". " ^ string_of_type ty

let string_of_context (ctx: context) : string =
  "{" ^ Context.fold (fun k v acc -> acc ^ k ^ " : " ^ string_of_scheme v ^ ", ") ctx "" ^ "}"
