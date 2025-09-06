type pattern = 
  | Just of string
  | Tuple of pattern list

type arith_op = 
| Plus 
| Minus 
| Mul
| Eq

type expr =
| Var of string
| App of expr * expr
| Lam of pattern * expr
| TupleE of expr list
| Int of int
| Arith of expr * arith_op * expr
| IfThenElse of expr * expr * expr

let rec print_pattern = function
  | Just name -> Printf.printf "Just(%s)" name
  | Tuple patterns ->
      Printf.printf "Tuple(";
      (match patterns with
      | [] -> ()
      | h :: t ->
          print_pattern h;
          List.iter (fun p -> Printf.printf ", "; print_pattern p) t);
      Printf.printf ")"

let print_arith_op = function
  | Plus -> Printf.printf "Plus"
  | Minus -> Printf.printf "Minus"
  | Mul -> Printf.printf "Mul"
  | Eq -> Printf.printf "Eq"

let rec print_expr = function
  | Var name -> Printf.printf "Var(%s)" name
  | App (e1, e2) -> 
      Printf.printf "App(";
      print_expr e1;
      Printf.printf ", ";
      print_expr e2;
      Printf.printf ")"
  | Lam (p, body) ->
      Printf.printf "Lam(";
      print_pattern p;
      Printf.printf ", ";
      print_expr body;
      Printf.printf ")"
  | TupleE exprs ->
      Printf.printf "TupleE(";
      (match exprs with
      | [] -> ()
      | h :: t ->
          print_expr h;
          List.iter (fun e -> Printf.printf ", "; print_expr e) t);
      Printf.printf ")"
  | Int i -> Printf.printf "Int(%d)" i
  | Arith (e1, op, e2) ->
      Printf.printf "Arith(";
      print_expr e1;
      Printf.printf ", ";
      print_arith_op op;
      Printf.printf ", ";
      print_expr e2;
      Printf.printf ")"
  | IfThenElse (c, t, e) ->
      Printf.printf "IfThenElse(";
      print_expr c;
      Printf.printf ", ";
      print_expr t;
      Printf.printf ", ";
      print_expr e;
      Printf.printf ")"


let rec short_print_pattern = function
  | Just name -> Printf.printf "%s" name
  | Tuple patterns ->
      Printf.printf "<";
      (match patterns with
      | [] -> ()
      | h :: t ->
          short_print_pattern h;
          List.iter (fun p -> Printf.printf ", "; short_print_pattern p) t);
      Printf.printf ">"

let short_print_arith_op = function
  | Plus -> Printf.printf "+"
  | Minus -> Printf.printf "-"
  | Mul -> Printf.printf "*"
  | Eq -> Printf.printf "=="

let rec short_print_expr = function
  | Var name -> Printf.printf "%s" name
  | App (e1, e2) -> 
      Printf.printf "(";
      short_print_expr e1;
      Printf.printf " ";
      short_print_expr e2;
      Printf.printf ")"
  | Lam (p, body) ->
      Printf.printf "\\";
      short_print_pattern p;
      Printf.printf " . ";
      short_print_expr body
  | TupleE exprs ->
      Printf.printf "<";
      (match exprs with
      | [] -> ()
      | h :: t ->
          short_print_expr h;
          List.iter (fun e -> Printf.printf ", "; short_print_expr e) t);
      Printf.printf ">"
  | Int i -> Printf.printf "%d" i
  | Arith (e1, op, e2) ->
      Printf.printf "(";
      short_print_expr e1;
      Printf.printf " ";
      short_print_arith_op op;
      Printf.printf " ";
      short_print_expr e2;
      Printf.printf ")"
  | IfThenElse (c, t, e) ->
      Printf.printf "ite ";
      short_print_expr c;
      Printf.printf " ";
      short_print_expr t;
      Printf.printf " ";
      short_print_expr e;