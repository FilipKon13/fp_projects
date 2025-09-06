type pattern = 
  | Just of string
  | Tuple of pattern list
type expr =
| Var of string
| App of expr * expr
| Lam of pattern * expr
| TupleE of expr list

let rec print_pattern = function
  | Just name -> Printf.printf "Just(%s)" name
  | Tuple patterns ->
      Printf.printf "Tuple(";
      List.iter (fun p -> print_pattern p; Printf.printf ", ") patterns;
      Printf.printf ")"

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
      List.iter (fun e -> print_expr e; Printf.printf ", ") exprs;
      Printf.printf ")"

let rec short_print_pattern = function
  | Just name -> Printf.printf "%s" name
  | Tuple patterns ->
      Printf.printf "<";
      List.iter (fun p -> short_print_pattern p; Printf.printf ", ") patterns;
      Printf.printf ">"

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
      List.iter (fun e -> short_print_expr e; Printf.printf ", ") exprs;
      Printf.printf ">"