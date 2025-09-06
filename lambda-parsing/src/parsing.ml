open Token
open Tokenization
module TokenInput = struct
  type stream = token list
  let to_string (s : stream) : string =
    String.concat " " (List.map token_to_string s)
end
open Monad.MakeParser(TokenInput)
open Expression

let debug (name : string) (p : 'a parser) : 'a parser =
  fun input ->
    let input_str = TokenInput.to_string input in
    let prefix = if String.length input_str > 80 then String.sub input_str 0 80 ^ "..." else input_str in
    Printf.printf "[%-16s] Trying on: \"%s\"\n" name prefix;
    match p input with
    | Some (result, rest) ->
        let rest_str = TokenInput.to_string rest in
        let rest_prefix = if String.length rest_str > 80 then String.sub rest_str 0 80 ^ "..." else rest_str in
        Printf.printf "[%-16s] Matched. Remaining: \"%s\"\n" name rest_prefix;
        Some (result, rest)
    | None ->
        Printf.printf "[%-16s] Failed.\n" name;
        None

let match_token (t : token) : unit parser = function
  | tok :: rest when tok = t -> Some ((), rest)
  | _ -> None

let vname : string parser = function
  | Word s :: rest -> Some (s, rest)
  | _ -> None

(* Patterns *)
let empty_tuple : pattern parser =
  match_token (Op LBrac) >> 
  match_token (Op RBrac) >> 
  return (Tuple [])
let just : pattern parser = 
  vname >>= fun name ->
  return (Just name)
let rec pattern : pattern parser = fun s ->
  (tuple <|> just) s
and non_empty_tuple : pattern parser = fun s ->
  (match_token (Op LBrac) >>
  pattern >>= fun h ->
  many (match_token (Op Comma) >> pattern) >>= fun t ->
  match_token (Op RBrac) >>
  return (Tuple (h :: t))) s
and tuple : pattern parser = fun s ->
  (empty_tuple <|> non_empty_tuple) s

let args : pattern list parser = some pattern

(* Expressions *)
let var : expr parser = 
  vname >>= fun name ->
  return (Var name)
let empty_tuple_expr : expr parser =
  match_token (Op LBrac) >> 
  match_token (Op RBrac) >> 
  return (TupleE [])

let literal : expr parser = function
  | Literal (Token.Int i) :: rest -> Some (Int i, rest)
  | _ -> None

let rec expr : expr parser = fun s ->
  (lam <|> let_in_expr <|> fun_lam <|> comp_expr) s
and comp_expr : expr parser = fun s ->
  (
    add_expr >>= fun e1 ->
    many (
      (match_token (Op Comp) >> return Eq) >>= fun op ->
      add_expr >>= fun e2 ->
      return (op, e2)
    ) >>= fun op_e2s ->
    return (List.fold_left (fun acc (op, e2) -> Arith (acc, op, e2)) e1 op_e2s)
  ) s
and add_expr : expr parser = fun s ->
  (
    mul_expr >>= fun e1 ->
    many (
      ((match_token (Op Plus) >> return Plus) <|> (match_token (Op Minus) >> return Minus)) >>= fun op ->
      mul_expr >>= fun e2 ->
      return (op, e2)
    ) >>= fun op_e2s ->
    return (List.fold_left (fun acc (op, e2) -> Arith (acc, op, e2)) e1 op_e2s)
  ) s
and mul_expr : expr parser = fun s ->
  (
    app_expr >>= fun e1 ->
    many (
      (match_token (Op Mul) >> return Mul) >>= fun op ->
      app_expr >>= fun e2 ->
      return (op, e2)
    ) >>= fun op_e2s ->
    return (List.fold_left (fun acc (op, e2) -> Arith (acc, op, e2)) e1 op_e2s)
  ) s
and app_expr : expr parser = fun s ->
  (
    some value >>= fun es ->
    match es with
    | [] -> fail
    | e::es' -> return (List.fold_left (fun acc e -> App(acc, e)) e es')
  ) s
and non_empty_tuple_expr : expr parser = fun s ->
  (match_token (Op LBrac) >>
  expr >>= fun h ->
  many (match_token (Op Comma) >> expr) >>= fun t ->
  match_token (Op RBrac) >>
  return (TupleE (h :: t))) s
and tuple_expr : expr parser = fun s ->
  (empty_tuple_expr <|> non_empty_tuple_expr) s
and value : expr parser = fun s ->
  (var <|> par_expr <|> tuple_expr <|> literal <|> ite_expr) s
and par_expr : expr parser = fun s ->
  (match_token (Op LParen) >> (expr << match_token (Op RParen))) s
and ite_expr : expr parser = fun s ->
  (match_token (Keyword Ite) >>
  value >>= fun c ->
  value >>= fun t ->
  value >>= fun e ->
  return (IfThenElse (c, t, e))) s
and let_in_expr : expr parser = fun s ->
  (match_token (Keyword Let) >>
  pattern >>= fun pat ->
  match_token (Op Eq) >>
  expr >>= fun value ->
  match_token (Keyword In) >>
  expr >>= fun body ->
  return (App (Lam (pat, body), value))) s
and fun_lam : expr parser = fun s ->
  (match_token (Keyword Fun) >>
  args >>= fun args ->
  match_token (Op Arrow) >>
  expr >>= fun body ->
  return (List.fold_right (fun v acc -> Lam (v, acc)) args body)) s
and lam : expr parser = fun s ->
  (match_token (Op Slash) >>
  args >>= fun args ->
  match_token (Op Dot) >>
  expr >>= fun body ->
  return (List.fold_right (fun v acc -> Lam (v, acc)) args body) ) s

let parse_expression (tokens : token list) : expr option =
  match expr tokens with
  | Some (e, []) -> Some e
  | Some (e, rest) ->
      Printf.printf "Unexpected tokens after expression: ";
      List.iter print_token rest;
      Printf.printf "\nParsed expression: ";
      print_expr e;
      Printf.printf "\n";
      None
  | None ->
      Printf.printf "Failed to parse expression.\n";
      None
