open Token
open Tokenization
module TokenInput = struct
  type stream = token list
  let to_string (s : stream) : string =
    String.concat " " (List.map token_to_string s)
end
open Monad.MakeParser(TokenInput)
open Expression

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

let rec expr : expr parser = fun s -> 
  (lam <|> let_in_expr <|> fun_lam <|> app <|> value) s
and non_empty_tuple_expr : expr parser = fun s ->
  (match_token (Op LBrac) >>
  expr >>= fun h ->
  many (match_token (Op Comma) >> expr) >>= fun t ->
  match_token (Op RBrac) >>
  return (TupleE (h :: t))) s
and tuple : expr parser = fun s ->
  (empty_tuple_expr <|> non_empty_tuple_expr) s
and value : expr parser = fun s ->
  (var <|> par_expr <|> tuple) s
and par_expr : expr parser = fun s ->
  (match_token (Op LParen) >> (expr << match_token (Op RParen))) s
and app : expr parser = fun s ->
  (some value >>= fun lst ->
  let rec get_app acc lst =
    match lst with 
      | [] -> acc
      | x :: xs -> get_app (App (acc, x)) xs
  in
  match lst with
  | x :: y :: r -> return (get_app (App (x, y)) r)
  | _ -> fail) s
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
