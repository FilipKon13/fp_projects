open Token
module CharInput = struct
  type stream = char list
  let to_string (s : stream) : string =
    String.of_seq (List.to_seq s)
end
open Monad.MakeParser(CharInput)

let debug (name : string) (p : 'a parser) : 'a parser =
  fun input ->
    let input_str = CharInput.to_string input in
    let prefix = if String.length input_str > 40 then String.sub input_str 0 40 ^ "..." else input_str in
    Printf.printf "[%-16s] Trying on: \"%s\"\n" name (String.escaped prefix);
    match p input with
    | Some (result, rest) ->
        let rest_str = CharInput.to_string rest in
        let rest_prefix = if String.length rest_str > 40 then String.sub rest_str 0 40 ^ "..." else rest_str in
        Printf.printf "[%-16s] Matched. Remaining: \"%s\"\n" name (String.escaped rest_prefix);
        Some (result, rest)
    | None ->
        Printf.printf "[%-16s] Failed.\n" name;
        None

(* Char utilities *)
let is_digit (c : char) : bool = '0' <= c && c <= '9'
let is_alpha (c : char) : bool = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
let is_whitespace (c : char) : bool = c = ' ' || c = '\n' || c = '\t'
let is_wordpart (c : char) : bool = is_alpha c || is_digit c || c = '_'

(* Basic parsers *)

let fchar (f : char -> bool) : char parser = function
  | c :: rest when f c -> Some (c, rest)
  | _ -> None

let chr (c : char) : unit parser = fchar (fun x -> x = c) >> return ()

let alpha : char parser = fchar is_alpha
let digit : char parser = fchar is_digit
let whitespace : unit parser = fchar is_whitespace >> return ()

let skip_whitespace : unit parser = some whitespace >> return ()

let lookup : char parser = fun input ->
  match input with
  | c :: _ -> Some (c, input)
  | _ -> None

(* Operators *)

let match_operator (c : char) (op : operator) : operator parser =
  chr c >> return op

let slash   : operator parser = match_operator '\\' Slash
let dot     : operator parser = match_operator '.' Dot
let lparen  : operator parser = match_operator '(' LParen
let rparen  : operator parser = match_operator ')' RParen
let eq      : operator parser = match_operator '=' Eq
let lbrac   : operator parser = match_operator '<' LBrac
let rbrac   : operator parser = match_operator '>' RBrac
let comma   : operator parser = match_operator ',' Comma
let plus    : operator parser = match_operator '+' Plus
let minus   : operator parser = match_operator '-' Minus
let mul     : operator parser = match_operator '*' Mul
let comp    : operator parser = chr '=' >> chr '=' >> return Comp
let arrow   : operator parser = chr '-' >> chr '>' >> return Arrow

(* Literals *)

let number_literal : literal parser = 
  some digit >>= fun digits ->
  if List.length digits = 1 || List.hd digits != '0' then
    let num_str = String.of_seq (List.to_seq digits) in
    return (Int (int_of_string num_str))
  else
    fail

let literal : token parser =
  number_literal >>= fun lit ->
  return (Literal lit)

(* `arrow` must be before `minus` and `comp` must be before `eq` *)
let oper : token parser =
  (slash <|> arrow <|> dot <|> lparen <|> rparen <|> comp <|> plus <|> minus <|> mul <|> eq <|> lbrac <|> rbrac <|> comma) >>= fun op -> 
  return (Op op)
let alphanum : char parser = alpha <|> digit
let word (s : string) : unit parser = 
  let rec aux chars = 
    match chars with
    | [] -> 
      lookup >>= fun c ->
        if is_wordpart c then fail
        else return ()
    | h :: t -> chr h >> aux t
  in
  aux (List.init (String.length s) (String.get s))
(* Keywords *)
let klet  : keyword parser = word "let" >> return Let
let kin   : keyword parser = word "in" >> return In
let kfun  : keyword parser = word "fun" >> return Fun
let kite  : keyword parser = word "ite" >> return Ite
let keyword : token parser =
  (klet <|> kin <|> kfun <|> kite) >>= fun k -> 
  return (Keyword k)
let name : token parser =
  alpha >>= fun h ->
  many (fchar is_wordpart) >>= fun t ->
  return (Word (String.make 1 h ^ String.concat "" (List.map (String.make 1) t)))
let eof : unit parser = function | [] -> Some ((), []) | _ -> None   
let comment : unit parser =
  chr '/' >> 
  chr '/' >> 
  many (fchar (fun c -> c != '\n')) >> 
  ((chr '\n' >> return ()) <|> eof) >> 
  return ()

let skip : unit parser = many (comment <|> whitespace) >> return ()

let token : token parser = keyword <|> oper <|> name <|> literal
let tokenizer : token list parser =
  skip >>
  many (token << skip) >>= fun tokens ->
  eof >>
  return tokens

let tokenize (input : string) : token list option =
  let input_list = List.init (String.length input) (String.get input) in
  Printf.printf "Tokenizing input: \"%s\"\n" input;
  match tokenizer input_list with
    | Some (tokens, []) -> Some tokens
    | Some (tokens, rest) -> 
        Printf.printf "Unexpected input after tokenization: %s\n" (String.of_seq (List.to_seq rest));
        None
    | None ->
        Printf.printf "Failed to tokenize input.\n";
        None
