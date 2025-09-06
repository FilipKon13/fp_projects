(* Tokens *)
type operator = 
  | Slash
  | Dot
  | LParen
  | RParen
  | LBrac
  | RBrac
  | Eq
  | Comma
  | Arrow

type keyword = 
  | Let
  | In
  | Fun

type literal = 
  | String of string
  | Int of int
  | Char of char

type token = 
  | Word of string
  | Op of operator
  | Keyword of keyword
  | Literal of literal

let token_to_string = function
  | Word w -> Printf.sprintf "Word(%s)" w
  | Op Slash -> "Slash"
  | Op Dot -> "Dot"
  | Op LParen -> "LParen"
  | Op RParen -> "RParen"
  | Op LBrac -> "LBrac"
  | Op RBrac -> "RBrac"
  | Op Eq -> "Eq"
  | Op Comma -> "Comma"
  | Op Arrow -> "Arrow"
  | Keyword Let -> "Let"
  | Keyword In -> "In"
  | Keyword Fun -> "Fun"
  | Literal (String s) -> Printf.sprintf "String(%s)" s
  | Literal (Int i) -> Printf.sprintf "Int(%d)" i
  | Literal (Char c) -> Printf.sprintf "Char(%c)" c

let print_token (t : token) = token_to_string t |> Printf.printf "%s"  
