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
  | Plus
  | Minus
  | Mul
  | Comp

type keyword = 
  | Let
  | In
  | Fun
  | Ite

type literal =
  | Int of int

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
  | Op Plus -> "Plus"
  | Op Minus -> "Minus"
  | Op Mul -> "Mul"
  | Op Comp -> "Comp"
  | Keyword Let -> "Let"
  | Keyword In -> "In"
  | Keyword Fun -> "Fun"
  | Keyword Ite -> "Ite"
  | Literal (Int i) -> Printf.sprintf "Int(%d)" i

let print_token (t : token) = token_to_string t |> Printf.printf "%s"  
