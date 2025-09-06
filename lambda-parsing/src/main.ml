open Printf

let main =
  let input = In_channel.input_all stdin in
  let tokens = Tokenization.tokenize input in
  Option.iter (fun ts ->
    printf "Tokens: ";
    List.iter (fun t -> Token.print_token t; printf " ") ts;
    printf "\n") tokens;
  let expr = Option.bind tokens Parsing.parse_expression in
  Option.iter Expression.print_expr expr;
  printf "\n";
  let normal = Option.map Eval.eval_all expr in
  Option.iter (fun nform -> 
    printf "Normal form: ";
    Expression.print_expr nform;
    printf "\n";
  ) normal;
  