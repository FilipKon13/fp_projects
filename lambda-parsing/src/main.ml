open Printf

let main =
  let input = In_channel.input_all stdin in
  let tokens = Tokenization.tokenize input in
  match tokens with
  | None ->
      eprintf "Tokenization failed.\n";
      exit 1
  | Some ts -> (
      printf "Tokens: ";
      List.iter (fun t -> Token.print_token t; printf " ") ts;
      printf "\n";
      match Parsing.parse_expression ts with
      | None ->
          eprintf "Parsing failed.\n";
          exit 1
      | Some expr ->
          Expression.print_expr expr;
          printf "\n";
          let normal = Eval.eval_all expr in
          printf "Normal form: ";
          Expression.print_expr normal;
          printf "\n";
          exit 0 )