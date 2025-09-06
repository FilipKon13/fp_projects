open Types
open Inference
open Utils
open Examples

let handle_example (e : expr) = 
  let fc = VarFactory.make_vf () in
  match type_inference fc default_context e with
    | Ok (ty, subst) ->
      Printf.printf "Expression: %s\nType: %s\n\n"
        (string_of_expr e)
        (string_of_type ty)
    | Error msg ->
      Printf.printf "Expression: %s\nError: %s\n\n"
        (string_of_expr e) msg

let () = 
  List.iter handle_example e_arr

