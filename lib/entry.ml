open Ast
open Type_check
open Evaluator
open Environment

let rec user_input prompt callback =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
    callback v;
    user_input prompt callback

let compile_pipeline source = 
  try
    let parsed = 
      source
      |> Parser.parse_string
    in
    let type_checked = 
      type_check type_env_init parsed

    in let evaluated = evaluate eval_env_init parsed
    in
     (pretty_print_value evaluated ^ " : " ^ (pretty_print_type type_checked)) 
    |> print_endline
  with
    TypeMisMatch(lhs_ty, rhs_ty) ->
      print_endline ("Error: Type mismatch between " ^ pretty_print_type lhs_ty ^ " and " ^ pretty_print_type rhs_ty)

let main () =
  Parser.pp_exceptions ();
  compile_pipeline |> user_input "Arith> "
