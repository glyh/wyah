open Ast
(*open Type_check*)
open Type_inference
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
  let type_inferenced = 
    inference_type type_env_init parsed

  in let evaluated = evaluate eval_env_init parsed
  in
   (pretty_print_value evaluated ^ " : " ^ (pretty_print_type type_inferenced)) 
  |> print_endline
  with
    UnificationFailure(cons) ->
      print_endline ("Error: Unification failure: " ^ show_cons cons)

let main () =
  Parser.pp_exceptions ();
  compile_pipeline |> user_input "Wyah> "
