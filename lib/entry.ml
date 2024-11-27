open Ast
open Type_inference
open Type_environment
open Evaluator
open Environment

let eval_env = ref eval_env_init
let type_env = ref type_env_init

let compile_pipeline source =
  Parser.pp_exceptions ();
  try
    let parsed = source |> Parser.parse_string in
    match parsed with
    | Expr exp ->
        let type_inferenced = inference_type !type_env exp in
        let evaluated = evaluate !eval_env exp in
        pretty_print_value evaluated ^ " : " ^ pretty_print_type type_inferenced
        |> print_endline
    | Definition (name, exp) ->
        let type_inferenced = inference_type type_env_init exp in
        let evaluated = evaluate eval_env_init exp in
        eval_env := EvalEnv.add name (ref (fun () -> evaluated)) !eval_env;
        type_env := TypeEnv.add name type_inferenced !type_env;
        name ^ " : " ^ pretty_print_type type_inferenced |> print_endline
  with UnificationFailure cons ->
    print_endline ("Error: Unification failure: " ^ show_cons cons)
