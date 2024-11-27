open Frontend
open Typecheck

type interactive_result =
  | Value of Environment.value * Typecheck.Type_environment.wyah_type
  | Definition of string * Type_environment.wyah_type

let string_of_result (r : interactive_result) =
  match r with
  | Value (evaluated, type_inferenced) ->
      Evaluator.pretty_print_value evaluated
      ^ " : "
      ^ Pretty_print_type.pretty_print_type type_inferenced
  | Definition (name, type_inferenced) ->
      name ^ " : " ^ Pretty_print_type.pretty_print_type type_inferenced

let wyah_pipeline eval_env type_env source =
  let parsed = source |> Parser.parse_string in
  match parsed with
  | Expr exp ->
      let type_inferenced = Type_inference.inference_type !type_env exp in
      let evaluated = Evaluator.evaluate !eval_env exp in
      Value (evaluated, type_inferenced)
  | Definition (name, exp) ->
      let type_inferenced = Type_inference.inference_type !type_env exp in
      let evaluated = Evaluator.evaluate !eval_env exp in
      eval_env :=
        Environment.EvalEnv.add name (ref (fun () -> evaluated)) !eval_env;
      type_env := Type_environment.TypeEnv.add name type_inferenced !type_env;
      Definition (name, type_inferenced)
