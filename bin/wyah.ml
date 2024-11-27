open Frontend
open Typecheck

let rec user_input prompt callback =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
      callback v;
      user_input prompt callback

let eval_env = ref Evaluate.Environment.eval_env_init
let type_env = ref Evaluate.Environment.type_env_init

let interactive_pipeline source =
  try
    Evaluate.Interactive.wyah_pipeline eval_env type_env source
    |> Evaluate.Interactive.string_of_result |> print_endline
  with Type_inference.UnificationFailure cons ->
    print_endline
      ("Error: Unification failure: " ^ Type_inference.show_cons cons)

let () =
  Parser.pp_exceptions ();
  interactive_pipeline |> user_input "Wyah> "
