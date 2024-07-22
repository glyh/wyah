let rec user_input prompt callback =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
    callback v;
    user_input prompt callback

let compile_pipeline source = 
  source
  |> Parser.parse_string
  |> Evaluator.evaluate
  |> Ast.pretty_print_value
  |> print_endline

let main () =
  compile_pipeline |> user_input "Arith> "
