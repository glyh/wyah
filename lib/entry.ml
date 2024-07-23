let rec user_input prompt callback =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
    callback v;
    user_input prompt callback

let compile_pipeline source = 
  let parsed = 
    source
    |> Parser.parse_string
  in
  let type_checked = 
    Type_check.type_check Type_check.Env.empty parsed
  in let evaluated = Evaluator.evaluate Evaluator.Env.empty parsed
  in
   ((Ast.pretty_print_value evaluated) ^ " : " ^ (Ast.pretty_print_type type_checked)) 
  |> print_endline

let main () =
  Parser.pp_exceptions ();
  compile_pipeline |> user_input "Arith> "
