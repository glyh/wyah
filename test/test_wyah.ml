open Wyah_compile

exception ExpectExpr

type run_result = Type_environment.wyah_type * Environment.value

let compile_and_run source : run_result =
  let parsed = Parser.parse_string source in
  let exp =
    match parsed with Expr exp -> exp | Definition _ -> raise ExpectExpr
  in
  let ty_inferred = Type_inference.inference_type !Entry.type_env exp in
  let evaluated = Evaluator.evaluate !Entry.eval_env exp in
  (ty_inferred, evaluated)

let pp (type_inferenced, evaluated) =
  Evaluator.pretty_print_value evaluated
  ^ " : "
  ^ Ast.pretty_print_type type_inferenced

let test_compile_1 () =
  Alcotest.(check string)
    "compile 1"
    ((Type_environment.t_int, Environment.Norm (Int 1)) |> pp)
    (compile_and_run "1" |> pp)

let rec fib n =
  if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)

let test_compile_fib n =
  let test_compile_fib_ () =
    Alcotest.(check string)
      ("fib " ^ string_of_int n)
      ((Type_environment.t_int, Environment.Norm (Int (fib n))) |> pp)
      (compile_and_run
         ({|
let rec fib n =
  if n == 0 then
    0
  else if n == 1 then
    1
  else
    fib (n - 1) + fib (n - 2)
in
fib |}
        ^ string_of_int n)
      |> pp)
  in
  test_compile_fib_

(* Run it *)
let () =
  let open Alcotest in
  run "Utils"
    [
      ("simple compile", [ test_case "1" `Quick test_compile_1 ]);
      ( "fib",
        [
          test_case "fib 7" `Quick (test_compile_fib 7);
          test_case "fib 13" `Slow (test_compile_fib 13);
        ] );
    ]
