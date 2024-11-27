open Evaluate.Interactive

let eval_env = ref Evaluate.Environment.eval_env_init
let type_env = ref Evaluate.Environment.type_env_init

let compile_and_run (source : string) =
  wyah_pipeline eval_env type_env source |> string_of_result

let test_compile_1 () =
  Alcotest.(check string) "compile 1" "1 : Int" (compile_and_run "1")

let rec fib n =
  if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)

let test_compile_fib n =
  let test_compile_fib_ () =
    Alcotest.(check string)
      ("fib " ^ string_of_int n)
      (Printf.sprintf "%d : Int" (fib n))
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
        ^ string_of_int n))
  in
  test_compile_fib_

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
