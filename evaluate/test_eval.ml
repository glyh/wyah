open Interactive

let eval_env = ref Environment.eval_env_init
let type_env = ref Environment.type_env_init

let compile_and_run (source : string) =
  wyah_pipeline eval_env type_env source |> string_of_result

let%test "compile 1" = "1 : Int" = compile_and_run "1"

let rec fib n =
  if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)

let fib_test n =
  Printf.sprintf "%d : Int" (fib n)
  = compile_and_run
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

let%test "fib 7" = fib_test 7
let%test "fib 20" = fib_test 20
