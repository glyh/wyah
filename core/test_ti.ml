open Template_instantiate

exception StackTooMany
exception NotValOnStack

let take_int_result state =
  match state.stack with
  | [] -> raise EmptyStack
  | [ addr ] ->
      let rec resolve_addr addr =
        match BatDynArray.get state.heap addr with
        | NNum i -> i
        | NInd addr -> resolve_addr addr
        | _ -> raise NotValOnStack
      in
      resolve_addr addr
  | _ -> raise StackTooMany

let test_ti code = code |> compile |> eval |> take_int_result

let test_throw_abort code =
  try
    let _ = code |> compile |> eval in
    false
  with Aborted -> true

let%test "Exer. 2.4" = test_ti "main = S K K 3;" = 3

let%test "Sect. 2.4" =
  test_ti
    {|
pair x y f = f x y;
fst p = p K;
snd p = p K1;
f x y = 
  letrec
    a = pair x b;
    b = pair y a
  in 
  fst (snd (snd (snd a)));
main = f 3 4;
|}
  = 4

let test_stats_opts opts code =
  let final_state = code |> compile |> eval_aux opts in
  final_state.stats

let%test "Exer. 2.13" =
  let source = {| 
main = twice twice twice I 3;
  |} in
  let stats = test_stats_opts (create_step_options () ~redirect:false) source in
  Printf.printf "Running 2.13 on Mark 1 takes %d steps\n" stats.steps;
  let stats = test_stats_opts (create_step_options () ~redirect:true) source in
  Printf.printf "Running 2.13 on Mark 2 takes %d steps\n" stats.steps;
  true

let test_ti_debug code =
  code |> compile
  |> eval_aux { debug_channel = Out_channel.stdout; redirect = true }
  |> take_int_result

let%test "Exer. 2.16" =
  test_ti "main = negate 3;" = -3
  && test_ti "main = twice negate 3;" = 3
  && test_ti "main = negate (I 3);" = -3

let%test "Exer. 2.17" =
  test_ti "main = 3 * 3 + 4 * 4 - 5 * 5;" = 0
  && test_ti "main = twice negate 3;" = 3

let%test "Sect. 2.7.2" =
  test_ti "main = if True 1 2;" = 1 && test_ti "main = if False 1 2;" = 2

let%test "Exer. 2.20" =
  test_ti "main = if (True & (False | (not False))) 1 0;" = 1
  && test_ti "main = if (or (xor True False) (and (not True) False)) 1 0;" = 1
  && test_ti "main = if (xor (or (and True False) True) (not False)) 1 0;" = 0

let%test "Exer. 2.21" =
  test_ti {|
fac n = if (n == 0) 1 (n * fac (n - 1));
main = fac 3;
|} = 6

let%test "Exer. 2.22" =
  test_ti {|
  main = fst (snd (fst (MkPair (MkPair 1 (MkPair 2 3)) 4)));
|} = 2

let%test "Exer. 2.24" =
  test_throw_abort {|main = head Nil; |}
  && test_throw_abort {| main = tail Nil; |}
  && test_ti {| main = length Nil; |} = 0
  && test_ti {| main = length (1 :: 2 :: 3 :: 4 :: ( 5 :: Nil) :: Nil); |} = 5
