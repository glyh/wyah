open Core_frontend.Parser
open Machine
open Compile
open Debug_machine

let%test "Exer. 3.3" =
  let sc_defs = parse_string "S f g x = f x (g x);" in
  let s_def = List.hd sc_defs in
  let heap = BatDynArray.create () in
  let _, node = compile_sc heap s_def in
  match heap_find heap node with
  | NGlobal (argc, code) ->
      argc = 3
      && BatDeque.eq code
           ~&[
               Push 2;
               Push 2;
               MkAp;
               Push 3;
               Push 2;
               MkAp;
               MkAp;
               Update 3;
               Pop 3;
               Unwind;
             ]
  | _ -> false

exception NotValOnStack

let take_int_result state =
  match state.stack with
  | addr :: _ -> (
      match heap_find state.heap addr with
      | NNum i -> i
      | _ -> raise NotValOnStack)
  | _ -> raise NotValOnStack

let test_ti code = code |> compile |> eval

let test_debug code =
  let start_state = code |> compile in
  let rec eval_debug state =
    print_string (string_of_state state);
    if is_final state then state else eval_debug (step state)
  in
  eval_debug start_state

let%test "Exer. 3.4" =
  let final_state = test_ti "main = S K K 3;" in
  Printf.printf "Testing `S K K 3` takes %d steps\n" final_state.stats.step;
  3 == take_int_result final_state

(*let%test "Exer. 3.4" =*)
(*  " main = S K K 3;" |> test_debug |> ignore;*)
(*  true*)
