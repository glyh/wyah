open Template_instantiate

exception StackTooMany
exception NotValOnStack

let take_result state =
  match state.stack with
  | [] -> raise EmptyStack
  | [ addr ] -> (
      match BatDynArray.get state.heap addr with
      | NNum i -> i
      | _ -> raise NotValOnStack)
  | _ -> raise StackTooMany

let test_ti code = code |> compile |> eval |> take_result
let%test "Exercise 2.4" = test_ti "main = S K K 3;" = 3

let%test "2.4" =
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

let%test "2.13" = test_ti {|
main = twice twice twice I 3;
|} = 3
