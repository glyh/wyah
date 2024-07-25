open Ast
open Type_environment

exception Unreachable

(* Reference: https://github.com/fetburner/Lazy/blob/master/Value.sml *)
type thunk = unit -> value
(* The boolean indicates whether the thunk is forcable *)
and thunk_wrap = bool * thunk ref
and value = 
  | Norm of atom
  | Closure of (thunk_wrap -> value)

let force (thw: thunk_wrap) : value = 
  let (forcable, th) = thw in
  let ret = !th () in
    if forcable then
      th := (fun () -> ret)
    else 
      ()
    ;
    ret

let wrap_unary (f: value -> value) : thunk_wrap = 
  (false, ref (fun () -> Closure (fun th1 ->
    let v1 = th1 |> force in
    f v1
  )))

let wrap_binary (f: value -> value -> value) : thunk_wrap =
  (false, ref (fun () -> Closure (fun th1 -> Closure (fun th2 -> 
    let v1 = th1 |> force in
    let v2 = th2 |> force in
    f v1 v2
  ))))


let put_char (ch: value): value = 
  match ch with
  | Norm(Char(c)) -> 
      print_char c;
      Norm(Unit)
  | _ -> raise Unreachable

let get_char (ch: value): value = 
  match ch with
  | Norm(Unit) -> 
      (* REF: https://stackoverflow.com/questions/13410159/how-to-read-one-character-at-a-time-from-user-input-at-each-keystroke-without*)
      let termio = Unix.tcgetattr Unix.stdin in
      let () =
          Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
              { termio with Unix.c_icanon = false } in
      let res = input_char stdin in
      Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
      Norm(Char res)
  | _ -> raise Unreachable

module EvalEnv = Map.Make(String)
type eval_env = thunk_wrap EvalEnv.t

let add_int (lhs: value) (rhs: value): value = 
  match lhs, rhs with
  | Norm(Int(i)), Norm(Int(j)) -> Norm(Int(i + j))
  | _ -> raise Unreachable

let sub_int (lhs: value) (rhs: value): value = 
  match lhs, rhs with
  | Norm(Int(i)), Norm(Int(j)) -> Norm(Int(i - j))
  | _ -> raise Unreachable

let equal_value (lhs: value) (rhs: value): value =
  match lhs, rhs with
  | Norm(a), Norm(b) ->
      Norm(Bool(compare a b == 0))
  | _ -> raise Unreachable

let ty_equal_value = 
  let a = gen_tvar () in 
    TForall((TypeVarSet.singleton a), TArrow(TVar a, TArrow(TVar a, t_bool)))

let env_init = 
  [
    "+", wrap_binary add_int, TArrow(t_int, TArrow(t_int, t_int));
    "==", wrap_binary equal_value, ty_equal_value;
    "-", wrap_binary sub_int, TArrow(t_int, TArrow(t_int, t_int));
    "putChar", wrap_unary put_char, TArrow(t_char, TIO t_unit);
    "getChar", wrap_unary get_char, TArrow(t_unit, TIO t_char);
    (*
      NOTE: bindIO, thenIO and returnIO can't be integrated for now as we don't have a 
      way to deal with parametric polymorphism
    *)
  ]

let eval_env_init: eval_env = 
  env_init
  |> List.map (fun (id, f, _) -> (id, f))
  |> List.to_seq
  |> EvalEnv.of_seq

let type_env_init: type_env = 
  env_init
  |> List.map (fun (id, _, ty) -> (id, ty))
  |> List.to_seq
  |> TypeEnv.of_seq
