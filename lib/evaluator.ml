open Ast
open Environment 

exception Unreachable

let char_escape c =
  match c with
  | '\'' -> "'\\''"
  | '\\' -> "'\\\\'"
  | c -> "'" ^ ([c] |> List.to_seq |> String.of_seq) ^ "'"

let pretty_print_value (v: value) =
  match v with
  | Norm(Int(i)) -> string_of_int i
  | Norm(Unit) -> "()"
  | Norm(Bool(b)) -> string_of_bool b
  | Norm(Char(c)) -> char_escape c
  | Closure(_) -> "<<closure>>"

let rec make_thunk (env: eval_env) (id: identifier) (body: expr) : (thunk_wrap -> value) = 
  fun th ->
    let env_new = EvalEnv.add id th env in
    evaluate env_new body

and evaluate (env: eval_env) (ast: expr): value =
  match ast with
  | Atom(a) -> Norm(a)
  | Var(name) -> 
      force (EvalEnv.find name env)
  | App(f, x) ->
    begin match evaluate env f with
    | Closure c -> c (true, ref (fun () -> evaluate env x))
    | _ -> raise Unreachable
    end
  | Lam(arg, _, body) ->
    Closure (make_thunk env arg body)
  | If(cond, then_clause, else_clause) ->
      begin match (evaluate env cond) with
      | Norm(Bool(cond)) ->
          evaluate env (if cond then then_clause else else_clause)
      | _ -> raise Unreachable
      end
