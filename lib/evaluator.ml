open Ast

exception Unreachable

module Env = Map.Make(String)
type value_env = value Env.t

let rec evaluate (env: value_env) (ast: expr): value =
  match ast with
  | Val(v) -> v
  | Var(name) -> Env.find name env
  | App(f, x_exp) ->
    let f = evaluate env f in
    let x_exp = evaluate env x_exp in
    begin match f with
    | Lam(x_name, _, body) ->
        evaluate (Env.add x_name x_exp env) body
    | _ -> raise Unreachable
    end
  | If(cond, then_clause, else_clause) ->
      begin match evaluate env cond with
      | Bool(true) -> evaluate env then_clause
      | Bool(false) -> evaluate env else_clause
      | _ -> raise Unreachable
      end
