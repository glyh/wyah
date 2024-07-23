open Ast

exception TypeMisMatch of value_type * value_type
exception NotCallable of value_type
exception MalformedAST of expr
exception Unimplemented
exception UnboundIdentifier of identifier

module Env = Map.Make(String)
type type_env = value_type Env.t
let rec type_check (env: type_env) (ast: expr): value_type = 
  match ast with
  | Val(Int _) -> TInt
  | Val(Bool _) -> TBool
  | Val(Lam(id, ty, body)) -> 
    let env_new = Env.add id ty env in
    let type_body = type_check env_new body in
    TArrow(ty, type_body)
  | Var(id) ->
    begin match Env.find_opt id env with
    | Some(ty) -> ty
    | None -> raise (UnboundIdentifier id)
    end
  | If(cond, then_clause, else_clause) ->
    let type_cond = type_check env cond in
    let type_then = type_check env then_clause in
    let type_else = type_check env else_clause in
    if type_cond == TBool
    then
        if type_then == type_else 
        then
            type_then
        else
            raise (TypeMisMatch(type_then, type_else))
    else
        raise (TypeMisMatch(type_cond, TBool))
  | App(f, x) ->
    let type_f = type_check env f in
    let type_x = type_check env x in
    match type_f with
    | TArrow(t_lhs, t_rhs) -> 
      if t_lhs == type_x 
      then 
        t_rhs
      else
        raise (TypeMisMatch(t_lhs, type_x))
    | t -> raise (NotCallable(t))

