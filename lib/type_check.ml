open Ast
open Environment

exception TypeMisMatch of value_type * value_type
exception NotCallable of value_type
exception MalformedAST of expr
exception Unimplemented
exception UnboundIdentifier of identifier

let rec type_check (env: type_env) (ast: expr): value_type = 
  match ast with
  | Atom(Int _) -> TInt
  | Atom(Bool _) -> TBool
  | Atom(Char _) -> TChar
  | Atom(Unit) -> TUnit
  | Lam(id, ty, body) -> 
    let env_new = TypeEnv.add id ty env in
    let type_body = type_check env_new body in
    TArrow(ty, type_body)
  | Var(id) ->
    begin match TypeEnv.find_opt id env with
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
      if compare t_lhs type_x == 0
      then 
        t_rhs
      else
        raise (TypeMisMatch(t_lhs, type_x))
    | t -> raise (NotCallable(t))

