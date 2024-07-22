open Ast

exception RuntimeTypeError of value * value_type
exception MalformedAST of expr
exception Unimplemented

let rec evaluate (ast: expr): value =
  match ast with
  | Val(v) -> v
  | If(cond, then_clause, else_clause) ->
      begin match evaluate cond with
      | Bool(true) -> evaluate then_clause
      | Bool(false) -> evaluate else_clause
      | v -> raise (RuntimeTypeError(v, TBool))
      end
  | App((Ident "pred"), inner) ->
      begin match evaluate inner with
      | Int(i) -> Int(i-1)
      | v -> raise (RuntimeTypeError(v, TInt))
      end

  | App((Ident "succ"), inner) ->
      begin match evaluate inner with
      | Int(i) -> Int(i+1)
      | v -> raise (RuntimeTypeError(v, TInt))
      end

  | App((Ident "iszero"), inner) ->
      begin match evaluate inner with
      | Int(i) -> Bool(i == 0)
      | v -> raise (RuntimeTypeError(v, TInt))
      end

  | _ -> raise (MalformedAST ast)
