(* templates for all ASTs *)

type name = string

type 'a _expr =
  | Var of name (* we can represent free vars *)
  | Num of int
  | Constr of int * int
  | Ap of 'a _expr * 'a _expr
  | Let of {
      recursive : bool;
      bindings : ('a * 'a _expr) list;
      body : 'a _expr;
    }
  | Case of { condition : 'a _expr; branches : (int * 'a list * 'a _expr) list }
  | Lam of 'a * 'a _expr

type 'a definition = { name : name; args : 'a list; body : 'a _expr }
type core_expr = name _expr
