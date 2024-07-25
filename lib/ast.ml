open Type_environment

exception Unreachable

type identifier = string
  [@@deriving eq]

type atom = 
  | Unit
  | Int of int
  | Char of char
  | Bool of bool
  [@@deriving eq]

and expr =
  | Atom of atom
  | Lam of identifier * expr
  | Fix of expr
  | LetIn of identifier * expr * expr
  | Var of identifier
  | If of expr * expr * expr
  | App of expr * expr
  [@@deriving eq]

let pretty_print_var_set (s: type_var_set) =
  s
  |> TypeVarSet.to_seq
  |> List.of_seq
  |> List.map string_of_int
  |> String.concat " "

let rec pretty_print_type (v: wyah_type) =
  match v with
  | TVar v -> string_of_int v
  | TCon ty -> ty
  | TIO inner -> "IO " ^ pretty_print_type inner 
  | TArrow(lhs, rhs) -> 
      begin match lhs with
      | TArrow _ ->
        "(" ^ (pretty_print_type lhs) ^ ") -> " ^ (pretty_print_type rhs) 
      | _ -> 
         (pretty_print_type lhs) ^ " -> " ^ (pretty_print_type rhs) 
      end
  | TForall(vars, inner) -> pretty_print_var_set vars ^ " . " ^ pretty_print_type inner
