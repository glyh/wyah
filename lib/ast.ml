type value_type = 
  | TUnit
  | TInt
  | TBool
  | TChar
  | TArrow of value_type * value_type
  | TIO of value_type
  [@@deriving eq]


type identifier = string
  [@@deriving eq]

type normalized = 
  | Unit
  | Int of int
  | Char of char
  | Bool of bool
  [@@deriving eq]

and expr =
  | Atom of normalized
  | Lam of identifier * value_type * expr
  | Var of identifier
  | If of expr * expr * expr
  | App of expr * expr
  [@@deriving eq]

let rec pretty_print_type (v: value_type) =
  match v with
  | TInt -> "Int"
  | TBool -> "Bool"
  | TChar -> "Char"
  | TIO inner -> "IO " ^ pretty_print_type inner 
  | TUnit -> "()"
  | TArrow(lhs, rhs) -> 
      begin match lhs with
      | TArrow _ ->
        "(" ^ (pretty_print_type lhs) ^ ") -> " ^ (pretty_print_type rhs) 
      | _ -> 
         (pretty_print_type lhs) ^ " -> " ^ (pretty_print_type rhs) 
      end
