type value_type = 
  | TInt
  | TBool
  | TArrow of value_type * value_type
  [@@deriving eq]


type identifier = string
  [@@deriving eq]

type value = 
  | Int of int
  | Bool of bool
  | Lam of identifier * value_type * expr
  [@@deriving eq]

and expr =
  | Val of value
  | Var of identifier
  | If of expr * expr * expr
  | App of expr * expr
  [@@deriving eq]

let rec pretty_print_type (v: value_type) =
  match v with
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow(lhs, rhs) -> 
      begin match lhs with
      | TArrow _ ->
        "(" ^ (pretty_print_type lhs) ^ ") -> " ^ (pretty_print_type rhs) 
      | _ -> 
         (pretty_print_type lhs) ^ " -> " ^ (pretty_print_type rhs) 
      end

let pretty_print_value (v: value) =
  match v with
  | Int(i) -> string_of_int i
  | Bool(b) -> string_of_bool b
  | Lam(_) -> "<<closure>>"
