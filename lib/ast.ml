type value_type = 
  | TInt
  | TBool

type value = 
  | Int of int
  | Bool of bool
  [@@deriving eq]

let pretty_print_value (v: value) =
  match v with
  | Int(i) -> string_of_int i
  | Bool(b) -> string_of_bool b

type expr =
  | Val of value
  | Ident of string
  | If of expr * expr * expr
  | App of expr * expr
  [@@deriving eq]
