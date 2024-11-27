exception Unreachable

type identifier = string [@@deriving eq]

type atom = Unit | Int of int | Char of char | Bool of bool [@@deriving eq]

and expr =
  | Atom of atom
  | Lam of identifier * expr
  | Fix of expr
  | LetIn of identifier * expr * expr
  | Var of identifier
  | If of expr * expr * expr
  | App of expr * expr
[@@deriving eq]

type definition = identifier * expr
type top_level = Expr of expr | Definition of definition
