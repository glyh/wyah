type tvar = int
  [@@deriving eq]

module TypeVarSet = Set.Make(struct
  type t = tvar
  let compare = compare
end) 

type type_var_set = TypeVarSet.t

let equal_type_var_set (s1: type_var_set) (s2: type_var_set) = 
  compare s1 s2 == 0

(* we consider type scheme to be valid type as well *)
type wyah_type = 
  | TForall of type_var_set * wyah_type
  | TVar of tvar
  | TCon of string (* Unit, Int, Bool, Char *)
  | TArrow of wyah_type * wyah_type
  | TIO of wyah_type
  [@@deriving eq]

let t_unit = TCon "()"
let t_int = TCon "Int"
let t_bool = TCon "Bool"
let t_char = TCon "Char"

module TypeEnv = Map.Make(struct 
  type t = string
  let compare = compare
end)
type type_env = wyah_type TypeEnv.t

let gen_tvar =
  let uid = ref 0 in
  fun () -> 
    uid := !uid + 1;
    !uid

