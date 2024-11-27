open Type_environment

let pretty_print_var_set (s : type_var_set) =
  s |> TypeVarSet.to_seq |> List.of_seq |> List.map string_of_int
  |> String.concat " "

let rec pretty_print_type (v : wyah_type) =
  match v with
  | TVar v -> string_of_int v
  | TCon ty -> ty
  | TIO inner -> "IO " ^ pretty_print_type inner
  | TArrow (lhs, rhs) -> (
      match lhs with
      | TArrow _ ->
          "(" ^ pretty_print_type lhs ^ ") -> " ^ pretty_print_type rhs
      | _ -> pretty_print_type lhs ^ " -> " ^ pretty_print_type rhs)
  | TForall (vars, inner) ->
      pretty_print_var_set vars ^ " . " ^ pretty_print_type inner
