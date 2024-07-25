open Type_environment
open Ast

(* NOTE:
    Some references:
    1. https://course.ccs.neu.edu/cs4410/lec_type-inference_notes.html
    2. https://cs3110.github.io/textbook/chapters/interp/inference.html
*)

exception UndefinedVariable of string
exception Unimplemented
exception NotEnoughConstraint of wyah_type

type type_constraint = wyah_type * wyah_type

exception UnificationFailure of type_constraint

let rec free_variables (scheme: wyah_type): type_var_set = 
  match scheme with
  | TForall(vars, inner) -> TypeVarSet.diff (free_variables inner) vars
  | TVar(v) -> TypeVarSet.singleton v
  | TArrow(lhs, rhs) -> TypeVarSet.union (free_variables lhs) (free_variables rhs)
  | TIO(inner) -> free_variables inner
  | _ -> TypeVarSet.empty

let free_variables_env (env: type_env): type_var_set = 
  TypeEnv.fold
    (fun _ v acc -> TypeVarSet.union acc (free_variables v))
    env
    TypeVarSet.empty

module MapSubs = Map.Make(struct
  type t = tvar
  let compare = compare
end)
type subst = wyah_type MapSubs.t

let rec sub_one_recursive (m: subst) (inner_ty: wyah_type): wyah_type =
  match inner_ty with
  | TVar(v) ->
      begin match MapSubs.find_opt v m with
      | None -> TVar(v)
      | Some(w) -> w
      end
  | TForall(vs, inner) -> TForall(vs, sub_one_recursive m inner) 
  | TArrow(lhs, rhs) -> TArrow(sub_one_recursive m lhs, sub_one_recursive m rhs)
  | TIO(inner) -> TIO(sub_one_recursive m inner)
  | t -> t

let instantiate (scheme: wyah_type): wyah_type = 
  match scheme with
  | TForall(vars, inner) -> 
      let vars_pair = 
        vars
        |> TypeVarSet.to_seq
        |> Seq.map (fun v -> (v, TVar (gen_tvar ())))
        |> MapSubs.of_seq
      in
        sub_one_recursive vars_pair inner
  | t -> t

let rec unify_one (c: type_constraint): (tvar * wyah_type) option * type_constraint list =
  match c with
  | (lhs, rhs) when compare lhs rhs == 0 ->
      None, []
  | (TForall (vs1, inner1),TForall (vs2, inner2)) ->
      if compare vs1 vs2 == 0
      then
        unify_one (inner1, inner2)
      else
        raise (UnificationFailure c)
  | (TCon lhs, TCon rhs) ->
      if lhs == rhs
      then
        None, []
      else
        raise (UnificationFailure (c))
  | (TVar v, ty) | (ty, TVar v) ->
      let fvs_rhs = free_variables ty in
      if TypeVarSet.mem v fvs_rhs
      then
        raise (UnificationFailure (c))
      else
        Some (v, ty), []
  | (TArrow(l1, l2), TArrow (r1, r2)) ->
      None, [l1, r1; l2, r2]
  | (TIO lhs, TIO rhs) ->
      unify_one (lhs, rhs)
  | _ ->
        raise (UnificationFailure (c))

let rec apply_sub_ty (s: subst) (ty: wyah_type): wyah_type = 
  match ty with
  | TForall(vs, inner) -> TForall(vs, apply_sub_ty s inner)
  | TVar(v) -> 
      begin match MapSubs.find_opt v s with
      | Some(to_sub) -> to_sub
      | None -> TVar(v)
      end
  | TArrow(lhs, rhs) ->
      TArrow(apply_sub_ty s lhs, apply_sub_ty s rhs)
  | TIO inner ->
      TIO(apply_sub_ty s inner)
  | t -> t

let apply_sub_sub (s_applied: subst) (s_target: subst): subst = 
  s_target
  |> MapSubs.map (apply_sub_ty s_applied)

let apply_sub_cs (s: subst) (cs: type_constraint list): type_constraint list =
  cs
  |> List.map (fun (tlhs, trhs) -> (apply_sub_ty s tlhs, apply_sub_ty s trhs)) 

let apply_sub_env (s: subst) (env: type_env): type_env = 
  TypeEnv.map (apply_sub_ty s) env

let rec unify (cs: type_constraint list): subst = 
  match cs with
  | [] -> MapSubs.empty
  | c :: cs ->
    begin match unify_one c with
    | Some(s_v, s_t), cs_new -> MapSubs.add s_v s_t (unify (cs_new @ (apply_sub_cs (MapSubs.singleton s_v s_t) cs)))
    | None, cs_new -> unify (cs_new @ cs)
    end

let rec generalize (cs: type_constraint list) (env: type_env) (var: identifier) (ty: wyah_type): type_env = 
  let sub_solved = unify cs in
  let env_new = apply_sub_env sub_solved env in
  let fv_env_new = free_variables_env env_new in
  let ty_new = apply_sub_ty sub_solved ty in
  let fv_ty_new = free_variables ty_new in 
  let vars_to_generalize = TypeVarSet.diff fv_ty_new fv_env_new in
  let ty_generalized = TForall(vars_to_generalize, ty_new) in
  TypeEnv.add var ty_generalized env_new
and inference_constraints (tenv: type_env) (exp: expr): (wyah_type * type_constraint list) = 
  match exp with
  | Atom(Unit) -> (t_unit, [])
  | Atom(Int _) -> (t_int, [])
  | Atom(Char _) -> (t_char, [])
  | Atom(Bool _) -> (t_bool, [])

  | LetIn(var, var_exp, inner) ->
      let (var_exp_ty, var_exp_cons) = inference_constraints tenv var_exp in
      let env_generalized = generalize var_exp_cons tenv var var_exp_ty in
      inference_constraints env_generalized inner

  | Fix(lam) ->
      let f_gen = TVar (gen_tvar ()) in
      let wrapped_gen = TVar (gen_tvar ()) in
      let (lam_ty, lam_cons) = inference_constraints tenv lam in
      f_gen, (lam_ty, TArrow(wrapped_gen, f_gen)) :: lam_cons

  | Var(var) -> 
      begin match TypeEnv.find_opt var tenv with
      | None -> raise (UndefinedVariable(var))
      | Some(scheme) -> (instantiate(scheme), [])
      end

  | Lam(var, body) -> 
      let var_gen = TVar (gen_tvar ()) in
      let tenv_new = 
        TypeEnv.add var var_gen tenv
      in
      let (body_type, constraints) = 
        inference_constraints tenv_new body
      in
      (TArrow(var_gen, body_type), constraints)

  | If(cond, then_clause, else_clause) ->
      let (cond_ty, cond_cons) = inference_constraints tenv cond in
      let (then_ty, then_cons) = inference_constraints tenv then_clause in
      let (else_ty, else_cons) = inference_constraints tenv else_clause in
      (then_ty, [(cond_ty, t_bool); (then_ty, else_ty)] @ cond_cons @ then_cons @ else_cons)

  | App(f, x) ->
      let (f_ty, f_cons) = inference_constraints tenv f in
      let (x_ty, x_cons) = inference_constraints tenv x in
      let var_gen = TVar (gen_tvar ()) in
      (var_gen, [(f_ty, TArrow(x_ty, var_gen))] @ f_cons @ x_cons)

let show_cons ((tlhs, trhs): type_constraint): string = 
  pretty_print_type tlhs ^ " = " ^ pretty_print_type trhs

let show_sub (smap: subst): string =
  MapSubs.fold
  (fun k v acc ->
    string_of_int k ^ " -> " ^ pretty_print_type v ^ "\n" ^ acc
  )
  smap
  ""

let inference_type (tenv: type_env) (exp: expr): wyah_type = 
  let (exp_ty, cons) = inference_constraints tenv exp in
  let sub = unify cons in 
  let type_sub = apply_sub_ty sub exp_ty in
  let rest_fvs = (free_variables type_sub) in
  if compare rest_fvs TypeVarSet.empty == 0
  then type_sub
  else TForall (rest_fvs, type_sub)
