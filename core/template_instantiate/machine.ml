open Batteries
open Core_frontend.Ast
open Core_frontend.Parser
open Prelude

type addr = int

type prim =
  | Eq
  | Ne
  | Ge
  | Gt
  | Le
  | Lt
  | Add
  | Sub
  | Mul
  | Div
  | Constr of { tag : int; arity : int }
  | If
  | CasePair
  | CaseList
  | Abort

type node =
  | NAp of addr * addr
  | NSupercomb of name * name list * core_expr
  | NNum of int
  | NInd of addr
  | NPrim of prim
  | NData of int * addr list

type value = VNum of int | VData of int * value list | VAbort
type ti_stats = { mutable steps : int }
type heap = node BatDynArray.t

module StringMap = BatMap.Make (String)

type env = addr StringMap.t

type ti_state = {
  stack : addr list;
  dump : addr list list;
  heap : heap;
  globals : env;
  stats : ti_stats;
}

exception Unimplemented
exception MainUndefined
exception EmptyStack
exception ValueAppliedAsFunc of node
exception NonApOnStack
exception ArgsNotEnough
exception UndefinedVariable of string
exception Unreachable of string
exception Aborted

let string_of_prim = function
  | Eq -> "eq"
  | Ne -> "ne"
  | Ge -> "ge"
  | Gt -> "gt"
  | Le -> "le"
  | Lt -> "lt"
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Constr { tag; arity } -> Printf.sprintf "con(%d, %d)" tag arity
  | If -> "if"
  | CasePair -> "casePair"
  | CaseList -> "caseList"
  | Abort -> "abort"

let string_of_node (node : node) =
  match node with
  | NAp (f, x) -> Printf.sprintf "NAp(%d, %d)" f x
  | NSupercomb (name, _, _) -> name
  | NNum i -> string_of_int i
  | NInd addr -> Printf.sprintf "&%d" addr
  | NPrim p -> "$" ^ string_of_prim p
  | NData (tag, children) ->
      Printf.sprintf "Pack{%d,%d}(%s)" tag (List.length children)
        (children |> List.map string_of_int |> String.concat ", ")

let string_of_state (s : ti_state) =
  let stack_to_str s = s |> List.map string_of_int |> String.concat " " in
  let stack_str = stack_to_str s.stack in
  let heap_str =
    s.heap |> BatDynArray.to_list
    |> List.mapi (fun idx node ->
           Printf.sprintf "%d:%s" idx (string_of_node node))
    |> String.concat " "
  in
  let globals_str =
    StringMap.to_seq s.globals
    |> Seq.map (fun (name, addr) -> Printf.sprintf "%s:%d" name addr)
    |> Seq.to_string ~sep:" " identity
  in
  let dump_str = s.dump |> List.map stack_to_str |> String.concat "\n" in

  Printf.sprintf
    {|=================
stack: %s
heap: %s
globals: %s
=================
DUMP START 
%s
DUMP END 
|}
    stack_str heap_str globals_str dump_str

let heap_alloc ?(addr = None) (heap : heap) node =
  match addr with
  | None ->
      BatDynArray.add heap node;
      -1 + BatDynArray.length heap
  | Some addr ->
      BatDynArray.set heap addr node;
      addr

let rec heap_alloc_val ?(addr = None) (heap : heap) value =
  match value with
  | VNum num -> heap_alloc heap (NNum num) ~addr
  | VData (tag, args) ->
      let args_alloc = List.map (heap_alloc_val heap) args in
      heap_alloc heap (NData (tag, args_alloc)) ~addr
  | VAbort -> heap_alloc heap (NPrim Abort) ~addr

let init_heap_scs (sc_defs : name definition list) : heap * env =
  let heap = BatDynArray.create () in
  let env =
    List.fold_left
      (fun env sc_def ->
        let { name; args; body } = sc_def in
        let idx = heap_alloc heap (NSupercomb (name, args, body)) in
        StringMap.add name idx env)
      StringMap.empty sc_defs
  in
  (heap, env)

let init_heap_prims (heap : heap) (env : env) : env =
  [
    ("+", Add);
    ("-", Sub);
    ("*", Mul);
    ("/", Div);
    ("if", If);
    ("==", Eq);
    ("!=", Ne);
    (">=", Ge);
    (">", Gt);
    ("<=", Le);
    ("<", Lt);
    ("casePair", CasePair);
    ("caseList", CaseList);
    ("abort", Abort);
  ]
  |> List.fold_left
       (fun env (sym, prim) ->
         let idx = heap_alloc heap (NPrim prim) in
         StringMap.add sym idx env)
       env

let compile program =
  let parsed = parse_string program in
  let sc_defs = prelude @ parsed in
  let heap, globals = init_heap_scs sc_defs in
  let globals = init_heap_prims heap globals in
  let address_of_main =
    match StringMap.find_opt "main" globals with
    | None -> raise MainUndefined
    | Some addr -> addr
  in
  let stack = [ address_of_main ] in
  { stack; dump = []; heap; globals; stats = { steps = 0 } }

let rec take_data (heap : heap) (node_addr : addr) =
  match BatDynArray.get heap node_addr with
  | NNum i -> Some (VNum i)
  | NData (tag, args) -> (
      let args_collected =
        args
        |> List.fold_left
             (fun acc ele ->
               match acc with
               | Some lst -> (
                   match take_data heap ele with
                   | Some arg -> Some (arg :: lst)
                   | None -> None)
               | None -> None)
             (Some [])
        |> Option.map List.rev
      in
      match args_collected with
      | None -> None
      | Some args -> Some (VData (tag, args)))
  | NInd addr -> take_data heap addr
  | _ -> None

let rec refer_data (heap : heap) (node_addr : addr) =
  match BatDynArray.get heap node_addr with
  | (NNum _ | NData _) as nd -> Some nd
  | NInd addr -> refer_data heap addr
  | _ -> None

let is_data (heap : heap) (node_addr : addr) =
  match take_data heap node_addr with Some _ -> true | None -> false

let is_final_state (state : ti_state) =
  List.is_empty state.dump
  &&
  match state.stack with
  | [] -> raise EmptyStack
  | [ addr ] -> is_data state.heap addr
  | _ -> false

let rec list_last_opt lst =
  match lst with [ e ] -> Some e | [] -> None | _ :: es -> list_last_opt es

let get_args (fun_node : addr) (arg_names : name list) (heap : heap)
    (stack_rest : addr list) (globals : env) : env * addr list * addr =
  let stack_left = ref stack_rest in
  let collect_arg globals arg_name =
    match !stack_left with
    | [] -> raise ArgsNotEnough
    | addr :: rest ->
        let env_new =
          let rec dispatch addr =
            let node = BatDynArray.get heap addr in
            match node with
            | NAp (_, arg) ->
                stack_left := rest;
                StringMap.add arg_name arg globals
            | NInd addr -> dispatch addr
            | _ -> raise NonApOnStack
          in
          dispatch addr
        in
        (env_new, addr)
  in
  let globals_new, app_spine =
    List.fold_left_map collect_arg globals arg_names
  in
  let app_root =
    match list_last_opt app_spine with None -> fun_node | Some addr -> addr
  in

  (globals_new, !stack_left, app_root)

type node_uref = addr option BatUref.t

let preallocate_recur_nodes (heap : heap) (bindings : core_expr StringMap.t)
    (env : env) =
  let uref_map : node_uref StringMap.t =
    bindings
    |> StringMap.map (fun rhs ->
           let uref =
             match rhs with
             | Var name when not (StringMap.mem name bindings) ->
                 let addr_already = StringMap.find name env in
                 BatUref.uref (Some addr_already)
             | _ -> BatUref.uref None
           in
           uref)
  in
  bindings
  |> StringMap.iter (fun name rhs ->
         match rhs with
         | Var name_referred when StringMap.mem name_referred bindings ->
             let ref_ours = StringMap.find name uref_map in
             let ref_to_union = StringMap.find name_referred uref_map in
             BatUref.unite ref_ours ref_to_union
         | _ -> ());

  StringMap.fold
    (fun name ref env ->
      let addr =
        match BatUref.uget ref with
        | None ->
            let addr = heap_alloc heap (NNum (-1)) in
            BatUref.uset ref (Some addr);
            addr
        | Some addr -> addr
      in
      StringMap.add name addr env)
    uref_map env

let rec instantiate ?(addr = None) (body : core_expr) heap (env : env) =
  match body with
  | Num n -> heap_alloc heap (NNum n) ~addr
  | Ap (f, x) ->
      let f = instantiate f heap env in
      let x = instantiate x heap env in
      heap_alloc heap (NAp (f, x)) ~addr
  | Var name -> (
      match StringMap.find_opt name env with
      | None -> raise (UndefinedVariable name)
      | Some addr_already -> (
          match addr with
          | None -> addr_already
          | Some addr ->
              if addr != addr_already then
                heap_alloc heap (NInd addr_already) ~addr:(Some addr)
              else addr))
  | Constr (tag, arity) -> heap_alloc heap (NPrim (Constr { tag; arity })) ~addr
  | Let { recursive = false; bindings; body } ->
      let env_new =
        bindings
        |> List.fold
             (fun env (name, exp) ->
               StringMap.add name (instantiate exp heap env) env)
             env
      in
      instantiate body heap env_new ~addr
  | Let { recursive = true; bindings; body } ->
      let env_preallocated =
        preallocate_recur_nodes heap (StringMap.of_list bindings) env
      in
      bindings
      |> List.iter (fun (name, exp) ->
             let pre_allocated_addr =
               StringMap.find_opt name env_preallocated
             in
             instantiate exp heap env_preallocated ~addr:pre_allocated_addr
             |> ignore);
      instantiate body heap env_preallocated ~addr
  | Case _ -> raise Unimplemented
  | Lam _ -> raise Unimplemented

type step_options = { debug_channel : Out_channel.t; redirect : bool }

let create_step_options ?(debug_channel = None) ?(redirect = true) () =
  let debug_channel =
    match debug_channel with
    | None -> Out_channel.open_bin "/dev/null"
    | Some ch -> ch
  in
  { debug_channel; redirect }

let take_arg stack_left heap =
  match !stack_left with
  | [] -> raise ArgsNotEnough
  | spine_addr :: rest ->
      let rec dispatch addr =
        let spine_node = BatDynArray.get heap addr in
        match spine_node with
        | NAp (_, arg) ->
            stack_left := rest;
            arg
        | NInd addr -> dispatch addr
        | _ -> raise NonApOnStack
      in
      (spine_addr, dispatch spine_addr)

let step_binary (state : ti_state) (stack_hd : addr) (stack_rest : addr list)
    (bop : node -> node -> node) : ti_state =
  let stack_left = ref stack_rest in
  let _, lhs_node = take_arg stack_left state.heap in
  match refer_data state.heap lhs_node with
  | Some lhs_node_referred -> (
      let binop_root, rhs_node = take_arg stack_left state.heap in
      match refer_data state.heap rhs_node with
      | Some rhs_node_referred ->
          let result_addr =
            heap_alloc state.heap
              (bop lhs_node_referred rhs_node_referred)
              ~addr:(Some binop_root)
          in
          { state with stack = result_addr :: !stack_left }
      | None ->
          {
            state with
            stack = [ rhs_node ];
            dump = (stack_hd :: stack_rest) :: state.dump;
          })
  | None ->
      {
        state with
        stack = [ lhs_node ];
        dump = (stack_hd :: stack_rest) :: state.dump;
      }

let step_constr (state : ti_state) (stack_hd : addr) (stack_rest : addr list)
    (tag : int) (arity : int) =
  let stack_left = ref stack_rest in
  let rec collect_args acc to_collect =
    match to_collect with
    | 0 ->
        (* we must start with 0, i.e. constr with no args *)
        (stack_hd, [])
    | 1 ->
        let spine_node, arg_node = take_arg stack_left state.heap in
        (spine_node, List.rev (arg_node :: acc))
    | x ->
        let _, arg_node = take_arg stack_left state.heap in
        collect_args (arg_node :: acc) (x - 1)
  in
  let override_node, args = collect_args [] arity in
  let result_addr =
    heap_alloc state.heap (NData (tag, args)) ~addr:(Some override_node)
  in
  { state with stack = result_addr :: !stack_left }

let step_if (state : ti_state) (stack_hd : addr) (stack_rest : addr list) :
    ti_state =
  let stack_left = ref stack_rest in
  let _, cond_node = take_arg stack_left state.heap in
  match refer_data state.heap cond_node with
  | Some cond_node_reffered ->
      let _, then_clause = take_arg stack_left state.heap in
      let override_node, else_clause = take_arg stack_left state.heap in
      let body_node =
        match cond_node_reffered with
        | NData (2, _) ->
            heap_alloc state.heap (NInd then_clause) ~addr:(Some override_node)
        | _ ->
            heap_alloc state.heap (NInd else_clause) ~addr:(Some override_node)
      in
      { state with stack = body_node :: !stack_left }
  | None ->
      {
        state with
        stack = [ cond_node ];
        dump = (stack_hd :: stack_rest) :: state.dump;
      }

let step_sc options (state : ti_state) (stack_hd : addr)
    (stack_rest : addr list) arg_names body =
  let env, stack_left, addr_app_root =
    get_args stack_hd arg_names state.heap stack_rest state.globals
  in
  let preset_alloc_dest =
    if options.redirect then Some addr_app_root else None
  in
  let result_addr = instantiate body state.heap env ~addr:preset_alloc_dest in
  { state with stack = result_addr :: stack_left }

let wrap_arith (op : int -> int -> int) (v1 : node) (v2 : node) =
  match (v1, v2) with
  | NNum n1, NNum n2 -> NNum (op n1 n2)
  | _ -> raise (Unreachable "wrap_arith")

let wrap_order (state : ti_state) (op : int -> int -> bool) (v1 : node)
    (v2 : node) =
  match (v1, v2) with
  | NNum n1, NNum n2 ->
      let sym_to_refer = if op n1 n2 then "True" else "False" in
      StringMap.find sym_to_refer state.globals |> BatDynArray.get state.heap
  | _ -> raise (Unreachable "wrap_arith")

let step_case_pair (state : ti_state) (stack_hd : addr) (stack_rest : addr list)
    : ti_state =
  let stack_left = ref stack_rest in
  let _, pair_node = take_arg stack_left state.heap in
  match refer_data state.heap pair_node with
  | Some (NData (1, [ fst_of_pair; snd_of_pair ])) ->
      let override_node, applied_fn = take_arg stack_left state.heap in
      let ap_node_inner =
        heap_alloc state.heap (NAp (applied_fn, fst_of_pair))
      in
      let ap_node =
        heap_alloc state.heap
          (NAp (ap_node_inner, snd_of_pair))
          ~addr:(Some override_node)
      in

      { state with stack = ap_node :: !stack_left }
  | _ ->
      {
        state with
        stack = [ pair_node ];
        dump = (stack_hd :: stack_rest) :: state.dump;
      }

let step_case_list (state : ti_state) (stack_hd : addr) (stack_rest : addr list)
    : ti_state =
  let stack_left = ref stack_rest in
  let _, list_node = take_arg stack_left state.heap in
  match refer_data state.heap list_node with
  | Some (NData (1, [])) ->
      let _, term_empty = take_arg stack_left state.heap in
      let override_node, _ = take_arg stack_left state.heap in
      let output =
        heap_alloc state.heap (NInd term_empty) ~addr:(Some override_node)
      in
      { state with stack = output :: !stack_left }
  | Some (NData (2, [ x; xs ])) ->
      let _, _ = take_arg stack_left state.heap in
      let override_node, applied_fn = take_arg stack_left state.heap in
      let ap_node_inner = heap_alloc state.heap (NAp (applied_fn, x)) in
      let ap_node =
        heap_alloc state.heap
          (NAp (ap_node_inner, xs))
          ~addr:(Some override_node)
      in

      { state with stack = ap_node :: !stack_left }
  | _ ->
      {
        state with
        stack = [ list_node ];
        dump = (stack_hd :: stack_rest) :: state.dump;
      }

let step_aux (options : step_options) (state : ti_state) =
  Out_channel.output_string options.debug_channel (string_of_state state);
  Out_channel.flush options.debug_channel;
  state.stats.steps <- state.stats.steps + 1;
  match state.stack with
  | [] -> raise EmptyStack
  | stack_hd :: stack_rest ->
      let rec dispatch addr =
        match BatDynArray.get state.heap addr with
        | NSupercomb (_, arg_names, body) ->
            step_sc options state stack_hd stack_rest arg_names body
        | NAp (f, _) -> { state with stack = f :: stack_hd :: stack_rest }
        | (NNum _ | NData _) as node -> (
            match state.dump with
            | [] -> raise (ValueAppliedAsFunc node)
            | stack :: dump_rest -> { state with stack; dump = dump_rest })
        | NInd addr -> dispatch addr
        | NPrim Add ->
            step_binary state stack_hd stack_rest (( + ) |> wrap_arith)
        | NPrim Sub ->
            step_binary state stack_hd stack_rest (( - ) |> wrap_arith)
        | NPrim Mul ->
            step_binary state stack_hd stack_rest (( * ) |> wrap_arith)
        | NPrim Div ->
            step_binary state stack_hd stack_rest (( / ) |> wrap_arith)
        | NPrim Eq ->
            step_binary state stack_hd stack_rest (( = ) |> wrap_order state)
        | NPrim Ne ->
            step_binary state stack_hd stack_rest (( != ) |> wrap_order state)
        | NPrim Ge ->
            step_binary state stack_hd stack_rest (( >= ) |> wrap_order state)
        | NPrim Gt ->
            step_binary state stack_hd stack_rest (( > ) |> wrap_order state)
        | NPrim Le ->
            step_binary state stack_hd stack_rest (( <= ) |> wrap_order state)
        | NPrim Lt ->
            step_binary state stack_hd stack_rest (( < ) |> wrap_order state)
        | NPrim (Constr { tag; arity }) ->
            step_constr state stack_hd stack_rest tag arity
        | NPrim If -> step_if state stack_hd stack_rest
        | NPrim CasePair -> step_case_pair state stack_hd stack_rest
        | NPrim CaseList -> step_case_list state stack_hd stack_rest
        | NPrim Abort -> raise Aborted
      in
      dispatch stack_hd

let rec eval_aux (options : step_options) state =
  if is_final_state state then state
  else state |> step_aux options |> eval_aux options

let eval state = eval_aux (create_step_options ()) state
