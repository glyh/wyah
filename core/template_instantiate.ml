open Batteries
open Core_frontend.Ast
open Core_frontend.Parser
open Prelude

exception Unimplemented
exception MainUndefined
exception EmptyStack
exception NumAppliedAsFunc
exception NonApOnStack
exception ArgsNotEnough
exception UndefinedVariable of string

type addr = int
type prim = Bor | Band | Eq | Ne | Ge | Gt | Le | Lt | Add | Sub | Mul | Div

type node =
  | NAp of addr * addr
  | NSupercomb of name * name list * core_expr
  | NNum of int
  | NInd of addr
  | NPrim of prim

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

let string_of_prim = function
  | Bor -> "bor"
  | Band -> "band"
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

let string_of_node (node : node) =
  match node with
  | NAp (f, x) -> Printf.sprintf "NAp(%d, %d)" f x
  | NSupercomb (name, _, _) -> name
  | NNum i -> string_of_int i
  | NInd addr -> Printf.sprintf "&%d" addr
  | NPrim p -> "$" ^ string_of_prim p

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
  [ ("+", Add); ("-", Sub); ("*", Mul); ("/", Div) ]
  |> List.fold_left
       (fun env (sym, prim) ->
         let idx = heap_alloc heap (NPrim prim) in
         StringMap.add sym idx env)
       env

let compile program =
  let parsed = parse_string program in
  let sc_defs = prelude @ extra_prelude @ parsed in
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
  | NNum i -> Some i
  | NInd addr -> take_data heap addr
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
  | Constr _ -> raise Unimplemented
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

let step_binary (state : ti_state) (stack_hd : addr) (stack_rest : addr list)
    (bop : int -> int -> int) : ti_state =
  let stack_left = ref stack_rest in
  let take_arg () =
    match !stack_left with
    | [] -> raise ArgsNotEnough
    | spine_addr :: rest ->
        let rec dispatch addr =
          let spine_node = BatDynArray.get state.heap addr in
          match spine_node with
          | NAp (_, arg) ->
              stack_left := rest;
              arg
          | NInd addr -> dispatch addr
          | _ -> raise NonApOnStack
        in
        (spine_addr, dispatch spine_addr)
  in
  let _, lhs_node = take_arg () in
  match take_data state.heap lhs_node with
  | Some lhs_data -> (
      let binop_root, rhs_node = take_arg () in
      match take_data state.heap rhs_node with
      | Some rhs_data ->
          let result_addr =
            heap_alloc state.heap
              (NNum (bop lhs_data rhs_data))
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
            let env, stack_left, addr_app_root =
              get_args stack_hd arg_names state.heap stack_rest state.globals
            in
            let preset_alloc_dest =
              if options.redirect then Some addr_app_root else None
            in
            let result_addr =
              instantiate body state.heap env ~addr:preset_alloc_dest
            in
            { state with stack = result_addr :: stack_left }
        | NAp (f, _) -> { state with stack = f :: stack_hd :: stack_rest }
        | NNum _ -> (
            match state.dump with
            | [] -> raise NumAppliedAsFunc
            | stack :: dump_rest -> { state with stack; dump = dump_rest })
        | NInd addr -> dispatch addr
        | NPrim Add -> step_binary state stack_hd stack_rest ( + )
        | NPrim Sub -> step_binary state stack_hd stack_rest ( - )
        | NPrim Mul -> step_binary state stack_hd stack_rest ( * )
        | NPrim Div -> step_binary state stack_hd stack_rest ( / )
        | NPrim _ -> raise Unimplemented
      in
      dispatch stack_hd

let rec eval_aux (options : step_options) state =
  if is_final_state state then state
  else state |> step_aux options |> eval_aux options

let eval state = eval_aux (create_step_options ()) state
