open Core_frontend.Ast
module StringMap = Map.Make (String)

type inst =
  | Unwind
  | PushGlobal of name
  | PushInt of int
  | Push of int
  | MkAp
  | Update of int
  | Pop of int

type code = inst BatDeque.t
type addr = Addr of int

type node =
  | NNum of int
  | NAp of addr * addr
  | NGlobal of int * code
  | NInd of addr

type heap = node BatDynArray.t
type stack = addr list

(* heap is ommited, we directly use ref to allocate and let OCaml clean up the memory *)

type env = addr StringMap.t
type stats = { mutable step : int }

type gm_state = {
  code : code;
  stack : stack;
  heap : heap;
  globals : env;
  stats : stats;
}

exception StackMissingUnwind
exception StackMissingApParam
exception StackMissingPushTarget
exception ExpectApForPushGot of node
exception NoCode
exception NoStackTop
exception NothingToPop

let ( ~& ) addr = Addr addr
let ( ~* ) (Addr addr) = addr
let is_final state = BatDeque.is_empty state.code

let push_global f state =
  let a = StringMap.find f state.globals in
  { state with stack = a :: state.stack }

let heap_alloc ?(addr = None) (heap : heap) node =
  match addr with
  | None ->
      BatDynArray.add heap node;
      ~&(-1 + BatDynArray.length heap)
  | Some addr ->
      BatDynArray.set heap ~*addr node;
      addr

let heap_find (heap : heap) addr =
  let (Addr addr) = addr in
  BatDynArray.get heap addr

let push_int n state =
  let num_addr = heap_alloc state.heap (NNum n) in
  { state with stack = num_addr :: state.stack }

let mkap state =
  match state.stack with
  | a1 :: a2 :: s ->
      let ap_addr = heap_alloc state.heap (NAp (a1, a2)) in
      { state with stack = ap_addr :: s }
  | _ -> raise StackMissingApParam

let push n state =
  let spine_node = List.nth_opt state.stack (n + 1) in
  match spine_node with
  | None -> raise StackMissingPushTarget
  | Some addr -> (
      match heap_find state.heap addr with
      | NAp (_an, an') -> { state with stack = an' :: state.stack }
      | node -> raise (ExpectApForPushGot node))

(* Unwind is already poped off the stack *)
let new_state state top_node =
  match top_node with
  | NNum _ -> { state with code = BatDeque.empty }
  | NInd target ->
      let stack = target :: (state.stack |> List.tl) in
      { state with stack; code = BatDeque.cons Unwind state.code }
  | NAp (a1, _) ->
      {
        state with
        stack = a1 :: state.stack;
        code = BatDeque.cons Unwind state.code;
      }
  | NGlobal (_argc, code) ->
      (* assume program being correctly typed, argc doesn't matter *)
      { state with code }

let update n state =
  match state.stack with
  | top :: stack_rest ->
      let to_redir = List.nth stack_rest n in
      heap_alloc state.heap (NInd top) ~addr:(Some to_redir) |> ignore;
      { state with stack = stack_rest }
  | [] -> raise NoStackTop

let pop n state =
  let rec drop n lst =
    if n = 0 then lst
    else
      match lst with _ :: rest -> drop (n - 1) rest | [] -> raise NothingToPop
  in
  { state with stack = drop n state.stack }

let unwind state =
  match state.stack with
  | top :: _ -> new_state state (heap_find state.heap top)
  | [] -> raise StackMissingUnwind

let dispatch = function
  | PushGlobal f -> push_global f
  | PushInt n -> push_int n
  | MkAp -> mkap
  | Push n -> push n
  | Update n -> update n
  | Pop n -> pop n
  | Unwind -> unwind

let step state =
  match BatDeque.front state.code with
  | None -> raise NoCode
  | Some (inst, code_rest) ->
      state.stats.step <- state.stats.step + 1;
      let state = { state with code = code_rest } in
      dispatch inst state

let rec eval state = if is_final state then state else eval (step state)
