open Core_frontend.Ast
open Core_frontend.Parser
open Machine
open Prelude

type compiled_sc = name * addr
type var_offset = int StringMap.t

exception Unimplemented

let ( ~& ) = BatDeque.of_list
let ( >< ) = BatDeque.append
let code_init = ~&[ PushGlobal "main"; Unwind ]

let rec compile_r expr (env : var_offset) argc =
  compile_c expr env >< ~&[ Update argc; Pop argc; Unwind ]

and compile_c expr env =
  match expr with
  | Var v -> (
      match StringMap.find_opt v env with
      | Some idx -> ~&[ Push idx ]
      | None -> ~&[ PushGlobal v ])
  | Num i -> ~&[ PushInt i ]
  | Ap (e1, e2) ->
      compile_c e2 env
      >< compile_c e1 (StringMap.map (fun idx -> idx + 1) env)
      >< ~&[ MkAp ]
  | _ -> raise Unimplemented

let compile_sc (heap : heap) (sc : name definition) : compiled_sc =
  let argc = List.length sc.args in
  let compile_env =
    sc.args
    |> List.mapi (fun idx arg -> (arg, idx))
    |> List.to_seq |> StringMap.of_seq
  in
  let compiled = compile_r sc.body compile_env argc in
  let global_addr = heap_alloc heap (NGlobal (argc, compiled)) in
  (sc.name, global_addr)

let init_globals (heap : heap) sc_defs =
  sc_defs |> List.map (compile_sc heap) |> List.to_seq |> StringMap.of_seq

let compile program =
  let parsed = parse_string program in
  let sc_defs = prelude @ parsed in
  let heap = BatDynArray.create () in
  let globals = init_globals heap sc_defs in
  { code = code_init; stack = []; heap; globals; stats = { step = 0 } }
