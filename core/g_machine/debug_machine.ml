open Machine

let string_of_inst = function
  | Unwind -> "Unwind"
  | PushGlobal name -> "PushGlobal " ^ name
  | PushInt i -> Printf.sprintf "PushInt %d" i
  | Push i -> Printf.sprintf "PushInt %d" i
  | MkAp -> "MkAp"
  | Update i -> Printf.sprintf "Update %d" i
  | Pop i -> Printf.sprintf "Pop %d" i

let string_of_code code =
  code |> BatDeque.to_list |> List.map string_of_inst |> String.concat ";"

let string_of_node = function
  | NNum n -> string_of_int n
  | NAp (f, x) -> Printf.sprintf "(Ap %d %d)" ~*f ~*x
  | NGlobal (_, code) -> Printf.sprintf "(Global [[%s]])" (string_of_code code)
  | NInd addr -> Printf.sprintf "(Ind %d)" ~*addr

let string_of_stack stack =
  stack |> List.map (fun addr -> string_of_int ~*addr) |> String.concat ";"

let string_of_heap heap =
  heap |> BatDynArray.to_list
  |> List.mapi (fun idx v -> Printf.sprintf "%d->%s" idx (string_of_node v))
  |> String.concat "\n"

let string_of_env env =
  env |> StringMap.to_seq
  |> Seq.map (fun (k, v) -> Printf.sprintf "%s->%d" k ~*v)
  |> List.of_seq |> String.concat ";"

let string_of_state (state : gm_state) =
  Printf.sprintf
    {|+++++++++++++++++
Code: %s
Stack: %s
=================
Heap: 
%s
=================
Globals: %s
=================
Stats: %d
|}
    (string_of_code state.code)
    (string_of_stack state.stack)
    (string_of_heap state.heap)
    (string_of_env state.globals)
    state.stats.step
