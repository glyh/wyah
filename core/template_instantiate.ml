open Batteries
open Syntax

type addr = int

type node =
  | NAp of addr * addr
  | NSupercomb of name * name list * core_expr
  | NNum of int

type ti_stats = { steps : int }

type ti_state = {
  stack : addr list;
  dump : unit; (* dump is not used for simple template instantiation machine *)
  heap : node BatDynArray.t;
  globals : (name, addr) BatHashtbl.t;
  stats : ti_stats;
}
