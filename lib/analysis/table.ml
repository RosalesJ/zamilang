open Core

type 'a t = (Symbol.t, 'a, Symbol.comparator_witness) Map.t

let empty = Map.empty (module Symbol)

let enter table symbol x =
  match Map.add table ~key:symbol ~data:x with
  | `Ok table -> table
  | `Duplicate -> assert false

let look table symbol = Map.find table symbol
