open Core

type 'a t = (Symbol.t, 'a, Symbol.comparator_witness) Map.t

let empty = Map.empty (module Symbol)

let enter table symbol x = Map.set table ~key:symbol ~data:x

let look table symbol = Map.find table symbol
