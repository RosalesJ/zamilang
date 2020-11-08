open Core

module T = struct
  type t = {
      hash : int;
      name : string
    } [@@deriving sexp]

  let compare x y = Int.compare x.hash y.hash
end

include T
include Comparator.Make (T)

let name { name; _ } = name

let make name = { hash = (String.hash name); name }
