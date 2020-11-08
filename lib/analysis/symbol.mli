open Core

type t

val make : string -> t

val name : t -> string

include Comparator.S with type t := t
