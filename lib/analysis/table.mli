type 'a t

val empty : 'a t

val enter : 'a t -> Symbol.t -> 'a -> 'a t

val look : 'a t -> Symbol.t -> 'a option
