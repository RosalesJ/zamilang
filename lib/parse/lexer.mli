val decide_reserved : ('a * string) list -> 'a Angstrom.t
val spaces : unit Angstrom.t
val is_alphanum : char -> bool
val int_l : string Angstrom.t
val string_l : string Angstrom.t

module Keyword : sig
  type t =
      While
    | For
    | To
    | Break
    | Let
    | In
    | End
    | Function
    | Var
    | Type
    | Array
    | If
    | Then
    | Else
    | Do
    | Of
    | Nil
  val reserved_alist : (t * string) list
  val find_str : t -> string
  val parse : t Angstrom.t
  val _peek : t option Angstrom.t
  val token : t -> unit Angstrom.t
end

module Operator : sig
  type t =
      Comma
    | Colon
    | Semicolon
    | R_Paren
    | L_Paren
    | R_Brack
    | L_Brack
    | R_Brace
    | L_Brace
    | Dot
    | Plus
    | Minus
    | Times
    | Slash
    | Eq
    | Not
    | Less
    | Less_Eq
    | Greater
    | Greater_Eq
    | And
    | Or
    | Def
  val reserved_alist : (t * string) list
  val parse : t Angstrom.t
  val peek : t option Angstrom.t
  val find_str : t -> string
  val token : t -> unit Angstrom.t
end
