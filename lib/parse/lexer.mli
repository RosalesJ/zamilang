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

  val parse : t Angstrom.t
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

  val parse : t Angstrom.t
  val token : t -> unit Angstrom.t
end

val int_literal : string Angstrom.t
val string_literal : string Angstrom.t
val identifier : string Angstrom.t
val type_id : string Angstrom.t
