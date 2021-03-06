open Core
open Common
open Angstrom

open struct
  let ( let* ) = Angstrom.(>>=)
  let return = Angstrom.return
end

let decide_reserved assoc =
  assoc
  |> List.map ~f:(fun (a, b) -> string b *> return a)
  |> choice

let is_whitespace = function ' ' | '\t' | '\n' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_alphanum = is_digit <||> is_alpha

let spaces = skip_while is_whitespace

(* TODO: Implement excape characters and more sophisticated strings *)
(* TODO: Fix these identifier parsers to include underscores and what not *)

let string_literal = spaces *> char '"' *> take_while (function '"' -> false | _ -> true) <* char '"'

let int_literal = spaces *> take_while1 is_digit

module Keyword = struct
  type t =
    | While
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

  let reserved_alist =
    [(While, "while"); (For, "for"); (To, "to"); (Break, "break"); (Let, "let");
     (In, "in"); (End, "end"); (Function, "function"); (Var, "var"); (Type, "type");
     (Array, "array"); (If, "if"); (Then, "then"); (Else, "else"); (Do, "do");
     (Of, "of"); (Nil, "nil")]

  let find_str = List.Assoc.find_exn reserved_alist ~equal:phys_equal

  let parse =
    let* reserved = spaces *> decide_reserved reserved_alist in
    let* next_char = peek_char in
    match next_char with
    | Some c when is_alphanum c -> fail "Not a keyword"
    | _ -> return reserved

  let token expected =
    let* found_keyword = parse in
    if not (phys_equal found_keyword expected) then
      fail (Printf.sprintf "Unexpected keyword: expected '%s' but found '%s'"
              (find_str expected)
              (find_str found_keyword))
    else
      return ()
end

module Operator = struct
  type t =
    | Comma
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

  let reserved_alist =
    [(Comma, ","); (Def, ":="); (Colon, ":"); (Semicolon, ";"); (R_Paren, ")"); (L_Paren, "(");
     (R_Brack, "]"); (L_Brack, "["); (R_Brace, "}"); (L_Brace, "{"); (Dot, ".");
     (Plus, "+"); (Minus, "-"); (Times, "*"); (Slash, "/"); (Eq, "="); (Not, "<>");
     (Less_Eq, "<="); (Less, "<"); (Greater_Eq, ">="); (Greater, ">"); (And, "&");
     (Or, "|")]

  let parse = spaces *> decide_reserved reserved_alist

  let find_str = List.Assoc.find_exn reserved_alist ~equal:phys_equal

  let token expected =
    let* found_op = parse in
    if not (phys_equal found_op expected) then
      fail (Printf.sprintf "Unexpected operator: expected '%s' but found '%s'"
              (find_str expected)
              (find_str found_op))
    else
      return ()
end

let identifier =
  let* id = spaces *> take_while1 is_alphanum in
  match List.find Keyword.reserved_alist ~f:(fun (_, b) -> String.(b = id)) with
  | None         -> return id
  | Some (_, kw) -> fail (Printf.sprintf "Reserved keyword '%s'" kw)

let type_id = identifier
