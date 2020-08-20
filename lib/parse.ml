open Core
open Angstrom
open Common

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

(* TODO: Implement excape characters and more sophisticated strings *)
let string_l = char '"' *> take_while (function '"' -> false | _ -> true) <* char '"'

let int_l = take_while1 is_digit

(* TODO: Fix these identifier parsers to include underscores and what not *)
let identifier = take_while1 is_alphanum
let type_id = identifier

let spaces = skip_while is_whitespace

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

  let find_str = List.Assoc.find_exn reserved_alist ~equal:(phys_equal)

  let parse =
    let* reserved = decide_reserved reserved_alist in
    let* next_char = peek_char in
    match next_char with
    | Some c when is_alphanum c -> fail "Not a keyword"
    | _ -> return reserved

let appear keyword =
  let* found_keyword = parse in
  if not (phys_equal found_keyword keyword) then
    fail (Printf.sprintf "Unexpected keyword: `%s`" (find_str found_keyword))
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

  let parse = decide_reserved reserved_alist

  let find_str = List.Assoc.find_exn reserved_alist ~equal:(phys_equal)

  let appear operator =
    let* found_op = parse in
    if not (phys_equal found_op operator) then
      fail (Printf.sprintf "Unexpected operator: `%s`" (find_str found_op))
    else
      return ()
end

type literal =
  | Int of int
  | String of string

type token =
  | Keyword of Keyword.t
  | Operator of Operator.t
  | Literal of literal
  | Identifier of string
  | Comment of string
  | EOF

module AST = struct
  module T = struct
    type symbol = string

    type var = VarSimple of symbol
             | VarField of var * symbol
             | VarSubscript of var * exp

    and exp = ExpVar    of var
            | ExpNil
            | ExpInt    of int
            | ExpString of string
            | ExpCall   of { func: symbol; args: exp list }
            | ExpOp     of { left: exp; oper: oper; right: exp }
            | ExpRecord of { fields: (symbol * exp) list; typ: symbol }
            | ExpSeq    of exp list
            | ExpAssign of { var: var; exp: exp }
            | ExpIf     of { test: exp; body: exp; else_body: exp option }
            | ExpWhile  of { test: exp; body: exp }
            | ExpFor    of { var: symbol; escape: bool ref; lo: exp; hi: exp; body: exp }
            | ExpBreak
            | ExpLet    of { decs: dec list; body: exp }
            | ExpArray  of { typ: symbol; size: exp; init: exp }

    and dec = DecFunction of fundec list
            | DecVar of { name: symbol; escape: bool ref; typ: symbol option; init: exp }
            | DecType of tydec list

    and typ = TypName of symbol
            | TypRecord of field list
            | TypArray of symbol

    and oper = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | Greater

    and tydec = {tyname: symbol; typ: typ}

    and field = {fname: symbol; escape: bool ref; ftyp:symbol }

    and fundec = {funname: symbol; params: field list; result: symbol option; body: exp}
  end

  let (<|>) a b = fun x -> a x <|> b x

  let exp_nil _ = Keyword.(appear Nil) *> return T.ExpNil
  let exp_break _ = Keyword.(appear Break) *> return T.ExpBreak

  let exp_seq exp =
    let* seq = Operator.(appear L_Paren) *> sep_by Operator.(appear Comma) exp <* Operator.(appear R_Paren) in
    return T.(ExpSeq seq)

  let exp_call exp =
    let* func = identifier <* spaces in
    let* args = Operator.(appear L_Paren) *> sep_by Operator.(appear Comma) exp <* Operator.(appear R_Paren) in
    return T.(ExpCall {func; args})

  let var_exp exp =
    let simple _ =
      let* name = identifier <* spaces in
      return T.(VarSimple name)
    in
    let subscript var =
      let* arr = var <* spaces in
      let* index = Operator.(appear L_Brack) *> spaces *> exp <* spaces <* Operator.(appear L_Brack) <* spaces in
      return T.(VarSubscript (arr, index))
    in
    let field var =
      let* record = var <* spaces in
      let* field_name = Operator.(appear Dot) *> spaces *> identifier <* spaces in
      return T.(VarField (record, field_name))
    in
    simple <|> subscript <|> field |> fix

  let exp_assign exp =
    let* var = var_exp exp <* spaces in
    let* exp = Operator.(appear Def) *> spaces *> exp <* spaces in
    return T.(ExpAssign {var; exp})

  let arr_create exp =
    let* typ = type_id <* spaces in
    let* size = Operator.(appear L_Brack) *> spaces *> exp <* spaces <* Operator.(appear L_Brack) in
    let* init = spaces *> Keyword.(appear Of) *> spaces *> exp <* spaces in
    return T.(ExpArray {typ; size; init})

  let record_create exp =
    let field =
      let* field_id = spaces *> identifier in
      let* field_val = spaces *> exp <* spaces in
      return (field_id, field_val)
    in
    let* typ = type_id <* spaces in
    let* fields = Operator.(appear L_Brace) *> spaces *> many field <* spaces <* Operator.(appear L_Brace) in
    return T.(ExpRecord {fields; typ})

  let exp_for exp =
    let* var = Keyword.(appear For) *> spaces *> identifier <* spaces in
    let* lo = Operator.(appear Def) *> spaces *> exp <* spaces in
    let* hi = Keyword.(appear To)   *> spaces *> exp <* spaces in
    let* body = Keyword.(appear Do) *> spaces *> exp <* spaces in
    let escape = ref false in
    return T.(ExpFor {var; hi; lo; body; escape})

  let exp_while exp =
    let* test = Keyword.(appear While) *> spaces *> exp <* spaces in
    let* body = Keyword.(appear Do) *> spaces *> exp <* spaces in
    return T.(ExpWhile {test; body})
  
  let exp_if exp =
    let* test = Keyword.(appear If)   *> spaces *> exp <* spaces in
    let* body = Keyword.(appear Then) *> spaces *> exp <* spaces in
    let* else_body = option None ((Keyword.(appear Else) *> spaces *> exp <* spaces) >>| fun x -> Some x) in
    return T.(ExpIf {test; body; else_body})

  let expression = exp_nil
                   <|> exp_break
                   <|> exp_if
                   <|> exp_for
                   <|> exp_while
                   <|> exp_assign
                   <|> arr_create
                   <|> record_create
                   |> fix
end

let parse str =
  match parse_string AST.expression ~consume:Consume.Prefix str with
  | Ok res -> res
  | Error e -> failwith e
