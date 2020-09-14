open Core
open Angstrom
open Common

(* TODO: Implement comments in some way*)
open struct
  let ( let* ) = Angstrom.(>>=)
  let return = Angstrom.return
end

let decide_reserved assoc =
  assoc
  |> List.map ~f:(fun (a, b) -> string b *> return a)
  |> choice

let peek_reserved assoc =
  let f accum (typ, keyword) =
    let* acc = accum in
    match acc with
    | Some _ as x -> return x
    | None ->
       let* result = peek_string (String.length keyword) in
       if String.(result = keyword) then
         return (Some typ)
       else return None
  in
  List.fold ~f ~init:(return None) assoc

let is_whitespace = function ' ' | '\t' | '\n' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_alphanum = is_digit <||> is_alpha

(* TODO: Implement excape characters and more sophisticated strings *)
let string_l = char '"' *> take_while (function '"' -> false | _ -> true) <* char '"'

let int_l = take_while1 is_digit

(* TODO: Fix these identifier parsers to include underscores and what not *)

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

  let find_str = List.Assoc.find_exn reserved_alist ~equal:phys_equal

  let parse =
    let* reserved = spaces *> decide_reserved reserved_alist in
    let* next_char = peek_char in
    match next_char with
    | Some c when is_alphanum c -> fail "Not a keyword"
    | _ -> return reserved

  let _peek = spaces *> peek_reserved reserved_alist

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

  let peek = spaces *> peek_reserved reserved_alist

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

(* type literal =
 *   | Int of int
 *   | String of string
 * 
 * type token =
 *   | Keyword of Keyword.t
 *   | Operator of Operator.t
 *   | Literal of literal
 *   | Identifier of string
 *   | Comment of string
 *   | EOF *)

module AST = struct
  module Op = Operator
  module Kw = Keyword
  
  module T = struct
    type symbol = string

    type lvalue = LvalueSimple of symbol
                | LvalueField of lvalue * symbol
                | LvalueSubscript of lvalue * exp

    and exp = ExpLvalue    of lvalue
            | ExpNil
            | ExpNegate of exp
            | ExpInt    of int
            | ExpString of string
            | ExpCall   of { func: symbol; args: exp list }
            | ExpOp     of { left: exp; oper: oper; right: exp }
            | ExpRecord of { fields: (symbol * exp) list; typ: symbol }
            | ExpSeq    of exp list
            | ExpAssign of { var: lvalue; exp: exp }
            | ExpIf     of { test: exp; body: exp; else_body: exp option }
            | ExpWhile  of { test: exp; body: exp }
            | ExpFor    of { var: symbol; escape: bool ref; lo: exp; hi: exp; body: exp }
            | ExpBreak
            | ExpLet    of { decs: dec list; body: exp list }
            | ExpArray  of { typ: symbol; size: exp; init: exp }

    and dec = DecFunction of {funname: symbol; params: field list; result: symbol option; body: exp}
            | DecVar of { name: symbol; escape: bool ref; typ: symbol option; init: exp }
            | DecType of { tyname: symbol; typ: typ }

    and typ = TypName of symbol
            | TypRecord of field list
            | TypArray of symbol

    and oper = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp | AndOp | OrOp

    and field = {fname: symbol; escape: bool ref; ftyp:symbol }

    let oper_of_op =
      let open Operator in
      function
      | Plus       -> Some PlusOp
      | Minus      -> Some MinusOp
      | Times      -> Some TimesOp
      | Slash      -> Some DivideOp
      | Eq         -> Some EqOp
      | Not        -> Some NeqOp
      | Less       -> Some LtOp
      | Less_Eq    -> Some LeOp
      | Greater    -> Some GtOp
      | Greater_Eq -> Some GeOp
      | And        -> Some AndOp
      | Or         -> Some OrOp
      | _          -> None
  end

  let (<|>) a b = fun x -> a x <|> b x

  let field_dec =
    let* fname = identifier <* Op.(token Colon) in
    let* ftyp = type_id in
    let escape = ref false in
    return T.({ fname; escape; ftyp })
  
  let type_decorator = option None (Op.(token Colon) *> type_id >>| fun x -> Some x)

  let var_dec exp =
    let* name = Kw.(token Var) *> identifier in
    let* typ = type_decorator in
    let* init = Op.(token Def) *> exp in
    let escape = ref false in
    return T.(DecVar { name; typ; init; escape })
    

  let fun_dec exp =
    let* funname = Kw.(token Function) *> identifier in
    let* params = Op.(token L_Paren *> sep_by (token Comma) field_dec <* token R_Paren) in
    let* result = type_decorator in
    let* body = Op.(token Eq) *> exp in
    return T.(DecFunction { funname; params; result; body })

  let ty =
    let open Angstrom in
    let arr_ty = Kw.(token Array *> token Of) *> type_id >>| fun x -> T.TypArray x in
    let rec_ty = Op.(token L_Brace *> sep_by (token Comma) field_dec <* token R_Brace) >>| fun x -> T.TypRecord x in
    let name_ty = type_id >>| fun x -> T.TypName x in
    arr_ty <|> rec_ty <|> name_ty

  let ty_dec _ =
    let* tyname = Kw.(token Type) *> type_id in
    let* typ = Op.(token Eq) *> ty in
    return T.(DecType {tyname; typ})

  let dec = ty_dec <|> fun_dec <|> var_dec

  let exp_let exp =
    let* decs = Kw.(token Let) *> many1 (dec exp) <* Kw.(token In) in
    let* body = sep_by Op.(token Semicolon) exp <* Kw.(token End) in
    return T.(ExpLet { decs; body})

  let exp_nil _ = Kw.(token Nil) *> return T.ExpNil
  let exp_break _ = Kw.(token Break) *> return T.ExpBreak

  let exp_seq exp =
    let* seq = Op.(token L_Paren) *> sep_by Op.(token Semicolon) exp <* Op.(token R_Paren) in
    return T.(ExpSeq seq)

  let exp_call exp =
    let* func = identifier in
    let* args = Op.(token L_Paren) *> sep_by Op.(token Comma) exp <* Op.(token R_Paren) in
    return T.(ExpCall {func; args})

  (* TODO: Make the use fix instead of this hackiness *)
  let lvalue exp =
    let rec recurse var = 
      let* c = peek_char_fail in
      match c with
      | '[' -> subscript var
      | '.' -> field var
      | _ -> return var
    and subscript var =
      let* index = Op.(token L_Brack) *> exp <* Op.(token R_Brack) in
      recurse T.(LvalueSubscript (var, index))
    and field var =
      let* id = Op.(token Dot) *> identifier in
      recurse T.(LvalueField (var, id))
    in
    let* id = identifier in
    recurse T.(LvalueSimple id)

  let exp_lvalue exp = lvalue exp >>| fun x -> T.ExpLvalue x

  let exp_assign exp =
    let* var = lvalue exp in
    let* exp = Op.(token Def) *> exp in
    return T.(ExpAssign {var; exp})

  let arr_create exp =
    let* typ = type_id in
    let* size = Op.(token L_Brack) *> exp <* Op.(token R_Brack) in
    let* init = Kw.(token Of) *> exp in
    return T.(ExpArray {typ; size; init})

  let record_create exp =
    let field =
      let* field_id = identifier in
      let* field_val = Op.(token Eq) *> exp in
      return (field_id, field_val)
    in
    let* typ = type_id in
    let* fields = Op.(token L_Brace) *> sep_by Op.(token Comma) field <* Op.(token R_Brace) in
    return T.(ExpRecord {fields; typ})

  let exp_for exp =
    let* var = Kw.(token For) *> identifier in
    let* lo = Op.(token Def) *> exp in
    let* hi = Kw.(token To) *> exp in
    let* body = Kw.(token Do) *> exp in
    let escape = ref false in
    return T.(ExpFor {var; hi; lo; body; escape})

  let exp_while exp =
    let* test = Kw.(token While) *> exp in
    let* body = Kw.(token Do) *> exp in
    return T.(ExpWhile {test; body})
  
  let exp_if exp =
    let* test = Kw.(token If) *> exp in
    let* body = Kw.(token Then) *> exp in
    let* else_body = option None ((Kw.(token Else) *> exp) >>| fun x -> Some x) in
    return T.(ExpIf {test; body; else_body})

  let int_lit _ = spaces *> int_l >>| fun x -> T.(ExpInt (Int.of_string x))
  let str_lit _ = spaces *> string_l >>| fun x -> T.(ExpString x)

  let exp_neg exp = Op.(token Minus) *> exp >>| fun e -> T.(ExpNegate e)

  let expression = exp_neg
                   <|> exp_nil
                   <|> int_lit
                   <|> str_lit
                   <|> exp_break
                   <|> exp_if
                   <|> exp_for
                   <|> exp_while
                   <|> exp_assign
                   <|> exp_call
                   <|> exp_seq
                   <|> exp_let
                   <|> arr_create
                   <|> record_create
                   <|> exp_lvalue

  let rec exp_oper exp =
    let oper_peek : T.oper option t =
      let* x = Op.peek in
      let z = Option.bind ~f:(T.oper_of_op) x in
      return z
    in
    let* left = expression exp in
    let* op = option None oper_peek in
    match op with
    | None -> return left
    | Some oper ->
       let* _ = Op.parse in
       let* right = exp_oper exp in
       return T.(ExpOp {left; oper; right})

  let top = fix exp_oper
end

let parse = parse_string AST.top ~consume:Consume.Prefix
