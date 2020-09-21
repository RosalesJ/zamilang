open Angstrom
open Core
open Lexer

module Op = Operator
module Kw = Keyword

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


(* TODO: Implement comments in some way*)
open struct
    let ( let* ) = Angstrom.(>>=)
    let return = Angstrom.return
  end

let identifier =
  let* id = spaces *> take_while1 is_alphanum in
  match List.find Keyword.reserved_alist ~f:(fun (_, b) -> String.(b = id)) with
  | None         -> return id
  | Some (_, kw) -> fail (Printf.sprintf "Reserved keyword '%s'" kw)

let type_id = identifier

let (<|>) a b = fun x -> a x <|> b x

let field_dec =
  let* fname = identifier <* Op.(token Colon) in
  let* ftyp = type_id in
  let escape = ref false in
  return ({ fname; escape; ftyp })

let type_decorator = option None (Op.(token Colon) *> type_id >>| fun x -> Some x)

let var_dec exp =
  let* name = Kw.(token Var) *> identifier in
  let* typ = type_decorator in
  let* init = Op.(token Def) *> exp in
  let escape = ref false in
  return (DecVar { name; typ; init; escape })


let fun_dec exp =
  let* funname = Kw.(token Function) *> identifier in
  let* params = Op.(token L_Paren *> sep_by (token Comma) field_dec <* token R_Paren) in
  let* result = type_decorator in
  let* body = Op.(token Eq) *> exp in
  return (DecFunction { funname; params; result; body })

let ty =
  let open Angstrom in
  let arr_ty = Kw.(token Array *> token Of) *> type_id >>| fun x -> TypArray x in
  let rec_ty = Op.(token L_Brace *> sep_by (token Comma) field_dec <* token R_Brace) >>| fun x -> TypRecord x in
  let name_ty = type_id >>| fun x -> TypName x in
  arr_ty <|> rec_ty <|> name_ty

let ty_dec _ =
  let* tyname = Kw.(token Type) *> type_id in
  let* typ = Op.(token Eq) *> ty in
  return (DecType {tyname; typ})

let dec = ty_dec <|> fun_dec <|> var_dec

let exp_let exp =
  let* decs = Kw.(token Let) *> many1 (dec exp) <* Kw.(token In) in
  let* body = sep_by Op.(token Semicolon) exp <* Kw.(token End) in
  return (ExpLet { decs; body})

let exp_nil _ = Kw.(token Nil) *> return ExpNil
let exp_break _ = Kw.(token Break) *> return ExpBreak

let exp_seq exp =
  let* seq = Op.(token L_Paren) *> sep_by Op.(token Semicolon) exp <* Op.(token R_Paren) in
  return (ExpSeq seq)

let exp_call exp =
  let* func = identifier in
  let* args = Op.(token L_Paren) *> sep_by Op.(token Comma) exp <* Op.(token R_Paren) in
  return (ExpCall {func; args})

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
    recurse (LvalueSubscript (var, index))
  and field var =
    let* id = Op.(token Dot) *> identifier in
    recurse (LvalueField (var, id))
  in
  let* id = identifier in
  recurse (LvalueSimple id)

let exp_lvalue exp = lvalue exp >>| fun x -> ExpLvalue x

let exp_assign exp =
  let* var = lvalue exp in
  let* exp = Op.(token Def) *> exp in
  return (ExpAssign {var; exp})

let arr_create exp =
  let* typ = type_id in
  let* size = Op.(token L_Brack) *> exp <* Op.(token R_Brack) in
  let* init = Kw.(token Of) *> exp in
  return (ExpArray {typ; size; init})

let record_create exp =
  let field =
    let* field_id = identifier in
    let* field_val = Op.(token Eq) *> exp in
    return (field_id, field_val)
  in
  let* typ = type_id in
  let* fields = Op.(token L_Brace) *> sep_by Op.(token Comma) field <* Op.(token R_Brace) in
  return (ExpRecord {fields; typ})

let exp_for exp =
  let* var = Kw.(token For) *> identifier in
  let* lo = Op.(token Def) *> exp in
  let* hi = Kw.(token To) *> exp in
  let* body = Kw.(token Do) *> exp in
  let escape = ref false in
  return (ExpFor {var; hi; lo; body; escape})

let exp_while exp =
  let* test = Kw.(token While) *> exp in
  let* body = Kw.(token Do) *> exp in
  return (ExpWhile {test; body})

let exp_if exp =
  let* test = Kw.(token If) *> exp in
  let* body = Kw.(token Then) *> exp in
  let* else_body = option None ((Kw.(token Else) *> exp) >>| fun x -> Some x) in
  return (ExpIf {test; body; else_body})

let int_lit _ = spaces *> int_l >>| fun x -> ExpInt (Int.of_string x)
let str_lit _ = spaces *> string_l >>| fun x -> ExpString x

let exp_neg exp = Op.(token Minus) *> exp >>| fun e -> ExpNegate e

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
  let oper_peek : oper option t =
    let* x = Op.peek in
    let z = Option.bind ~f:oper_of_op x in
    return z
  in
  let* left = expression exp in
  let* op = option None oper_peek in
  match op with
  | None -> return left
  | Some oper ->
     let* _ = Op.parse in
     let* right = exp_oper exp in
     return (ExpOp {left; oper; right})

let top = fix exp_oper

let parse = parse_string top ~consume:Consume.Prefix
