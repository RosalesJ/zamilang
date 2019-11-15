open Angstrom

type keyword =
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

type operator =
  | Comma
  | Colon
  | SemiColon
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

type literal =
  | Int of int
  | String of string

type token =
  | Keyword of keyword
  | Operator of operator
  | Literal of literal
  | Identifier of string
  | Comment of string
  | EOF

let comma = char ',' *> return Comma
let colon = char ':' *> return Colon
let semiColon = char ';' *> return R_Paren
let r_Paren = char ')' *> return R_Paren
let l_Paren = char '(' *> return L_Paren
let r_Brack = char ']' *> return R_Brack
let l_Brack = char '[' *> return L_Brack
let r_Brace = char '}' *> return R_Brace
let l_Brace = char '{' *> return L_Brace
let dot = char '.' *> return Dot
let plus = char '+' *> return Plus
let minus = char '-' *> return Minus
let times = char '*' *> return Times
let slash = char '/' *> return Slash
let eq = char '=' *> return Eq
let neg = string "<>" *> return Not
let less = char '<' *> return Less
let less_eq = string "<=" *> return Less_Eq
let greater = char '>' *> return Greater
let greater_eq = string ">=" *> return Greater_Eq
let amp = char '&' *> return And
let bar = char '|' *> return Or
let def = string ":=" *> return Def

let operator =  comma
            <|> def
            <|> colon
            <|> semiColon
            <|> r_Paren
            <|> l_Paren
            <|> r_Brack
            <|> l_Brack
            <|> r_Brace
            <|> l_Brace
            <|> dot
            <|> plus
            <|> minus
            <|> times
            <|> slash
            <|> eq
            <|> neg
            <|> less_eq
            <|> less
            <|> greater_eq
            <|> greater
            <|> amp
            <|> bar
  
let while_k = string "while" *> return While
let for_k = string "for" *> return For
let to_k = string "to" *> return To
let break_k = string "break" *> return Break
let let_k = string "let" *> return Let
let in_k = string "in" *> return In
let end_k = string "end" *> return End
let function_k = string "function" *> return Function
let var_k = string "var" *> return Var
let type_k = string "type" *> return Type
let array_k = string "array" *> return Array
let if_k = string "if" *> return If
let then_k = string "then" *> return Then
let else_k = string "else" *> return Else
let do_k = string "do" *> return Do
let of_k = string "of" *> return Of
let nil_k = string "nil" *> return Nil

let keyword = while_k
           <|> for_k
           <|> to_k
           <|> break_k
           <|> let_k
           <|> in_k
           <|> end_k
           <|> function_k
           <|> var_k
           <|> type_k
           <|> array_k
           <|> if_k
           <|> then_k
           <|> else_k
           <|> do_k
           <|> of_k
           <|> nil_k

(* TODO: Implement excape characters and more sophisticated strings *)
let string_l = char '"' *> take_while (function '"' -> false | _ -> true) <* char '"'
  >>| fun s -> String s

let int_l = take_while1 (function '0' .. '9' -> true | _ -> false)
  >>| fun n -> Int (int_of_string n)

let literal = string_l <|> int_l


let identifier = take_while1 (function 'a' .. 'z' | 'A' .. 'Z' | '0'.. '9' -> true | _ -> false)
  >>| fun id -> Identifier id


let token = (keyword >>| fun x -> Keyword x)
            <|> (operator >>| fun x -> Operator x)
            <|> (literal >>| fun x -> Literal x)
            <|> identifier
            (* <|> return EOF *)

let is_whitespace = function ' ' | '\t' | '\n' -> true | _ -> false

let spaces = take_while is_whitespace

let lex str =
  match parse_string (many (spaces *> token <* spaces)) str with
  | Ok v -> v
  | Error msg -> failwith msg


