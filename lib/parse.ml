open Angstrom

type keyword =
  | While
  | For
  | To
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
  | Lit of literal
  | Identifier of string
  | Comment of string
  | EOF

type mana =
  [ `A of int
  | `B of string ]

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
let neg = string "not" *> return Not
let less = char '<' *> return Less
let less_eq = string "<=" *> return Less_Eq
let greater = char '>' *> return 
let greater_eq = string ">=" *> return Greater_Eq
let amp = char '&' *> return And
let bar = char '|' *> return Or
let def = string ":=" *> return Def

let _while = string "while" *> return While

let _for = string "for" *> return For
