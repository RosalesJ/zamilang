open Angstrom
open Common

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

let is_whitespace = function ' ' | '\t' | '\n' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_alphanum = is_digit <||> is_alpha

let reserved_keywords =
  [("while", While); ("for", For); ("to", To); ("break", Break); ("let", Let);
   ("in", In); ("end", End); ("function", Function); ("var", Var); ("type", Type);
   ("array", Array); ("if", If); ("then", Then); ("else", Else); ("do", Do);
   ("of", Of); ("nil", Nil)]

let reserved_operators =
  [(",", Comma); (":", Colon); (";", R_Paren); (")", R_Paren); ("(", L_Paren);
   ("]", R_Brack); ("[", L_Brack); ("}", R_Brace); ("{", L_Brace); (".", Dot);
   ("+", Plus); ("-", Minus); ("*", Times); ("/", Slash); ("=", Eq); ("<>", Not);
   ("<", Less); ("<=", Less_Eq); (">", Greater); (">=", Greater_Eq); ("&", And);
   ("|", Or); (":=", Def)]

let decide_reserved assoc =
  assoc 
  |> List.map (function (a, b) -> string a *> return b)
  |> choice

let keyword = decide_reserved reserved_keywords
  >>= fun r ->
  peek_char >>= function
  | Some c when is_alphanum c -> fail "Not a thing"
  | _ -> return r

let operator =  decide_reserved reserved_operators

(* TODO: Implement excape characters and more sophisticated strings *)
let string_l = char '"' *> take_while (function '"' -> false | _ -> true) <* char '"'
  >>| fun s -> String s

let int_l = take_while1 is_digit
  >>| fun n -> Int (int_of_string n)

let literal = string_l <|> int_l


let identifier = take_while1 is_alphanum
  >>| fun id -> Identifier id


let token = (keyword >>| fun x -> Keyword x)
            <|> (operator >>| fun x -> Operator x)
            <|> (literal >>| fun x -> Literal x)
            <|> identifier
            (* <|> return EOF *)

let spaces = take_while is_whitespace

let lex str =
  match parse_string (many (spaces *> token)) str with
  | Ok v -> v
  | Error msg -> failwith msg
