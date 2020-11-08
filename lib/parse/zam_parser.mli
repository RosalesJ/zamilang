type symbol = string
type var = VarSimple of symbol
         | VarField of var * symbol
         | VarSubscript of var * exp
and exp = ExpVar of var
        | ExpNil
        | ExpInt of int
        | ExpString of symbol
        | ExpCall of { func : symbol; args : exp list }
        | ExpOp of { left : exp; oper : oper; right : exp }
        | ExpRecord of { fields : (symbol * exp) list; typ : symbol }
        | ExpSeq of exp list
        | ExpAssign of { var : var; exp : exp }
        | ExpIf of { test : exp; body : exp; else_body : exp option }
        | ExpWhile of { test : exp; body : exp }
        | ExpFor of { var : symbol; escape : bool ref; lo : exp; hi : exp; body : exp }
        | ExpBreak
        | ExpLet of { decs : dec list; body : exp list }
        | ExpArray of { typ : symbol; size : exp; init : exp }
and dec = DecFunction of fundec list
        | DecVar of { name : symbol; escape : bool ref; typ : symbol option; init : exp; }
        | DecType of tydec list
and fundec = { funname: symbol; params: field list; result: symbol option; body: exp }
and tydec = { tyname: symbol; typ: typ }
and typ = TypName of symbol
        | TypRecord of field list
        | TypArray of symbol
and oper = OpPlus
         | OpMinus
         | OpTimes
         | OpDivide
         | OpEq
         | OpNeq
         | OpLt
         | OpLe
         | OpGt
         | OpGe
and field = { fname : symbol; escape : bool ref; ftyp : symbol }

val parse : string -> (exp, string) result
