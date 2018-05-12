exception OpConversion of string

type apsType = Int | Bool | ArrowType of apsType list * apsType

type op = Add | Mul | Sub | Div	| Eq | Lt | Not | And | Or		      
			      
let string_of_op op =
  match op with
  | Add -> "add"
  | Mul -> "mul"
  | Sub -> "sub"
  | Div -> "div"
  | Eq -> "eq"
  | Lt -> "lt"
  | Not -> "not"
  | And -> "and"
  | Or -> "or"

let op_of_string op =
  match op with
  | "add" -> Add
  | "mul" -> Mul
  | "sub" -> Sub
  | "div" -> Div
  | "eq" -> Eq
  | "lt" -> Lt
  | "not" -> Not
  | "and" -> And
  | "or" -> Or 
  | _ -> raise (OpConversion op)

type arg = Arg of string * apsType
			     
type expr =
  ASTNum of int
| ASTId of string
| ASTBool of bool
| ASTPrim of op * expr * expr
| ASTNot of expr
| ASTIf of expr * expr * expr
| ASTApplication of expr * expr list
| ASTAbs of arg list * expr

type dec =
    Const of string * apsType * expr
  | Fun of string * apsType * arg list * expr
  | FunRec of string * apsType * arg list * expr
  
let rec args_type args =
  List.map (fun a -> let Arg(_, t) = a in t) args

let rec args_name args =
  List.map (fun a -> let Arg(name, _) = a in name) args

let fun_type t args =
  ArrowType((args_type args), t)

type stat = Echo of expr

type cmd =  StatCmd of stat | DecCmd of dec
					      
