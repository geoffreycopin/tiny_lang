exception OpConversion of string

type apsType = Int | Bool | ArrowType of apsFuncType * apsType
and apsFuncType = Empty | FuncType of apsType * apsFuncType

type op = Add | Mul | Sub | Div			      
			      
let string_of_op op =
  match op with
  | Add -> "add"
  | Mul -> "mul"
  | Sub -> "sub"
  | Div -> "div"

let op_of_string op =
  match op with
  | "add" -> Add
  | "mul" -> Mul
  | "sub" -> Sub
  | "div" -> Div
  | _ -> raise (OpConversion op)

type arg = Arg of string * apsType

type args = Args of arg	* args | EmptyArg	     
			     
type expr =
  ASTNum of int
| ASTId of string
| ASTBool of bool
| ASTPrim of op * expr * expr
| ASTIf of expr * expr * expr
| ASTApplication of expr * expr
| ASTAbs of args * expr
| ASTEmpty
	       
let is_empty ast =
  match ast with
  | ASTEmpty -> false
  | _ -> true

type dec =
    Const of string * apsType * expr
  | Fun of string * apsType * args * expr
  | FunRec of string * apsType * args * expr

type stat = Echo of expr

type cmds =  StatCmd of stat * cmds | DecCmd of dec * cmds | EmptyCmd 
					      
