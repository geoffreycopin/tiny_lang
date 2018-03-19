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
| ASTExpressions of expr * expr
| ASTEmpty
	       
let is_empty ast =
  match ast with
  | ASTEmpty -> false
  | _ -> true
