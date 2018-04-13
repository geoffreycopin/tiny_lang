open Ast

type value = Num of int

let string_of_value v =
  match v with
    Num(n) -> "Num(" ^ string_of_int n ^ ")"

let int_of_value v =
  match v with
    Num(n) -> n
                  
type primitive = Add | Mul | Sub | Div | Not | And | Or | Eq | Lt

module Env = Map.Make(String)

let prim_of_op op =
  match op with
    Ast.Add -> Add
  | Ast.Sub -> Sub
  | Ast.Mul -> Mul
  | Ast.Div -> Div

let eval_unary_prim p v =
  match p, v with
    Not, 0 -> 1
  | Not, 1 -> 0
  | _ -> failwith "Fatal error in eval_unary_prim :("
		    
let eval_binary_prim prim val1 val2 =
  match prim, val1, val2 with
    And, 1, 1 -> 1
  | And, _, _ -> 0
  | Or, 0, 0 -> 0
  | Or, _, _ -> 1
  | Eq, v1, v2 -> if v1 = v2 then 1 else 0 
  | Lt, v1, v2 -> if v1 < v2 then 1 else 0
  | Add, v1, v2 -> v1 + v2
  | Sub, v1, v2 -> v1 - v2
  | Mul, v1, v2 -> v1 * v2
  | Div, v1, v2 -> v1 / v2
  | _ -> failwith "Fatal error in eval_binary_prim :'("

let rec eval_expr env expr =
  match expr with
    ASTNum(n) -> Num(n)
  | ASTPrim(op, e1, e2) -> eval_ast_prim env op e1 e2
  | ASTBool(true) -> Num(1)
  | ASTBool(false) -> Num(0)
  | ASTId(id) -> Env.find id env
  | _ -> failwith "Unsupported operation"

and eval_ast_prim env op e1 e2 =
  let v1 = int_of_value (eval_expr env e1) in
  let v2 = int_of_value (eval_expr env e2) in
  let prim = prim_of_op op in
  let result = eval_binary_prim prim v1 v2 in
  Num(result)
			  
let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let e = Parser.expr Lexer.token lexbuf in
    print_endline (string_of_value (eval_expr Env.empty e))
  with Lexer.Eof -> exit 0
