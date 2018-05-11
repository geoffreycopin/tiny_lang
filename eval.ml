open Ast

module Env = Map.Make(String)
   
type value =
  Num of int
| Closure of expr * value Env.t * string list

let string_of_value v =
  match v with
    Num(n) -> "Num(" ^ string_of_int n ^ ")"
  | Closure(_) -> "Closure"

let int_of_value v =
  match v with
    Num(n) -> n
  | _ -> failwith "Cannot convert to int"
     
let eval_unary_prim p v =
  match p, v with
    Not, 0 -> 1
  | Not, 1 -> 0
  | _ -> failwith "Fatal error in eval_unary_prim :("
		    
let eval_binary_prim prim val1 val2 =
  match prim, val1, val2 with
    Ast.And, 1, 1 -> 1
  | Ast.And, _, _ -> 0
  | Ast.Or, 0, 0 -> 0
  | Ast.Or, _, _ -> 1
  | Ast.Eq, v1, v2 -> if v1 = v2 then 1 else 0 
  | Ast.Lt, v1, v2 -> if v1 < v2 then 1 else 0
  | Ast.Add, v1, v2 -> v1 + v2
  | Ast.Sub, v1, v2 -> v1 - v2
  | Ast.Mul, v1, v2 -> v1 * v2
  | Ast.Div, v1, v2 -> v1 / v2
  | _ -> failwith "Fatal error in eval_binary_prim :'("

let rec eval_expr env expr =
  match expr with
    ASTNum(n) -> Num(n)
  | ASTId(id) -> Env.find id env
  | ASTBool(true) -> Num(1)
  | ASTBool(false) -> Num(0)
  | ASTPrim(op, e1, e2) -> eval_ast_prim env op e1 e2
  | ASTNot(e) -> eval_ast_not env e
  | ASTIf(cond, cons, alt) -> eval_if env cond cons alt
  | ASTAbs(args, e) -> Closure(e, env, args_name args)
  | _ -> failwith "Unsupported operation"

and eval_ast_prim env op e1 e2 =
  let v1 = int_of_value (eval_expr env e1) in
  let v2 = int_of_value (eval_expr env e2) in
  let result = eval_binary_prim op v1 v2 in
  Num(result)

and eval_ast_not env expr =
  match eval_expr env expr with
  | Num(0) -> Num(1)
  | Num(1) -> Num(0)
  | _ -> failwith "This program is not properly typed !"

and eval_if env cond cons alt =
  match eval_expr env cond with
  | Num(1) -> eval_expr env cons
  | Num(0) -> eval_expr env alt
  | _ -> failwith "This program is not properly typed !"
			  
let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let e = Parser.expr Lexer.token lexbuf in
    print_endline (string_of_value (eval_expr Env.empty e))
  with Lexer.Eof -> exit 0
