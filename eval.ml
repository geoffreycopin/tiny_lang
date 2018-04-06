open Ast

type value = Natural of int
type primitive = Add | Mul | Sub | Div | Not | And | Or | Eq | Lt

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

let eval_expr env expr =
  match expr with
    ASTNum(n) -> n
  | ASTPrim(op, ASTNum(v1), ASTNum(v2)) -> eval_binary_prim (prim_of_op op) v1 v2
  | _ -> failwith "Unsupported operation"
			  
let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let e = Parser.prog Lexer.token lexbuf in
    print_int (eval_expr [] e);
    print_char '\n'
  with Lexer.Eof -> exit 0
