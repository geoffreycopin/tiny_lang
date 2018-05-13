open Ast

module Env = Map.Make(String)

type memVal = Free | Any | Number of int
type memory = memVal array

let empty_mem = Array.make 1024 Free

let in_env addr env =
  Env.exists (fun _ v -> v = addr ) env
              
let restrict mem env =
  Array.map (fun idx _ -> in_env idx)
         
type value =
  Num of int
| Closure of expr * value Env.t * string list
| RecClosure of string * expr * value Env.t * string list

let string_of_value v =
  match v with
    Num(n) -> "Num(" ^ string_of_int n ^ ")"
  | Closure(_) -> "Closure"
  | RecClosure(_) -> "RecClosure"

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

let rec eval_expr mem env expr =
  match expr with
    ASTNum(n) -> Num(n)
  | ASTId(id) -> Env.find id env
  | ASTBool(true) -> Num(1)
  | ASTBool(false) -> Num(0)
  | ASTPrim(op, e1, e2) -> eval_ast_prim mem env op e1 e2
  | ASTNot(e) -> eval_ast_not mem env e
  | ASTIf(cond, cons, alt) -> eval_if mem env cond cons alt
  | ASTAbs(args, e) -> Closure(e, env, args_name args)
  | ASTApplication(e, args) -> eval_app mem env e args

and eval_ast_prim mem env op e1 e2 =
  let v1 = int_of_value (eval_expr mem env e1) in
  let v2 = int_of_value (eval_expr mem env e2) in
  let result = eval_binary_prim op v1 v2 in
  Num(result)

and eval_ast_not mem env expr =
  match eval_expr mem env expr with
  | Num(0) -> Num(1)
  | Num(1) -> Num(0)
  | _ -> failwith "This program is not properly typed !"

and eval_if mem env cond cons alt =
  match eval_expr mem env cond with
  | Num(1) -> eval_expr mem env cons
  | Num(0) -> eval_expr mem env alt
  | _ -> failwith "This program is not properly typed !"

and eval_app mem env e args =
  match eval_expr mem env e with
    Closure(body, closureEnv, argsNames) ->
      let closureEnv = add_args_to_env mem env closureEnv argsNames args in
      eval_expr mem closureEnv body
  | RecClosure(name, body, closureEnv, argsNames) as rc ->
     let closureEnv = add_args_to_env mem env closureEnv argsNames args in
     let closureEnv = Env.add name rc closureEnv in
     eval_expr mem closureEnv body
  | _ -> failwith "This program is not preperly typed !"

and add_args_to_env mem currentEnv newEnv names values =
  match names, values with
    n::names, v::values -> let newVal = eval_expr mem currentEnv v in
                           let newEnv = Env.add n newVal newEnv in
                           add_args_to_env mem currentEnv newEnv names values
  | _ -> newEnv

let eval_statement mem env s =
  match s with
    Echo(e) -> print_int (int_of_value (eval_expr mem env e)); print_char '\n'
  | _ -> failwith "Unsupported operation"

let eval_declaration mem env d =
  match d with
    Const(name, _, e) -> Env.add name (eval_expr mem env e) env
  | Fun(name, _, args, e) -> let c = Closure(e, env, args_name args) in
                             Env.add name c env
  | FunRec(name, _, args, e) -> let rc = RecClosure(name, e, env, args_name args) in
                                Env.add name rc env
  | _ -> failwith "Unsupported operation"

let rec eval_prog mem env cmds =
  match cmds with
    StatCmd(s)::t -> eval_statement mem env s; eval_prog mem env t
  | DecCmd(d)::t -> let env = eval_declaration mem env d in
                    eval_prog mem env t
  | [] -> ()
        
let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let e = Parser.prog Lexer.token lexbuf in
    eval_prog empty_mem Env.empty e;
  with Lexer.Eof -> exit 0

let type_of_arg a = let Arg(_, t) = a in t
