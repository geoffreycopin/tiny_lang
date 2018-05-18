open Ast

module Env = Map.Make(String)

type memVal = Free | Any | Number of int
type memory = memVal array

let empty_mem = Array.make 1024 Free
              
let num_at_address mem address =
  match Array.get mem address with
    Number(n) -> n
  | Any -> failwith "Accès à une addresse mémoire contenant <Any>"
  | _ -> failwith "Addresse mémoire invalide!"

let set_val mem addr v =
  let new_mem = Array.copy mem in
  new_mem.(addr) <- v;
  new_mem

let alloc mem =
  let rec alloc_addr mem_list current_addr =
    match mem_list with
      Free::_ -> current_addr
    | _::t -> alloc_addr t (current_addr + 1)
    | _ -> failwith "Out of memory"
  in
  let addr = alloc_addr (Array.to_list mem) 0 in
  let new_mem = Array.copy mem in
  new_mem.(addr) <- Any;
  addr, new_mem
         
type value =
  Num of int
| Address of int
| Closure of expr * value Env.t * string list
| RecClosure of string * expr * value Env.t * string list
| PClosure of cmd list * value Env.t * string list
| PRecClosure of string * cmd list * value Env.t * string list

let addr_in_env addr env =
  Env.exists (fun k v -> v = Address(addr)) env

let restrict mem env =
  Array.mapi (fun idx v -> if addr_in_env idx env then v else Free) mem

let addr_of_value v =
  match v with
    Address(n) -> n
  | _ -> failwith "Invalid address"

let string_of_value v =
  match v with
    Num(n) -> "Num(" ^ string_of_int n ^ ")"
  | Address(n) -> "Address(" ^ string_of_int n ^ ")" 
  | Closure(_) -> "Closure"
  | RecClosure(_) -> "RecClosure"
  | PClosure(_) -> "PClosure"
  | PRecClosure(_) -> "PRecClosure"

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

let eval_id mem env id =
  match Env.find id env with
    Address(a) -> Num(num_at_address mem a)
  | _ as other -> other

let rec eval_expr mem env expr =
  match expr with
    ASTNum(n) -> Num(n)
  | ASTId(id) -> eval_id mem env id
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
  
and eval_statement mem env s =
  match s with
    Echo(e) -> eval_echo mem env e
  | Set(id, e) -> eval_set mem env id e
  | IfStat(cons, cond, alt) -> eval_if_stmt mem env cons cond alt
  | While(cond, b) -> eval_while mem env cond b
  | Call(name, args) -> eval_call mem env name args

and eval_echo mem env e =
  let v = (int_of_value (eval_expr mem env e)) in
  Printf.printf "%d\n" v;
  mem, env

and eval_set mem env id e =
  let v = Number(int_of_value (eval_expr mem env e)) in
  let addr = addr_of_value (Env.find id env) in
  let mem = set_val mem addr v in
  mem, env

and eval_if_stmt mem env cond cons alt =
  let mem, env1 = match eval_expr mem env cond with
      Num(1) -> eval_block mem env cons
    | Num(0) -> eval_block mem env alt
    | _ -> failwith "This program is not properly typed !"
  in
  let mem = restrict mem env in
  mem, env

and eval_while mem env cond b =
  match eval_expr mem env cond with
    Num(0) -> mem, env
  | Num(1) -> let mem, env1 = eval_block mem env b in
              let mem = restrict mem env in
              eval_while mem env1 cond b
  | _ -> failwith "This program is not preperly typed !"

and eval_call mem env name args =
  match Env.find name env with
    PClosure(b, e, argsNames) ->
     let c_env = add_args_to_env mem env e argsNames args in
     let mem, c_env = eval_block mem c_env b in
     let mem = restrict mem env in
     mem, env
  | PRecClosure(name, b, e, argsNames) as pr ->
     let c_env = add_args_to_env mem env e argsNames args in
     let c_env = Env.add name pr c_env in
     let mem, c_env = eval_block mem c_env b in
     mem, env
  | _ -> failwith "This program is not properly typed !"
    
  
and alloc_var mem env name =
  let (addr, new_mem) = alloc mem in
  let a = Address(addr) in
  let new_env = (Env.add name a env) in
  new_mem, new_env

and eval_fun_dec mem env r name args e =
  let closure = if r then RecClosure(name, e, env, args_name args)
                else Closure(e, env, args_name args) in
  mem, Env.add name closure env

and eval_proc_dec mem env r name args cmds =
  let pclosure = if r then PRecClosure(name, cmds, env, args_name args)
                 else PClosure(cmds, env, args_name args) in
  mem, Env.add name pclosure env
  
and eval_declaration mem env d =
  match d with
    Const(name, _, e) -> mem, Env.add name (eval_expr mem env e) env
  | Fun(name, _, args, e) -> eval_fun_dec mem env false name args e
  | FunRec(name, _, args, e) -> eval_fun_dec mem env true name args e
  | Var(name, _) -> alloc_var mem env name
  | Proc(name, args, cmds) -> eval_proc_dec mem env false name args cmds
  | ProcRec(name, args, cmds) -> eval_proc_dec mem env true name args cmds

and eval_block mem env cmds =
  match cmds with
    StatCmd(s)::t -> let mem, env = eval_statement mem env s in
                     eval_block mem env t
  | DecCmd(d)::t -> let mem, env = eval_declaration mem env d in
                    eval_block mem env t
  | [] -> mem, env
                               
and  eval_prog mem env cmds =
  let _ = eval_block mem env cmds in
  ()
        
let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let e = Parser.prog Lexer.token lexbuf in
    eval_prog empty_mem Env.empty e;
  with Lexer.Eof -> exit 0
