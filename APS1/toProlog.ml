open Ast
   
let rec print_prolog e =
  match e with
  | ASTNum(n) -> Printf.printf "%d" n
  | ASTBool(b) -> Printf.printf "%b" b
  | ASTId(id) -> Printf.printf "\"%s\"" id
  | ASTNot(e) -> print_string "not("; print_prolog e; print_string ")"
  | ASTIf(cond, cons, alt) -> print_if cond cons alt
  | ASTPrim(op, e1, e2) -> print_prim op e1 e2
  | ASTApplication(first, next) -> print_application first next;
  | ASTAbs(args, expr) -> print_abs args expr

and print_prim op e1 e2 =
  Printf.printf "%s(" (string_of_op op);
  print_prolog e1;
  Printf.printf ", ";
  print_prolog e2;
  Printf.printf ")"

and print_if cond cons alt =
  Printf.printf "if(";
  print_prolog cond;
  Printf.printf ", ";
  print_prolog cons;
  Printf.printf ", ";
  print_prolog alt;
  Printf.printf ")"

and print_exprs e =
   let rec print_exprs_rec exprs =
    match exprs with
      [h] -> print_prolog h
    | h::t -> print_prolog h; print_string ", "; print_exprs t;
    | _ -> ()
   in
   print_char '[';
   print_exprs_rec e;
   print_char ']'
  
and print_application func args =
  let rec print_args args =
    match args with
      [h] -> print_prolog h
    | h::t -> print_prolog h; print_string ", "; print_args t;
    | _ -> ()
  in
  print_string "app(";
  print_prolog func;
  print_string ", [";
  print_args args;
  print_string "])"

and print_type t =
  match t with
  | Int -> print_string "int"
  | Bool -> print_string "bool"
  | Void -> print_string "void"
  | ArrowType(h, t) ->
     print_string "arrow(";
     print_types h;
     print_string ", ";
     print_type t;
     print_string ")"

and print_types t =
  let rec print_types_rec types =
    match types with
      [h] -> print_type h
    | h::t -> print_type h; print_string ", "; print_types_rec t
    | [] -> ()
  in
  print_string "[";
  print_types_rec t;
  print_string "]"

and print_arg a =
  let Arg(ident, t) = a in
  print_string "(\"";
  print_string ident;
  print_string "\", ";
  print_type t;
  print_string ")"
      
and print_args args =
  print_char '[';
  let rec print_args_rec args =
    match args with
    | [h] -> print_arg h;
    | h::t -> print_arg h; print_string ", "; print_args_rec t
    | _ -> ()
  in
  print_args_rec args;
  print_char ']'

and print_abs a e =
  print_string "abs(";
  print_args a;
  print_string ", ";
  print_prolog e;
  print_string ")"
			       
and print_dec d =
  match d with
  | Const(id, t, e) -> Printf.printf "const(\"%s\", " id;
		       print_type t;
		       print_string ", ";
		       print_prolog e;
	               print_char ')'
  | Fun(id, t, a, e) -> Printf.printf "fun(\"%s\", " id;
		        print_type (fun_type t a);
			print_string ", ";
		        print_args a;
			print_string ", ";
		        print_prolog e;
		        print_string ")"
  | FunRec(id, t, a, e) -> Printf.printf "funRec(\"%s\", " id;
			   print_type (fun_type t a);
			   print_string ", ";
			   print_args a;
			   print_string ", ";
			   print_prolog e;
			   print_string ")"
  | Var(id, t) -> Printf.printf "var(\"%s\", " id;
                  print_type t;
                  print_char ')'
  | Proc(id, a, cmds) -> print_proc false id a cmds
  | ProcRec(id, a, cmds) -> print_proc true id a cmds

and print_proc r id args cmds =
  if r then print_string "procRec" else print_string "proc";
  Printf.printf "(\"%s\", " id;
  print_args args;
  print_string ", ";
  print_commands cmds;
  print_char ')'

and print_stat s =
  match s with
    Echo(e) -> print_string "echo("; print_prolog e; print_char ')'
  | Set(id, expr) -> Printf.printf "set(\"%s\", " id;
                     print_prolog expr;
                     print_char ')'
  | IfStat(expr, cons, alt) -> print_string "if(";
                               print_prolog expr;
                               print_string ", ";
                               print_commands cons;
                               print_string ", ";
                               print_commands alt;
                               print_char ')'
  | While(expr, cmds) -> print_string "while(";
                         print_prolog expr;
                         print_string ", ";
                         print_commands cmds;
                         print_char ')'
  | Call(id, exprs) -> Printf.printf "call(\"%s\", " id;
                       print_exprs exprs;
                       print_char ')'

and print_command c =
  match c with
  | StatCmd(s) -> print_stat s
  | DecCmd(d) -> print_dec d

and print_commands c =
  let rec print_commands_rec (c: Ast.cmd list) =
    match c with
    | [h] -> print_command h
    | h::t -> print_command h; print_string ", "; print_commands_rec t
    | _ -> ()
  in
  print_string "[";
  print_commands_rec c;
  print_string "]"

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let e = Parser.prog Lexer.token lexbuf in
    print_commands e;
    print_string ".\n"
  with Lexer.Eof -> exit 0
