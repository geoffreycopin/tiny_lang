open Ast
   
let rec print_prolog e =
  match e with
  | ASTNum(n) -> Printf.printf "%d" n
  | ASTBool(b) -> Printf.printf "%b" b
  | ASTId(id) -> Printf.printf "%s" id
  | ASTNot(e) -> print_string "not("; print_prolog e; print_string ")"
  | ASTIf(cond, cons, alt) -> print_if cons cond alt
  | ASTPrim(op, e1, e2) -> print_prim op e1 e2
  | ASTApplication(first, next) -> print_application first next;
  | ASTAbs(args, expr) -> print_abs args expr
  | ASTEmpty -> ()

and print_prim op e1 e2 =
  Printf.printf "%s(" (string_of_op op);
  print_prolog e1;
  Printf.printf " ";
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

and print_application first next =
  print_string "app(";
  let rec print_application_rec first next =
    print_prolog first;
    match next with
    | ASTEmpty -> ()
    | ASTApplication (h, t) -> print_string ", "; print_application_rec h t
    | _ as e -> print_prolog e
  in
  print_application_rec first next;
  print_string ")"

and print_type t =
  match t with
  | Int -> print_string "int"
  | Bool -> print_string "bool"
  | ArrowType(h, t) ->
     print_string "arrow(";
     print_types h;
     print_string ", ";
     print_type t;
     print_string ")"

and print_types t =
  print_string "[";
  List.iter (print_type) t;
  print_string "]"

and print_arg a =
  let Arg(ident, t) = a in
  print_string "(";
  print_string ident;
  print_string ", ";
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
			       
let print_dec d =
  match d with
  | Const(id, t, e) -> print_string "const(";
		       print_string (id ^ ", ");
		       print_type t;
		       print_string ", ";
		       print_prolog e;
	               print_string ")"
  | Fun(id, t, a, e) -> print_string "fun(";
		        print_string (id ^ ", ");
		        print_type (fun_type t a);
			print_string ", ";
		        print_args a;
			print_string ", ";
		        print_prolog e;
		        print_string ")"
  | FunRec(id, t, a, e) -> print_string "funRec(";
			   print_string (id ^ ", ");
			   print_type (fun_type t a);
			   print_string ", ";
			   print_args a;
			   print_string ", ";
			   print_prolog e;
			   print_string ")"

let print_stat s =
  let Echo(e) = s in
  print_string "echo(";
  print_prolog e;
  print_string ")"

let print_command c =
  match c with
  | StatCmd(s) -> print_stat s
  | DecCmd(d) -> print_dec d

let print_commands c =
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
    print_char '\n'
  with Lexer.Eof -> exit 0
