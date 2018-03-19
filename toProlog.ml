open Ast
   
let rec print_prolog e =
  match e with
  | ASTNum(n) -> Printf.printf "%d" n
  | ASTBool(b) -> Printf.printf "%b" b
  | ASTId(id) -> Printf.printf "%s" id
  | ASTIf(cond, cons, alt) -> print_if cons cond alt
  | ASTPrim(op, e1, e2) -> print_prim op e1 e2
  | ASTExpressions(first, next) -> print_expressions first next;
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

and print_expressions first next =
  print_string "seq(";
  let rec print_expressions_rec first next =
    print_prolog first;
    match next with
    | ASTEmpty -> ()
    | ASTExpressions (h, t) -> print_string ", "; print_expressions_rec h t
    | _ as e -> print_prolog e
  in
  print_expressions_rec first next;
  print_string ")"

let rec print_type t =
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
  let rec print_types_rec head tail =
    print_type head;
    match tail with
    | Empty -> ()
    | FuncType(h, t) -> print_string ", "; print_types_rec h t
  in
  match t with
  | Empty -> ()
  | FuncType(h, t) -> print_types_rec h t; print_string "]"       

let print_arg a =
  let Arg(ident, t) = a in
  print_string "(";
  print_string ident;
  print_string ", ";
  print_type t;
  print_string ")"
      
let print_args a =
  print_string "[";
  let rec print_args_rec head tail =
    print_arg head;
    match tail with
    | EmptyArg -> ()
    | Args(h, t) -> print_string ", "; print_args_rec h t
  in
  match a with
  | EmptyArg -> ()
  | Args(h, t) -> print_args_rec h t
							
let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let e = Parser.args Lexer.token lexbuf in
    print_args e;
    print_char '\n'
  with Lexer.Eof -> exit 0
