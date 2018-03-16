open Ast
   
let rec print_prolog e =
  match e with
  | ASTNum(n) -> Printf.printf "%d" n
  | ASTBool(b) -> Printf.printf "%b" b
  | ASTId(id) -> Printf.printf "%s" id
  | ASTIf(cond, cons, alt) -> print_if cons cond alt
  | ASTPrim(op, e1, e2) -> print_prim op e1 e2
  | ASTExpressions(first, next) -> print_string "seq("; print_expressions first next; print_string ")"
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
  match (first, next) with
  | (f, ASTEmpty) -> print_prolog f
  | (f, n) -> print_prolog f; print_string ", "; print_prolog n
    
            
       

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let e = Parser.line Lexer.token lexbuf in
    print_prolog e;
    print_char '\n'
  with Lexer.Eof -> exit 0
