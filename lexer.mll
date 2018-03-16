{
    open Parser
    exception Eof
}

rule token = parse 
    [' ' '\t'] 	   { token lexbuf }
  | ['\n']	   { EOL }
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | "if"           { IF }
  | "add"          { PLUS }
  | "sub"	   { MINUS }
  | "mul"	   { TIMES }
  | "div"	   { DIV }
  | "true" 	   { TRUE}
  | "false" 	   { FALSE }
  | '('		   { LPAR }
  | ')'		   { RPAR }
  | "bool"         { BOOL }
  | "int"	   { INT }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof		   { raise Eof }