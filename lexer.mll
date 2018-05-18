{
    open Parser
    exception Eof
}

rule token = parse 
    [' ' '\t' '\n'] 	   { token lexbuf }
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | "*"	           { STAR }
  | "if"           { IF }
  | "add"          { PLUS }
  | "sub"	   { MINUS }
  | "mul"	   { TIMES }
  | "div"	   { DIV }
  | "eq"	   { EQ }
  | "lt"           { LT }
  | "not"          { NOT }
  | "and"	   { AND }
  | "or"	   { OR }
  | "true" 	   { TRUE}
  | "false" 	   { FALSE }
  | "nth" 	   { NTH }
  | "alloc"        { ALLOC }
  | "len"          { LEN }
  | "SET"  	   { SET }
  | '('		   { LPAR }
  | ')'		   { RPAR }
  | ']'            { RBRACK }		   
  | '['            { LBRACK }
  | "bool"         { BOOL }
  | "int"	   { INT }
  | "vec"	   { VEC }
  | "void"         { VOID }
  | "CONST"        { CONST }		   
  | "REC"          { REC }
  | "FUN"          { FUN }
  | "ECHO"         { ECHO }
  | "VAR"	   { VAR}
  | "PROC"	   { PROC}
  | "SET"	   { SET }
  | "IF" 	   { IF }
  | "WHILE" 	   { WHILE }
  | "CALL"	   { CALL }
  | "->"           { ARROW }
  | ':'            { COLON }
  | ','            { COMMA }
  | ';'            { SEMICOLON }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof		   { raise Eof }
