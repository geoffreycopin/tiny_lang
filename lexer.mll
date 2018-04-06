{
    open Parser
    exception Eof
}

rule token = parse 
    [' ' '\t'] 	   { token lexbuf }
  | ['\n']	   { EOL }
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | "if"           { IF }
  | "+"            { PLUS }
  | "-"	           { MINUS }
  | "*"	           { TIMES }
  | "/"	           { DIV }
  | "true" 	   { TRUE}
  | "false" 	   { FALSE }
  | '('		   { LPAR }
  | ')'		   { RPAR }
  | ']'            { RBRACK }		   
  | '['            { LBRACK }
  | "bool"         { BOOL }
  | "int"	   { INT }
  | "CONST"        { CONST }		   
  | "REC"          { REC }
  | "FUN"          { FUN }
  | "ECHO"         { ECHO }		   
  | '*'            { STAR }
  | "->"           { ARROW }
  | ':'            { COLON }
  | ','            { COMMA }
  | ';'            { SEMICOLON }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof		   { raise Eof }
