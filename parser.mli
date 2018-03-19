type token =
  | NUM of (int)
  | IDENT of (string)
  | TRUE
  | FALSE
  | IF
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LPAR
  | RPAR
  | EOL
  | BOOL
  | INT
  | STAR
  | ARROW
  | COLON
  | COMMA

val args :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.args
