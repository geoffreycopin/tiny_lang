type token =
  | STAR
  | NUM of (int)
  | IDENT of (string)
  | TRUE
  | FALSE
  | IF
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EQ
  | LT
  | NOT
  | AND
  | OR
  | LPAR
  | RPAR
  | LBRACK
  | RBRACK
  | EOL
  | BOOL
  | INT
  | VOID
  | VEC
  | ARROW
  | COLON
  | SEMICOLON
  | COMMA
  | CONST
  | FUN
  | REC
  | ECHO
  | VAR
  | PROC
  | SET
  | WHILE
  | CALL
  | NTH
  | ALLOC

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.cmd list
