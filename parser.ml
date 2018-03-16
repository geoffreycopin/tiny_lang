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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 20 "parser.ml"
let yytransl_const = [|
  259 (* TRUE *);
  260 (* FALSE *);
  261 (* IF *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* TIMES *);
  265 (* DIV *);
  266 (* LPAR *);
  267 (* RPAR *);
  268 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\006\000\004\000\005\000\
\005\000\005\000\005\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\003\000\004\000\005\000\000\000\014\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\000\007\000\000\000\008\000\
\009\000\010\000\011\000\006\000"

let yydgoto = "\002\000\
\008\000\022\000\023\000"

let yysindex = "\001\000\
\033\255\000\000\000\000\000\000\000\000\000\000\023\255\000\000\
\245\254\033\255\033\255\033\255\033\255\033\255\033\255\000\000\
\033\255\033\255\033\255\033\255\033\255\033\255\248\254\033\255\
\249\254\250\254\252\254\253\254\000\000\000\000\003\255\000\000\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\016\000"

let yytablesize = 43
let yytable = "\009\000\
\016\000\001\000\030\000\032\000\033\000\015\000\034\000\035\000\
\017\000\018\000\019\000\020\000\021\000\036\000\012\000\024\000\
\025\000\026\000\027\000\028\000\000\000\000\000\031\000\003\000\
\004\000\005\000\006\000\010\000\011\000\012\000\013\000\014\000\
\007\000\003\000\004\000\005\000\006\000\029\000\000\000\000\000\
\000\000\000\000\007\000"

let yycheck = "\001\000\
\012\001\001\000\011\001\011\001\011\001\007\000\011\001\011\001\
\010\000\011\000\012\000\013\000\014\000\011\001\011\001\017\000\
\018\000\019\000\020\000\021\000\255\255\255\255\024\000\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\001\001\002\001\003\001\004\001\022\000\255\255\255\255\
\255\255\255\255\010\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  IF\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LPAR\000\
  RPAR\000\
  EOL\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 19 "parser.mly"
                             ( _1 )
# 116 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 23 "parser.mly"
                                  ( ASTNum(_1) )
# 123 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 24 "parser.mly"
                                  ( ASTId(_1) )
# 130 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "parser.mly"
                                  ( ASTBool(true) )
# 136 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "parser.mly"
                                  ( ASTBool(false) )
# 142 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 27 "parser.mly"
                                  ( ASTIf(_3, _4, _5) )
# 151 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 28 "parser.mly"
                                  ( ASTExpressions(_2, _3) )
# 159 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 29 "parser.mly"
                                  ( ASTPrim(Ast.Add, _3, _4) )
# 167 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 30 "parser.mly"
                                  ( ASTPrim(Ast.Sub, _3, _4) )
# 175 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 31 "parser.mly"
                                  ( ASTPrim(Ast.Mul, _3, _4) )
# 183 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 32 "parser.mly"
                                  ( ASTPrim(Ast.Div, _3, _4) )
# 191 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
                                  ( _1 )
# 198 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 36 "parser.mly"
                                  ( ASTExpressions(_1, _2) )
# 206 "parser.ml"
               : 'exprs))
(* Entry line *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let line (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
