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
  | ARROW
  | COLON
  | SEMICOLON
  | COMMA
  | CONST
  | FUN
  | REC
  | ECHO

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 38 "parser.ml"
let yytransl_const = [|
  257 (* STAR *);
  260 (* TRUE *);
  261 (* FALSE *);
  262 (* IF *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIV *);
  267 (* EQ *);
  268 (* LT *);
  269 (* NOT *);
  270 (* AND *);
  271 (* OR *);
  272 (* LPAR *);
  273 (* RPAR *);
  274 (* LBRACK *);
  275 (* RBRACK *);
  276 (* EOL *);
  277 (* BOOL *);
  278 (* INT *);
  279 (* ARROW *);
  280 (* COLON *);
  281 (* SEMICOLON *);
  282 (* COMMA *);
  283 (* CONST *);
  284 (* FUN *);
  285 (* REC *);
  286 (* ECHO *);
    0|]

let yytransl_block = [|
  258 (* NUM *);
  259 (* IDENT *);
    0|]

let yylhs = "\255\255\
\002\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\011\000\011\000\003\000\003\000\003\000\004\000\004\000\
\006\000\005\000\005\000\007\000\007\000\007\000\008\000\009\000\
\009\000\009\000\010\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\006\000\004\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\004\000\
\004\000\001\000\002\000\001\000\001\000\005\000\001\000\003\000\
\003\000\001\000\003\000\004\000\007\000\008\000\002\000\001\000\
\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\003\000\004\000\005\000\000\000\000\000\
\036\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\
\019\000\007\000\000\000\020\000\021\000\025\000\017\000\027\000\
\000\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\015\000\000\000\000\000\006\000\000\000\000\000\024\000\000\000\
\022\000"

let yydgoto = "\002\000\
\034\000\000\000\066\000\067\000\022\000\023\000\000\000\000\000\
\000\000\000\000\035\000"

let yysindex = "\002\000\
\054\255\000\000\000\000\000\000\000\000\000\000\037\255\001\255\
\000\000\054\255\054\255\054\255\054\255\054\255\054\255\054\255\
\054\255\054\255\054\255\054\255\237\254\000\255\250\254\054\255\
\054\255\054\255\054\255\054\255\054\255\054\255\004\255\054\255\
\054\255\054\255\005\255\242\254\054\255\001\255\054\255\013\255\
\016\255\017\255\020\255\043\255\044\255\000\000\045\255\046\255\
\000\000\000\000\242\254\000\000\000\000\000\000\000\000\000\000\
\047\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\053\255\042\255\000\000\242\254\242\254\000\000\049\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\048\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\051\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\050\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\255\255\000\000\221\255\005\000\031\000\000\000\000\000\000\000\
\000\000\000\000\037\000"

let yytablesize = 74
let yytable = "\009\000\
\054\000\051\000\001\000\021\000\036\000\020\000\052\000\053\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
\032\000\033\000\037\000\038\000\046\000\050\000\039\000\040\000\
\041\000\042\000\043\000\044\000\045\000\058\000\047\000\048\000\
\059\000\060\000\072\000\055\000\061\000\057\000\003\000\004\000\
\005\000\006\000\010\000\011\000\012\000\013\000\014\000\015\000\
\016\000\017\000\018\000\019\000\007\000\069\000\008\000\003\000\
\004\000\005\000\006\000\062\000\063\000\064\000\065\000\068\000\
\070\000\073\000\026\000\018\000\056\000\007\000\049\000\008\000\
\023\000\071\000"

let yycheck = "\001\000\
\036\000\016\001\001\000\003\001\024\001\007\000\021\001\022\001\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000\019\001\026\001\017\001\017\001\024\000\025\000\
\026\000\027\000\028\000\029\000\030\000\017\001\032\000\033\000\
\017\001\017\001\070\000\037\000\017\001\039\000\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\001\001\018\001\002\001\
\003\001\004\001\005\001\017\001\017\001\017\001\017\001\017\001\
\023\001\017\001\019\001\017\001\038\000\016\001\034\000\018\001\
\023\001\069\000"

let yynames_const = "\
  STAR\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  EQ\000\
  LT\000\
  NOT\000\
  AND\000\
  OR\000\
  LPAR\000\
  RPAR\000\
  LBRACK\000\
  RBRACK\000\
  EOL\000\
  BOOL\000\
  INT\000\
  ARROW\000\
  COLON\000\
  SEMICOLON\000\
  COMMA\000\
  CONST\000\
  FUN\000\
  REC\000\
  ECHO\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 38 "parser.mly"
                             ( _1 )
# 201 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                                  ( ASTNum(_1) )
# 208 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "parser.mly"
                                  ( ASTId(_1) )
# 215 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                                  ( ASTBool(true) )
# 221 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                                  ( ASTBool(false) )
# 227 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 46 "parser.mly"
                                  ( ASTIf(_3, _4, _5) )
# 236 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 47 "parser.mly"
                                  ( ASTApplication(_2, _3) )
# 244 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
                                  ( ASTPrim(Ast.Add, _3, _4) )
# 252 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                                  ( ASTPrim(Ast.Sub, _3, _4) )
# 260 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                                  ( ASTPrim(Ast.Mul, _3, _4) )
# 268 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                                  ( ASTPrim(Ast.Div, _3, _4) )
# 276 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                                  ( ASTPrim(Ast.Eq, _3, _4) )
# 284 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 53 "parser.mly"
                                  ( ASTPrim(Ast.Lt, _3, _4) )
# 292 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 54 "parser.mly"
                                  ( ASTPrim(Ast.And, _3, _4) )
# 300 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                                  ( ASTPrim(Ast.Or, _3, _4) )
# 308 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                                  ( ASTNot(_3) )
# 315 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                                  ( ASTAbs(_2, _4) )
# 323 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 60 "parser.mly"
                                  ( ASTApplication(_1, ASTEmpty) )
# 330 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 61 "parser.mly"
                                  ( ASTApplication(_1, _2) )
# 338 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                                  ( Ast.Bool )
# 344 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                                  ( Ast.Int )
# 350 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.apsType list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    Obj.repr(
# 67 "parser.mly"
                                     ( Ast.ArrowType(_2, _4) )
# 358 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 71 "parser.mly"
                                 ( [ _1 ] )
# 365 "parser.ml"
               : Ast.apsType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.apsType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType list) in
    Obj.repr(
# 72 "parser.mly"
                                 ( _1::_3 )
# 373 "parser.ml"
               : Ast.apsType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 76 "parser.mly"
                                  ( Ast.Arg(_1, _3) )
# 381 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 80 "parser.mly"
                                  ( [ _1 ] )
# 388 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 81 "parser.mly"
                                  ( _1::_3 )
# 396 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 85 "parser.mly"
                                                     ( Ast.Const(_2, _3, _4) )
# 405 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 86 "parser.mly"
                                                     ( Ast.Fun(_2, _3, _5, _7) )
# 415 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 87 "parser.mly"
                                                     ( Ast.FunRec(_3, _4, _6, _8) )
# 425 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 91 "parser.mly"
                                ( Ast.Echo(_2) )
# 432 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 95 "parser.mly"
                                         ( [ Ast.StatCmd(_1)] )
# 439 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 96 "parser.mly"
                                         ( Ast.DecCmd(_1)::_3 )
# 447 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 97 "parser.mly"
                                         ( Ast.StatCmd(_1)::_3 )
# 455 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 101 "parser.mly"
                                        ( _2 )
# 462 "parser.ml"
               : Ast.cmd list))
(* Entry expr *)
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
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
