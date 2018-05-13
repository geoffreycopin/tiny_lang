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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 44 "parser.ml"
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
  279 (* VOID *);
  280 (* ARROW *);
  281 (* COLON *);
  282 (* SEMICOLON *);
  283 (* COMMA *);
  284 (* CONST *);
  285 (* FUN *);
  286 (* REC *);
  287 (* ECHO *);
  288 (* VAR *);
  289 (* PROC *);
  290 (* SET *);
  291 (* WHILE *);
  292 (* CALL *);
    0|]

let yytransl_block = [|
  258 (* NUM *);
  259 (* IDENT *);
    0|]

let yylhs = "\255\255\
\002\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\008\000\008\000\003\000\003\000\003\000\003\000\004\000\
\004\000\006\000\005\000\005\000\009\000\009\000\009\000\009\000\
\009\000\009\000\010\000\010\000\010\000\010\000\010\000\011\000\
\011\000\011\000\001\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\006\000\004\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\004\000\
\004\000\001\000\002\000\001\000\001\000\001\000\005\000\001\000\
\003\000\003\000\001\000\003\000\004\000\007\000\008\000\003\000\
\006\000\007\000\002\000\003\000\004\000\003\000\003\000\001\000\
\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\044\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\003\000\004\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\043\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\020\000\021\000\022\000\000\000\
\000\000\000\000\032\000\000\000\000\000\036\000\038\000\000\000\
\039\000\041\000\042\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\037\000\000\000\000\000\029\000\000\000\000\000\000\000\
\000\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\000\000\000\000\000\007\000\026\000\017\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\025\000\
\000\000\000\000\000\000\033\000\000\000\006\000\023\000\030\000\
\000\000\034\000\031\000"

let yydgoto = "\002\000\
\004\000\000\000\083\000\084\000\049\000\050\000\064\000\065\000\
\014\000\015\000\016\000"

let yysindex = "\007\000\
\247\254\000\000\048\255\000\000\009\255\017\255\012\255\009\255\
\021\255\014\255\027\255\009\255\040\255\019\255\023\255\028\255\
\000\000\000\000\000\000\000\000\123\255\053\255\247\254\037\255\
\037\255\054\255\000\000\037\255\043\255\059\255\009\255\247\254\
\009\255\048\255\048\255\000\000\009\255\009\255\009\255\009\255\
\009\255\009\255\009\255\009\255\009\255\009\255\009\255\045\255\
\055\255\051\255\247\254\037\255\000\000\000\000\000\000\009\255\
\067\255\037\255\000\000\053\255\070\255\000\000\000\000\009\255\
\000\000\000\000\000\000\009\255\009\255\009\255\009\255\009\255\
\009\255\009\255\072\255\009\255\009\255\073\255\037\255\009\255\
\053\255\000\000\090\255\068\255\000\000\053\255\075\255\076\255\
\053\255\000\000\009\255\077\255\079\255\080\255\081\255\082\255\
\084\255\000\000\085\255\087\255\000\000\000\000\000\000\000\000\
\037\255\037\255\086\255\053\255\247\254\088\255\089\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\091\255\009\255\092\255\000\000\247\254\000\000\000\000\000\000\
\009\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\093\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\094\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\095\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\234\255\000\000\250\255\004\000\198\255\000\000\251\255\213\255\
\000\000\000\000\227\255"

let yytablesize = 141
let yytable = "\023\000\
\051\000\088\000\027\000\078\000\066\000\067\000\032\000\001\000\
\003\000\063\000\017\000\018\000\019\000\020\000\025\000\047\000\
\029\000\056\000\057\000\024\000\090\000\059\000\104\000\028\000\
\021\000\062\000\022\000\107\000\082\000\031\000\110\000\068\000\
\069\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\026\000\033\000\030\000\034\000\018\000\036\000\018\000\
\035\000\123\000\085\000\087\000\052\000\005\000\018\000\048\000\
\058\000\053\000\054\000\055\000\060\000\061\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\079\000\099\000\100\000\
\102\000\080\000\103\000\006\000\007\000\081\000\008\000\009\000\
\010\000\011\000\012\000\013\000\086\000\111\000\124\000\089\000\
\098\000\101\000\105\000\106\000\108\000\112\000\109\000\113\000\
\114\000\115\000\116\000\121\000\117\000\118\000\130\000\119\000\
\122\000\126\000\125\000\127\000\120\000\000\000\129\000\040\000\
\027\000\000\000\000\000\000\000\128\000\000\000\024\000\000\000\
\000\000\000\000\000\000\131\000\017\000\018\000\019\000\020\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\046\000\021\000\000\000\022\000"

let yycheck = "\005\000\
\023\000\060\000\008\000\047\000\034\000\035\000\012\000\001\000\
\018\001\032\000\002\001\003\001\004\001\005\001\003\001\021\000\
\003\001\024\000\025\000\003\001\064\000\028\000\081\000\003\001\
\016\001\031\000\018\001\086\000\051\000\003\001\089\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\030\001\003\001\030\001\026\001\017\001\019\001\019\001\
\026\001\108\000\056\000\058\000\016\001\006\001\026\001\003\001\
\003\001\021\001\022\001\023\001\018\001\003\001\068\000\069\000\
\070\000\071\000\072\000\073\000\074\000\025\001\076\000\077\000\
\079\000\019\001\080\000\028\001\029\001\027\001\031\001\032\001\
\033\001\034\001\035\001\036\001\018\001\091\000\109\000\018\001\
\017\001\017\001\001\001\024\001\018\001\017\001\019\001\017\001\
\017\001\017\001\017\001\106\000\017\001\017\001\125\000\017\001\
\019\001\017\001\019\001\017\001\105\000\255\255\019\001\019\001\
\019\001\255\255\255\255\255\255\122\000\255\255\024\001\255\255\
\255\255\255\255\255\255\129\000\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001"

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
  VOID\000\
  ARROW\000\
  COLON\000\
  SEMICOLON\000\
  COMMA\000\
  CONST\000\
  FUN\000\
  REC\000\
  ECHO\000\
  VAR\000\
  PROC\000\
  SET\000\
  WHILE\000\
  CALL\000\
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
# 40 "parser.mly"
                             ( _1 )
# 258 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "parser.mly"
                                  ( ASTNum(_1) )
# 265 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
                                  ( ASTId(_1) )
# 272 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                                  ( ASTBool(true) )
# 278 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                                  ( ASTBool(false) )
# 284 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
                                  ( ASTIf(_3, _4, _5) )
# 293 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 49 "parser.mly"
                                  ( ASTApplication(_2, _3) )
# 301 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                                  ( ASTPrim(Ast.Add, _3, _4) )
# 309 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                                  ( ASTPrim(Ast.Sub, _3, _4) )
# 317 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                                  ( ASTPrim(Ast.Mul, _3, _4) )
# 325 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 53 "parser.mly"
                                  ( ASTPrim(Ast.Div, _3, _4) )
# 333 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 54 "parser.mly"
                                  ( ASTPrim(Ast.Eq, _3, _4) )
# 341 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                                  ( ASTPrim(Ast.Lt, _3, _4) )
# 349 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                                  ( ASTPrim(Ast.And, _3, _4) )
# 357 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                                  ( ASTPrim(Ast.Or, _3, _4) )
# 365 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 58 "parser.mly"
                                  ( ASTNot(_3) )
# 372 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 59 "parser.mly"
                                  ( ASTAbs(_2, _4) )
# 380 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                                  ( [_1] )
# 387 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 63 "parser.mly"
                                  ( _1::_2 )
# 395 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                                  ( Ast.Bool )
# 401 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                                  ( Ast.Int )
# 407 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                                  ( Ast.Void )
# 413 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.apsType list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    Obj.repr(
# 70 "parser.mly"
                                     ( Ast.ArrowType(_2, _4) )
# 421 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 74 "parser.mly"
                                 ( [ _1 ] )
# 428 "parser.ml"
               : Ast.apsType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.apsType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType list) in
    Obj.repr(
# 75 "parser.mly"
                                 ( _1::_3 )
# 436 "parser.ml"
               : Ast.apsType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 79 "parser.mly"
                                  ( Ast.Arg(_1, _3) )
# 444 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 83 "parser.mly"
                                  ( [ _1 ] )
# 451 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 84 "parser.mly"
                                  ( _1::_3 )
# 459 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 88 "parser.mly"
                                                     ( Ast.Const(_2, _3, _4) )
# 468 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 89 "parser.mly"
                                                     ( Ast.Fun(_2, _3, _5, _7) )
# 478 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 90 "parser.mly"
                                                     ( Ast.FunRec(_3, _4, _6, _8) )
# 488 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 91 "parser.mly"
                                                     ( Ast.Var(_2, _3) )
# 496 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 92 "parser.mly"
                                                     ( Ast.Proc(_2, _4, _6) )
# 505 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 93 "parser.mly"
                                                     ( Ast.ProcRec(_3, _5, _7) )
# 514 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 97 "parser.mly"
                                  ( Ast.Echo(_2) )
# 521 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 98 "parser.mly"
                                  ( Ast.Set(_2, _3) )
# 529 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 99 "parser.mly"
                                  ( Ast.IfStat(_2, _3, _4) )
# 538 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 100 "parser.mly"
                                  ( Ast.While(_2, _3) )
# 546 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 101 "parser.mly"
                                  ( Ast.Call(_2, _3) )
# 554 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 105 "parser.mly"
                                         ( [ Ast.StatCmd(_1)] )
# 561 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 106 "parser.mly"
                                         ( Ast.DecCmd(_1)::_3 )
# 569 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 107 "parser.mly"
                                         ( Ast.StatCmd(_1)::_3 )
# 577 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 111 "parser.mly"
                                        ( _2 )
# 584 "parser.ml"
               : Ast.cmd list))
(* Entry prog *)
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
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.cmd list)
