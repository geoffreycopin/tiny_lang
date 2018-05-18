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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 47 "parser.ml"
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
  280 (* VEC *);
  281 (* ARROW *);
  282 (* COLON *);
  283 (* SEMICOLON *);
  284 (* COMMA *);
  285 (* CONST *);
  286 (* FUN *);
  287 (* REC *);
  288 (* ECHO *);
  289 (* VAR *);
  290 (* PROC *);
  291 (* SET *);
  292 (* WHILE *);
  293 (* CALL *);
  294 (* NTH *);
  295 (* ALLOC *);
    0|]

let yytransl_block = [|
  258 (* NUM *);
  259 (* IDENT *);
    0|]

let yylhs = "\255\255\
\002\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\008\000\008\000\003\000\003\000\003\000\
\003\000\003\000\004\000\004\000\006\000\005\000\005\000\009\000\
\009\000\009\000\009\000\009\000\009\000\010\000\010\000\010\000\
\010\000\010\000\012\000\012\000\011\000\011\000\011\000\001\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\006\000\004\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\004\000\
\004\000\005\000\004\000\001\000\002\000\001\000\001\000\001\000\
\005\000\004\000\001\000\003\000\003\000\001\000\003\000\004\000\
\007\000\008\000\003\000\006\000\007\000\002\000\003\000\004\000\
\003\000\003\000\001\000\005\000\001\000\003\000\003\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\049\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\003\000\004\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\038\000\000\000\000\000\000\000\043\000\000\000\
\000\000\000\000\000\000\000\000\000\000\048\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\022\000\023\000\024\000\000\000\000\000\000\000\035\000\000\000\
\000\000\000\000\039\000\041\000\000\000\042\000\046\000\047\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\040\000\000\000\000\000\000\000\032\000\000\000\000\000\000\000\
\000\000\000\000\021\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\000\000\000\000\017\000\007\000\
\029\000\019\000\031\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\008\000\009\000\010\000\011\000\
\012\000\013\000\014\000\015\000\018\000\026\000\028\000\000\000\
\000\000\000\000\036\000\000\000\044\000\006\000\025\000\033\000\
\000\000\037\000\034\000"

let yydgoto = "\002\000\
\004\000\000\000\091\000\092\000\053\000\054\000\069\000\070\000\
\014\000\015\000\016\000\033\000"

let yysindex = "\008\000\
\249\254\000\000\054\255\000\000\098\255\012\255\255\254\098\255\
\026\255\001\255\003\255\098\255\028\255\019\255\020\255\030\255\
\000\000\000\000\000\000\000\000\139\255\047\255\249\254\004\255\
\004\255\049\255\000\000\004\255\035\255\055\255\000\000\021\255\
\098\255\249\254\098\255\054\255\054\255\000\000\098\255\098\255\
\098\255\098\255\098\255\098\255\098\255\098\255\098\255\098\255\
\098\255\098\255\098\255\039\255\048\255\038\255\249\254\040\255\
\000\000\000\000\000\000\098\255\063\255\004\255\000\000\047\255\
\067\255\003\255\000\000\000\000\098\255\000\000\000\000\000\000\
\098\255\098\255\098\255\098\255\098\255\098\255\098\255\075\255\
\098\255\098\255\098\255\077\255\079\255\004\255\098\255\047\255\
\000\000\004\255\096\255\073\255\000\000\047\255\086\255\087\255\
\047\255\098\255\000\000\098\255\088\255\091\255\092\255\093\255\
\094\255\095\255\000\000\100\255\102\255\103\255\000\000\000\000\
\000\000\000\000\000\000\104\255\004\255\004\255\105\255\047\255\
\249\254\106\255\109\255\110\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\111\255\
\098\255\112\255\000\000\249\254\000\000\000\000\000\000\000\000\
\098\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\114\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\115\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\247\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\090\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\234\255\000\000\245\255\252\255\216\255\000\000\251\255\210\255\
\000\000\000\000\241\255\056\000"

let yytablesize = 178
let yytable = "\023\000\
\055\000\025\000\027\000\029\000\085\000\031\000\034\000\020\000\
\001\000\020\000\003\000\068\000\060\000\061\000\024\000\051\000\
\063\000\020\000\032\000\056\000\071\000\072\000\099\000\096\000\
\057\000\058\000\059\000\067\000\028\000\026\000\035\000\030\000\
\089\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\036\000\037\000\115\000\
\038\000\052\000\095\000\062\000\064\000\119\000\093\000\056\000\
\122\000\065\000\066\000\005\000\057\000\058\000\059\000\090\000\
\086\000\088\000\087\000\100\000\101\000\102\000\103\000\104\000\
\105\000\106\000\113\000\108\000\109\000\110\000\116\000\138\000\
\094\000\114\000\006\000\007\000\097\000\008\000\009\000\010\000\
\011\000\012\000\013\000\107\000\123\000\111\000\124\000\112\000\
\117\000\118\000\139\000\017\000\018\000\019\000\020\000\120\000\
\125\000\121\000\136\000\126\000\127\000\128\000\129\000\130\000\
\135\000\021\000\027\000\022\000\131\000\146\000\132\000\133\000\
\134\000\098\000\000\000\137\000\140\000\141\000\142\000\143\000\
\000\000\000\000\145\000\144\000\045\000\030\000\000\000\000\000\
\000\000\000\000\000\000\147\000\017\000\018\000\019\000\020\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\021\000\000\000\022\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\049\000\050\000"

let yycheck = "\005\000\
\023\000\003\001\008\000\003\001\051\000\003\001\012\000\017\001\
\001\000\019\001\018\001\034\000\024\000\025\000\003\001\021\000\
\028\000\027\001\016\001\016\001\036\000\037\000\069\000\064\000\
\021\001\022\001\023\001\033\000\003\001\031\001\003\001\031\001\
\055\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\027\001\027\001\088\000\
\019\001\003\001\062\000\003\001\018\001\094\000\060\000\016\001\
\097\000\003\001\038\001\006\001\021\001\022\001\023\001\024\001\
\026\001\028\001\019\001\073\000\074\000\075\000\076\000\077\000\
\078\000\079\000\086\000\081\000\082\000\083\000\090\000\120\000\
\018\001\087\000\029\001\030\001\018\001\032\001\033\001\034\001\
\035\001\036\001\037\001\017\001\098\000\017\001\100\000\017\001\
\001\001\025\001\121\000\002\001\003\001\004\001\005\001\018\001\
\017\001\019\001\118\000\017\001\017\001\017\001\017\001\017\001\
\117\000\016\001\025\001\018\001\017\001\140\000\017\001\017\001\
\017\001\066\000\255\255\019\001\019\001\017\001\017\001\017\001\
\255\255\255\255\019\001\137\000\019\001\019\001\255\255\255\255\
\255\255\255\255\255\255\145\000\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\038\001\039\001"

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
  VEC\000\
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
  NTH\000\
  ALLOC\000\
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
# 41 "parser.mly"
                             ( _1 )
# 285 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 45 "parser.mly"
                                  ( ASTNum(_1) )
# 292 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "parser.mly"
                                  ( ASTId(_1) )
# 299 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                                  ( ASTBool(true) )
# 305 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                                  ( ASTBool(false) )
# 311 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                                  ( ASTIf(_3, _4, _5) )
# 320 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 50 "parser.mly"
                                  ( ASTApplication(_2, _3) )
# 328 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                                  ( ASTPrim(Ast.Add, _3, _4) )
# 336 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                                  ( ASTPrim(Ast.Sub, _3, _4) )
# 344 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 53 "parser.mly"
                                  ( ASTPrim(Ast.Mul, _3, _4) )
# 352 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 54 "parser.mly"
                                  ( ASTPrim(Ast.Div, _3, _4) )
# 360 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                                  ( ASTPrim(Ast.Eq, _3, _4) )
# 368 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                                  ( ASTPrim(Ast.Lt, _3, _4) )
# 376 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                                  ( ASTPrim(Ast.And, _3, _4) )
# 384 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 58 "parser.mly"
                                  ( ASTPrim(Ast.Or, _3, _4) )
# 392 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 59 "parser.mly"
                                  ( ASTNot(_3) )
# 399 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 60 "parser.mly"
                            ( ASTAlloc(_3) )
# 406 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
                                  ( ASTNth(_3, _4) )
# 414 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                                  ( ASTAbs(_2, _4) )
# 422 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                                  ( [_1] )
# 429 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 66 "parser.mly"
                                  ( _1::_2 )
# 437 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                                  ( Ast.Bool )
# 443 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                                  ( Ast.Int )
# 449 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                                  ( Ast.Void )
# 455 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.apsType list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    Obj.repr(
# 73 "parser.mly"
                                     ( Ast.ArrowType(_2, _4) )
# 463 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    Obj.repr(
# 74 "parser.mly"
                                  ( Ast.Vec(_3) )
# 470 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 78 "parser.mly"
                                 ( [ _1 ] )
# 477 "parser.ml"
               : Ast.apsType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.apsType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType list) in
    Obj.repr(
# 79 "parser.mly"
                                 ( _1::_3 )
# 485 "parser.ml"
               : Ast.apsType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 83 "parser.mly"
                                  ( Ast.Arg(_1, _3) )
# 493 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 87 "parser.mly"
                                  ( [ _1 ] )
# 500 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 88 "parser.mly"
                                  ( _1::_3 )
# 508 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 92 "parser.mly"
                                                     ( Ast.Const(_2, _3, _4) )
# 517 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 93 "parser.mly"
                                                     ( Ast.Fun(_2, _3, _5, _7) )
# 527 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 94 "parser.mly"
                                                     ( Ast.FunRec(_3, _4, _6, _8) )
# 537 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 95 "parser.mly"
                                                     ( Ast.Var(_2, _3) )
# 545 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 96 "parser.mly"
                                                     ( Ast.Proc(_2, _4, _6) )
# 554 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 97 "parser.mly"
                                                     ( Ast.ProcRec(_3, _5, _7) )
# 563 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 101 "parser.mly"
                                  ( Ast.Echo(_2) )
# 570 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 102 "parser.mly"
                                  ( Ast.Set(_2, _3) )
# 578 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 103 "parser.mly"
                                  ( Ast.IfStat(_2, _3, _4) )
# 587 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 104 "parser.mly"
                                  ( Ast.While(_2, _3) )
# 595 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 105 "parser.mly"
                                  ( Ast.Call(_2, _3) )
# 603 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "parser.mly"
                                  ( ASTId(_1) )
# 610 "parser.ml"
               : 'lval))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'lval) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 110 "parser.mly"
                                  ( ASTNth(_3, _4) )
# 618 "parser.ml"
               : 'lval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 114 "parser.mly"
                                         ( [ Ast.StatCmd(_1)] )
# 625 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 115 "parser.mly"
                                         ( Ast.DecCmd(_1)::_3 )
# 633 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 116 "parser.mly"
                                         ( Ast.StatCmd(_1)::_3 )
# 641 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 120 "parser.mly"
                                        ( _2 )
# 648 "parser.ml"
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
