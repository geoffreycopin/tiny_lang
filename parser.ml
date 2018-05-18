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
  | LEN

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 48 "parser.ml"
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
  296 (* LEN *);
    0|]

let yytransl_block = [|
  258 (* NUM *);
  259 (* IDENT *);
    0|]

let yylhs = "\255\255\
\002\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\008\000\008\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\006\000\005\000\005\000\
\009\000\009\000\009\000\009\000\009\000\009\000\010\000\010\000\
\010\000\010\000\010\000\012\000\012\000\011\000\011\000\011\000\
\001\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\006\000\004\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\004\000\
\004\000\005\000\004\000\004\000\001\000\002\000\001\000\001\000\
\001\000\005\000\004\000\001\000\003\000\003\000\001\000\003\000\
\004\000\007\000\008\000\003\000\006\000\007\000\002\000\003\000\
\004\000\003\000\003\000\001\000\005\000\001\000\003\000\003\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\050\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\003\000\004\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\039\000\000\000\000\000\000\000\044\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\023\000\024\000\025\000\000\000\000\000\000\000\036\000\
\000\000\000\000\000\000\040\000\042\000\000\000\043\000\047\000\
\048\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\041\000\000\000\000\000\000\000\033\000\000\000\
\000\000\000\000\000\000\000\000\022\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\
\017\000\019\000\007\000\030\000\020\000\032\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\018\000\
\027\000\029\000\000\000\000\000\000\000\037\000\000\000\045\000\
\006\000\026\000\034\000\000\000\038\000\035\000"

let yydgoto = "\002\000\
\004\000\000\000\093\000\094\000\054\000\055\000\070\000\071\000\
\014\000\015\000\016\000\033\000"

let yysindex = "\003\000\
\244\254\000\000\088\255\000\000\015\255\012\255\254\254\015\255\
\029\255\255\254\005\255\015\255\045\255\024\255\032\255\046\255\
\000\000\000\000\000\000\000\000\098\255\063\255\244\254\041\255\
\041\255\064\255\000\000\041\255\050\255\073\255\000\000\042\255\
\015\255\244\254\015\255\088\255\088\255\000\000\015\255\015\255\
\015\255\015\255\015\255\015\255\015\255\015\255\015\255\015\255\
\015\255\015\255\015\255\015\255\055\255\066\255\059\255\244\254\
\245\254\000\000\000\000\000\000\015\255\065\255\041\255\000\000\
\063\255\070\255\005\255\000\000\000\000\015\255\000\000\000\000\
\000\000\015\255\015\255\015\255\015\255\015\255\015\255\015\255\
\072\255\015\255\015\255\015\255\074\255\075\255\076\255\041\255\
\015\255\063\255\000\000\041\255\095\255\101\255\000\000\063\255\
\080\255\108\255\063\255\015\255\000\000\015\255\082\255\111\255\
\112\255\113\255\114\255\115\255\000\000\116\255\122\255\123\255\
\000\000\000\000\000\000\000\000\000\000\000\000\124\255\041\255\
\041\255\125\255\063\255\244\254\126\255\129\255\130\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\131\255\015\255\132\255\000\000\244\254\000\000\
\000\000\000\000\000\000\015\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\133\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\134\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\117\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\247\255\000\000\254\255\029\000\215\255\000\000\251\255\213\255\
\000\000\000\000\017\000\083\000"

let yytablesize = 153
let yytable = "\023\000\
\025\000\029\000\027\000\001\000\057\000\003\000\034\000\031\000\
\087\000\058\000\059\000\060\000\092\000\056\000\024\000\052\000\
\017\000\018\000\019\000\020\000\032\000\061\000\062\000\098\000\
\069\000\064\000\101\000\068\000\026\000\030\000\021\000\028\000\
\022\000\074\000\075\000\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\091\000\035\000\
\118\000\021\000\036\000\021\000\072\000\073\000\122\000\095\000\
\057\000\125\000\037\000\021\000\097\000\058\000\059\000\060\000\
\038\000\053\000\063\000\065\000\102\000\103\000\104\000\105\000\
\106\000\107\000\108\000\066\000\110\000\111\000\112\000\067\000\
\088\000\141\000\096\000\117\000\089\000\116\000\090\000\099\000\
\109\000\119\000\113\000\114\000\115\000\005\000\126\000\120\000\
\127\000\123\000\128\000\017\000\018\000\019\000\020\000\039\000\
\040\000\041\000\042\000\043\000\044\000\045\000\046\000\047\000\
\048\000\021\000\142\000\022\000\006\000\007\000\139\000\008\000\
\009\000\010\000\011\000\012\000\013\000\121\000\124\000\129\000\
\130\000\131\000\132\000\133\000\134\000\149\000\147\000\049\000\
\050\000\051\000\135\000\136\000\137\000\028\000\150\000\140\000\
\143\000\144\000\145\000\146\000\138\000\100\000\148\000\046\000\
\031\000"

let yycheck = "\005\000\
\003\001\003\001\008\000\001\000\016\001\018\001\012\000\003\001\
\052\000\021\001\022\001\023\001\024\001\023\000\003\001\021\000\
\002\001\003\001\004\001\005\001\016\001\024\000\025\000\065\000\
\034\000\028\000\070\000\033\000\031\001\031\001\016\001\003\001\
\018\001\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\056\000\003\001\
\090\000\017\001\027\001\019\001\036\000\037\000\096\000\061\000\
\016\001\099\000\027\001\027\001\063\000\021\001\022\001\023\001\
\019\001\003\001\003\001\018\001\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\003\001\082\000\083\000\084\000\038\001\
\026\001\123\000\018\001\089\000\019\001\088\000\028\001\018\001\
\017\001\092\000\017\001\017\001\017\001\006\001\100\000\001\001\
\102\000\018\001\017\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\124\000\018\001\029\001\030\001\121\000\032\001\
\033\001\034\001\035\001\036\001\037\001\025\001\019\001\017\001\
\017\001\017\001\017\001\017\001\017\001\143\000\140\000\038\001\
\039\001\040\001\017\001\017\001\017\001\025\001\148\000\019\001\
\019\001\017\001\017\001\017\001\120\000\067\000\019\001\019\001\
\019\001"

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
  LEN\000\
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
# 282 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 45 "parser.mly"
                                  ( ASTNum(_1) )
# 289 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "parser.mly"
                                  ( ASTId(_1) )
# 296 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
                                  ( ASTBool(true) )
# 302 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                                  ( ASTBool(false) )
# 308 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                                  ( ASTIf(_3, _4, _5) )
# 317 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 50 "parser.mly"
                                  ( ASTApplication(_2, _3) )
# 325 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                                  ( ASTPrim(Ast.Add, _3, _4) )
# 333 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                                  ( ASTPrim(Ast.Sub, _3, _4) )
# 341 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 53 "parser.mly"
                                  ( ASTPrim(Ast.Mul, _3, _4) )
# 349 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 54 "parser.mly"
                                  ( ASTPrim(Ast.Div, _3, _4) )
# 357 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                                  ( ASTPrim(Ast.Eq, _3, _4) )
# 365 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                                  ( ASTPrim(Ast.Lt, _3, _4) )
# 373 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                                  ( ASTPrim(Ast.And, _3, _4) )
# 381 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 58 "parser.mly"
                                  ( ASTPrim(Ast.Or, _3, _4) )
# 389 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 59 "parser.mly"
                                  ( ASTNot(_3) )
# 396 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 60 "parser.mly"
                            ( ASTAlloc(_3) )
# 403 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
                                  ( ASTNth(_3, _4) )
# 411 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                                  ( ASTLen(_3) )
# 418 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                                  ( ASTAbs(_2, _4) )
# 426 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                                  ( [_1] )
# 433 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 67 "parser.mly"
                                  ( _1::_2 )
# 441 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                                  ( Ast.Bool )
# 447 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                                  ( Ast.Int )
# 453 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                                  ( Ast.Void )
# 459 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.apsType list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    Obj.repr(
# 74 "parser.mly"
                                     ( Ast.ArrowType(_2, _4) )
# 467 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    Obj.repr(
# 75 "parser.mly"
                                  ( Ast.Vec(_3) )
# 474 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 79 "parser.mly"
                                 ( [ _1 ] )
# 481 "parser.ml"
               : Ast.apsType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.apsType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType list) in
    Obj.repr(
# 80 "parser.mly"
                                 ( _1::_3 )
# 489 "parser.ml"
               : Ast.apsType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 84 "parser.mly"
                                  ( Ast.Arg(_1, _3) )
# 497 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 88 "parser.mly"
                                  ( [ _1 ] )
# 504 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 89 "parser.mly"
                                  ( _1::_3 )
# 512 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 93 "parser.mly"
                                                     ( Ast.Const(_2, _3, _4) )
# 521 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 94 "parser.mly"
                                                     ( Ast.Fun(_2, _3, _5, _7) )
# 531 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 95 "parser.mly"
                                                     ( Ast.FunRec(_3, _4, _6, _8) )
# 541 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 96 "parser.mly"
                                                     ( Ast.Var(_2, _3) )
# 549 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 97 "parser.mly"
                                                     ( Ast.Proc(_2, _4, _6) )
# 558 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 98 "parser.mly"
                                                     ( Ast.ProcRec(_3, _5, _7) )
# 567 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 102 "parser.mly"
                                  ( Ast.Echo(_2) )
# 574 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lval) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 103 "parser.mly"
                                  ( Ast.Set(_2, _3) )
# 582 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 104 "parser.mly"
                                  ( Ast.IfStat(_2, _3, _4) )
# 591 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 105 "parser.mly"
                                  ( Ast.While(_2, _3) )
# 599 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 106 "parser.mly"
                                  ( Ast.Call(_2, _3) )
# 607 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
                                  ( ASTId(_1) )
# 614 "parser.ml"
               : 'lval))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'lval) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 111 "parser.mly"
                                  ( ASTNth(_3, _4) )
# 622 "parser.ml"
               : 'lval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 115 "parser.mly"
                                         ( [ Ast.StatCmd(_1)] )
# 629 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 116 "parser.mly"
                                         ( Ast.DecCmd(_1)::_3 )
# 637 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 117 "parser.mly"
                                         ( Ast.StatCmd(_1)::_3 )
# 645 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 121 "parser.mly"
                                        ( _2 )
# 652 "parser.ml"
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
