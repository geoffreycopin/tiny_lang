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
\002\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\011\000\011\000\003\000\003\000\003\000\004\000\004\000\
\006\000\005\000\005\000\008\000\008\000\008\000\009\000\010\000\
\010\000\010\000\001\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\006\000\004\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\004\000\
\004\000\001\000\002\000\001\000\001\000\005\000\001\000\003\000\
\003\000\001\000\003\000\004\000\007\000\008\000\002\000\001\000\
\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\036\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\003\000\004\000\
\005\000\000\000\000\000\031\000\000\000\000\000\035\000\000\000\
\020\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\033\000\034\000\000\000\000\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\000\000\000\000\019\000\
\007\000\025\000\017\000\027\000\024\000\000\000\000\000\000\000\
\000\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\015\000\022\000\029\000\000\000\006\000\030\000"

let yydgoto = "\002\000\
\004\000\000\000\046\000\047\000\042\000\043\000\061\000\008\000\
\009\000\010\000\062\000"

let yysindex = "\014\000\
\239\254\000\000\238\254\000\000\013\255\004\255\038\255\250\254\
\009\255\254\254\248\254\248\254\032\255\000\000\000\000\000\000\
\000\000\063\255\033\255\000\000\238\254\238\254\000\000\248\254\
\000\000\000\000\038\255\019\255\248\254\038\255\038\255\038\255\
\038\255\038\255\038\255\038\255\038\255\038\255\038\255\038\255\
\014\255\020\255\025\255\000\000\000\000\057\255\037\255\000\000\
\033\255\043\255\038\255\038\255\038\255\038\255\038\255\038\255\
\038\255\045\255\038\255\038\255\038\255\047\255\248\254\038\255\
\033\255\248\254\248\254\064\255\033\255\038\255\065\255\067\255\
\068\255\069\255\070\255\071\255\000\000\072\255\073\255\000\000\
\000\000\000\000\000\000\000\000\000\000\074\255\038\255\075\255\
\078\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\038\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\077\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\079\255\000\000\000\000\076\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\080\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\248\255\026\000\209\255\000\000\249\255\000\000\
\000\000\240\255\039\000"

let yytablesize = 100
let yytable = "\020\000\
\003\000\068\000\027\000\028\000\044\000\045\000\012\000\024\000\
\005\000\006\000\040\000\007\000\025\000\026\000\001\000\011\000\
\023\000\084\000\021\000\048\000\050\000\088\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\013\000\022\000\029\000\041\000\049\000\063\000\064\000\014\000\
\015\000\016\000\017\000\070\000\071\000\072\000\073\000\074\000\
\075\000\076\000\065\000\078\000\079\000\018\000\082\000\019\000\
\083\000\066\000\086\000\067\000\069\000\077\000\089\000\081\000\
\014\000\015\000\016\000\017\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\018\000\099\000\
\019\000\090\000\087\000\091\000\092\000\093\000\094\000\095\000\
\096\000\097\000\098\000\085\000\102\000\100\000\101\000\032\000\
\018\000\026\000\023\000\080\000"

let yycheck = "\007\000\
\018\001\049\000\011\000\012\000\021\000\022\000\003\001\016\001\
\027\001\028\001\018\000\030\001\021\001\022\001\001\000\003\001\
\019\001\065\000\025\001\027\000\029\000\069\000\030\000\031\000\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\029\001\025\001\003\001\003\001\018\001\024\001\019\001\002\001\
\003\001\004\001\005\001\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\026\001\059\000\060\000\016\001\063\000\018\001\
\064\000\001\001\067\000\023\001\018\001\017\001\070\000\017\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\087\000\
\018\001\017\001\019\001\017\001\017\001\017\001\017\001\017\001\
\017\001\017\001\017\001\066\000\100\000\019\001\017\001\019\001\
\017\001\019\001\023\001\061\000"

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
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 38 "parser.mly"
                             ( _1 )
# 216 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                                  ( ASTNum(_1) )
# 223 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "parser.mly"
                                  ( ASTId(_1) )
# 230 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                                  ( ASTBool(true) )
# 236 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                                  ( ASTBool(false) )
# 242 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 46 "parser.mly"
                                  ( ASTIf(_3, _4, _5) )
# 251 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 47 "parser.mly"
                                  ( ASTApplication(_2, _3) )
# 259 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
                                  ( ASTPrim(Ast.Add, _3, _4) )
# 267 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                                  ( ASTPrim(Ast.Sub, _3, _4) )
# 275 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                                  ( ASTPrim(Ast.Mul, _3, _4) )
# 283 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                                  ( ASTPrim(Ast.Div, _3, _4) )
# 291 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                                  ( ASTPrim(Ast.Eq, _3, _4) )
# 299 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 53 "parser.mly"
                                  ( ASTPrim(Ast.Lt, _3, _4) )
# 307 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 54 "parser.mly"
                                  ( ASTPrim(Ast.And, _3, _4) )
# 315 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                                  ( ASTPrim(Ast.Or, _3, _4) )
# 323 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                                  ( ASTNot(_3) )
# 330 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                                  ( ASTAbs(_2, _4) )
# 338 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 60 "parser.mly"
                                  ( ASTApplication(_1, ASTEmpty) )
# 345 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 61 "parser.mly"
                                  ( ASTApplication(_1, _2) )
# 353 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                                  ( Ast.Bool )
# 359 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                                  ( Ast.Int )
# 365 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.apsType list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    Obj.repr(
# 67 "parser.mly"
                                     ( Ast.ArrowType(_2, _4) )
# 373 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 71 "parser.mly"
                                 ( [ _1 ] )
# 380 "parser.ml"
               : Ast.apsType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.apsType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType list) in
    Obj.repr(
# 72 "parser.mly"
                                 ( _1::_3 )
# 388 "parser.ml"
               : Ast.apsType list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 76 "parser.mly"
                                  ( Ast.Arg(_1, _3) )
# 396 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 80 "parser.mly"
                                  ( [ _1 ] )
# 403 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg list) in
    Obj.repr(
# 81 "parser.mly"
                                  ( _1::_3 )
# 411 "parser.ml"
               : Ast.arg list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 85 "parser.mly"
                                                     ( Ast.Const(_2, _3, _4) )
# 420 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 86 "parser.mly"
                                                     ( Ast.Fun(_2, _3, _5, _7) )
# 430 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 87 "parser.mly"
                                                     ( Ast.FunRec(_3, _4, _6, _8) )
# 440 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 91 "parser.mly"
                                ( Ast.Echo(_2) )
# 447 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 95 "parser.mly"
                                         ( [ Ast.StatCmd(_1)] )
# 454 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 96 "parser.mly"
                                         ( Ast.DecCmd(_1)::_3 )
# 462 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 97 "parser.mly"
                                         ( Ast.StatCmd(_1)::_3 )
# 470 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 101 "parser.mly"
                                        ( _2 )
# 477 "parser.ml"
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
