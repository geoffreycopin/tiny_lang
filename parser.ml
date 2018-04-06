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
  | LBRACK
  | RBRACK
  | EOL
  | BOOL
  | INT
  | STAR
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
# 33 "parser.ml"
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
  268 (* LBRACK *);
  269 (* RBRACK *);
  270 (* EOL *);
  271 (* BOOL *);
  272 (* INT *);
  273 (* STAR *);
  274 (* ARROW *);
  275 (* COLON *);
  276 (* SEMICOLON *);
  277 (* COMMA *);
  278 (* CONST *);
  279 (* FUN *);
  280 (* REC *);
  281 (* ECHO *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\002\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\010\000\010\000\003\000\003\000\003\000\
\011\000\011\000\004\000\005\000\005\000\007\000\007\000\007\000\
\008\000\009\000\009\000\009\000\001\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\006\000\004\000\005\000\
\005\000\005\000\005\000\001\000\002\000\001\000\001\000\005\000\
\001\000\003\000\003\000\001\000\003\000\004\000\007\000\008\000\
\002\000\001\000\003\000\003\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\030\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\003\000\004\000\
\005\000\000\000\025\000\000\000\000\000\000\000\000\000\014\000\
\015\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\028\000\029\000\000\000\000\000\022\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\000\007\000\018\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\000\009\000\010\000\
\011\000\016\000\019\000\021\000\023\000\000\000\006\000\024\000"

let yydgoto = "\002\000\
\004\000\000\000\038\000\053\000\054\000\048\000\008\000\009\000\
\010\000\049\000\039\000"

let yysindex = "\003\000\
\254\254\000\000\007\255\000\000\014\255\003\255\005\255\253\254\
\000\255\008\255\018\255\018\255\029\255\000\000\000\000\000\000\
\000\000\060\255\000\000\007\255\007\255\021\255\018\255\000\000\
\000\000\005\255\030\255\018\255\005\255\005\255\005\255\005\255\
\005\255\005\255\000\000\000\000\000\000\026\255\027\255\000\000\
\042\255\034\255\005\255\005\255\005\255\005\255\005\255\005\255\
\036\255\018\255\018\255\031\255\032\255\035\255\042\255\005\255\
\040\255\041\255\043\255\045\255\000\000\000\000\000\000\046\255\
\018\255\042\255\005\255\059\255\047\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\061\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\055\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\048\255\
\000\000\000\000\000\000\000\000\062\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\246\255\000\000\204\255\249\255\000\000\000\000\
\248\255\028\000\027\000"

let yytablesize = 77
let yytable = "\019\000\
\026\000\027\000\068\000\001\000\012\000\014\000\015\000\016\000\
\017\000\003\000\034\000\035\000\036\000\076\000\018\000\011\000\
\020\000\042\000\040\000\021\000\022\000\043\000\044\000\045\000\
\046\000\047\000\013\000\023\000\005\000\006\000\028\000\007\000\
\024\000\025\000\037\000\056\000\057\000\058\000\059\000\060\000\
\064\000\041\000\050\000\052\000\051\000\055\000\062\000\067\000\
\069\000\065\000\070\000\071\000\066\000\072\000\075\000\073\000\
\074\000\079\000\012\000\077\000\014\000\015\000\016\000\017\000\
\029\000\030\000\031\000\032\000\033\000\018\000\080\000\078\000\
\017\000\026\000\020\000\061\000\063\000"

let yycheck = "\007\000\
\011\000\012\000\055\000\001\000\002\001\001\001\002\001\003\001\
\004\001\012\001\018\000\020\000\021\000\066\000\010\001\002\001\
\020\001\028\000\026\000\020\001\013\001\029\000\030\000\031\000\
\032\000\033\000\024\001\010\001\022\001\023\001\002\001\025\001\
\015\001\016\001\014\001\043\000\044\000\045\000\046\000\047\000\
\051\000\012\001\017\001\002\001\018\001\012\001\011\001\013\001\
\056\000\019\001\011\001\011\001\021\001\011\001\065\000\011\001\
\011\001\011\001\011\001\067\000\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\078\000\013\001\
\018\001\013\001\013\001\048\000\050\000"

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
  LBRACK\000\
  RBRACK\000\
  EOL\000\
  BOOL\000\
  INT\000\
  STAR\000\
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
# 36 "parser.mly"
                             ( _1 )
# 184 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 40 "parser.mly"
                                  ( ASTNum(_1) )
# 191 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "parser.mly"
                                  ( ASTId(_1) )
# 198 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
                                  ( ASTBool(true) )
# 204 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
                                  ( ASTBool(false) )
# 210 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 44 "parser.mly"
                                  ( ASTIf(_3, _4, _5) )
# 219 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 45 "parser.mly"
                                  ( ASTApplication(_2, _3) )
# 227 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 46 "parser.mly"
                                  ( ASTPrim(Ast.Add, _3, _4) )
# 235 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 47 "parser.mly"
                                  ( ASTPrim(Ast.Sub, _3, _4) )
# 243 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
                                  ( ASTPrim(Ast.Mul, _3, _4) )
# 251 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                                  ( ASTPrim(Ast.Div, _3, _4) )
# 259 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                                  ( ASTApplication(_1, ASTEmpty) )
# 266 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 53 "parser.mly"
                                  ( ASTApplication(_1, _2) )
# 274 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
                                  ( Ast.Bool )
# 280 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
                                  ( Ast.Int )
# 286 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    Obj.repr(
# 59 "parser.mly"
                                     ( Ast.ArrowType(_2, _4) )
# 294 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 63 "parser.mly"
                                  ( Ast.FuncType(_1, Ast.Empty) )
# 301 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.apsType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 64 "parser.mly"
                                  ( Ast.FuncType(_1, _3) )
# 309 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 68 "parser.mly"
                                  ( Ast.Arg(_1, _3) )
# 317 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 72 "parser.mly"
                                  ( Ast.Args(_1, Ast.EmptyArg) )
# 324 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 75 "parser.mly"
                                  ( Ast.Args(_1, _3) )
# 332 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 79 "parser.mly"
                                                     ( Ast.Const(_2, _3, _4) )
# 341 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 80 "parser.mly"
                                                     ( Ast.Fun(_2, _3, _5, _7) )
# 351 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 81 "parser.mly"
                                                     ( Ast.FunRec(_3, _4, _6, _8) )
# 361 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 85 "parser.mly"
                                ( Ast.Echo(_2) )
# 368 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 89 "parser.mly"
                                         ( Ast.StatCmd(_1, Ast.EmptyCmd) )
# 375 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 90 "parser.mly"
                                         ( Ast.DecCmd (_1, _3) )
# 383 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 91 "parser.mly"
                                         ( Ast.StatCmd(_1, _3) )
# 391 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.cmds) in
    Obj.repr(
# 95 "parser.mly"
                                            ( _2 )
# 398 "parser.ml"
               : Ast.cmds))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.cmds)
