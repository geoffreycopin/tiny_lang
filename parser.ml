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
\002\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\010\000\010\000\003\000\003\000\
\003\000\011\000\011\000\004\000\005\000\005\000\006\000\006\000\
\006\000\007\000\008\000\008\000\008\000\009\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\006\000\004\000\005\000\
\005\000\005\000\005\000\004\000\001\000\002\000\001\000\001\000\
\005\000\001\000\003\000\003\000\001\000\003\000\004\000\007\000\
\008\000\002\000\001\000\003\000\003\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\003\000\004\000\005\000\000\000\000\000\
\031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\014\000\007\000\000\000\015\000\016\000\020\000\022\000\
\012\000\000\000\008\000\009\000\010\000\011\000\000\000\000\000\
\006\000\000\000\000\000\019\000\000\000\017\000"

let yydgoto = "\002\000\
\024\000\000\000\047\000\017\000\018\000\000\000\000\000\000\000\
\000\000\025\000\048\000"

let yysindex = "\002\000\
\013\255\000\000\000\000\000\000\000\000\000\000\028\255\002\255\
\000\000\013\255\013\255\013\255\013\255\013\255\013\255\242\254\
\003\255\026\255\013\255\013\255\013\255\013\255\013\255\013\255\
\030\255\248\254\002\255\013\255\013\255\031\255\032\255\033\255\
\034\255\000\000\000\000\248\254\000\000\000\000\000\000\000\000\
\000\000\035\255\000\000\000\000\000\000\000\000\036\255\029\255\
\000\000\248\254\248\254\000\000\037\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\038\255\000\000\000\000\000\000\000\000\000\000\000\000\039\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\040\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\255\255\000\000\231\255\000\000\022\000\000\000\000\000\000\000\
\000\000\028\000\004\000"

let yytablesize = 58
let yytable = "\009\000\
\039\000\036\000\001\000\016\000\026\000\015\000\037\000\038\000\
\019\000\020\000\021\000\022\000\023\000\003\000\004\000\005\000\
\006\000\029\000\030\000\031\000\032\000\033\000\007\000\027\000\
\008\000\053\000\041\000\042\000\003\000\004\000\005\000\006\000\
\010\000\011\000\012\000\013\000\014\000\007\000\028\000\008\000\
\035\000\043\000\044\000\045\000\046\000\049\000\051\000\054\000\
\040\000\013\000\021\000\034\000\050\000\052\000\000\000\000\000\
\000\000\018\000"

let yycheck = "\001\000\
\026\000\010\001\001\000\002\001\019\001\007\000\015\001\016\001\
\010\000\011\000\012\000\013\000\014\000\001\001\002\001\003\001\
\004\001\019\000\020\000\021\000\022\000\023\000\010\001\021\001\
\012\001\051\000\028\000\029\000\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\013\001\012\001\
\011\001\011\001\011\001\011\001\011\001\011\001\018\001\011\001\
\027\000\011\001\013\001\024\000\017\001\050\000\255\255\255\255\
\255\255\018\001"

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
# 171 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 40 "parser.mly"
                                  ( ASTNum(_1) )
# 178 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "parser.mly"
                                  ( ASTId(_1) )
# 185 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
                                  ( ASTBool(true) )
# 191 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
                                  ( ASTBool(false) )
# 197 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 44 "parser.mly"
                                  ( ASTIf(_3, _4, _5) )
# 206 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 45 "parser.mly"
                                  ( ASTApplication(_2, _3) )
# 214 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 46 "parser.mly"
                                  ( ASTPrim(Ast.Add, _3, _4) )
# 222 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 47 "parser.mly"
                                  ( ASTPrim(Ast.Sub, _3, _4) )
# 230 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
                                  ( ASTPrim(Ast.Mul, _3, _4) )
# 238 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                                  ( ASTPrim(Ast.Div, _3, _4) )
# 246 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                                  ( ASTAbs(_2, _4) )
# 254 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 53 "parser.mly"
                                  ( ASTApplication(_1, ASTEmpty) )
# 261 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 54 "parser.mly"
                                  ( ASTApplication(_1, _2) )
# 269 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
                                  ( Ast.Bool )
# 275 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                                  ( Ast.Int )
# 281 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    Obj.repr(
# 60 "parser.mly"
                                     ( Ast.ArrowType(_2, _4) )
# 289 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 64 "parser.mly"
                                  ( Ast.FuncType(_1, Ast.Empty) )
# 296 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.apsType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 65 "parser.mly"
                                  ( Ast.FuncType(_1, _3) )
# 304 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 69 "parser.mly"
                                  ( Ast.Arg(_1, _3) )
# 312 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 73 "parser.mly"
                                  ( Ast.Args(_1, Ast.EmptyArg) )
# 319 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 76 "parser.mly"
                                  ( Ast.Args(_1, _3) )
# 327 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 80 "parser.mly"
                                                     ( Ast.Const(_2, _3, _4) )
# 336 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 81 "parser.mly"
                                                     ( Ast.Fun(_2, _3, _5, _7) )
# 346 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.apsType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 82 "parser.mly"
                                                     ( Ast.FunRec(_3, _4, _6, _8) )
# 356 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 86 "parser.mly"
                                ( Ast.Echo(_2) )
# 363 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 90 "parser.mly"
                                         ( Ast.StatCmd(_1, Ast.EmptyCmd) )
# 370 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 91 "parser.mly"
                                         ( Ast.DecCmd (_1, _3) )
# 378 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 92 "parser.mly"
                                         ( Ast.StatCmd(_1, _3) )
# 386 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.cmds) in
    Obj.repr(
# 96 "parser.mly"
                                            ( _2 )
# 393 "parser.ml"
               : Ast.cmds))
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
