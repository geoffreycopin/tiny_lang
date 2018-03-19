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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 26 "parser.ml"
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
  269 (* BOOL *);
  270 (* INT *);
  271 (* STAR *);
  272 (* ARROW *);
  273 (* COLON *);
  274 (* COMMA *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\002\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\006\000\006\000\003\000\003\000\003\000\
\007\000\007\000\004\000\001\000\001\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\006\000\004\000\005\000\
\005\000\005\000\005\000\001\000\002\000\001\000\001\000\005\000\
\001\000\003\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\014\000\015\000\019\000\021\000\000\000\000\000\000\000\000\000\
\018\000\000\000\016\000"

let yydgoto = "\002\000\
\004\000\000\000\013\000\005\000\000\000\000\000\014\000"

let yysindex = "\001\000\
\001\255\000\000\245\254\000\000\246\254\247\254\001\255\247\254\
\000\000\000\000\000\000\000\000\248\254\249\254\247\254\247\254\
\000\000\000\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\253\254\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\007\000\000\000\250\255\000\000\000\000\000\000\002\000"

let yytablesize = 17
let yytable = "\011\000\
\008\000\001\000\003\000\009\000\010\000\006\000\015\000\007\000\
\016\000\018\000\019\000\020\000\017\000\012\000\000\000\000\000\
\017\000"

let yycheck = "\006\000\
\010\001\001\000\002\001\013\001\014\001\017\001\015\001\018\001\
\016\001\016\000\011\001\000\000\016\001\007\000\255\255\255\255\
\015\000"

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
  BOOL\000\
  INT\000\
  STAR\000\
  ARROW\000\
  COLON\000\
  COMMA\000\
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
# 28 "parser.mly"
                             ( _1 )
# 124 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 32 "parser.mly"
                                  ( ASTNum(_1) )
# 131 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "parser.mly"
                                  ( ASTId(_1) )
# 138 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
                                  ( ASTBool(true) )
# 144 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
                                  ( ASTBool(false) )
# 150 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 36 "parser.mly"
                                  ( ASTIf(_3, _4, _5) )
# 159 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 37 "parser.mly"
                                  ( ASTExpressions(_2, _3) )
# 167 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 38 "parser.mly"
                                  ( ASTPrim(Ast.Add, _3, _4) )
# 175 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 39 "parser.mly"
                                  ( ASTPrim(Ast.Sub, _3, _4) )
# 183 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 40 "parser.mly"
                                  ( ASTPrim(Ast.Mul, _3, _4) )
# 191 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 41 "parser.mly"
                                  ( ASTPrim(Ast.Div, _3, _4) )
# 199 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 44 "parser.mly"
                                  ( ASTExpressions(_1, ASTEmpty) )
# 206 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 45 "parser.mly"
                                  ( ASTExpressions(_1, _2) )
# 214 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                                  ( Ast.Bool )
# 220 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                                  ( Ast.Int )
# 226 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.apsType) in
    Obj.repr(
# 51 "parser.mly"
                                     ( Ast.ArrowType(_2, _4) )
# 234 "parser.ml"
               : Ast.apsType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 55 "parser.mly"
                                  ( Ast.FuncType(_1, Ast.Empty) )
# 241 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.apsType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 56 "parser.mly"
                                  ( Ast.FuncType(_1, _3) )
# 249 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.apsType) in
    Obj.repr(
# 60 "parser.mly"
                                  ( Ast.Arg(_1, _3) )
# 257 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 64 "parser.mly"
                                  ( Ast.Args(_1, Ast.EmptyArg) )
# 264 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 67 "parser.mly"
                                  ( Ast.Args(_1, _3) )
# 272 "parser.ml"
               : Ast.args))
(* Entry args *)
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
let args (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.args)
