%{
    open Ast
%}

%token STAR
%token <int> NUM
%token <string> IDENT
%token TRUE FALSE
%token IF
%token PLUS MINUS TIMES DIV EQ LT NOT AND OR
%token LPAR RPAR LBRACK RBRACK
%token EOL
%token BOOL INT VOID VEC
%token STAR ARROW
%token COLON SEMICOLON
%token COMMA
%token CONST
%token FUN
%token REC
%token ECHO
%token VAR PROC SET IF WHILE CALL
%token SET NTH ALLOC
       
%start prog

%type <Ast.expr> line
%type <Ast.apsType> simpleType
%type <Ast.apsType list> types
%type <Ast.arg list> args
%type <Ast.arg> arg				       
%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.dec> dec
%type <Ast.stat> stat
%type <Ast.cmd list> cmds				   
%type <Ast.cmd list> prog
		   
%%
  
line:
expr EOL                     { $1 }
;

expr:
    NUM                           { ASTNum($1) }
  | IDENT                         { ASTId($1) }
  | TRUE                          { ASTBool(true) }
  | FALSE                         { ASTBool(false) }
  | LPAR IF expr expr expr RPAR   { ASTIf($3, $4, $5) }
  | LPAR expr exprs RPAR          { ASTApplication($2, $3) }
  | LPAR PLUS expr expr RPAR      { ASTPrim(Ast.Add, $3, $4) }
  | LPAR MINUS expr expr RPAR     { ASTPrim(Ast.Sub, $3, $4) }
  | LPAR TIMES expr expr RPAR     { ASTPrim(Ast.Mul, $3, $4) }
  | LPAR DIV expr expr RPAR       { ASTPrim(Ast.Div, $3, $4) }
  | LPAR EQ expr expr RPAR        { ASTPrim(Ast.Eq, $3, $4) }
  | LPAR LT expr expr RPAR        { ASTPrim(Ast.Lt, $3, $4) }
  | LPAR AND expr expr RPAR       { ASTPrim(Ast.And, $3, $4) }
  | LPAR OR expr expr RPAR        { ASTPrim(Ast.Or, $3, $4) }
  | LPAR NOT expr RPAR            { ASTNot($3) }
  | LPAR ALLOC expr RPAR 	  { ASTAlloc($3) }
  | LPAR NTH expr expr RPAR       { ASTNth($3, $4) }	
  | LBRACK args RBRACK expr       { ASTAbs($2, $4) }

exprs:
    expr                          { [$1] } 
  | expr exprs                    { $1::$2 }
;

simpleType:
    BOOL                          { Ast.Bool }
  | INT                           { Ast.Int }
  | VOID                          { Ast.Void }
  | LPAR types ARROW simpleType RPAR { Ast.ArrowType($2, $4) }
  | LPAR VEC simpleType RPAR      { Ast.Vec($3) }	
;

types:
    simpleType                   { [ $1 ] }
  | simpleType STAR types        { $1::$3 }	
;

arg:
    IDENT COLON simpleType        { Ast.Arg($1, $3) }
;

args:
    arg                           { [ $1 ] }
  | arg COMMA args                { $1::$3 } 
  ;

dec:
    CONST IDENT simpleType expr                      { Ast.Const($2, $3, $4) }
  | FUN IDENT simpleType LBRACK args RBRACK expr     { Ast.Fun($2, $3, $5, $7) }
  | FUN REC IDENT simpleType LBRACK args RBRACK expr { Ast.FunRec($3, $4, $6, $8) }
  | VAR IDENT simpleType                             { Ast.Var($2, $3) }
  | PROC IDENT LBRACK args RBRACK prog               { Ast.Proc($2, $4, $6) }
  | PROC REC IDENT LBRACK args RBRACK prog           { Ast.ProcRec($3, $5, $7) }
  ;

stat:
    ECHO expr                     { Ast.Echo($2) }
  | SET lval expr                 { Ast.Set($2, $3) }
  | IF expr prog prog             { Ast.IfStat($2, $3, $4) }
  | WHILE expr prog               { Ast.While($2, $3) }
  | CALL IDENT exprs              { Ast.Call($2, $3) }
  ;

lval:
    IDENT                         { ASTId($1) }
  | LPAR NTH lval expr RPAR       { ASTNth($3, $4) }
  ;

cmds:
    stat                                 { [ Ast.StatCmd($1)] }
  | dec SEMICOLON cmds                   { Ast.DecCmd($1)::$3 }
  | stat SEMICOLON cmds                  { Ast.StatCmd($1)::$3 }
  ;

prog:
    LBRACK cmds RBRACK                  { $2 }
  ;
      
      
      
      
