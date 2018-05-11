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
%token BOOL INT 
%token STAR ARROW
%token COLON SEMICOLON
%token COMMA
%token CONST
%token FUN
%token REC
%token ECHO
       
%start expr

%type <Ast.expr> line
%type <Ast.apsType> simpleType
%type <Ast.apsType list> types
%type <Ast.arg list> args
%type <Ast.arg> arg				       
%type <Ast.expr> expr
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
  | LBRACK args RBRACK expr       { ASTAbs($2, $4) }

exprs:
    expr                          { ASTApplication($1, ASTEmpty) } 
  | expr exprs                    { ASTApplication($1, $2) }
;

simpleType:
    BOOL                          { Ast.Bool }
  | INT                           { Ast.Int }
  | LPAR types ARROW simpleType RPAR { Ast.ArrowType($2, $4) }
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
  ;

stat:
  ECHO expr                     { Ast.Echo($2) }
  ;

cmds:
    stat                                 { [ Ast.StatCmd($1)] }
  | dec SEMICOLON cmds                   { Ast.DecCmd($1)::$3 }
  | stat SEMICOLON cmds                  { Ast.StatCmd($1)::$3 }
  ;

prog:
    LBRACK cmds RBRACK                  { $2 }
  ;
      
      
      
      
