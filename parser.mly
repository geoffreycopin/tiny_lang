%{
    open Ast
%}

%token <int> NUM
%token <string> IDENT
%token TRUE FALSE
%token IF
%token PLUS MINUS TIMES DIV
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
       
%start prog

%type <Ast.expr> line
%type <Ast.apsType> simpleType
%type <Ast.arg> arg
%type <Ast.args> args				       
%type <Ast.expr> expr
%type <Ast.dec> dec
%type <Ast.stat> stat
%type <Ast.cmds> cmds				   
%type <Ast.cmds> prog
		   
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
    simpleType                    { Ast.FuncType($1, Ast.Empty) }
  | simpleType STAR types         { Ast.FuncType($1, $3) }
;

arg:
    IDENT COLON simpleType        { Ast.Arg($1, $3) }
;

args:
    arg                           { Ast.Args($1, Ast.EmptyArg) }
 // arg EOL                       { Ast.args($1, Ast.emptyArg) } pour tester la règle
 // seule 
  | arg COMMA args                { Ast.Args($1, $3) } 
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
    stat                                 { Ast.StatCmd($1, Ast.EmptyCmd) }
  | dec SEMICOLON cmds                   { Ast.DecCmd ($1, $3) }
  | stat SEMICOLON cmds                  { Ast.StatCmd($1, $3) }
  ;

prog:
    LBRACK cmds RBRACK EOL                  { $2 }
  ;
      
      
      
      
