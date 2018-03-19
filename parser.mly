%{
    open Ast
%}

%token <int> NUM
%token <string> IDENT
%token TRUE FALSE
%token IF
%token PLUS MINUS TIMES DIV
%token LPAR RPAR
%token EOL
%token BOOL INT 
%token STAR ARROW
%token COLON
%token COMMA
       
%start args

%type <Ast.expr> line
%type <Ast.apsType> simpleType
%type <Ast.args> args				       
%type <Ast.expr> expr
		   
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
  | LPAR expr exprs RPAR          { ASTExpressions($2, $3) }
  | LPAR PLUS expr expr RPAR      { ASTPrim(Ast.Add, $3, $4) }
  | LPAR MINUS expr expr RPAR     { ASTPrim(Ast.Sub, $3, $4) }
  | LPAR TIMES expr expr RPAR     { ASTPrim(Ast.Mul, $3, $4) }
  | LPAR DIV expr expr RPAR       { ASTPrim(Ast.Div, $3, $4) }

exprs:
    expr                          { ASTExpressions($1, ASTEmpty) } 
  | expr exprs                    { ASTExpressions($1, $2) }
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

args:
    IDENT COLON simpleType        { Ast.Args(Ast.Arg($1, $3), Ast.EmptyArg) }
  | IDENT COLON simpleType COMMA args { Ast.Args(Ast.Arg($1, $3), $5) } 
;     
      
