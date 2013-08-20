%{

open HOF_syntax

%}

%token LPAREN RPAREN COMMA DOT
%token PLUS MINUS EQUALSEQUALS LESSTHAN STAR
%token LAMBDA IF THEN ELSE TRUE FALSE
%token EOF
%token<string> ID
%token<int> INT

%start program
%type <HOF_syntax.exp> program

%%

args1 :
  | exp { [$1] }
  | exp COMMA args1 { $1 :: $3 }

args :
  | { [] }
  | args1 { $1 }

ids1 :
  | ID { [$1] }
  | ID COMMA ids1 { $1 :: $3 }

ids :
  | { [] }
  | ids1 { $1 }

atom :
  | INT { Int $1 }
  | TRUE { True }
  | FALSE { False }
  | ID { Id $1 }
  | LPAREN exp RPAREN { $2 }

add :
  | atom { $1 }
  | add MINUS atom { Op2 (SUB, $1, $3) }
  | add PLUS atom { Op2 (ADD, $1, $3) }

mul :
  | add { $1 }
  | mul STAR add { Op2 (MUL, $1, $3) }

eq : 
  | mul { $1 }
  | mul EQUALSEQUALS mul { Op2 (EQ, $1, $3) } 
  | mul LESSTHAN mul { Op2 (LT, $1, $3) }

exp :
  | eq { $1 }
  | atom LPAREN args RPAREN { Apply ($1, $3) }
  | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
  | LAMBDA LPAREN ids RPAREN DOT exp { Lambda ($3, $6) }

program :
  | exp EOF { $1 }

%%
