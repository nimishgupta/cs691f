%{

open Arith_syntax

%}

%token LPAREN RPAREN 
%token PLUS STAR EQUALS
%token LET IN
%token EOF
%token<string> ID
%token<int> INT

%start program
%type <Arith_syntax.exp> program

%%

atom :
  | INT { Int $1 }
  | ID { Id $1 }
  | LPAREN exp RPAREN { $2 }

mul :
  | atom { $1 }
  | mul STAR atom { Mul ($1, $3) }

add :
  | mul { $1 }
  | add PLUS mul { Add ($1, $3) }

exp :
  | LET ID EQUALS exp IN exp { Let ($2, $4, $6) }
  | add { $1 }

program :
  | exp EOF { $1 }

%%
