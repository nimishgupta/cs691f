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

add :
  | atom { $1 }
  | add PLUS atom { Add ($1, $3) }

mul :
  | add { $1 }
  | mul STAR add { Mul ($1, $3) }

exp :
  | LET ID EQUALS exp IN exp { Let ($2, $4, $6) }
  | mul { $1 }

program :
  | exp EOF { $1 }

%%
