%{

open HOF_syntax

let rec unique_ids (ids : id list) : unit =
  match ids with
    | [] -> ()
    | x :: ids' -> 
      if List.mem x ids then raise (Lexparse_util.Error "duplicate identifier")
      else unique_ids ids'

%}

%token LPAREN RPAREN COMMA DOT EQUALS
%token PLUS MINUS STAR
%token LET IN LAMBDA IF0 THEN ELSE
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
  | ID { Id $1 }
  | LPAREN exp RPAREN { $2 }

mul :
  | atom { $1 }
  | mul STAR atom { Mul ($1, $3) }

add :
  | mul { $1 }
  | add MINUS mul { Sub ($1, $3) }
  | add PLUS mul { Add ($1, $3) }

exp :
  | add { $1 }
  | atom LPAREN args RPAREN { Apply ($1, $3) }
  | IF0 exp THEN exp ELSE exp { If0 ($2, $4, $6) }
  | LAMBDA LPAREN ids RPAREN DOT exp { unique_ids $3; Lambda ($3, $6) }
  | LET ID EQUALS exp IN exp { Let ($2, $4, $6) }

program :
  | exp EOF { $1 }

%%
