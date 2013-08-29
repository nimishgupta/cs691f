%{

open HOF_sugar

let rec unique_ids (ids : id list) : unit =
  match ids with
    | [] -> ()
    | x :: ids' -> 
      if List.mem x ids' then raise (Lexparse_util.Error "duplicate identifier")
      else unique_ids ids'

%}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COLON COMMA DOT EQUALS
%token PLUS MINUS STAR RARROW
%token AMPAMP PIPEPIPE EQEQ COLONCOLON 
%token LET IN LAMBDA IF0 THEN ELSE IF TRUE FALSE HEAD TAIL EMPTY EMPTYQ 
%token EOF
%token<string> ID
%token<int> INT

%start program
%type <HOF_sugar.exp> program

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

fields1 :
  | ID COLON exp { [($1, $3)] }
  | ID COLON exp COMMA fields1 { ($1,$3) :: $5 }

fields :
  | { [] }
  | fields1 { $1 }

atom :
  | INT { Int $1 }
  | ID { Id $1 }
  | TRUE { True }
  | FALSE { False }
  | EMPTY { Empty }
  | LPAREN exp RPAREN { $2 }
  | atom DOT ID { GetField ($1, $3) }
  | atom LBRACK ID RARROW exp RBRACK { SetField ($1, $3, $5) }
  | atom LPAREN args RPAREN { Apply ($1, $3) }
  | HEAD LPAREN exp RPAREN { Head $3 }
  | TAIL LPAREN exp RPAREN { Tail $3 }
  | EMPTYQ LPAREN exp RPAREN { IsEmpty $3 }
  | LBRACE fields RBRACE { Record $2 }

list :
  | atom { $1 }
  | atom COLONCOLON list { Cons ($1, $3) }

mul :
  | list { $1 }
  | mul STAR list { Mul ($1, $3) }

add :
  | mul { $1 }
  | add MINUS mul { Sub ($1, $3) }
  | add PLUS mul { Add ($1, $3) }

cmp :
  | add { $1 }
  | add EQEQ add { IntEq ($1, $3) }

or_ :
  | cmp { $1 }
  | or_ PIPEPIPE cmp { Or ($1, $3) }

and_ :
  | or_ { $1 }
  | and_ AMPAMP or_ { And ($1, $3) }

exp :
  | and_ { $1 }
  | IF0 exp THEN exp ELSE exp { If0 ($2, $4, $6) }
  | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
  | LAMBDA LPAREN ids RPAREN DOT exp { unique_ids $3; Lambda ($3, $6) }
  | LET ID EQUALS exp IN exp { Let ($2, $4, $6) }

program :
  | exp EOF { $1 }

%%
