%{

open Typed_syntax

let rec projn (e : exp) (n : int) : exp =
  if n < 1 then raise (Lexparse_util.Error "field index must be positive")
  else if n = 1 then ProjL e
  else if n = 2 then ProjR e
  else projn (ProjL e) (n - 1)


let rec unique_ids (ids : id list) : unit =
  match ids with
    | [] -> ()
    | x :: ids' -> 
      if List.mem x ids' then raise (Lexparse_util.Error "duplicate identifier")
      else unique_ids ids'

%}

%token LPAREN RPAREN LANGLE RANGLE COLON COMMA EQUALS DOT
%token PLUS MINUS STAR RARROW EMPTYQ
%token COLONCOLON
%token NUM BOOL LIST IF THEN ELSE LET IN EMPTY HEAD TAIL TRUE FALSE FUN FIX
%token EOF
%token<string> ID
%token<int> INT

%start program
%type <Typed_syntax.exp> program

%%

typs :
  | typ { $1 }
  | typs COMMA typ { TPair ($1, $3) }

list_typ :
  | NUM { TNum }
  | BOOL { TBool }
  | list_typ LIST { TList $1 }
  | LPAREN typs RPAREN { $2 }
   
fn_typ :
  | list_typ { $1 }
  | list_typ RARROW fn_typ { TFun ($1, $3) }

typ :
  | fn_typ { $1 }

exps :
  | exp { $1 }
  | exps COMMA exp { Pair ($1, $3) }

atom :
  | INT { Int $1 }
  | ID { Id $1 }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | EMPTY LANGLE typ RANGLE { Empty $3 }
  | LPAREN exps RPAREN { $2 }
  | atom DOT INT { projn $1 $3 }
   
app :
  | atom { $1 }
  | HEAD atom { Head $2 }
  | TAIL atom { Tail $2 }
  | EMPTYQ atom { IsEmpty $2 }
  | app atom { App ($1, $2) }

list_ :
  | app { $1 }
  | app COLONCOLON list_ { Cons ($1, $3) }

mul :
  | list_ { $1 }
  | mul STAR list_ { Arith (Times, $1, $3) }

add :
  | mul { $1 }
  | add MINUS mul { Arith (Minus, $1, $3) }
  | add PLUS mul { Arith (Plus, $1, $3) }

cmp :
  | add { $1 }
  | add EQUALS add { Cmp (EQ, $1, $3) }

or_ :
  | cmp { $1 }
  /* | or_ PIPEPIPE cmp { Or ($1, $3) } */

and_ :
  | or_ { $1 }
  /* | and_ AMPAMP or_ { And ($1, $3) } */

exp :
  | and_ { $1 }
  | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
  | LET ID EQUALS exp IN exp { Let ($2, $4, $6) }
  | FUN LPAREN ID COLON typ RPAREN RARROW exp { Fun ($3, $5, $8) }
  | FIX LPAREN ID COLON typ RPAREN RARROW exp { Fix ($3, $5, $8) }

program :
  | exp EOF { $1 }
%%
