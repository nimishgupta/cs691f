%{

open SystemF_syntax

%}

%token LPAREN RPAREN LANGLE RANGLE COLON DOT EQUALS SEMISEMI
%token RARROW
%token FUN TYPFUN FORALL INT_TYPE
%token EOF
%token<Identifier.t> ID
%token<int> INT

%start program
%start prelude
%type <SystemF_syntax.exp> program
%type <(Identifier.t * SystemF_syntax.exp) list > prelude

%%

atom_typ :
  | LPAREN typ RPAREN { $2 }
  | INT_TYPE { TInt }
  | ID { TId $1 }
   
fn_typ :
  | atom_typ { $1 }
  | atom_typ RARROW fn_typ { TFun ($1, $3) }

typ :
  | FORALL ID DOT typ { TForall ($2, $4) }
  | fn_typ { $1 }

atom :
  | INT { Int ($startpos, $1) }
  | ID { Id ($startpos, $1) }
  | LPAREN exp RPAREN { $2 }
   
app :
  | atom { $1 }
  | app atom { App ($startpos, $1, $2) }
  | app LANGLE typ RANGLE { TypApp ($startpos, $1, $3) }

exp :
  | app { $1 }
  | FUN LPAREN ID COLON typ RPAREN RARROW exp { Fun ($startpos, $3, $5, $8) }
  | TYPFUN ID RARROW exp { TypFun ($startpos, $2, $4) }

program :
  | exp EOF { $1 }

defs :
  | { [] }
  | ID EQUALS exp SEMISEMI defs { ($1, $3) :: $5 }

prelude :
  | defs EOF { $1 } 
%%
