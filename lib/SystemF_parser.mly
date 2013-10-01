%{

open SystemF_sugar

%}

%token LPAREN RPAREN LANGLE RANGLE LLBRACKET RRBRACKET COLON DOT EQUALS SEMISEMI 
%token RARROW
%token FUN TYPFUN FORALL INT_TYPE TYPE EXP
%token EOF
%token<Identifier.t> ID
%token<int> INT

%start commands
%type <SystemF_sugar.cmd list> commands

%%

typ_list :
  | { [] }
  | atom_typ typ_list { $1 :: $2 }

exp_typ_list :
  | { [] }
  | atom exp_typ_list { (Left $1) :: $2 }
  | LANGLE typ RANGLE exp_typ_list { (Right $2) :: $4 }

id_tid_list :
  | { [] }
  | ID id_tid_list { (Left $1) :: $2 }
  | LANGLE ID RANGLE id_tid_list { (Right $2) :: $4 }

id_list :
  | { [] }
  | ID id_list { $1 :: $2 }

atom_typ :
  | LPAREN typ RPAREN { $2 }
  | INT_TYPE { TInt }
  | ID { TId $1 }
  | LLBRACKET ID typ_list RRBRACKET
    { TAbbrv ($2, $3) }
   
fn_typ :
  | atom_typ { $1 }
  | atom_typ RARROW fn_typ { TFun ($1, $3) }

typ :
  | FORALL ID DOT typ { TForall ($2, $4) }
  | fn_typ { $1 }

atom :
  | INT { Int (Pos.mk $startpos $endpos, $1) }
  | ID { Id (Pos.mk $startpos $endpos, $1) }
  | LPAREN exp RPAREN { $2 }
  | LLBRACKET ID exp_typ_list RRBRACKET 
    { Abbrv (Pos.mk $startpos $endpos, $2, $3) }
   
app :
  | atom { $1 }
  | app atom { App (Pos.mk $startpos $endpos, $1, $2) }
  | app LANGLE typ RANGLE { TypApp (Pos.mk $startpos $endpos, $1, $3) }

exp :
  | app { $1 }
  | FUN LPAREN ID COLON typ RPAREN RARROW exp
    { Fun (Pos.mk $startpos $endpos, $3, $5, $8) }
  | TYPFUN ID RARROW exp { TypFun (Pos.mk $startpos $endpos, $2, $4) }

command :
  | TYPE LLBRACKET ID id_list RRBRACKET EQUALS typ
    { NewAbbrv ($3, TypAbbrv ($4, $7)) }
  | EXP LLBRACKET ID id_tid_list RRBRACKET EQUALS exp
    { NewAbbrv ($3, ExpAbbrv ($4, $7)) }
  | exp
    { Exp $1 }

commands :
  | EOF { [] }
  | command EOF { [$1] }
  | command SEMISEMI commands { $1 :: $3 }


%%
