%{

  open AST
  open Exp

%}

%token <int32> INTCONST
%token <float> FLOATCONST
%token <bool> BOOLCONST
%token <string> ID

%token DEF LET WHILE IF THEN ELSE REF INT FLOAT BOOL UNIT TT IN

%token NOT DEREF
%token PLUS MINUS TIMES DIV
%token AND OR LT INT_EQ DEFEQ

%token LPAREN RPAREN LBRACE RBRACE SEMI EQ COLON COMMA EOF 

(* AS NEEDED, ADD ASSOCIATIVITY/PRECEDENCE DIRECTIVES HERE, AS 
   DESCRIBED IN THE ASSIGNMENT TEXT ON THE COURSE WEBSITE *)              

%start <(AST.ty, unit Exp.exp) AST.prog> prog

%%

mytype:
| LPAREN ty = mytype RPAREN 
  { ty }
(* ADD PRODUCTIONS HERE FOR GRUMPY'S TYPES *)
  
unop:
| MINUS
  { UMinus }
| NOT
  { UNot }
| DEREF
  { UDeref }

%inline binop:
| PLUS
  { BPlus }
(* ADD PRODUCTIONS HERE FOR GRUMPY'S BOOLEAN OPERATORS *)    

id:
| i = ID
  { Id i }

exp_list:
| l = separated_list(COMMA, exp)
  { l }
  
exp:
(* ADD ONE PRODUCTION HERE FOR THE NONTERMINAL raw_exp *)  
| LPAREN e = exp RPAREN 
  { e }

raw_exp:
(* ADD PRODUCTIONS HERE FOR GRUMPY'S EXPRESSION CONSTRUCTORS *)  
| n = INTCONST
  { EInt n }

(* THE FOLLOWING NONTERMINALS/PRODUCTIONS ARE GIVEN FOR YOU: *)  
arg:
| arg = id COLON arg_ty = mytype
  { mk_tid arg arg_ty }			    
  
arg_list:
| l = separated_list(COMMA, arg)
  { l }		    
  
fundef:
| DEF nm = id LPAREN args = arg_list RPAREN COLON ret_ty = mytype LBRACE body = exp RBRACE
  { {nm; args; ret_ty; body} }
(* END GIVEN *)
  
(* FIX THE PROG RULE BELOW: *)

prog:
| _ = INTCONST
  { {fundefs = [];
     result = {start_of = $startpos;
     	       end_of = $endpos;
	       exp_of = EUnit;
	       ety_of = ()}}
   }


	

