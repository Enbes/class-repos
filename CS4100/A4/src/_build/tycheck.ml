(**
	Name: Corbin Dotson
	OUID: cd725014
*)

open Batteries
open BatFormat

open Lexing

open Lexer
open AST
open Exp

(** Declare a new exception type, Ty_error, which takes as 
    its first argument a message describing the type error *)
       
exception Ty_error of string 

(** Given an error string, raise the associated type error *)
			
let raise_ty_err (err : string) = raise (Ty_error err)

(** The type of type environments \Gamma, mapping identifiers to the
    types they've been assigned *)
					
type ty_env = ty Symtab.t

(** The type of the global environment \Delta, mapping function names 
    (identifiers) to the types of their arguments and to their 
    result type *)
		 
type globs_env = (ty list * ty) Symtab.t

(** delta is a file-scope reference to the global type environment *)
				
let delta : globs_env ref = ref (Symtab.create ())

(** Return the argument types and return type associated with 
    function id [f] in [delta], or raise a type error. *)
				
let ety_of_funid (f : id) : (ty list * ty) =
  match Symtab.get f !delta with
  | None ->
     raise_ty_err
       (pp_to_string
	  (fun ppf ->
	   fprintf ppf "unbound function identifier '%a'" pp_id f))
  | Some tys -> tys

(** Is id [x] bound in environment [gamma]? *)
				
let is_bound (gamma : ty_env) (x : id) : bool =
  BatOption.is_some (Symtab.get x gamma)

(** The tycheck function checks whether [e] is well-typed in 
    environment [gamma]. If so, the function returns the new 
    expression equal to [e] but annotated with its type (in 
    the [ety_of] field -- see type ['a exp] in [exp.mli] for 
    additional information). *)

let rec tycheck (gamma : ty_env) (e : 'a exp) : ty exp =
  match e.exp_of with
  | EInt i -> { e with exp_of = EInt i; ety_of = TyInt }
  | EFloat f -> { e with exp_of = EFloat f; ety_of = TyFloat }
  | EId x -> 
      (match Symtab.get x gamma with
      | None ->
	 raise_ty_err
	   (pp_to_string (fun ppf -> fprintf ppf "unbound identifier '%a'@ at position %a" 
					     pp_id x pp_pos e))
      | Some t -> { e with exp_of = EId x; ety_of = t })
  | ESeq l ->
      (* check e1 = TyUnit *)
      (match l with
      | [] -> raise_ty_err "ESeq: empty list"
      | e' :: l' ->
         let r1 = assert_ty gamma e' TyUnit in
            (* check that e2 exists *)
             match l' with
             | [] -> raise_ty_err "ESeq: more arguments expected"
             | e'' :: l'' ->
                let r2 = tycheck gamma e'' in
                let nl = [r1; r2] in
                (* tycheck all exps in l'', put them in nl *)
                let rec f r el nl =
                   match el with
                   (* if empty, then previous exp type is type of ESeq *)
                   | [] -> { e with exp_of = ESeq nl; ety_of = r.ety_of }
                   | ex :: li' -> 
                      (* if more in list, and this exp is not TyUnit, throw error, else append 
                         tychecked exp to new list and continue *)
                      if ty_eq r.ety_of TyUnit
                      then 
                         let nr = tycheck gamma ex in
                         f nr li' (List.append nl [nr])
                      else raise_ty_err "ESeq: expression is not of type TyUnit"
                in f r2 l'' nl)
  (**| ECall(x, l) -> *)
  | ERef e' -> 
      let r = tycheck gamma e' in
      { e with exp_of = ERef r; ety_of = TyRef r.ety_of }
  | EUnop(u, e') -> tycheck_unop e gamma u e'
  | EBinop(b, e1, e2) -> tycheck_binop e gamma b e1 e2
  | EIf(e1, e2, e3) -> 
      let r1 = assert_ty gamma e1 TyBool in
      let r2 = tycheck gamma e2 in
      let r3 = tycheck gamma e3 in
         if ty_eq r2.ety_of r3.ety_of
         then { e with exp_of = EIf(r1, r2, r3); ety_of = r2.ety_of }
         else raise_ty_err "EIf: expression types do not match"
  | ELet(x, e1, e2) -> 
      let r1 = tycheck gamma e1 in
      let gamma' = Symtab.set x r1.ety_of gamma in
      let r2 = tycheck gamma' e2 in
      { e with exp_of = ELet(x, r1, r2); ety_of = r2.ety_of }
  | EScope e' -> 
      let r = tycheck gamma e' in
      { e with exp_of = EScope r; ety_of = r.ety_of }
  | EUnit -> { e with exp_of = EUnit; ety_of = TyUnit }
  | ETrue -> { e with exp_of = ETrue; ety_of = TyBool }
  | EFalse -> { e with exp_of = EFalse; ety_of = TyBool }
  | EWhile(e1, e2) ->  
      let r1 = assert_ty gamma e1 TyBool in
      let r2 = tycheck gamma e2 in
      { e with exp_of = EWhile(r1, r2); ety_of = TyUnit }
  | _ -> raise_ty_err "Unimplemented"

(** [assert_ty gamma e t]: Raise a type error if [e] does not 
    have type [t] in [gamma]. Returns a type-annotated version 
    of [e] (just as in [tycheck]) *)
	  
and assert_ty (gamma : ty_env) (e : 'a exp) (t : ty) : ty exp =
  let r = tycheck gamma e in
     if ty_eq r.ety_of t
     then r
     else raise_ty_err "assert_ty: e does not have type t in gamma"

(** [assert_arith gamma e]: Raise a type error if [e] does not have an
    arithmetic type (see [exp.ml] and [exp.mli] for the definition of
    "arithmetic type". Returns a type-annotated version of [e]
    (just as in [tycheck]) *)
	    
and assert_arith (gamma : ty_env) (e : 'a exp) : ty exp =
  let r = tycheck gamma e in
     if is_arith_ty r.ety_of
     then r
     else raise_ty_err "assert_arith: e does not have an arithmetic type"

(** [tycheck_unop e gamma u e2]: 
    Assumes [e = EUnop(u, e2)]. 
    Checks that [EUnop(u, e2)] is well-typed in [gamma].
    Returns a type-annotated version of [e]. *)
		  
and tycheck_unop (e : 'a exp) (gamma : ty_env) (u : unop) (e2 : 'a exp) : ty exp =
  match u with
  | UNot -> 
     let r = assert_ty gamma e2 TyBool in
     { e with exp_of = EUnop(u, r); ety_of = TyBool }
  | UMinus -> 
     let r = assert_arith gamma e2 in
     { e with exp_of = EUnop(u, r); ety_of = r.ety_of }
  | UDeref -> 
     let r = tycheck gamma e2 in
        match r.ety_of with
        | TyRef t -> { e with exp_of = EUnop(u, r); ety_of = t }
        | _ -> raise_ty_err "tycheck_unop: UDeref expects an argument of type TyRef"

(** [tycheck_binop e gamma b e1 e2]: 
    Assumes [e = EBinop(b, e1, e2)]. 
    Checks that [EBinop(b, e1, e2)] is well-typed in [gamma].
    Returns a type-annotated version of [e]. *)
		  
and tycheck_binop (e : 'a exp) (gamma : ty_env)
                  (b : binop) (e1 : 'a exp) (e2 : 'a exp) : ty exp =
  match b with
  | BPlus ->
     let r1 = assert_arith gamma e1 in
     let r2 = assert_arith gamma e2 in
        if ty_eq r1.ety_of r2.ety_of
        then { e with exp_of = EBinop(b, r1, r2); ety_of = r1.ety_of }
        else raise_ty_err "tycheck_binop: BPlus expects both arguments to be of the same type"
  | BMinus ->
     let r1 = assert_arith gamma e1 in
     let r2 = assert_arith gamma e2 in
        if ty_eq r1.ety_of r2.ety_of
        then { e with exp_of = EBinop(b, r1, r2); ety_of = r1.ety_of }
        else raise_ty_err "tycheck_binop: BMinus expects both arguments to be of the same type"
  | BTimes ->
     let r1 = assert_arith gamma e1 in
     let r2 = assert_arith gamma e2 in
        if ty_eq r1.ety_of r2.ety_of
        then { e with exp_of = EBinop(b, r1, r2); ety_of = r1.ety_of }
        else raise_ty_err "tycheck_binop: BTimes expects both arguments to be of the same type"
  | BDiv ->
     let r1 = assert_arith gamma e1 in
     let r2 = assert_arith gamma e2 in
        if ty_eq r1.ety_of r2.ety_of
        then { e with exp_of = EBinop(b, r1, r2); ety_of = r1.ety_of }
        else raise_ty_err "tycheck_binop: BDiv expects both arguments to be of the same type"
  | BAnd ->
     let r1 = assert_ty gamma e1 TyBool in
     let r2 = assert_ty gamma e2 TyBool in
     { e with exp_of = EBinop(b, r1, r2); ety_of = TyBool }
  | BOr ->
     let r1 = assert_ty gamma e1 TyBool in
     let r2 = assert_ty gamma e2 TyBool in
     { e with exp_of = EBinop(b, r1, r2); ety_of = TyBool }
  | BLt ->
     let r1 = assert_arith gamma e1 in
     let r2 = assert_arith gamma e2 in
        if ty_eq r1.ety_of r2.ety_of
        then { e with exp_of = EBinop(b, r1, r2); ety_of = TyBool }
        else raise_ty_err "tycheck_binop: BLt expects both arguments to be of the same type"
  | BIntEq ->
     let r1 = assert_arith gamma e1 in
     let r2 = assert_arith gamma e2 in
        if ty_eq r1.ety_of r2.ety_of
        then { e with exp_of = EBinop(b, r1, r2); ety_of = TyBool }
        else raise_ty_err "tycheck_binop: BIntEq expects both arguments to be of the same type"
  | BUpdate ->
     let r1 = tycheck gamma e1 in
     let r2 = tycheck gamma e2 in
     (match r1.ety_of with
     | TyRef t ->
         if ty_eq r2.ety_of t
         then { e with exp_of = EBinop(b, r1, r2); ety_of = TyUnit }
         else raise_ty_err "tycheck_binop: BUpdate expects new value to be of same type as previous"
     | _ -> raise_ty_err "tycheck_binop: BUpdate expects the first argument to be of type TyRef")

(** [tycheck_fundef f]:
    Checks that function [f] is well-typed. 
    Returns a type-annotated version of [f]. *)
	 
let tycheck_fundef (f : (ty, 'a exp) fundef) : (ty, ty exp) fundef =
  raise_ty_err "Unimplemented"          

(** [tycheck_prog p]:
    Checks that program [p] is well-typed. 
    Returns a type-annotated version of [p]. *)
		  
let tycheck_prog (p : (ty, 'a exp) prog) : (ty, ty exp) prog =
  let exps = (tycheck (Symtab.create()) p.result) in
  {p with fundefs = []; result = exps}
  (*match p.fundefs with
  | [] -> {p with fundefs = []; result = exps}
  | f' :: l ->
     let funs = [tycheck_fundef f'] in
     let rec f fl =
        match fl with
        | [] -> fl
        | f'' :: l' -> f (List.append funs [tycheck_fundef f''])
     in let nfuns = f l in
     {p with fundefs = nfuns; result = exps}*)
