open Batteries
open BatFormat       

open Lexing       
open AST

type 'a raw_exp =
| EInt of int32
| EFloat of float
| EId of id
| ESeq of ('a exp) list
| ECall of id * ('a exp) list
| ERef of 'a exp
| EUnop of unop * 'a exp
| EBinop of binop * 'a exp * 'a exp
| EIf of 'a exp * 'a exp * 'a exp
| ELet of id * 'a exp * 'a exp
| EScope of 'a exp			   
| EUnit
| ETrue
| EFalse
| EWhile of 'a exp * 'a exp 
		    
and 'a exp = 
  { start_of : Lexing.position;
    end_of : Lexing.position;
    exp_of : 'a raw_exp;
    ety_of : 'a }

let rec ty_eq (t1 : ty) (t2 : ty) : bool =
  match t1, t2 with
  | TyInt, TyInt -> true
  | TyFloat, TyFloat -> true
  | TyBool, TyBool -> true
  | TyRef t1, TyRef t2 -> ty_eq t1 t2
  | TyUnit, TyUnit -> true
  | _, _ -> false

let exp_seq (e1 : 'a exp) (e2 : 'a exp) : 'a raw_exp =
  match e1.exp_of, e2.exp_of with
  | ESeq el1, ESeq el2 -> ESeq (el1 @ el2)
  | _, ESeq el2 -> ESeq (e1 :: el2)
  | ESeq el1, _ -> ESeq (el1 @ [e2])
  | _, _ -> ESeq (e1 :: e2 :: [])

let subst_var (new_id : id) (old_id : id) (e : 'a exp) : 'a exp =
  let rec go_raw e0 =
    match e0 with
    | EInt _ -> e0
    | EFloat _ -> e0
    | EId x -> if x = old_id then EId new_id else e0
    | ESeq el -> ESeq (BatList.map go el)
    | ECall (f, el) ->
       ECall (f, BatList.map go el)
    | ERef e1 -> ERef (go e1)
    | EUnop (u, e1) -> EUnop (u, go e1)
    | EBinop (b, e1, e2) -> EBinop (b, go e1, go e2)
    | EIf (e1, e2, e3) -> EIf (go e1, go e2, go e3)
    | ELet (x, e1, e2) ->
       let e1' = go e1 in
       let e2' = go e2 in
       if x = old_id then ELet (x, e1', e2 (* avoid capture *))
       else ELet (x, e1', e2')
    | EScope e1 -> EScope (go e1)
    | EUnit -> e0
    | ETrue -> e0
    | EFalse -> e0
    | EWhile (e1, e2) -> EWhile (go e1, go e2)
  and go e0 =
    { e0 with exp_of = go_raw e0.exp_of }
  in go e

let pp_tabbed ppf pp v =
  open_tbox ();
  print_tab ();
  pp ppf v;
  close_tbox ()

let rec pp_exp ppf (e : 'a exp) : unit = 
  fprintf ppf "%a" pp_raw_exp e.exp_of
	     
and pp_raw_exp ppf (e : 'a raw_exp) : unit =
  match e with
  | EInt n -> pp_int ppf (Int32.to_int n)
  | EFloat f -> pp_float ppf f
  | EId x -> fprintf ppf "%a" pp_id x 
  | ESeq el -> pp_list ppf pp_exp pp_semi el
  | ECall(x, el) -> fprintf ppf "%a(%a)" pp_id x
			    (fun ppf0 -> pp_list ppf0 pp_exp pp_comma) el
  | ERef(e2) -> fprintf ppf "(ref %a)" pp_exp e2
  | EUnop(u, e2) -> fprintf ppf "%a %a" pp_unop u pp_exp e2
  | EBinop(b, e1, e2) -> fprintf ppf "(%a %a %a)" pp_exp e1 pp_binop b pp_exp e2
  | EIf(e1, e2, e3) ->
     fprintf ppf "(if %a then@ " pp_exp e1; pp_tabbed ppf pp_exp e2;
     fprintf ppf "@ else@ "; pp_tabbed ppf pp_exp e3;
     fprintf ppf ")"
  | ELet(x, e1, e2) ->
     fprintf ppf "(let %a@ =@ %a in@ %a)" pp_id x pp_exp e1 pp_exp e2
  | EScope(e2) ->
     fprintf ppf "{@ %a@ }" pp_exp e2
  | EUnit -> fprintf ppf "tt"
  | ETrue -> fprintf ppf "true"
  | EFalse -> fprintf ppf "false"
  | EWhile(e1, e2) ->
     fprintf ppf "while %a do@ @[<1>%a@]"
	     pp_exp e1 pp_exp e2

let rec pp_texp ppf (e : ty exp) : unit = 
  fprintf ppf "(%a : %a)" pp_raw_texp e.exp_of pp_ty e.ety_of

and pp_raw_texp ppf (e : 'a raw_exp) : unit =
  match e with
  | EInt n -> pp_int ppf (Int32.to_int n)
  | EFloat f -> pp_float ppf f
  | EId x -> fprintf ppf "%a" pp_id x 
  | ESeq el -> pp_list ppf pp_texp pp_semi el
  | ECall(x, el) -> fprintf ppf "%a(%a)" pp_id x
			    (fun ppf0 -> pp_list ppf0 pp_texp pp_comma) el
  | ERef(e2) -> fprintf ppf "(ref %a)" pp_texp e2
  | EUnop(u, e2) -> fprintf ppf "%a %a" pp_unop u pp_texp e2
  | EBinop(b, e1, e2) -> fprintf ppf "(%a %a %a)" pp_texp e1 pp_binop b pp_texp e2
  | EIf(e1, e2, e3) ->
     fprintf ppf "(if %a then@ " pp_texp e1; pp_tabbed ppf pp_texp e2;
     fprintf ppf "@ else@ "; pp_tabbed ppf pp_texp e3;
     fprintf ppf ")"
  | ELet(x, e1, e2) ->
     fprintf ppf "(let %a@ =@ %a in@ %a)" pp_id x pp_texp e1 pp_texp e2
  | EScope(e2) ->
     fprintf ppf "{@ %a@ }" pp_texp e2
  | EUnit -> fprintf ppf "tt"
  | ETrue -> fprintf ppf "true"
  | EFalse -> fprintf ppf "false"
  | EWhile(e1, e2) ->
     fprintf ppf "while %a do@ @[<1>%a@]"
	     pp_texp e1 pp_texp e2

let pp_pos ppf (e : 'a exp) : unit =
  let start_pos = e.start_of in
  let end_pos = e.end_of in
  fprintf ppf "[%d:%d-%d:%d]"
	  start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
	  end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)	  
					
       
