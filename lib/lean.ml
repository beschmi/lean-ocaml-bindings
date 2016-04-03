open Ctypes

module LI = LeanInternal


type name                = LeanInternal.name
type list_name           = LeanInternal.list_name
type options             = LeanInternal.options
type univ                = LeanInternal.univ
type list_univ           = LeanInternal.list_univ
type ios                 = LeanInternal.ios
type env                 = LeanInternal.env
type decl                = LeanInternal.decl
type cert_decl           = LeanInternal.cert_decl
type expr                = LeanInternal.expr
type list_expr           = LeanInternal.list_expr
type macro_def           = LeanInternal.macro_def
type inductive_type      = LeanInternal.inductive_type
type list_inductive_type = LeanInternal.list_inductive_type
type inductive_decl      = LeanInternal.inductive_decl
type type_checker        = LeanInternal.type_checker
type cnstr_seq           = LeanInternal.cnstr_seq


module Name = struct

  open LI.Name
  
  let eq = eq
  let pp = to_string

  type view =
    | Anon
    | Str of string
    | Idx of string * int

  let view n =
    if      is_str n then Str (get_str n)
    else if is_idx n then Idx (get_str (get_prefix n), get_idx n)
    else Anon

  let mk v =
    match v with
    | Anon     -> mk_anon ()
    | Str s    -> mk_str ~str:s @@ mk_anon ()
    | Idx(s,i) -> mk_idx ~idx:i @@ mk_str ~str:s @@ mk_anon ()
 
  open LI.ListName

  let view_list nl =
    let rec go acc nl =
      if is_cons nl then (
        let n  = head nl in
        let nl = tail nl in
        go (n::acc) nl
      ) else ( List.rev acc )
    in
    go [] nl

  let mk_list ns =
    List.fold_left (fun nl n -> mk_cons n nl) (mk_nil ()) (List.rev ns)

end

(* * Options *)

(* * Universes *)
(* * Expression *)
(* * Environment *)
(* * IO state *)
(* * Inductive types *)
(* * Inductive declarations *)
(* * Modules *)
(* * Parser *)
(* * Type checker *)
