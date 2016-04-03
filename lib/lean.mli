(* * Types *)

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

(* * Names *)

(* type name *)

(* type list_name *)

module Name : sig
  type view =
    | Anon
    | Str of string
    | Idx of string * int
 
  val eq : name -> name -> bool
  val pp : name -> string

  val view : name -> view
  val mk   : view -> name

  val view_list : list_name -> name list
  val mk_list   : name list -> list_name
end
(* * Options *)
(* * Universe *)
(* * List of universes *)
(* * Expression *)
(* * Environment *)
(* * IO state *)
(* * Inductive types *)
(* * Inductive type list *)
(* * Inductive declarations *)
(* * Modules *)
(* * Parser *)
(* * Type checker *)
