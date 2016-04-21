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

  val view_list : list_name -> view list
  val mk_list   : view list -> list_name
end
(* * Options *)
(* * Universe *)
(* * List of universes *)
(* * Expression *)
(* * IO state *)
module Ios : sig
  val mk : ?options:options -> unit -> ios
end
(* * Environment *)
module Env : sig
  val mk : ?filenames:list_name -> ios -> env
end
(* * Inductive types *)
(* * Inductive type list *)
(* * Inductive declarations *)
(* * Modules *)
(* * Parser *)
(* * Type checker *)

(* * Declarations *)
module Decl : sig
  val to_string : decl -> string
end
                
(* * EnvParser *)
module type LeanFiles = sig
  val _olean : string list
  val _lean : string list
end

module GetExprParser (LF : LeanFiles) : sig
  type t = expr
  (*type _1ary = t -> t
  type _2ary = t -> t -> t
  type _nary = t list -> t*)

  val to_string : t -> string
  val to_pp_string : t -> string
                           
  val get     : string -> t
                            
  val as_1ary : t -> t -> t
  val as_2ary : t -> t -> t -> t
  val as_nary : t -> t list -> t

  val (<@) : string -> string -> t (* "Lean argument feeding" *)
end
