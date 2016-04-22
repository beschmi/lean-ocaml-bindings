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
                
(* * Universes *)
module Univ : sig
  val zero : univ
  val one : univ
  val mk : int -> univ
end

(* * List of universes *)

(* * Expression *)
module Expr : sig
  val forall : string * expr -> (expr -> expr) -> expr
  val ty_prop   : expr
  val ty_type   : expr
  val (|:)      : string -> expr -> string * expr
end
                
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
  val to_string : ?pp: env * ios -> decl -> string
end
                
(* * EnvParser *)
module type LeanFiles = sig
  val _olean : string list
  val _lean : string list
end

module GetExprParser (LF : LeanFiles) : sig
  type t = expr
  val to_string : t -> string
  val to_pp_string : t -> string                           
  val get     : string -> t
  val get_with_univ_params : string -> t * list_name                            
  val as_1ary : t -> t -> t
  val as_2ary : t -> t -> t -> t
  val as_nary : t -> t list -> t
  val (<@) : string -> string -> t (* Syntactic sugar for "Lean argument feeding" *)

                                   
  val add_proof_obligation:
    ?prefix:string -> ?name:string -> ?univ_params:list_name -> expr -> unit
  val export_proof_obligations : string -> unit
end
                         
