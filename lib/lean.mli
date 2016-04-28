(* * Lean (high-level) interface *)

(* ** Types *)

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
type inductive_type      = LeanInternal.ind_type
type list_inductive_type = LeanInternal.list_ind_type
type inductive_decl      = LeanInternal.ind_decl
type type_checker        = LeanInternal.type_checker
type cnstr_seq           = LeanInternal.cnstr_seq
type binder_kind         = LeanInternal.binder_kind
(* ** Name *)

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

(* ** Option *)           
(* ** Universes *)

module Univ : sig
  val zero : univ
  val one : univ
  val mk : int -> univ
end

(* ** Expression *)

module Expr : sig
  (*val mk_const     : string                                 -> expr
  val mk_var       : Unsigned.uint                          -> expr
  val mk_sort      : univ                                   -> expr

  val mk_app       : expr -> expr                           -> expr
  val mk_lambda    : name -> ty:expr -> expr -> binder_kind -> expr
  val mk_pi        : name -> ty:expr -> expr -> binder_kind -> expr
  val mk_macro     : macro_def -> list_expr                 -> expr
  val mk_local     : name -> expr                           -> expr
  val mk_local_ext : name -> name -> expr -> binder_kind    -> expr
  val mk_metavar   : name -> expr                           -> expr*)
  
  val mk_forall : string * expr -> ?binder_kind : binder_kind-> (expr -> expr) -> expr
  val ty_prop   : expr
  val ty_type   : expr
  val (|:)      : string -> expr -> string * expr
end
                
(* ** IO state *)

module Ios : sig
  val mk : ?options:options -> unit -> ios
end
               
(* ** Environment *)

module Env : sig
  val mk : ?filenames:list_name -> ios -> env
end
               
(* ** Inductive type *)
(* ** Inductive declaration *)
(* ** Module *)
(* ** Parser *)
(* ** Type checker *)
(* ** Declaration *)

module Decl : sig
  val to_string : ?pp: env * ios -> decl -> string
end
                
(* ** EnvParser *)

module type LeanFiles = sig
  val _olean : string list
  val _lean : string list
end

module GetExprParser (LF : LeanFiles) : sig
  type t = expr
  val to_string : t -> string
  val to_pp_string : t -> string
  val get_type : t -> t
  val get     : string -> t
  val get_with_univ_params : string -> t * list_name                            
  val as_1ary : t -> t -> t
  val as_2ary : t -> t -> t -> t
  val as_nary : t -> t list -> t
  val (<@) : t -> t -> t (* Syntactic sugar for "Lean argument feeding" *)

  (* Nats and Integers *)
  val lint_of_int    : int -> t
  val lnat_of_posint : int -> t
                       
  (* Proof obligation generation *)                  
  val add_proof_obligation :
    ?prefix:string -> ?name:string -> ?univ_params:list_name -> expr -> unit
  val proof_obligations_to_string : unit -> string
  val export_proof_obligations : ?univ_params:list_name -> string -> unit
end
                         
