(* * Lean (high-level) interface *)

(* ** Types *)

type uint                = Unsigned.UInt.t
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

(* ** View Modules Signatures *)
module type BaseView = sig
  type t
  type view
  val eq : t -> t -> bool
  val str : t -> string
  val view : t -> view
  val mk   : view -> t
end         
module type BaseViewWithList =
  (sig
    include BaseView
    type list_t
    val view_list : list_t -> view list
    val list_mk   : view list -> list_t
  end)
    
(* ** Name *)    
type name_view =
  | NAnon
  | NStr of string
  | NIdx of string * int

module Name :
(sig
  include BaseViewWithList with
            type t = name and
            type list_t = list_name and
            type view = name_view
end) 
  
(* ** Option *)           
(* ** Universes *)
module Univ :
(sig
  include BaseViewWithList with
            type t = univ and
            type list_t = list_univ and
            type view = int
  val zero : t
  val one : t
end)

(* ** Expression *)
type expr_view =
    | ExprVar      of uint
    | ExprSort     of univ
    | ExprConst    of name * list_univ (* Univ.expr_view list *)
    | ExprApp      of expr_view * expr_view
    | ExprLambda   of name * expr * expr_view * binder_kind
    | ExprPi       of name * expr * expr_view * binder_kind
    | ExprMacro    of macro_def * (expr_view list)
    | ExprLocal    of name * expr_view                           
    | ExprLocalExt of name * name * expr_view * binder_kind    
    | ExprMetavar  of name * expr_view
    | ExprRaw      of expr

module Expr :
(sig
  include BaseViewWithList with
            type t = expr and
            type list_t = list_expr and
            type view = expr_view
  type ty = t
  val pp : ?envios : env * ios -> t -> string
  val mk_forall : string * expr -> ?binder_kind : binder_kind-> (expr -> view) -> view
  val ty_prop   : expr
  val ty_type   : expr
  val (|:)      : string -> expr -> string * expr
end)
  
                                               
(* ** IO state *)
module Ios :
(sig(*
  include BaseView*)
  val mk : ?options:options -> unit -> ios
end)
               
(* ** Environment *)
module Env :
(sig(*
  include BaseView*)
  val mk : ?filenames:list_name -> ios -> env
end)
               
(* ** Inductive type *)
(* ** Inductive declaration *)
(* ** Module *)
(* ** Parser *)
(* ** Type checker *)
(* ** Declaration *)
type decl_view =
  | DeclAxiom of Name.t * Expr.ty
  | DeclConst of Name.t * Expr.ty
  | DeclDef   of Name.t * Expr.ty * Expr.t
  | DeclThm   of Name.t * Expr.ty * Expr.t

module Decl :
(sig(*
  include BaseView with
            type t = decl and
            type view = decl_view*)
  val to_string : ?pp: env * ios -> decl -> string
end)
                
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
    ?prefix:string -> ?name:string -> ?univ_params:list_name -> t -> unit
  val proof_obligations_to_string : unit -> string
  val export_proof_obligations : ?univ_params:list_name -> string -> unit
end
                         
