open Ffi_bindings

(* * Lean types *)

type name
type list_name
type options
type univ
type list_univ
type ios
type env
type decl
type cert_decl
type expr
type list_expr
type macro_def
type inductive_type
type list_inductive_type
type inductive_decl
type type_checker
type cnstr_seq

(* * Exception *)

type exc_kind =
  | Null_Exception
  | System_Exception
  | Out_Of_Memory
  | Interrupted
  | Kernel_Exception
  | Unifier_Exception
  | Tactic_Exception
  | Parser_Exception
  | Other_Exception

(* * Universe kinds *)

type univ_kind =
  | Univ_Zero
  | Univ_Succ
  | Univ_Max
  | Univ_Imax
  | Univ_Param
  | Univ_Global
  | Univ_Meta

(* * Expression kinds *)

type expr_kind =
  | Expr_var
  | Expr_sort
  | Expr_const
  | Expr_local
  | Expr_meta
  | Expr_app
  | Expr_lambda
  | Expr_pi
  | Expr_let
  | Expr_macro

(* * Binder kinds *)

type binder_kind =
  | Binder_default
  | Binder_implicit
  | Binder_strict_implicit
  | Binder_inst_implicit

(* * Declaration kinds *)

type decl_kind =
  | Decl_const
  | Decl_axiom
  | Decl_def
  | Decl_thm

(* General List module type signature *)
module type List = sig
  type t
  type list_t

  val to_list : list_t -> t list
  val of_list : t list -> list_t
  val ( @: ) : t -> list_t -> list_t
  val ( ! ) : t -> list_t
         
  val mk_nil  : unit -> list_t
  val mk_cons : t -> list_t -> list_t

  val is_cons : list_t -> bool
  val eq      : list_t -> list_t -> bool
  val head    : list_t -> t
  val tail    : list_t -> list_t
end
         
(* * Names *)

module Name : sig
  val mk_anon : unit -> name
  val mk_str  : name -> str:string -> name
  val mk_idx  : name -> idx:int -> name

  val eq       : name -> name -> bool
  val lt       : name -> name -> bool
  val quick_lt : name -> name -> bool

  val is_str  : name -> bool
  val is_anon : name -> bool
  val is_idx  : name -> bool

  val get_prefix : name -> name
  val get_idx    : name -> int
  val get_str    : name -> string
  val to_string  : name -> string
end

module ListName : List with
         type t = name and
         type list_t = list_name

(* * Options *)

module Options : sig

  val mk_empty : unit -> options

  val join : options -> options -> options

  val set_bool   : options -> name -> bool -> options
  val set_int    : options -> name -> int -> options
  val set_uint   : options -> name -> Unsigned.uint -> options
  val set_double : options -> name -> float -> options
  val set_string : options -> name -> string -> options

  val get_bool   : options -> name -> bool
  val get_int    : options -> name -> int
  val get_uint   : options -> name -> Unsigned.uint
  val get_double : options -> name -> float
  val get_string : options -> name -> string

  val eq : options -> options -> bool

  val empty    : options -> bool
  val contains : options -> name -> bool

  val to_string : options -> string

end

(* * Universe *)

module Univ : sig

  val mk_zero : unit -> univ
  val mk_succ : univ -> univ
  val mk_max  : univ -> univ -> univ
  val mk_imax : univ -> univ -> univ
  val mk_param  : name -> univ
  val mk_global : name -> univ
  val mk_meta   : name -> univ

  val get_pred : univ -> univ
  val get_max_lhs : univ -> univ
  val get_max_rhs : univ -> univ
  val normalize : univ -> univ

  val eq : univ -> univ -> bool
  val lt : univ -> univ -> bool
  val quick_lt : univ -> univ -> bool
  val geq : univ -> univ -> bool

  val to_string : univ -> string

  val to_string_using : univ -> options -> string

  val kind : univ -> univ_kind

  val get_name : univ -> name
                           
  val instantiate : univ -> list_name -> list_univ -> univ
end

(* * List of universes *)
module ListUniv : List with
         type t = univ and
         type list_t = list_univ
                         
(* * Expression *)

module Expr : sig
  val mk_var                  : Unsigned.uint                       -> expr
  val mk_sort                 : univ                                -> expr
  val mk_const                : name -> list_univ                   -> expr
  val mk_app                  : expr -> expr                        -> expr
  val mk_lambda               : name -> expr -> expr -> binder_kind -> expr
  val mk_pi                   : name -> expr -> expr -> binder_kind -> expr
  val mk_macro                : macro_def -> list_expr              -> expr
  val mk_local                : name -> expr                        -> expr
  val mk_local_ext            : name -> name -> expr -> binder_kind -> expr
  val mk_metavar              : name -> expr                        -> expr

  (*val macro_def_del         : macro_def -> unit*)

  val macro_def_eq            : macro_def -> macro_def -> bool
  val macro_def_to_string     : macro_def -> string

  val to_string               : expr -> string
  val get_kind                : expr -> expr_kind
                           
  val eq                      : expr -> expr -> bool
  val lt                      : expr -> expr -> bool
  val quick_lt                : expr -> expr -> bool

  val get_var_idx             : expr -> Unsigned.uint
  val get_sort_univ           : expr -> univ
  val get_const_name          : expr -> name
  val get_const_univ          : expr -> list_univ
  val get_app_fun             : expr -> expr
  val get_app_arg             : expr -> expr
  val get_mlocal_name         : expr -> name
  val get_mlocal_type         : expr -> expr
  val get_local_pp_name       : expr -> name
  val get_local_binder_kind   : expr -> binder_kind
  val get_binding_name        : expr -> name
  val get_binding_domain      : expr -> expr
  val get_binding_body        : expr -> expr
  val get_binding_binder_kind : expr -> binder_kind
  val get_macro_def           : expr -> macro_def
  val get_macro_args          : expr -> list_expr

  val to_pp_string : env -> ios -> expr -> string
end

module ListExpr : List with
         type t = expr and
         type list_t = list_expr
                       
(* * Environment *)
module Env : sig
  val mk_std         : Unsigned.uint -> env
  val mk_hott        : Unsigned.uint -> env

  val add_univ       : env -> name -> env
  val add            : env -> cert_decl -> env
  val replace        : env -> cert_decl -> env

  val trust_level    : env -> Unsigned.uint
  val proof_irrel    : env -> bool
  val impredicative  : env -> bool

  val contains_univ : env -> name -> bool
  val contains_decl  : env -> name -> bool

  val get_decl       : env -> name -> decl
  val is_descendant  : env -> env -> bool
  val forget         : env -> env

(* FIXME: deal with callbacks into ocaml
  val for_each_decl

  val for_each_univ 
 *)
  (* Inductives *)
  val add_inductive                         : env -> inductive_decl -> env
  val is_inductive_type                     : env -> name ->           inductive_decl
  val is_constructor                        : env -> name ->           name
  val is_recursor                           : env -> name ->           name

  val get_inductive_type_num_indices        : env -> name ->           Unsigned.uint
  val get_inductive_type_num_minor_premises : env -> name ->           Unsigned.uint
  val get_inductive_type_num_type_formers   : env -> name ->           Unsigned.uint
  val get_inductive_type_has_dep_elim       : env -> name ->           bool

  (* Modules *)
  val import : env -> ios -> list_name -> env
  val export : env -> olean_file:string -> unit
end
               
(* * IO state *)

module Ios : sig
  val mk_std           : options -> ios
  val mk_buffered      : options -> ios

  val is_std           : ios -> bool
  val set_options      : ios -> options -> unit
  val get_options      : ios -> options
  val get_regular      : ios -> string
  val get_diagnostic   : ios -> string
  val reset_regular    : ios -> unit
  val reset_diagnostic : ios -> unit

  (* FIXME : exception in input 
  val exception_to_pp_string : env -> ios -> exc -> string *)               
end
               
(* * Inductive types *)
module InductiveType : sig
  val mk                : name -> expr -> list_expr -> inductive_type
  val get_recursor_name : name -> name

  val get_name          : inductive_type -> name
  val get_type          : inductive_type -> expr
  val get_constructors  : inductive_type -> list_expr                                       
end
                         
(* * Inductive type list *)
module ListInductiveType : List with
         type t = inductive_type and
         type list_t = list_inductive_type
                             
(* * Inductive declarations *)
module InductiveDecl : sig
  val mk              : list_name -> Unsigned.uint -> list_inductive_type -> inductive_decl
                                                                  
  val get_univ_params : inductive_decl -> list_name
  val get_num_params  : inductive_decl -> Unsigned.uint 
  val get_types       : inductive_decl -> list_inductive_type
end
                         
(* * Modules *)
module Module : sig
  val get_std_path : unit -> string
  val get_hott_path : unit -> string
end
                  
(* * Parser *)
module Parse : sig
  val file : env -> ios -> string -> env * ios
  val commands : env -> ios -> string -> env * ios
  val expr : env -> ios -> string -> expr * list_name
end
                  
(* * Type checker *)
module TypeChecker : sig
  val mk : env -> type_checker

  val infer : type_checker -> expr -> (expr * cnstr_seq)
  val check : type_checker -> expr -> (expr * cnstr_seq)
  val whnf  : type_checker -> expr -> (expr * cnstr_seq)

  val is_def_eq : type_checker -> expr -> expr -> (bool * cnstr_seq)
end

(* * Declarations *)
module Decl : sig
  val mk_axiom    : name -> univ_params:list_name -> ty:expr -> decl
  val mk_const    : name -> univ_params:list_name -> ty:expr -> decl
  val mk_def      : name ->
                    univ_params:list_name -> ty:expr -> value:expr -> height:Unsigned.uint -> normalized:bool
                    -> decl
  val mk_def_with : env -> name ->
                    univ_params:list_name -> ty:expr -> value:expr -> normalized:bool
                    -> decl                         
  val mk_thm      : name ->
                    univ_params:list_name -> ty:expr -> value:expr -> height:Unsigned.uint
                    -> decl
  val mk_thm_with : env -> name ->
                    univ_params:list_name -> ty:expr -> value:expr
                    -> decl
  val get_kind        : decl -> decl_kind
  val get_name        : decl -> name
  val get_univ_params : decl -> list_name
  val get_type        : decl -> expr
  val get_value       : decl -> expr
  val get_height      : decl -> Unsigned.uint
  val get_conv_opt    : decl -> bool
  val check           : env -> decl -> cert_decl
end
                
                       
