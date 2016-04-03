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

module ListName : sig
  val mk_nil  : unit -> list_name
  val mk_cons : name -> list_name -> list_name

  val is_cons : list_name -> bool
  val eq      : list_name -> list_name -> bool
  val head    : list_name -> name
  val tail    : list_name -> list_name
end

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

end

(* * List of universes *)

module ListUniv : sig

  val mk_nil : unit -> list_univ
  val mk_cons : univ -> list_univ -> list_univ
  
  val head : list_univ -> univ
  val tail : list_univ -> list_univ

  val is_cons : list_univ -> bool

  val eq : list_univ -> list_univ -> bool

  val instantiate : univ -> list_name -> list_univ -> univ

end

(* * Expression *)
(* * Environment *)
(* * IO state *)
(* * Inductive types *)
(* * Inductive type list *)
(* * Inductive declarations *)
(* * Modules *)
(* * Parser *)
(* * Type checker *)

module TypeChecker : sig
end
