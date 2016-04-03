open Ffi_bindings

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

type name

type list_name

module NameInternal : sig
  val mk_anon : unit -> name
  val mk_str  : name -> str:string -> name
  val mk_idx  : name -> idx:int -> name

  val eq       : name -> name -> bool
  val lt       : name -> name -> bool
  val quick_lt : name -> name -> bool

  val is_str  : name -> bool
  val is_anon : name -> bool
  val is_idx  : name -> bool

  val get_idx   : name -> int            
  val get_str   : name -> string
  val to_string : name -> string
end

module ListNameInternal : sig
  val mk_nil  : unit -> list_name
  val mk_cons :  name -> list_name -> list_name

  val is_cons : list_name -> bool
  val eq      : list_name -> list_name -> bool
  val head    : list_name -> name
  val tail    : list_name -> list_name
end

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
