open Ffi_bindings

(* * Lean_exception *)

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
                       
(* * Lean_name *)

type name

(* ** creation and deletion *)

val name_mk_anon : unit -> name
val name_mk_str  : name -> str:string -> name
val name_mk_idx  : name -> idx:int -> name

(* ** indicator and comparison *)

val name_is_str  : name -> bool
val name_is_anon : name -> bool
val name_is_idx  : name -> bool

val name_eq       : name -> name -> bool
val name_lt       : name -> name -> bool
val name_quick_lt : name -> name -> bool

(* ** destruction *)

val name_get_idx : name -> int      
      
val name_get_str : name -> string
      
val name_to_string : name -> string
      
(* * Lean_list_name *)

type list_name

val list_name_mk_nil : unit -> list_name

val list_name_mk_cons :  name -> list_name -> list_name

(* ** Indicator, equality and destructor functions *)

val list_name_is_cons : list_name -> bool

val list_name_eq : list_name -> list_name -> bool

val list_name_head : list_name -> name

val list_name_tail : list_name -> list_name
