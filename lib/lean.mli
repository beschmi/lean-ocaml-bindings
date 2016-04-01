type exc =
  | Null_Exception
  | System_Exception
  | Out_Of_Memory
  | Interrupted
  | Kernel_Exception
  | Unifier_Exception
  | Tactic_Exception
  | Parser_Exception
  | Other_Exception
                                 
val conv_exc : int Ffi_generated_types.const -> exc

val raise_exception : ?del:bool -> Ffi_bindings.Bindings(Ffi_generated).Lean_exception.t -> 'a
val conv_bool : int Ffi_generated_types.const -> bool
                       
(* * Lean_name *)

(* ** creation and deletion *)
val name_mk_anonymous : unit -> Ffi_bindings.Bindings(Ffi_generated).Lean_name.t
  
val name_mk_str : Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> string -> Ffi_bindings.Bindings(Ffi_generated).Lean_name.t
  
val name_mk_str_of_ano : string -> Ffi_bindings.Bindings(Ffi_generated).Lean_name.t 

val name_mk_idx : Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> int -> Ffi_bindings.Bindings(Ffi_generated).Lean_name.t
  
val name_mk_idx_of_ano : int -> Ffi_bindings.Bindings(Ffi_generated).Lean_name.t

val name_del : Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> unit
  
(* ** indicator and comparison *)

val name_is_str : Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> bool
val name_is_anonymous : Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> bool
val name_is_idx       : Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> bool

val name_eq       : Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> bool
val name_lt       : Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> bool
val name_quick_lt : Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> bool

(* ** destruction *)

val name_get_idx : Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> int      
      
val name_get_str : Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> string
      
val name_to_string : Ffi_bindings.Bindings(Ffi_generated).Lean_name.t -> string
      
(* * Lean_list_name *)
