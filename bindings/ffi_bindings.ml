open Ctypes

(* * Types module *)

module Types (F: Cstubs.Types.TYPE) = struct
  open F

  module Lean_bool = struct
    let lean_true  = constant "lean_true" int
    let lean_false = constant "lean_false" int
  end

  module Lean_exception_kind = struct
    let lean_null_exception    = constant "LEAN_NULL_EXCEPTION"    int
    let lean_system_exception  = constant "LEAN_SYSTEM_EXCEPTION"  int
    let lean_out_of_memory     = constant "LEAN_OUT_OF_MEMORY"     int
    let lean_interrupted       = constant "LEAN_INTERRUPTED"       int
    let lean_kernel_exception  = constant "LEAN_KERNEL_EXCEPTION"  int
    let lean_unifier_exception = constant "LEAN_UNIFIER_EXCEPTION" int
    let lean_tactic_exception  = constant "LEAN_TACTIC_EXCEPTION"  int
    let lean_parser_exception  = constant "LEAN_PARSER_EXCEPTION"  int
    let lean_other_exception   = constant "LEAN_OTHER_EXCEPTION"   int
  end

  module Lean_univ_kind = struct
    let lean_univ_zero   = constant "LEAN_UNIV_ZERO" int
    let lean_univ_succ   = constant "LEAN_UNIV_SUCC" int
    let lean_univ_max    = constant "LEAN_UNIV_MAX" int
    let lean_univ_imax   = constant "LEAN_UNIV_IMAX" int
    let lean_univ_param  = constant "LEAN_UNIV_PARAM" int
    let lean_univ_global = constant "LEAN_UNIV_GLOBAL" int
    let lean_univ_meta   = constant "LEAN_UNIV_META" int
  end

end

(* * Bindings module *)

module Bindings (F : Cstubs.FOREIGN) = struct
  open F

(* ** Typedefs *)

  module Lean_Typedef (TN : sig val type_name : string end) : sig
    type t
    val t : t Ctypes.typ
    val allocate : ?finalise:(t -> unit) -> unit -> t ptr
  end = struct
    type t = unit ptr
    let t = typedef (ptr void) TN.type_name
    
    let allocate ?finalise () =
      let finalise = match finalise with
        | Some f -> Some (fun p -> f !@p)
        | None   -> None
      in
      allocate ?finalise t null
  end
  
  module Lean_exception = Lean_Typedef(struct let type_name = "lean_exception" end)
  module Lean_name      = Lean_Typedef(struct let type_name = "lean_name"      end)
  module Lean_list_name = Lean_Typedef(struct let type_name = "lean_list_name" end)
  module Lean_univ      = Lean_Typedef(struct let type_name = "lean_univ"      end)
  module Lean_list_univ = Lean_Typedef(struct let type_name = "lean_list_univ" end)
  module Lean_options   = Lean_Typedef(struct let type_name = "lean_options"   end)

  let lean_exception_allocate = Lean_exception.allocate
  let lean_name_allocate      = Lean_name.allocate
  let lean_name_list_allocate = Lean_list_name.allocate
  let lean_univ_allocate      = Lean_univ.allocate
  let lean_list_univ_allocate = Lean_list_univ.allocate
  let lean_options            = Lean_options.allocate

(* ** Lean strings (strings returned by lean) *)

  let lean_bool = int

  let lean_string = typedef (ptr char) "const char*"
  
  let lean_string_del = foreign "lean_string_del" (lean_string @-> returning void)

  let lean_string_allocate () = allocate lean_string (from_voidp char null)

(* ** Lean exceptions *)
                                         
  (* FIXME: use Types.TYPE.enum instead to deal with lean_exception_kind<>int *)  
  let lean_exception_kind = int

  let lean_exception = Lean_exception.t

  let lean_exception_del =
    foreign "lean_exception_del" (lean_exception @-> returning void)

  let lean_exception_get_message =
    foreign "lean_exception_get_message" (lean_exception @-> returning lean_string)

  let lean_exception_get_detailed_message =
    foreign "lean_exception_get_detailed_message" (lean_exception @-> returning lean_string)

  let lean_exception_get_kind =
    foreign "lean_exception_get_kind" (lean_exception @-> returning lean_exception_kind)

(* ** Lean names *)
                                     
  let lean_name = Lean_name.t

  let lean_name_del =
    foreign "lean_name_del" (lean_name @-> returning void)

  let lean_name_mk_anonymous =
    foreign "lean_name_mk_anonymous"
      (ptr lean_name @-> ptr lean_exception @-> returning lean_bool)

  let lean_name_mk_str =
    foreign "lean_name_mk_str"
      (lean_name @-> string @->
       ptr lean_name @-> ptr lean_exception @-> returning lean_bool)

  let lean_name_mk_idx =
    foreign "lean_name_mk_idx"
      (lean_name @-> uint @->
       ptr lean_name @-> ptr lean_exception @-> returning lean_bool)

  let lean_name_is_anonymous =
    foreign "lean_name_is_anonymous" (lean_name @-> returning lean_bool)

  let lean_name_is_str =
    foreign "lean_name_is_str" (lean_name @-> returning lean_bool)

  let lean_name_is_idx =
    foreign "lean_name_is_idx" (lean_name @-> returning lean_bool)

  let lean_name_eq =
    foreign "lean_name_eq" (lean_name @-> lean_name @-> returning lean_bool)

  let lean_name_lt =
    foreign "lean_name_lt" (lean_name @-> lean_name @-> returning lean_bool)

  let lean_name_quick_lt =
    foreign "lean_name_quick_lt" (lean_name @-> lean_name @-> returning lean_bool)

  let lean_name_get_prefix =
    foreign "lean_name_get_prefix"
      (lean_name @-> ptr lean_name @-> ptr lean_exception @-> returning lean_bool)

  let lean_name_get_str =
    foreign "lean_name_get_str"
      (lean_name @-> ptr lean_string @-> ptr lean_exception @-> returning lean_bool)

  let lean_name_get_idx =
    foreign "lean_name_get_idx"
      (lean_name @-> ptr uint @-> ptr lean_exception @-> returning lean_bool)

  let lean_name_to_string =
    foreign "lean_name_to_string"
      (lean_name @-> ptr lean_string @-> ptr lean_exception @-> returning lean_bool)

(* ** Lean list name *)
                                          
  let lean_list_name = Lean_list_name.t
  let lean_list_name_allocate = Lean_list_name.allocate

  let lean_list_name_mk_nil = 
    foreign "lean_list_name_mk_nil"
       (ptr lean_list_name @-> ptr lean_exception @-> returning lean_bool)

  let lean_list_name_mk_cons =
    foreign "lean_list_name_mk_cons"
      (lean_name @-> lean_list_name @->
       ptr lean_list_name @-> ptr lean_exception @-> returning lean_bool)

  let lean_list_name_del =
    foreign "lean_list_name_del" (lean_list_name @-> returning void)

  let lean_list_name_is_cons =
    foreign "lean_list_name_is_cons" (lean_list_name @-> returning lean_bool)

  let lean_list_name_eq =
    foreign "lean_list_name_eq" (lean_list_name @-> lean_list_name @-> returning lean_bool)

  let lean_list_name_head =
    foreign "lean_list_name_head"
      (lean_list_name @-> ptr lean_name @-> ptr lean_exception @-> returning lean_bool)

  let lean_list_name_tail =
    foreign "lean_list_name_tail"
      (lean_list_name @-> ptr lean_list_name @-> ptr lean_exception @-> returning lean_bool)

(* ** Lean options *)
  let lean_options = Lean_options.t

  let lean_options_mk_empty =
    foreign "lean_options_mk_empty" (ptr lean_options @-> ptr lean_exception @-> returning lean_bool)

  let lean_options_set_bool =
    foreign "lean_options_set_bool" (lean_options @-> lean_name @-> lean_bool
      @-> ptr lean_options @-> ptr lean_exception @-> returning lean_bool)

  let lean_options_set_int =
    foreign "lean_options_set_int" (lean_options @-> lean_name @-> int
      @-> ptr lean_options @-> ptr lean_exception @-> returning lean_bool)

  let lean_options_set_unsigned =
    foreign "lean_options_set_unsigned" (lean_options @-> lean_name @-> uint
      @-> ptr lean_options @-> ptr lean_exception @-> returning lean_bool)

  let lean_options_set_double =
    foreign "lean_options_set_double" (lean_options @-> lean_name @-> double
      @-> ptr lean_options @-> ptr lean_exception @-> returning lean_bool)

  let lean_options_set_string =
    foreign "lean_options_set_string" (lean_options @-> lean_name @-> string
      @-> ptr lean_options @-> ptr lean_exception @-> returning lean_bool)

  let lean_options_join =
    foreign "lean_options_join" (lean_options @-> lean_options
      @-> ptr lean_options @-> ptr lean_exception @-> returning lean_bool)

  let lean_options_del =
    foreign "lean_options_del" (lean_options @-> returning void)

  let lean_options_to_string =
    foreign "lean_options_to_string"
      (lean_options @-> ptr lean_string @-> ptr lean_exception @-> returning lean_bool)

  let lean_options_eq =
    foreign "lean_options_eq" (lean_options @-> lean_options @-> returning lean_bool)
  
  let lean_options_empty =
    foreign "lean_options_empty" (lean_options @-> returning lean_bool)

  let lean_options_contains =
    foreign "lean_options_contains" (lean_options @-> lean_name @-> returning lean_bool)
  
  let lean_options_get_bool =
    foreign "lean_options_get_bool" (lean_options @-> lean_name
      @-> ptr lean_bool @-> ptr lean_exception @-> returning lean_bool)

  let lean_options_get_int =
    foreign "lean_options_get_int" (lean_options @-> lean_name
      @-> ptr int @-> ptr lean_exception @-> returning lean_bool)

  let lean_options_get_unsigned =
    foreign "lean_options_get_unsigned" (lean_options @-> lean_name
      @-> ptr uint @-> ptr lean_exception @-> returning lean_bool)

  let lean_options_get_double =
    foreign "lean_options_get_double" (lean_options @-> lean_name
      @-> ptr double @-> ptr lean_exception @-> returning lean_bool)

  let lean_options_get_string =
    foreign "lean_options_get_string" (lean_options @-> lean_name
      @-> ptr lean_string @-> ptr lean_exception @-> returning lean_bool)

(* ** Lean universe *)
  
  (* FIXME: use Types.TYPE.enum instead to deal with lean_univ_kind<>int *)
  let lean_univ_kind = int

  let lean_univ = Lean_univ.t

  let lean_univ_allocate = Lean_univ.allocate

  let lean_univ_mk_zero =
    foreign "lean_univ_mk_zero" (ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_mk_succ =
    foreign "lean_univ_mk_succ"
      (lean_univ @-> ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_mk_max =
    foreign "lean_univ_mk_max"
      (lean_univ @-> lean_univ @-> ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_mk_imax =
    foreign "lean_univ_mk_imax"
      (lean_univ @-> lean_univ @-> ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)
  
  let lean_univ_mk_param =
    foreign "lean_univ_mk_param"
      (lean_name @-> ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_mk_global =
    foreign "lean_univ_mk_global"
      (lean_name @-> ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_mk_meta =
    foreign "lean_univ_mk_meta"
      (lean_name @-> ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_to_string =
    foreign "lean_univ_to_string"
      (lean_univ @-> ptr lean_string @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_to_string_using =
    foreign "lean_univ_to_string_using"
      (lean_univ @-> lean_options @-> ptr lean_string @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_del =
    foreign "lean_univ_del" (lean_univ @-> returning void)

  let lean_univ_kind =
    foreign "lean_univ_get_kind" (lean_univ @-> returning lean_univ_kind)

  let lean_univ_eq =
    foreign "lean_univ_eq"
      (lean_univ @-> lean_univ @-> ptr lean_bool @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_lt =
    foreign "lean_univ_lt"
      (lean_univ @-> lean_univ @-> ptr lean_bool @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_quick_lt =
    foreign "lean_univ_quick_lt"
      (lean_univ @-> lean_univ @-> ptr lean_bool @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_geq =
    foreign "lean_univ_geq"
      (lean_univ @-> lean_univ @-> ptr lean_bool @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_get_pred =
    foreign "lean_univ_get_pred"
      (lean_univ @-> ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_get_max_lhs =
    foreign "lean_univ_get_max_lhs"
      (lean_univ @-> ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_get_max_rhs =
    foreign "lean_univ_get_max_rhs"
      (lean_univ @-> ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_get_name =
    foreign "lean_univ_get_name"
      (lean_univ @-> ptr lean_name @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_normalize =
    foreign "lean_univ_normalize"
      (lean_univ @-> ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)

(* ** Lean universe list *)

  let lean_list_univ = Lean_list_univ.t

  let lean_list_univ_mk_nil =
    foreign "lean_list_univ_mk_nil"
      (ptr lean_list_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_list_univ_mk_cons =
    foreign "lean_list_univ_mk_cons" (lean_univ @-> lean_list_univ
      @-> ptr lean_list_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_list_univ_del =
    foreign "lean_list_univ_del" (lean_list_univ @-> returning void)

  let lean_list_univ_is_cons =
    foreign "lean_list_univ_is_cons" (lean_list_univ @-> returning lean_bool)
  
  let lean_list_univ_eq =
    foreign "lean_list_univ_eq" (lean_list_univ @-> lean_list_univ
      @-> ptr lean_bool @-> ptr lean_exception @-> returning lean_bool)

  let lean_list_univ_head =
    foreign "lean_list_univ_head"
      (lean_list_univ @-> ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_list_univ_tail =
    foreign "lean_list_univ_tail"
      (lean_list_univ @-> ptr lean_list_univ @-> ptr lean_exception @-> returning lean_bool)

  let lean_univ_instantiate =
    foreign "lean_univ_instantiate" (lean_univ @-> lean_list_name @-> lean_list_univ
      @-> ptr lean_univ @-> ptr lean_exception @-> returning lean_bool)

end
