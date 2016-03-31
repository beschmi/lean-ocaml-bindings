open Ctypes

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
end

module Bindings (F : Cstubs.FOREIGN) = struct
  open F
(* * Lean bool *)
  let lean_bool = int
(* * Lean string *)
  module Lean_string =
    struct
      (* FIXME : where are pointers here ? -> lean_string_del type *)
    let t : string option Ctypes.typ = string_opt
    type t = string option

    let const_t : t Ctypes.typ = typedef string_opt "const char*"
    (* Potential FIXME : remember we can set an actual ~finalise:(fun ...) arg *)
    let allocate fin () =
      allocate_n ~finalise:fin t ~count:(sizeof (ptr char))
                                          
  end

  let lean_string = Lean_string.t
  let lean_string_const = Lean_string.const_t
  let lean_string_del =
    foreign "lean_string_del" (lean_string @-> returning void)
  let lean_string_allocate = Lean_string.allocate (fun p -> ignore(!@ p)) (* FIXME ! *)

(* * Structures by name *)         
  module New_lean_typedef (Id : sig val id : string end) : sig
    type t
    val ct : t Ctypes.typ
    val allocate : unit -> t ptr                             
  end = struct
    type t = unit ptr
    let ct : t Ctypes.typ = typedef (ptr void) Id.id
                                    
    (* Potential FIXME : remember we can set an actual ~finalise:(fun ...) arg *)
    let allocate () = allocate_n ?finalise:None ct ~count:(sizeof ct)
  end
                                                            
(* * Lean exceptions *)
  module Lean_exception = New_lean_typedef(struct let id = "lean_exception" end)
                                         
  (* FIXME: use Types.TYPE.enum instead to deal with lean_exception_kind<>int *)  
  let lean_exception_kind = int
  let lean_exception = Lean_exception.ct
  let lean_exception_allocate = Lean_exception.allocate
  let lean_exception_del =
    foreign "lean_exception_del" (lean_exception @-> returning void)
  let lean_exception_get_message =
    foreign "lean_exception_get_message" (lean_exception @-> returning lean_string_const)
  let lean_exception_get_detailed_message =
    foreign "lean_exception_get_detailed_message" (lean_exception @-> returning lean_string_const)
  let lean_exception_get_kind =
    foreign "lean_exception_get_kind" (lean_exception @-> returning lean_exception_kind)

(* * Lean names *)
  module Lean_name = New_lean_typedef (struct let id = "lean_name" end)
                                     
  let lean_name = Lean_name.ct
  let lean_name_allocate = Lean_name.allocate

(* ** Creation and deletion *)
  let lean_name_del =
    foreign "lean_name_del" (lean_name @-> returning void)

  let lean_name_mk_anonymous =
    foreign "lean_name_mk_anonymous"
      (ptr lean_name @-> ptr lean_exception @-> returning lean_bool)

  let lean_name_mk_str =
    foreign "lean_name_mk_str"
      (lean_name @-> lean_string @->
       ptr lean_name @-> ptr lean_exception @-> returning lean_bool)

  let lean_name_mk_idx =
    foreign "lean_name_mk_idx"
      (lean_name @-> uint @->
       ptr lean_name @-> ptr lean_exception @-> returning lean_bool)

(* ** Indicator functions *)
  let lean_name_is_anonymous =
    foreign "lean_name_is_anonymous" (lean_name @-> returning lean_bool)

  let lean_name_is_str =
    foreign "lean_name_is_str" (lean_name @-> returning lean_bool)

  let lean_name_is_idx =
    foreign "lean_name_is_idx" (lean_name @-> returning lean_bool)

(* ** Equality and comparison *)
  let lean_name_eq =
    foreign "lean_name_eq" (lean_name @-> lean_name @-> returning lean_bool)

  let lean_name_lt =
    foreign "lean_name_lt" (lean_name @-> lean_name @-> returning lean_bool)

  let lean_name_quick_lt =
    foreign "lean_name_quick_lt" (lean_name @-> lean_name @-> returning lean_bool)

(* ** Destructors *)
  let lean_name_get_prefix =
    foreign "lean_name_get_prefix"
      (lean_name @-> ptr lean_name @-> ptr lean_exception @-> returning lean_bool)

  let lean_name_get_str =
    foreign "lean_name_get_str"
      (lean_name @-> ptr lean_string_const @-> ptr lean_exception @-> returning lean_bool)

  let lean_name_get_idx =
    foreign "lean_name_get_idx"
      (lean_name @-> ptr uint @-> ptr lean_exception @-> returning lean_bool)

  let lean_name_to_string =
    foreign "lean_name_to_string"
      (lean_name @-> ptr lean_string_const @-> ptr lean_exception @-> returning lean_bool)

(* * Lean list name *)
(* ** Module abstraction *)
  module Lean_list_name = New_lean_typedef(struct let id = "lean_list_name" end)
                                          
  let lean_list_name = Lean_list_name.ct
  let lean_list_name_allocate = Lean_list_name.allocate

(* ** Creation and deletion *)
  let lean_list_name_mk_nil = 
    foreign "lean_list_name_mk_nil"
       (ptr lean_list_name @-> ptr lean_exception @-> returning lean_bool)

  let lean_list_name_mk_cons =
    foreign "lean_list_name_mk_cons"
      (lean_name @-> lean_list_name @->
       ptr lean_list_name @-> ptr lean_exception @-> returning lean_bool)
  let lean_list_name_del =
    foreign "lean_list_name_del" (lean_list_name @-> returning void)

(* ** Indicator, equality and destructor functions *)
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

end


