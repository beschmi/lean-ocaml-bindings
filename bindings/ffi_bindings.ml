(* * Bindings for Lean *)
open Ctypes

(* ** Types module *)

module Types (F: Cstubs.Types.TYPE) = struct
  open F

  module Bool = struct
    let true_  = constant "lean_true" int
    let false_ = constant "lean_false" int
  end

  module Exception_kind = struct
    let null_exception    = constant "LEAN_NULL_EXCEPTION"    int
    let system_exception  = constant "LEAN_SYSTEM_EXCEPTION"  int
    let out_of_memory     = constant "LEAN_OUT_OF_MEMORY"     int
    let interrupted       = constant "LEAN_INTERRUPTED"       int
    let kernel_exception  = constant "LEAN_KERNEL_EXCEPTION"  int
    let unifier_exception = constant "LEAN_UNIFIER_EXCEPTION" int
    let tactic_exception  = constant "LEAN_TACTIC_EXCEPTION"  int
    let parser_exception  = constant "LEAN_PARSER_EXCEPTION"  int
    let other_exception   = constant "LEAN_OTHER_EXCEPTION"   int
  end

  module Univ_kind = struct
    let univ_zero   = constant "LEAN_UNIV_ZERO" int
    let univ_succ   = constant "LEAN_UNIV_SUCC" int
    let univ_max    = constant "LEAN_UNIV_MAX" int
    let univ_imax   = constant "LEAN_UNIV_IMAX" int
    let univ_param  = constant "LEAN_UNIV_PARAM" int
    let univ_global = constant "LEAN_UNIV_GLOBAL" int
    let univ_meta   = constant "LEAN_UNIV_META" int
  end

  module Expr_kind = struct
    let expr_var    = constant "LEAN_EXPR_VAR"    int
    let expr_sort   = constant "LEAN_EXPR_SORT"   int
    let expr_const  = constant "LEAN_EXPR_CONST"  int
    let expr_local  = constant "LEAN_EXPR_LOCAL"  int
    let expr_meta   = constant "LEAN_EXPR_META"   int
    let expr_app    = constant "LEAN_EXPR_APP"    int
    let expr_lambda = constant "LEAN_EXPR_LAMBDA" int
    let expr_pi     = constant "LEAN_EXPR_PI"     int
    let expr_let    = constant "LEAN_EXPR_LET"    int
    let expr_macro  = constant "LEAN_EXPR_MACRO"  int
  end

  module Binder_kind = struct
    let binder_default         = constant "LEAN_BINDER_DEFAULT"         int
    let binder_implicit        = constant "LEAN_BINDER_IMPLICIT"        int
    let binder_strict_implicit = constant "LEAN_BINDER_STRICT_IMPLICIT" int
    let binder_inst_implicit   = constant "LEAN_BINDER_INST_IMPLICIT"   int
  end
                         
  module Decl_kind = struct    
    let decl_const = constant "LEAN_DECL_CONST" int 
    let decl_axiom = constant "LEAN_DECL_AXIOM" int 
    let decl_def   = constant "LEAN_DECL_DEF"   int
    let decl_thm   = constant "LEAN_DECL_THM"   int
  end 

  let trust_high = constant "LEAN_TRUST_HIGH" int

end

(* ** Bindings module *)

module Bindings (F : Cstubs.FOREIGN) = struct
  open F

  let lean_bool = int

  let ret_bool = returning lean_bool

  let bool_allocate   () = allocate lean_bool 0
  let uint_allocate   () = allocate uint (Unsigned.UInt.of_int 0)
  let double_allocate () = allocate double 0.0
  let int_allocate    () = allocate int 0
 
(* *** Typedefs *)

  module Typedef (TN : sig val type_name : string end) : sig
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

  module Exception     = Typedef(struct let type_name = "lean_exception"           end)

  module Name          = Typedef(struct let type_name = "lean_name"                end)
  module List_name     = Typedef(struct let type_name = "lean_list_name"           end)

  module Univ          = Typedef(struct let type_name = "lean_univ"                end)
  module List_univ     = Typedef(struct let type_name = "lean_list_univ"           end)

  module Options       = Typedef(struct let type_name = "lean_options"             end)

  module Ios           = Typedef(struct let type_name = "lean_ios"                 end)

  module Env           = Typedef(struct let type_name = "lean_env"                 end)
  module Decl          = Typedef(struct let type_name = "lean_decl"                end)
  module Cert_decl     = Typedef(struct let type_name = "lean_cert_decl"           end)

  module Expr          = Typedef(struct let type_name = "lean_expr"                end)
  module List_expr     = Typedef(struct let type_name = "lean_list_expr"           end)
  module Macro_def     = Typedef(struct let type_name = "lean_macro_def"           end)

  module Ind_type      = Typedef(struct let type_name = "lean_inductive_type"      end)
  module List_ind_type = Typedef(struct let type_name = "lean_list_inductive_type" end)
  module Ind_decl      = Typedef(struct let type_name = "lean_inductive_decl"      end)

  module Type_checker  = Typedef(struct let type_name = "lean_type_checker"        end)
  module Cnstr_seq     = Typedef(struct let type_name = "lean_cnstr_seq"           end)


  let exc                    = Exception.t

  let name                   = Name.t
  let list_name              = List_name.t

  let univ                   = Univ.t
  let list_univ              = List_univ.t

  let options                = Options.t

  let ios                    = Ios.t

  let env                    = Env.t
  let decl                   = Decl.t
  let cert_decl              = Cert_decl.t

  let expr                   = Expr.t
  let list_expr              = List_expr.t
  let macro_def              = Macro_def.t

  let ind_type               = Ind_type.t
  let list_ind_type          = List_ind_type.t
  let ind_decl               = Ind_decl.t

  let type_checker           = Type_checker.t
  let cnstr_seq              = Cnstr_seq.t


  let exception_allocate     = Exception.allocate

  let name_allocate          = Name.allocate
  let list_name_allocate     = List_name.allocate

  let univ_allocate          = Univ.allocate
  let list_univ_allocate     = List_univ.allocate

  let options_allocate       = Options.allocate

  let ios_allocate           = Ios.allocate

  let env_allocate           = Env.allocate
  let decl_allocate          = Decl.allocate
  let cert_decl_allocate     = Cert_decl.allocate

  let expr_allocate          = Expr.allocate
  let list_expr_allocate     = List_expr.allocate
  let macro_def_allocate     = Macro_def.allocate

  let ind_type_allocate      = Ind_type.allocate
  let list_ind_type_allocate = List_ind_type.allocate
  let ind_decl_allocate      = Ind_decl.allocate

  let type_checker_allocate  = Type_checker.allocate
  let cnstr_seq_allocate     = Cnstr_seq.allocate

(* *** Lean strings (strings returned by lean) *)

  let lean_string = typedef (ptr char) "const char*"

  let string_del = foreign "lean_string_del" (lean_string @-> returning void)

  let string_allocate () = allocate lean_string (from_voidp char null)

(* *** Lean exceptions *)

  (* FIXME: use Types.TYPE.enum instead to deal with exception_kind<>int *)
  let exception_kind = int

  let exception_del = foreign
    "lean_exception_del" (exc @-> returning void)

  let exception_get_message = foreign
    "lean_exception_get_message" (exc @-> returning lean_string)

  let exception_get_detailed_message = foreign
    "lean_exception_get_detailed_message" (exc @-> returning lean_string)

  let exception_get_kind = foreign "lean_exception_get_kind" (exc @-> returning exception_kind)

(* *** Lean names *)

  let name_del = foreign "lean_name_del" (name @-> returning void)

  let name_mk_anonymous = foreign
    "lean_name_mk_anonymous" (ptr name @-> ptr exc @-> ret_bool)

  let name_mk_str = foreign
    "lean_name_mk_str" (name @-> string @-> ptr name @-> ptr exc @-> ret_bool)

  let name_mk_idx = foreign
    "lean_name_mk_idx" (name @-> uint @-> ptr name @-> ptr exc @-> ret_bool)

  let name_is_anonymous = foreign "lean_name_is_anonymous" (name @-> ret_bool)

  let name_is_str = foreign "lean_name_is_str" (name @-> ret_bool)

  let name_is_idx = foreign "lean_name_is_idx" (name @-> ret_bool)

  let name_eq = foreign "lean_name_eq" (name @-> name @-> ret_bool)

  let name_lt = foreign "lean_name_lt" (name @-> name @-> ret_bool)

  let name_quick_lt = foreign "lean_name_quick_lt" (name @-> name @-> ret_bool)

  let name_get_prefix = foreign
    "lean_name_get_prefix" (name @-> ptr name @-> ptr exc @-> ret_bool)

  let name_get_str = foreign
    "lean_name_get_str" (name @-> ptr lean_string @-> ptr exc @-> ret_bool)

  let name_get_idx = foreign
    "lean_name_get_idx" (name @-> ptr uint @-> ptr exc @-> ret_bool)

  let name_to_string = foreign
    "lean_name_to_string" (name @-> ptr lean_string @-> ptr exc @-> ret_bool)

(* *** Lean list name *)

  let list_name_mk_nil = foreign
    "lean_list_name_mk_nil" (ptr list_name @-> ptr exc @-> ret_bool)

  let list_name_mk_cons = foreign
    "lean_list_name_mk_cons" (name @-> list_name
      @-> ptr list_name @-> ptr exc @-> ret_bool)

  let list_name_del = foreign "lean_list_name_del" (list_name @-> returning void)

  let list_name_is_cons = foreign "lean_list_name_is_cons" (list_name @-> ret_bool)

  let list_name_eq = foreign "lean_list_name_eq" (list_name @-> list_name @-> ret_bool)

  let list_name_head = foreign
    "lean_list_name_head" (list_name @-> ptr name @-> ptr exc @-> ret_bool)

  let list_name_tail = foreign
    "lean_list_name_tail" (list_name @-> ptr list_name @-> ptr exc @-> ret_bool)

(* *** Lean options *)

  let options_mk_empty = foreign "lean_options_mk_empty" (ptr options @-> ptr exc @-> ret_bool)

  let options_del = foreign "lean_options_del" (options @-> returning void)

  let options_join = foreign
    "lean_options_join" (options @-> options @-> ptr options @-> ptr exc @-> ret_bool)

  let options_set_bool = foreign
    "lean_options_set_bool" (options @-> name @-> lean_bool @-> ptr options @-> ptr exc @-> ret_bool)

  let options_set_int = foreign
    "lean_options_set_int" (options @-> name @-> int @-> ptr options @-> ptr exc @-> ret_bool)

  let options_set_uint = foreign
    "lean_options_set_unsigned" (options @-> name @-> uint @-> ptr options @-> ptr exc @-> ret_bool)

  let options_set_double = foreign
    "lean_options_set_double" (options @-> name @-> double @-> ptr options @-> ptr exc @-> ret_bool)

  let options_set_string = foreign
    "lean_options_set_string" (options @-> name @-> string @-> ptr options @-> ptr exc @-> ret_bool)

  let options_get_bool = foreign
    "lean_options_get_bool" (options @-> name @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let options_get_int = foreign
    "lean_options_get_int" (options @-> name @-> ptr int @-> ptr exc @-> ret_bool)

  let options_get_uint = foreign
    "lean_options_get_unsigned" (options @-> name @-> ptr uint @-> ptr exc @-> ret_bool)

  let options_get_double = foreign
    "lean_options_get_double" (options @-> name @-> ptr double @-> ptr exc @-> ret_bool)

  let options_get_string = foreign
    "lean_options_get_string" (options @-> name @-> ptr lean_string @-> ptr exc @-> ret_bool)

  let options_eq = foreign "lean_options_eq" (options @-> options @-> ret_bool)

  let options_empty = foreign "lean_options_empty" (options @-> ret_bool)

  let options_contains = foreign "lean_options_contains" (options @-> name @-> ret_bool)

  let options_to_string = foreign
    "lean_options_to_string" (options @-> ptr lean_string @-> ptr exc @-> ret_bool)

(* *** Lean universe *)

  (* FIXME: use Types.TYPE.enum instead to deal with lean_univ_kind<>int *)
  let univ_kind = int

  let univ_mk_zero = foreign "lean_univ_mk_zero" (ptr univ @-> ptr exc @-> ret_bool)

  let univ_mk_succ = foreign "lean_univ_mk_succ" (univ @-> ptr univ @-> ptr exc @-> ret_bool)

  let univ_mk_max = foreign "lean_univ_mk_max" (univ @-> univ @-> ptr univ @-> ptr exc @-> ret_bool)

  let univ_mk_imax = foreign
    "lean_univ_mk_imax" (univ @-> univ @-> ptr univ @-> ptr exc @-> ret_bool)

  let univ_mk_param = foreign "lean_univ_mk_param" (name @-> ptr univ @-> ptr exc @-> ret_bool)

  let univ_mk_global = foreign "lean_univ_mk_global" (name @-> ptr univ @-> ptr exc @-> ret_bool)

  let univ_mk_meta = foreign "lean_univ_mk_meta" (name @-> ptr univ @-> ptr exc @-> ret_bool)

  let univ_to_string = foreign
    "lean_univ_to_string" (univ @-> ptr lean_string @-> ptr exc @-> ret_bool)

  let univ_to_string_using = foreign
    "lean_univ_to_string_using" (univ @-> options @-> ptr lean_string @-> ptr exc @-> ret_bool)

  let univ_del = foreign "lean_univ_del" (univ @-> returning void)

  let univ_kind = foreign "lean_univ_get_kind" (univ @-> returning univ_kind)

  let univ_eq = foreign "lean_univ_eq" (univ @-> univ @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let univ_lt = foreign "lean_univ_lt" (univ @-> univ @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let univ_quick_lt = foreign
    "lean_univ_quick_lt" (univ @-> univ @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let univ_geq = foreign "lean_univ_geq" (univ @-> univ @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let univ_get_pred = foreign "lean_univ_get_pred" (univ @-> ptr univ @-> ptr exc @-> ret_bool)

  let univ_get_max_lhs = foreign
    "lean_univ_get_max_lhs" (univ @-> ptr univ @-> ptr exc @-> ret_bool)

  let univ_get_max_rhs = foreign
    "lean_univ_get_max_rhs" (univ @-> ptr univ @-> ptr exc @-> ret_bool)

  let univ_get_name = foreign "lean_univ_get_name" (univ @-> ptr name @-> ptr exc @-> ret_bool)

  let univ_normalize = foreign "lean_univ_normalize" (univ @-> ptr univ @-> ptr exc @-> ret_bool)

(* *** Lean universe list *)

  let list_univ_mk_nil = foreign "lean_list_univ_mk_nil" (ptr list_univ @-> ptr exc @-> ret_bool)

  let list_univ_mk_cons = foreign
    "lean_list_univ_mk_cons" (univ @-> list_univ @-> ptr list_univ @-> ptr exc @-> ret_bool)

  let list_univ_del = foreign "lean_list_univ_del" (list_univ @-> returning void)

  let list_univ_is_cons = foreign "lean_list_univ_is_cons" (list_univ @-> ret_bool)

  let list_univ_eq = foreign
    "lean_list_univ_eq" (list_univ @-> list_univ @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let list_univ_head = foreign
    "lean_list_univ_head" (list_univ @-> ptr univ @-> ptr exc @-> ret_bool)

  let list_univ_tail = foreign
    "lean_list_univ_tail" (list_univ @-> ptr list_univ @-> ptr exc @-> ret_bool)

  let univ_instantiate = foreign
    "lean_univ_instantiate" (univ @-> list_name @-> list_univ @-> ptr univ @-> ptr exc @-> ret_bool)

(* *** Lean expression *)

  let expr_kind = int

  let binder_kind = typedef int "lean_binder_kind" (* FIXME: int does not seem to be right size *)

  let expr_mk_var = foreign "lean_expr_mk_var" (uint @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_mk_sort = foreign "lean_expr_mk_sort" (univ @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_mk_const = foreign
    "lean_expr_mk_const" (name @-> list_univ @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_mk_app = foreign
    "lean_expr_mk_app" (expr @-> expr @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_mk_lambda = foreign
    "lean_expr_mk_lambda" (name @-> expr @-> expr @-> binder_kind
      @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_mk_pi = foreign
    "lean_expr_mk_pi" (name @-> expr @-> expr @-> binder_kind @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_mk_macro = foreign
    "lean_expr_mk_macro" (macro_def @-> list_expr @-> ptr expr  @-> ptr exc @-> ret_bool)

  let expr_mk_local = foreign
    "lean_expr_mk_local" (name @-> expr @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_mk_local_ext = foreign
    "lean_expr_mk_local_ext" (name @-> name @-> expr @-> binder_kind
      @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_mk_metavar = foreign
    "lean_expr_mk_metavar" (name @-> expr @-> ptr expr @-> ptr exc @-> ret_bool)

  let macro_def_eq = foreign
    "lean_macro_def_eq" (macro_def @-> macro_def @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let macro_def_to_string = foreign
    "lean_macro_def_to_string" (macro_def @-> ptr lean_string @-> ptr exc @-> ret_bool)

  let expr_to_string = foreign
    "lean_expr_to_string" (expr @-> ptr lean_string @-> ptr exc @-> ret_bool)

  let expr_eq = foreign
    "lean_expr_eq" (expr @-> expr @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let expr_lt = foreign
    "lean_expr_lt"  (expr @-> expr @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let expr_quick_lt = foreign
    "lean_expr_quick_lt" (expr @-> expr @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let expr_get_var_idx = foreign
    "lean_expr_get_var_idx" (expr @-> ptr uint @-> ptr exc @-> ret_bool)

  let expr_get_sort_univ = foreign
    "lean_expr_get_sort_univ" (expr @-> ptr univ @-> ptr exc @-> ret_bool)

  let expr_get_const_name = foreign
    "lean_expr_get_const_name" (expr @-> ptr name @-> ptr exc @-> ret_bool)

  let expr_get_const_univs = foreign
    "lean_expr_get_const_univs" (expr @-> ptr list_univ @-> ptr exc @-> ret_bool)

  let expr_get_app_fun = foreign
    "lean_expr_get_app_fun" (expr @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_get_app_arg = foreign
    "lean_expr_get_app_arg" (expr @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_get_mlocal_name = foreign
    "lean_expr_get_mlocal_name" (expr @-> ptr name @-> ptr exc @-> ret_bool)

  let expr_get_mlocal_type = foreign
    "lean_expr_get_mlocal_type" (expr @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_get_local_pp_name = foreign
    "lean_expr_get_local_pp_name" (expr @-> ptr name @-> ptr exc @-> ret_bool)

  let expr_get_local_binder_kind = foreign
    "lean_expr_get_local_binder_kind" (expr @-> ptr binder_kind @-> ptr exc @-> ret_bool)

  let expr_get_binding_name = foreign
    "lean_expr_get_binding_name" (expr @-> ptr name @-> ptr exc @-> ret_bool)

  let expr_get_binding_domain = foreign
    "lean_expr_get_binding_domain" (expr @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_get_binding_body = foreign
    "lean_expr_get_binding_body" (expr @-> ptr expr @-> ptr exc @-> ret_bool)

  let expr_get_binding_binder_kind = foreign
    "lean_expr_get_binding_binder_kind" (expr @-> ptr binder_kind @-> ptr exc @-> ret_bool)

  let expr_get_macro_def = foreign
    "lean_expr_get_macro_def" (expr @-> ptr macro_def @-> ptr exc @-> ret_bool)

  let expr_get_macro_args = foreign
    "lean_expr_get_macro_args" (expr @-> ptr list_expr @-> ptr exc @-> ret_bool)


  let list_expr_mk_nil = foreign "lean_list_expr_mk_nil" (ptr list_expr @-> ptr exc @-> ret_bool)

  let list_expr_mk_cons = foreign
    "lean_list_expr_mk_cons" (expr @-> list_expr @-> ptr list_expr @-> ptr exc @-> ret_bool)

  let list_expr_is_cons = foreign "lean_list_expr_is_cons" (list_expr @-> ret_bool)


  let list_expr_eq = foreign
    "lean_list_expr_eq" (list_expr @-> list_expr @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let list_expr_head = foreign
    "lean_list_expr_head" (list_expr @-> ptr expr @-> ptr exc @-> ret_bool)

  let list_expr_tail = foreign
    "lean_list_expr_tail" (list_expr @-> ptr list_expr @-> ptr exc @-> ret_bool)

  let expr_get_kind = foreign "lean_expr_get_kind" (expr @-> returning expr_kind)

  let expr_del = foreign "lean_expr_del" (expr @-> returning void)

  let macro_def_del = foreign "lean_macro_def_del" (macro_def @-> returning void)

  let list_expr_del = foreign "lean_list_expr_del" (list_expr @-> returning void)

  let decl_del = foreign "lean_decl_del" (decl @-> returning void)

  let cert_decl_del = foreign "lean_cert_decl_del" (cert_decl @-> returning void) 

(* *** Lean environment *)

  let env_mk_std = foreign "lean_env_mk_std" (uint @-> ptr env @-> ptr exc @-> ret_bool)

  let env_mk_hott = foreign "lean_env_mk_hott" (uint @-> ptr env @-> ptr exc @-> ret_bool)

  let env_add_univ = foreign "lean_env_add_univ" (env @-> name @-> ptr env @-> ptr exc @-> ret_bool)

  let env_add = foreign "lean_env_add" (env @-> cert_decl @-> ptr env @-> ptr exc @-> ret_bool)

  let env_replace = foreign
    "lean_env_replace" (env @-> cert_decl @-> ptr env @-> ptr exc @-> ret_bool)

  let env_del = foreign "lean_env_del" (env @-> returning void)

  let env_trust_level = foreign "lean_env_trust_level" (env @-> returning uint)

  let env_proof_irrel = foreign "lean_env_proof_irrel" (env @-> ret_bool)

  let env_impredicative = foreign "lean_env_impredicative" (env @-> ret_bool)

  let env_contains_univ = foreign "lean_env_contains_univ" (env @-> name @-> ret_bool)

  let env_contains_decl = foreign "lean_env_contains_decl" (env @-> name @-> ret_bool)

  let env_get_decl = foreign
    "lean_env_get_decl" (env @-> name @-> ptr decl @-> ptr exc @-> ret_bool)

  let env_is_descendant = foreign "lean_env_is_descendant" (env @-> env @-> ret_bool)

  let env_forget = foreign "lean_env_forget" (env @-> ptr env @-> ptr exc @-> ret_bool)

  (* FIXME: deal with callbacks into ocaml
  let env_for_each_decl =
    foreign "lean_env_for_each_decl" (env @-> ptr void (* decl @-> returning void *) @->
      ptr exc @-> ret_bool)

  (* FIXME: deal with callbacks into ocaml *)
  let env_for_each_univ =
    foreign "lean_env_for_each_univ" (env @-> ptr void (* name @-> returning void *) @->
      ptr exc @-> ret_bool)
  *)

(* *** Lean IO state *)

  let ios_mk_std = foreign "lean_ios_mk_std" (options @-> ptr ios @-> ptr exc @-> ret_bool)

  let ios_mk_buffered = foreign
    "lean_ios_mk_buffered" (options @-> ptr ios @-> ptr exc @-> ret_bool)

  let ios_del = foreign "lean_ios_del" (ios @-> returning void)

  let ios_is_std = foreign "lean_ios_is_std" (ios @-> ret_bool)

  let ios_set_options = foreign "lean_ios_set_options" (ios @-> options @-> ptr exc @-> ret_bool)

  let ios_get_options = foreign
    "lean_ios_get_options" (ios @-> ptr options @-> ptr exc @-> ret_bool)

  let ios_get_regular = foreign
    "lean_ios_get_regular" (ios @-> ptr lean_string @-> ptr exc @-> ret_bool)

  let ios_get_diagnostic = foreign
    "lean_ios_get_diagnostic" (ios @-> ptr lean_string @-> ptr exc @-> ret_bool)

  let ios_reset_regular = foreign "lean_ios_reset_regular" (ios @-> ptr exc @-> ret_bool)

  let ios_reset_diagnostic = foreign "lean_ios_reset_diagnostic" (ios @-> ptr exc @-> ret_bool)

  let expr_to_pp_string = foreign
    "lean_expr_to_pp_string" (env @-> ios @-> expr @-> ptr lean_string @-> ptr exc @-> ret_bool)

  let exception_to_pp_string = foreign
    "lean_exception_to_pp_string" (env @-> ios @-> exc @-> ptr lean_string @-> ptr exc @-> ret_bool)

(* *** Lean inductive types *)

  let ind_type_del = foreign "lean_inductive_type_del" (ind_type @-> returning void)

  let ind_type_mk = foreign
    "lean_inductive_type_mk" (name @-> expr @-> list_expr
      @-> ptr ind_type @-> ptr exc @-> ret_bool)

  let get_recursor_name = foreign
    "lean_get_recursor_name" (name @-> ptr name @-> ptr exc @-> ret_bool)

  let ind_type_get_name = foreign
    "lean_inductive_type_get_name" (ind_type @-> ptr name @-> ptr exc @-> ret_bool)

  let ind_type_get_type = foreign
    "lean_inductive_type_get_type" (ind_type @-> ptr expr @-> ptr exc @-> ret_bool)

  let ind_type_get_constructors = foreign
    "lean_inductive_type_get_constructors" (ind_type
      @-> ptr list_expr @-> ptr exc @-> ret_bool)

(* *** Lean inductive type list *)

  let list_ind_type_mk_nil = foreign
    "lean_list_inductive_type_mk_nil" (ptr list_ind_type  @-> ptr exc @-> ret_bool)

  let list_ind_type_mk_cons = foreign
    "lean_list_inductive_type_mk_cons" (ind_type @-> list_ind_type
      @-> ptr list_ind_type  @-> ptr exc @-> ret_bool)

  let list_ind_type_is_cons = foreign
    "lean_list_inductive_type_is_cons" (list_ind_type @-> ret_bool)

  let list_ind_type_eq = foreign
    "lean_list_inductive_type_eq" (list_ind_type @-> list_ind_type
      @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let list_ind_type_head = foreign
    "lean_list_inductive_type_head" (list_ind_type
      @-> ptr ind_type @-> ptr exc @-> ret_bool)

  let list_ind_type_tail = foreign
    "lean_list_inductive_type_tail" (list_ind_type
      @-> ptr list_ind_type @-> ptr exc @-> ret_bool)

  let list_ind_type_del = foreign
    "lean_list_inductive_type_del" (list_ind_type @-> returning void)

(* *** Lean inductive declarations *)

  let ind_decl_mk = foreign
    "lean_inductive_decl_mk" (list_name @-> uint @-> list_ind_type
      @-> ptr ind_decl @-> ptr exc @-> ret_bool)

  let ind_decl_get_univ_params = foreign
    "lean_inductive_decl_get_univ_params" (ind_decl
      @-> ptr list_name @-> ptr exc @-> ret_bool)

  let ind_decl_get_num_params = foreign
    "lean_inductive_decl_get_num_params" (ind_decl @-> ptr uint @-> ptr exc @-> ret_bool)

  let ind_decl_get_types = foreign
    "lean_inductive_decl_get_types" (ind_decl
      @-> ptr list_ind_type @-> ptr exc @-> ret_bool)

  let env_add_ind = foreign
    "lean_env_add_inductive" (env @-> ind_decl @-> ptr env @-> ptr exc @-> ret_bool)

  let env_is_ind_type = foreign
    "lean_env_is_inductive_type" (env @-> name @-> ptr ind_decl @-> ptr exc @-> ret_bool)

  let env_is_constructor = foreign
    "lean_env_is_constructor" (env @-> name @-> ptr name @-> ptr exc @-> ret_bool)

  let env_is_recursor = foreign
    "lean_env_is_recursor" (env @-> name @-> ptr name @-> ptr exc @-> ret_bool)

  let env_get_ind_type_num_indices = foreign
    "lean_env_get_inductive_type_num_indices" (env @-> name @-> ptr uint @-> ptr exc @-> ret_bool)

  let env_get_ind_type_num_minor_premises = foreign
    "lean_env_get_inductive_type_num_minor_premises" (env @-> name
      @-> ptr uint @-> ptr exc @-> ret_bool)

  let env_get_ind_type_num_type_formers = foreign
    "lean_env_get_inductive_type_num_type_formers" (env @-> name
      @-> ptr uint @-> ptr exc @-> ret_bool)

  let env_get_ind_type_has_dep_elim = foreign
    "lean_env_get_inductive_type_has_dep_elim" (env @-> name
      @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let ind_decl_del = foreign "lean_inductive_decl_del" (ind_decl @-> returning void)

(* *** Lean modules *)

  let env_import = foreign
    "lean_env_import" (env @-> ios @-> list_name @-> ptr env @-> ptr exc @-> ret_bool)

  let env_export = foreign "lean_env_export" (env @-> string @-> ptr exc @-> ret_bool)

  let get_std_path = foreign "lean_get_std_path" (ptr lean_string @-> ptr exc @-> ret_bool)

  let get_hott_path = foreign "lean_get_hott_path" (ptr lean_string @-> ptr exc @-> ret_bool)

(* *** Lean parser *)

  let parse_file = foreign
    "lean_parse_file" (env @-> ios @-> string @-> ptr env @-> ptr ios @-> ptr exc @-> ret_bool)

  let parse_commands = foreign
    "lean_parse_commands" (env @-> ios @-> string @-> ptr env @-> ptr ios @-> ptr exc @-> ret_bool)

  let parse_expr = foreign
    "lean_parse_expr" (env @-> ios @-> string
      @-> ptr expr @-> ptr list_name @-> ptr exc @-> ret_bool)

(* *** Lean type checker *)

  let type_checker_mk = foreign
    "lean_type_checker_mk" (env @-> ptr type_checker @-> ptr exc @-> ret_bool)

  let type_checker_del = foreign "lean_type_checker_del" (type_checker @-> returning void)

  let cnstr_seq_del = foreign "lean_cnstr_seq_del" (cnstr_seq @-> returning void)

  let type_checker_infer = foreign
    "lean_type_checker_infer" (type_checker @-> expr
      @-> ptr expr @-> ptr cnstr_seq @-> ptr exc @-> ret_bool)

  let type_checker_check = foreign
    "lean_type_checker_check" (type_checker @-> expr
      @-> ptr expr @-> ptr cnstr_seq @-> ptr exc @-> ret_bool)

  let type_checker_whnf = foreign
    "lean_type_checker_whnf" (type_checker @-> expr
      @-> ptr expr @-> ptr cnstr_seq @-> ptr exc @-> ret_bool)

  let type_checker_is_def_eq = foreign
    "lean_type_checker_is_def_eq" (type_checker @-> expr @-> expr
      @-> ptr lean_bool @-> ptr cnstr_seq @-> ptr exc @-> ret_bool)


(* *** Lean declarations *)

  (* FIXME: use Types.TYPE.enum instead to deal with lean_decl_kind<>int *)
  let decl_kind = int

  let decl_mk_axiom = foreign
   "lean_decl_mk_axiom" (name @-> list_name @-> expr @-> ptr decl @-> ptr exc @-> ret_bool)

  let decl_mk_const = foreign
   "lean_decl_mk_const" (name @-> list_name @-> expr @-> ptr decl @-> ptr exc @-> ret_bool)

  let decl_mk_def = foreign
   "lean_decl_mk_def" (name @-> list_name @-> expr @-> expr @-> uint @-> lean_bool @->
     ptr decl @-> ptr exc @-> ret_bool)

  let decl_mk_def_with = foreign
   "lean_decl_mk_def_with" (env @-> name  @-> list_name @-> expr @-> expr @-> lean_bool @->
     ptr decl @-> ptr exc @-> ret_bool)

  let decl_mk_thm = foreign
   "lean_decl_mk_thm" (name @-> list_name @-> expr @-> expr @-> uint @->
     ptr decl @-> ptr exc @-> ret_bool)

  let decl_mk_thm_with = foreign
   "lean_decl_mk_thm_with" (env @-> name  @-> list_name @-> expr @-> expr @->
     ptr decl @-> ptr exc @-> ret_bool)

  let decl_get_kind = foreign
   "lean_decl_get_kind" (decl @-> returning decl_kind)

  let decl_get_name = foreign
   "lean_decl_get_name" (decl @->
     ptr name @-> ptr exc @-> ret_bool)

  let decl_get_univ_params = foreign
   "lean_decl_get_univ_params" (decl @-> ptr list_name @-> ptr exc @-> ret_bool)

  let decl_get_type = foreign
   "lean_decl_get_type" (decl @-> ptr expr @-> ptr exc @-> ret_bool)

  let decl_get_value = foreign
   "lean_decl_get_value" (decl @-> ptr expr @-> ptr exc @-> ret_bool)

  let decl_get_height = foreign
   "lean_decl_get_height" (decl @-> ptr uint @-> ptr exc @-> ret_bool)

  let decl_get_conv_opt = foreign
   "lean_decl_get_conv_opt" (decl @-> ptr lean_bool @-> ptr exc @-> ret_bool)

  let decl_check = foreign 
   "lean_decl_check" (env @-> decl @-> ptr cert_decl @-> ptr exc @-> ret_bool)  

end
                                         
