(* * Lean internals *)

open Ctypes
open LeanUtil

module B = Ffi_bindings.Bindings(Ffi_generated)
module LeanT = Ffi_bindings.Types(Ffi_generated_types)

(* ** Lean types *)

type name          = B.Name.t
type list_name     = B.List_name.t
type options       = B.Options.t
type univ          = B.Univ.t
type list_univ     = B.List_univ.t
type ios           = B.Ios.t
type env           = B.Env.t
type decl          = B.Decl.t
type cert_decl     = B.Cert_decl.t
type expr          = B.Expr.t
type list_expr     = B.List_expr.t
type macro_def     = B.Macro_def.t
type ind_type      = B.Ind_type.t
type list_ind_type = B.List_ind_type.t
type ind_decl      = B.Ind_decl.t
type type_checker  = B.Type_checker.t
type cnstr_seq     = B.Cnstr_seq.t
type lean_exc      = B.Exception.t

(* ** Strings returned by Lean *)

let to_string ls =
  match coerce B.lean_string string_opt ls with
  | Some s -> s
  | None   -> failwith "to_string: cannot convert NULL"

let deref_string_ptr ls_p =
  let ls = !@ls_p in
  let s = to_string ls in
  B.string_del ls;
  s

(* ** Conversion of constants *)
(* *** Booleans *)

let to_bool lb =
  LeanT.Bool.(
    if      lb = true_  then true
    else if lb = false_ then false
    else failwith "Unexpected Lean_bool : not lean_true nor lean_false")

let from_bool b =
  if b then LeanT.Bool.true_ else LeanT.Bool.false_

(* *** Unsigned ints *)

let uint_of_int = Unsigned.UInt.of_int
let uint_to_int = Unsigned.UInt.to_int

(* *** Exceptions *)

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

let to_exc_kind c =
  LeanT.Exception_kind.(
    if      c = null_exception    then Null_Exception
    else if c = system_exception  then System_Exception
    else if c = out_of_memory     then Out_Of_Memory
    else if c = interrupted       then Interrupted
    else if c = kernel_exception  then Kernel_Exception
    else if c = unifier_exception then Unifier_Exception
    else if c = tactic_exception  then Tactic_Exception
    else if c = parser_exception  then Parser_Exception
    else if c = other_exception   then Other_Exception
    else assert false)

exception Lean_exception of exc_kind * string

let raise_exception ex =
  let s = to_string (B.exception_get_detailed_message ex) in
  let exc = to_exc_kind (B.exception_get_kind ex) in
  raise (Lean_exception(exc,s))

(* *** Universe kinds *)

type univ_kind =
  | Univ_Zero
  | Univ_Succ
  | Univ_Max
  | Univ_Imax
  | Univ_Param
  | Univ_Global
  | Univ_Meta

let to_univ_kind c =
  LeanT.Univ_kind.(
    if      c = univ_zero   then Univ_Zero
    else if c = univ_succ   then Univ_Succ
    else if c = univ_max    then Univ_Max
    else if c = univ_imax   then Univ_Imax
    else if c = univ_param  then Univ_Param
    else if c = univ_global then Univ_Global
    else if c = univ_meta   then Univ_Meta
    else assert false
  )

(* *** Expression kinds *)

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

let to_expr_kind c =
  LeanT.Expr_kind.(
    if c = expr_var         then Expr_var
    else if c = expr_sort   then Expr_sort
    else if c = expr_const  then Expr_const
    else if c = expr_local  then Expr_local
    else if c = expr_meta   then Expr_meta
    else if c = expr_app    then Expr_app
    else if c = expr_lambda then Expr_lambda
    else if c = expr_pi     then Expr_pi
    else if c = expr_let    then Expr_let
    else if c = expr_macro  then Expr_macro
    else assert false
  )

(* *** Binder kinds *)

type binder_kind =
  | Binder_default
  | Binder_implicit
  | Binder_strict_implicit
  | Binder_inst_implicit

let to_binder_kind c =
  LeanT.Binder_kind.(
    if      c = binder_default         then Binder_default
    else if c = binder_implicit        then Binder_implicit
    else if c = binder_strict_implicit then Binder_strict_implicit
    else if c = binder_inst_implicit   then Binder_inst_implicit
    else assert false
  )

let from_binder_kind bk =
  LeanT.Binder_kind.(
    match bk with
    | Binder_default         -> binder_default
    | Binder_implicit        -> binder_implicit
    | Binder_strict_implicit -> binder_strict_implicit
    | Binder_inst_implicit   -> binder_inst_implicit
  )

(* *** Declaration kinds *)

type decl_kind =
  | Decl_const
  | Decl_axiom
  | Decl_def
  | Decl_thm

let to_decl_kind c =
  LeanT.Decl_kind.(
    if      c = decl_const then Decl_const
    else if c = decl_axiom then Decl_axiom
    else if c = decl_def   then Decl_def
    else if c = decl_thm   then Decl_thm
    else assert false
  )

(* ** Finalizers *)

let deref_ptr finaliser e_p =
  let e = !@e_p in
  Gc.finalise finaliser e;
  e

let deref_exception_ptr     = deref_ptr B.exception_del

let deref_name_ptr          = deref_ptr B.name_del

let deref_list_name_ptr     = deref_ptr B.list_name_del

let deref_options_ptr       = deref_ptr B.options_del

let deref_univ_ptr          = deref_ptr B.univ_del

let deref_list_univ_ptr     = deref_ptr B.list_univ_del

let deref_expr_ptr          = deref_ptr B.expr_del

let deref_list_expr_ptr     = deref_ptr B.list_expr_del

let deref_macro_def_ptr     = deref_ptr B.macro_def_del

let deref_env_ptr           = deref_ptr B.env_del

let deref_decl_ptr          = deref_ptr B.decl_del

let deref_cert_decl_ptr     = deref_ptr B.cert_decl_del

let deref_ios_ptr           = deref_ptr B.ios_del

let deref_ind_type_ptr      = deref_ptr B.ind_type_del

let deref_list_ind_type_ptr = deref_ptr B.list_ind_type_del

let deref_ind_decl_ptr      = deref_ptr B.ind_decl_del

let deref_type_checker_ptr  = deref_ptr B.type_checker_del

let deref_cnstr_seq_ptr     = deref_ptr B.cnstr_seq_del

(* ** with_* functions *)

let with_exn f g =
  let e_p = B.exception_allocate () in
  let lb = to_bool (f e_p) in
  if lb then g ()
  else raise_exception (deref_exception_ptr e_p)

let with_unit f  = with_exn f (fun () -> ())

let with_wrapper alloc deref (f : _ ptr -> _) =
  let n_p = alloc () in
  with_exn
    (fun e_p -> f n_p e_p)
    (fun () -> deref n_p)

let with_pair_wrapper alloc1 alloc2 deref1 deref2 (f : _ ptr -> _ ptr -> _) =
  let n_p1 = alloc1 () and n_p2 = alloc2 () in
  with_exn
    (fun e_p -> f n_p1 n_p2 e_p)
    (fun () -> (deref1 n_p1, deref2 n_p2))

let with_bool          = with_wrapper B.bool_allocate          (fun p -> to_bool (!@ p))
let with_string        = with_wrapper B.string_allocate        deref_string_ptr
let with_uint          = with_wrapper B.uint_allocate          (!@)
let with_int           = with_wrapper B.int_allocate           (!@)
let with_double        = with_wrapper B.double_allocate        (!@)
let with_name          = with_wrapper B.name_allocate          deref_name_ptr
let with_list_name     = with_wrapper B.list_name_allocate     deref_list_name_ptr
let with_options       = with_wrapper B.options_allocate       deref_options_ptr
let with_univ          = with_wrapper B.univ_allocate          deref_univ_ptr
let with_list_univ     = with_wrapper B.list_univ_allocate     deref_list_univ_ptr
let with_expr          = with_wrapper B.expr_allocate          deref_expr_ptr
let with_list_expr     = with_wrapper B.list_expr_allocate     deref_list_expr_ptr
let with_macro_def     = with_wrapper B.macro_def_allocate     deref_macro_def_ptr
let with_env           = with_wrapper B.env_allocate           deref_env_ptr
let with_decl          = with_wrapper B.decl_allocate          deref_decl_ptr
let with_cert_decl     = with_wrapper B.cert_decl_allocate     deref_cert_decl_ptr
let with_ios           = with_wrapper B.ios_allocate           deref_ios_ptr
let with_ind_type      = with_wrapper B.ind_type_allocate      deref_ind_type_ptr
let with_list_ind_type = with_wrapper B.list_ind_type_allocate deref_list_ind_type_ptr
let with_ind_decl      = with_wrapper B.ind_decl_allocate      deref_ind_decl_ptr
let with_type_checker  = with_wrapper B.type_checker_allocate  deref_type_checker_ptr

let with_env_and_ios = with_pair_wrapper
  B.env_allocate B.ios_allocate deref_env_ptr deref_ios_ptr

let with_expr_and_list_name = with_pair_wrapper
  B.expr_allocate B.list_name_allocate deref_expr_ptr deref_list_name_ptr

let with_expr_and_cnstr_seq = with_pair_wrapper
  B.expr_allocate B.cnstr_seq_allocate deref_expr_ptr deref_cnstr_seq_ptr

let with_bool_and_cnstr_seq = with_pair_wrapper
  B.bool_allocate B.cnstr_seq_allocate (fun p -> to_bool (!@ p)) deref_cnstr_seq_ptr

(* ** Modules for list types *)

module type ListBase = sig
  type t
  type elem_t
  
  val mk_nil  : unit -> t
  val mk_cons : elem_t -> t -> t
  val head    : t -> elem_t
  val tail    : t -> t
  val is_cons : t -> bool
  val eq      : t -> t -> bool
end

module type List = sig
  include ListBase
  val is_nil : t -> bool
                                 
  type view =
      | Nil
      | Cons of elem_t * t
  val view    : t -> view
  val to_list : t -> elem_t list
  val of_list : elem_t list -> t
end

module ListMake (LB : ListBase) = struct
  include LB
  
  let is_nil l = not (is_cons l)

  type view =
      | Nil
      | Cons of elem_t * t
  let view l =
    if is_cons l then Cons(head l, tail l)
    else Nil

  let to_list l =
    let rec go acc l =
      if is_cons l
      then go ((head l)::acc) (tail l)
      else List.rev acc
    in
    go [] l

  let of_list xs =
    let rec go acc xs =
      match xs with
      | x::xs -> go (mk_cons x acc) xs
      | []    -> acc
    in
    go (mk_nil ()) (List.rev xs)
end

(* ** Modules for pointer conversion *)

module type UnsafeVoidp = sig
  type t
  val unsafe_from_voidp : unit ptr -> t
  val unsafe_to_voidp   : t -> unit ptr
end

(* ** Name *)

module Name = struct
  include (B.Name : UnsafeVoidp with type t = name)
  let mk_anon () = with_name B.name_mk_anonymous
  let append_str n ~str = with_name (B.name_mk_str n str)
  let append_idx n ~idx = with_name (fun n_p e_p ->
    B.name_mk_idx n idx n_p e_p)

  let get_prefix n = with_name   (B.name_get_prefix n)
  let get_idx    n = with_uint   (B.name_get_idx n)
  let get_str    n = with_string (B.name_get_str n)
  let to_string  n = with_string (B.name_to_string n)

  let is_str   n     = to_bool (B.name_is_str n)
  let is_anon  n     = to_bool (B.name_is_anonymous n)
  let is_idx   n     = to_bool (B.name_is_idx n)
  let eq       n1 n2 = to_bool (B.name_eq n1 n2)
  let lt       n1 n2 = to_bool (B.name_lt n1 n2)
  let quick_lt n1 n2 = to_bool (B.name_quick_lt n1 n2)
end

module ListName = struct
  module Base = struct
    type elem_t = name
    type t = list_name

    let mk_nil  ()   = with_list_name B.list_name_mk_nil
    let mk_cons n ln = with_list_name (B.list_name_mk_cons n ln)

    let is_cons ln = to_bool (B.list_name_is_cons ln)
    let eq ln1 ln2 = to_bool (B.list_name_eq ln1 ln2)

    let head ln = with_name (B.list_name_head ln)
    let tail ln = with_list_name (B.list_name_tail ln)
  end
  include ListMake(Base)
  include (B.List_name : UnsafeVoidp with type t := t)
end

(* ** Option *)

module Options = struct
  let mk_empty () = with_options B.options_mk_empty

  let join o1 o2 = with_options (B.options_join o1 o2)

  let set_bool   o n b = with_options (fun op ep -> B.options_set_bool o n (from_bool b) op ep)
  let set_int    o n i = with_options (B.options_set_int o n i)
  let set_uint   o n i = with_options (B.options_set_uint o n i)
  let set_double o n d = with_options (B.options_set_double o n d)
  let set_string o n s = with_options (B.options_set_string o n s)

  let get_bool   o n = with_bool   (B.options_get_bool o n)
  let get_int    o n = with_int    (B.options_get_int o n)
  let get_uint   o n = with_uint   (B.options_get_uint o n)
  let get_double o n = with_double (B.options_get_double o n)
  let get_string o n = with_string (B.options_get_string o n)

  let eq o1 o2 = to_bool (B.options_eq o1 o2)

  let empty o = to_bool (B.options_empty o)

  let contains o n = to_bool (B.options_contains o n)

  let to_string o = with_string (B.options_to_string o)
end

(* ** Universe *)

module Univ = struct
  include (B.Univ : UnsafeVoidp with type t = univ)
  let mk_zero ()    = with_univ B.univ_mk_zero
  let mk_succ u     = with_univ (B.univ_mk_succ u)
  let mk_max  u1 u2 = with_univ (B.univ_mk_max u1 u2)
  let mk_imax u1 u2 = with_univ (B.univ_mk_imax u1 u2)
  let mk_param n    = with_univ (B.univ_mk_param n)
  let mk_global n   = with_univ (B.univ_mk_global n)
  let mk_meta n     = with_univ (B.univ_mk_meta n)

  let get_pred u    = with_univ (B.univ_get_pred u)
  let get_max_lhs u = with_univ (B.univ_get_max_lhs u)
  let get_max_rhs u = with_univ (B.univ_get_max_rhs u)
  let normalize u   = with_univ (B.univ_normalize u)

  let eq       u1 u2 = with_bool (B.univ_eq u1 u2)
  let lt       u1 u2 = with_bool (B.univ_lt u1 u2)
  let quick_lt u1 u2 = with_bool (B.univ_quick_lt u1 u2)
  let geq      u1 u2 = with_bool (B.univ_geq u1 u2)

  let to_string u = with_string (B.univ_to_string u)

  let to_string_using u o = with_string (B.univ_to_string_using u o)

  let kind u = to_univ_kind (B.univ_kind u)

  let get_name u = with_name (B.univ_get_name u)

  let instantiate u ln lu = with_univ (B.univ_instantiate u ln lu)
end

(* ** List of universes *)

module ListUniv = struct
  module Base = struct
    type elem_t = univ
    type t = list_univ

    let mk_nil ()    = with_list_univ B.list_univ_mk_nil
    let mk_cons u lu = with_list_univ (B.list_univ_mk_cons u lu)

    let head lu = with_univ (B.list_univ_head lu)
    let tail lu = with_list_univ (B.list_univ_tail lu)

    let is_cons lu = to_bool (B.list_univ_is_cons lu)

    let eq lu1 lu2 = with_bool (B.list_univ_eq lu1 lu2)
  end
  include ListMake(Base)
  include (B.List_univ : UnsafeVoidp with type t := t)
end

(* ** Expression *)

module Expr = struct
  include (B.Expr : UnsafeVoidp with type t = expr)
  let (!) = from_binder_kind
  let mk_var i                  = with_expr (B.expr_mk_var i)
  let mk_sort u                 = with_expr (B.expr_mk_sort u)
  let mk_const n lu             = with_expr (B.expr_mk_const n lu)
  let mk_app f a                = with_expr (B.expr_mk_app f a)
  let mk_lambda k n ~ty b       = with_expr (B.expr_mk_lambda n ty b !k)
  let mk_pi k n ~ty b           = with_expr (B.expr_mk_pi n ty b !k)
  let mk_macro m args           = with_expr (B.expr_mk_macro m args)
  let mk_local n t              = with_expr (B.expr_mk_local n t)
  let mk_local_ext n pp_n t k   = with_expr (B.expr_mk_local_ext n pp_n t !k)
  let mk_metavar n t            = with_expr (B.expr_mk_metavar n t)

  let macro_def_eq m1 m2        = with_bool (B.macro_def_eq m1 m2)
  let macro_def_to_string m     = with_string (B.macro_def_to_string m)

  let to_string e               = with_string (B.expr_to_string e)
  let get_kind e                = B.expr_get_kind e |> to_expr_kind

  let eq       u1 u2            = with_bool (B.expr_eq u1 u2)
  let lt       u1 u2            = with_bool (B.expr_lt u1 u2)
  let quick_lt u1 u2            = with_bool (B.expr_quick_lt u1 u2)

  let get_var_idx e             = with_uint (B.expr_get_var_idx e)
  let get_sort_univ e           = with_univ (B.expr_get_sort_univ e)
  let get_const_name e          = with_name (B.expr_get_const_name e)
  let get_const_univ e          = with_list_univ (B.expr_get_const_univs e)
  let get_app_fun e             = with_expr (B.expr_get_app_fun e)
  let get_app_arg e             = with_expr (B.expr_get_app_arg e)
  let get_mlocal_name e         = with_name (B.expr_get_mlocal_name e)
  let get_mlocal_type e         = with_expr (B.expr_get_mlocal_type e)
  let get_local_pp_name e       = with_name (B.expr_get_local_pp_name e)
  let get_local_binder_kind e   = with_int (B.expr_get_local_binder_kind e) |> to_binder_kind
  let get_binding_name e        = with_name (B.expr_get_binding_name e)
  let get_binding_domain e      = with_expr (B.expr_get_binding_domain e)
  let get_binding_body e        = with_expr (B.expr_get_binding_body e)
  let get_binding_binder_kind e = with_int (B.expr_get_binding_binder_kind e) |> to_binder_kind
  let get_macro_def e           = with_macro_def (B.expr_get_macro_def e)
  let get_macro_args e          = with_list_expr (B.expr_get_macro_args e)

  let to_pp_string env ios expr = with_string(B.expr_to_pp_string env ios expr)
end

module ListExpr = struct
  module Base = struct
    type elem_t = expr
    type t = list_expr

    let mk_nil ()    = with_list_expr B.list_expr_mk_nil
    let mk_cons u lu = with_list_expr (B.list_expr_mk_cons u lu)

    let head lu = with_expr (B.list_expr_head lu)
    let tail lu = with_list_expr (B.list_expr_tail lu)

    let is_cons lu = B.list_expr_is_cons lu |> to_bool

    let eq lu1 lu2 = with_bool (B.list_expr_eq lu1 lu2)
  end
  include ListMake(Base)
  include (B.List_expr : UnsafeVoidp with type t := t)
end

(* ** Environment *)

module Env = struct
  include (B.Env : UnsafeVoidp with type t = env)

  let mk_std i = with_env (B.env_mk_std i)
  let mk_hott i = with_env (B.env_mk_hott i)

  let trust_high = uint_of_int LeanT.trust_high

  let add_univ e n = with_env (B.env_add_univ e n)
  let add e d = with_env (B.env_add e d)
  let replace e d = with_env (B.env_replace e d)

  let trust_level e = B.env_trust_level e
  let proof_irrel e = B.env_proof_irrel e |> to_bool
  let impredicative e = B.env_impredicative e |> to_bool

  let contains_univ e n = B.env_contains_univ e n |> to_bool
  let contains_decl e n = B.env_contains_decl e n |> to_bool

  let get_decl e n = with_decl (B.env_get_decl e n)

  let is_descendant e1 e2 = B.env_is_descendant e1 e2 |> to_bool

  let forget e = with_env (B.env_forget e)

  (* Inductives *)

  let add_ind env ind_decl = with_env (B.env_add_ind env ind_decl)

  let is_ind_type    env name = with_ind_decl (B.env_is_ind_type env name)
  let is_constructor env name = with_name     (B.env_is_constructor env name)
  let is_recursor    env name = with_name     (B.env_is_recursor env name)

  let get_ind_type_num_indices env name = with_uint (B.env_get_ind_type_num_indices env name)
  let get_ind_type_num_minor_premises env name = with_uint
    (B.env_get_ind_type_num_minor_premises env name)
  let get_ind_type_num_type_formers env name = with_uint
    (B.env_get_ind_type_num_type_formers env name)
  let get_ind_type_has_dep_elim env name = with_bool (B.env_get_ind_type_has_dep_elim env name)

  (* Modules *)
  let import env ios modules = with_env (B.env_import env ios modules)
  let export env ~olean_file = with_unit (B.env_export env olean_file)
end

(* ** IO state *)

module Ios = struct
  let mk_std      options = with_ios (B.ios_mk_std options)
  let mk_buffered options = with_ios (B.ios_mk_buffered options)

  let set_options ios options = with_unit (B.ios_set_options ios options)

  let is_std           ios = B.ios_is_std ios |> to_bool
  let get_options      ios = with_options (B.ios_get_options ios)
  let get_regular      ios = with_string (B.ios_get_regular ios)
  let get_diagnostic   ios = with_string (B.ios_get_diagnostic ios)
  let reset_regular    ios = with_unit (B.ios_reset_regular ios)
  let reset_diagnostic ios = with_unit (B.ios_reset_diagnostic ios)

(* FIXME : exception in input
  val exception_to_pp_string : env -> ios -> exc -> string *)
end

(* ** Inductive types *)

module IndType = struct
 let mk n e le = with_ind_type (B.ind_type_mk n e le)
 let get_recursor_name n = with_name (B.get_recursor_name n)

 let get_name         it = with_name      (B.ind_type_get_name it)
 let get_type         it = with_expr      (B.ind_type_get_type it)
 let get_constructors it = with_list_expr (B.ind_type_get_constructors it)
end

(* ** Inductive type list *)

module ListIndType = struct
  module Base = struct
    type elem_t = ind_type
    type t = list_ind_type
 
    let mk_nil  ()     = with_list_ind_type (B.list_ind_type_mk_nil)
    let mk_cons it lit = with_list_ind_type (B.list_ind_type_mk_cons it lit)
    let is_cons lit    = B.list_ind_type_is_cons lit |> to_bool
    let eq  lit1 lit2  = with_bool (B.list_ind_type_eq lit1 lit2)
    let head lit       = with_ind_type      (B.list_ind_type_head lit)
    let tail lit       = with_list_ind_type (B.list_ind_type_tail lit)
  end
  include ListMake(Base)
  include (B.List_ind_type : UnsafeVoidp with type t := t)
end

(* ** Inductive declarations *)

module IndDecl = struct
 let mk ns i list_itypes = with_ind_decl (B.ind_decl_mk ns i list_itypes)

 let get_univ_params idecl = with_list_name     (B.ind_decl_get_univ_params idecl)
 let get_num_params  idecl = with_uint          (B.ind_decl_get_num_params idecl)
 let get_types       idecl = with_list_ind_type (B.ind_decl_get_types idecl)
end

(* ** Modules *)

module Module = struct
  let get_std_path  () = with_string (B.get_std_path)
  let get_hott_path () = with_string (B.get_hott_path)
end

(* ** Parser *)

module Parse = struct
  let file     env ios fname = with_env_and_ios        (B.parse_file env ios fname)
  let commands env ios str   = with_env_and_ios        (B.parse_commands env ios str)
  let expr     env ios str   = with_expr_and_list_name (B.parse_expr env ios str)
end

(* ** Type checker *)

module TypeChecker = struct
  let mk env                  = with_type_checker       (B.type_checker_mk env)
  let infer ty_chkr expr      = with_expr_and_cnstr_seq (B.type_checker_infer ty_chkr expr)
  let check ty_chkr expr      = with_expr_and_cnstr_seq (B.type_checker_check ty_chkr expr)
  let whnf  ty_chkr expr      = with_expr_and_cnstr_seq (B.type_checker_whnf  ty_chkr expr)
  let is_def_eq ty_chkr e1 e2 = with_bool_and_cnstr_seq (B.type_checker_is_def_eq ty_chkr e1 e2)
end

(* ** Declarations *)

module Decl = struct
  include (B.Decl : UnsafeVoidp with type t = decl)
            
  let (!) = from_bool

  let mk_axiom n ~univ_params ~ty = with_decl (B.decl_mk_axiom n univ_params ty)
  let mk_const n ~univ_params ~ty = with_decl (B.decl_mk_const n univ_params ty)

  let mk_def n ~univ_params ~ty ~value ~height ~conv_opt =
    with_decl (B.decl_mk_def n univ_params ty value height !conv_opt)

  let mk_def_with env n ~univ_params ~ty ~value ~conv_opt =
    with_decl (B.decl_mk_def_with env n univ_params ty value !conv_opt)

  let mk_thm n ~univ_params ~ty ~value ~height =
    with_decl (B.decl_mk_thm n univ_params ty value height)

  let mk_thm_with env n ~univ_params ~ty ~value =
    with_decl (B.decl_mk_thm_with env n univ_params ty value)

  let get_kind        decl =                 B.decl_get_kind decl |> to_decl_kind
  let get_name        decl = with_name      (B.decl_get_name decl)
  let get_univ_params decl = with_list_name (B.decl_get_univ_params decl)
  let get_type        decl = with_expr      (B.decl_get_type decl)
  let get_value       decl = with_expr      (B.decl_get_value decl)
  let get_height      decl = with_uint      (B.decl_get_height decl)
  let get_conv_opt    decl = with_bool      (B.decl_get_conv_opt decl)
  let check env       decl = with_cert_decl (B.decl_check env decl)
end
