open Ctypes

module LeanB = Ffi_bindings.Bindings(Ffi_generated)
module LeanT = Ffi_bindings.Types(Ffi_generated_types)

(* * Lean types *)

type name                = LeanB.Name.t
type list_name           = LeanB.List_name.t
type options             = LeanB.Options.t
type univ                = LeanB.Univ.t
type list_univ           = LeanB.List_univ.t
type ios                 = LeanB.Ios.t
type env                 = LeanB.Env.t
type decl                = LeanB.Decl.t
type cert_decl           = LeanB.Cert_decl.t
type expr                = LeanB.Expr.t
type list_expr           = LeanB.List_expr.t
type macro_def           = LeanB.Macro_def.t
type inductive_type      = LeanB.Inductive_type.t
type list_inductive_type = LeanB.List_inductive_type.t
type inductive_decl      = LeanB.Inductive_decl.t
type type_checker        = LeanB.Type_checker.t
type cnstr_seq           = LeanB.Cnstr_seq.t

(* * Strings returned by Lean *)

let to_string ls =
  match coerce LeanB.lean_string string_opt ls with
  | Some s -> s
  | None   -> failwith "to_string: cannot convert NULL"

let deref_string_ptr ls_p =
  let ls = !@ls_p in
  let s = to_string ls in
  LeanB.string_del ls;
  s

(* * Conversion of constants *)

(* ** Booleans *)

let to_bool lb =
  LeanT.Bool.(
    if      lb = true_  then true
    else if lb = false_ then false
    else failwith "Unexpected Lean_bool : not lean_true nor lean_false")

let from_bool b =
  if b then LeanT.Bool.true_ else LeanT.Bool.false_

(* ** Unsigned ints *)

let uint_of_int = Unsigned.UInt.of_int
let uint_to_int = Unsigned.UInt.to_int

(* ** Exceptions *)

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
  let s = to_string (LeanB.exception_get_detailed_message ex) in
  let exc = to_exc_kind (LeanB.exception_get_kind ex) in
  raise (Lean_exception(exc,s))

(* ** Universe kinds *)

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

let from_univ_kind uk =
  LeanT.Univ_kind.(
    match uk with
    | Univ_Zero   -> univ_zero
    | Univ_Succ   -> univ_succ
    | Univ_Max    -> univ_max
    | Univ_Imax   -> univ_imax
    | Univ_Param  -> univ_param
    | Univ_Global -> univ_global
    | Univ_Meta   -> univ_meta
  )


(* ** Expression kinds *)

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

let from_expr_kind ek =
  LeanT.Expr_kind.(
    match ek with
    | Expr_var    -> expr_var
    | Expr_sort   -> expr_sort
    | Expr_const  -> expr_const
    | Expr_local  -> expr_local
    | Expr_meta   -> expr_meta
    | Expr_app    -> expr_app
    | Expr_lambda -> expr_lambda
    | Expr_pi     -> expr_pi
    | Expr_let    -> expr_let
    | Expr_macro  -> expr_macro
  )

(* ** Binder kinds *)

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

(* * Finalizers and with_* functions *)

let deref_ptr finaliser e_p =
  let e = !@e_p in
  Gc.finalise finaliser e;
  e

let deref_exception_ptr       = deref_ptr LeanB.exception_del

let deref_name_ptr            = deref_ptr LeanB.name_del

let deref_list_name_ptr       = deref_ptr LeanB.list_name_del

let deref_options_ptr         = deref_ptr LeanB.options_del

let deref_univ_ptr            = deref_ptr LeanB.univ_del

let deref_list_univ_ptr       = deref_ptr LeanB.list_univ_del

let deref_expr_ptr            = deref_ptr LeanB.expr_del

let deref_list_expr_ptr       = deref_ptr LeanB.list_expr_del

let deref_macro_def_ptr       = deref_ptr LeanB.macro_def_del
                              
let deref_env_ptr             = deref_ptr LeanB.env_del

let deref_decl_ptr            = deref_ptr LeanB.decl_del

let deref_ios_ptr             = deref_ptr LeanB.ios_del

let deref_inductive_type_ptr  = deref_ptr LeanB.inductive_type_del

let deref_list_inductive_type_ptr = deref_ptr LeanB.list_inductive_type_del

let deref_inductive_decl_ptr  = deref_ptr LeanB.inductive_decl_del
                          
                               
let with_exn f g =
  let e_p = LeanB.exception_allocate () in
  let lb = to_bool (f e_p) in
  if lb then g ()
  else raise_exception (deref_exception_ptr e_p)

let with_wrapper alloc deref (f : _ ptr -> _) =
  let n_p = alloc () in
  with_exn
    (fun e_p -> f n_p e_p)
    (fun () -> deref n_p)

let with_pair_wrapper allocs derefs (f : _ ptr -> _ ptr -> _) =
  let alloc1,alloc2 = allocs and
      deref1,deref2 = derefs in
  let n_p1 = alloc1 () and n_p2 = alloc2 () in
  with_exn
    (fun e_p -> f n_p1 n_p2 e_p)
    (fun () -> (deref1 n_p1, deref2 n_p2))
    
let with_bool      = with_wrapper LeanB.bool_allocate      (fun p -> to_bool (!@ p))
let with_string    = with_wrapper LeanB.string_allocate    deref_string_ptr
let with_uint      = with_wrapper LeanB.uint_allocate      (!@)
let with_int       = with_wrapper LeanB.int_allocate       (!@)
let with_double    = with_wrapper LeanB.double_allocate    (!@)
                                  
(* FIXME : how do we handle a function with no "return ptr" ?
I have chosen to return unit, thus needing a specific 'with_unit' exception catcher*)
let with_unit f    = with_exn     f (fun () -> ())(*with_wrapper LeanB.unit_allocate      (!@)*)
                                  
let with_name      = with_wrapper LeanB.name_allocate      deref_name_ptr
let with_list_name = with_wrapper LeanB.list_name_allocate deref_list_name_ptr
let with_options   = with_wrapper LeanB.options_allocate   deref_options_ptr
let with_univ      = with_wrapper LeanB.univ_allocate      deref_univ_ptr
let with_list_univ = with_wrapper LeanB.list_univ_allocate deref_list_univ_ptr
let with_expr      = with_wrapper LeanB.expr_allocate      deref_expr_ptr
let with_list_expr = with_wrapper LeanB.list_expr_allocate deref_list_expr_ptr
let with_macro_def = with_wrapper LeanB.macro_def_allocate deref_macro_def_ptr
let with_env       = with_wrapper LeanB.env_allocate       deref_env_ptr
let with_decl      = with_wrapper LeanB.decl_allocate      deref_decl_ptr
let with_ios       = with_wrapper LeanB.ios_allocate       deref_ios_ptr
let with_inductive_type =
                     with_wrapper LeanB.inductive_type_allocate
                                                           deref_inductive_type_ptr
let with_list_inductive_type =
                     with_wrapper LeanB.list_inductive_type_allocate
                                                           deref_list_inductive_type_ptr
let with_inductive_decl =
                     with_wrapper LeanB.inductive_decl_allocate
                                                           deref_inductive_decl_ptr


                                                           
let with_env_and_ios        = with_pair_wrapper
                                (LeanB.env_allocate ?finalise:None, LeanB.ios_allocate ?finalise:None)
                                (deref_env_ptr, deref_ios_ptr)
                         
let with_expr_and_list_name = with_pair_wrapper
                                (LeanB.expr_allocate ?finalise:None, LeanB.list_name_allocate ?finalise:None)
                                (deref_expr_ptr, deref_list_name_ptr)

module type ListBase = sig
  type t
  type list_t
                            
  val mk_nil  : unit -> list_t
  val mk_cons : t -> list_t -> list_t

  val is_cons : list_t -> bool
  val eq      : list_t -> list_t -> bool
  val head    : list_t -> t
  val tail    : list_t -> list_t
end

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
                                              
module ListMake (X : ListBase) = struct
  type t = X.t
  type list_t = X.list_t
                  
  let mk_nil = X.mk_nil
  let mk_cons = X.mk_cons
  let (@:) = mk_cons
  let (!) x = x @: (mk_nil ())
                  
  let is_cons = X.is_cons
  let eq = X.eq
  let head = X.head
  let tail = X.tail

  let rec to_list = function
    | l when is_cons l -> (head l) :: (to_list l)
    | _ -> []

  let rec of_list = function
    | x :: xs -> mk_cons x (of_list xs)
    | _ -> mk_nil ()
end
                                   

                                
(* * Names *)

module Name = struct
 
  let mk_anon () = with_name LeanB.name_mk_anonymous
  let mk_str n ~str = with_name (LeanB.name_mk_str n str)
  let mk_idx n ~idx = with_name (fun n_p e_p ->
    LeanB.name_mk_idx n (uint_of_int idx) n_p e_p)

  let get_prefix n = with_name   (LeanB.name_get_prefix n)
  let get_idx    n = with_uint   (LeanB.name_get_idx n) |> uint_to_int (* FIXME: use uint here? *)
  let get_str    n = with_string (LeanB.name_get_str n)
  let to_string  n = with_string (LeanB.name_to_string n)

  let is_str   n     = to_bool (LeanB.name_is_str n)
  let is_anon  n     = to_bool (LeanB.name_is_anonymous n)
  let is_idx   n     = to_bool (LeanB.name_is_idx n)
  let eq       n1 n2 = to_bool (LeanB.name_eq n1 n2)
  let lt       n1 n2 = to_bool (LeanB.name_lt n1 n2)
  let quick_lt n1 n2 = to_bool (LeanB.name_quick_lt n1 n2)

end

module ListName = ListMake (struct
  type t = name
  type list_t = list_name
                  
  let mk_nil  ()   = with_list_name LeanB.list_name_mk_nil
  let mk_cons n ln = with_list_name (LeanB.list_name_mk_cons n ln)

  let is_cons ln = to_bool (LeanB.list_name_is_cons ln)
  let eq ln1 ln2 = to_bool (LeanB.list_name_eq ln1 ln2)

  let head ln = with_name (LeanB.list_name_head ln)
  let tail ln = with_list_name (LeanB.list_name_tail ln)

end)

(* * Options *)

module Options = struct

  let mk_empty () = with_options LeanB.options_mk_empty

  let join o1 o2 = with_options (LeanB.options_join o1 o2)
 
  let set_bool o n b = with_options (fun o_p e_p ->
    LeanB.options_set_bool o n (from_bool b) o_p e_p)
  let set_int    o n i = with_options (LeanB.options_set_int o n i)
  let set_uint   o n i = with_options (LeanB.options_set_uint o n i)
  let set_double o n d = with_options (LeanB.options_set_double o n d)
  let set_string o n s = with_options (LeanB.options_set_string o n s)
 
  let get_bool   o n = with_bool   (LeanB.options_get_bool o n)
  let get_int    o n = with_int    (LeanB.options_get_int o n)
  let get_uint   o n = with_uint   (LeanB.options_get_uint o n)
  let get_double o n = with_double (LeanB.options_get_double o n)
  let get_string o n = with_string (LeanB.options_get_string o n)

  let eq o1 o2 = to_bool (LeanB.options_eq o1 o2)
  
  let empty o = to_bool (LeanB.options_empty o)
  
  let contains o n = to_bool (LeanB.options_contains o n)

  let to_string o = with_string (LeanB.options_to_string o)

end

(* * Universes *)

module Univ = struct

  let mk_zero ()    = with_univ LeanB.univ_mk_zero
  let mk_succ u     = with_univ (LeanB.univ_mk_succ u)
  let mk_max  u1 u2 = with_univ (LeanB.univ_mk_max u1 u2)
  let mk_imax u1 u2 = with_univ (LeanB.univ_mk_imax u1 u2)
  let mk_param n    = with_univ (LeanB.univ_mk_param n) 
  let mk_global n   = with_univ (LeanB.univ_mk_global n) 
  let mk_meta n     = with_univ (LeanB.univ_mk_meta n) 

  let get_pred u    = with_univ (LeanB.univ_get_pred u) 
  let get_max_lhs u = with_univ (LeanB.univ_get_max_lhs u) 
  let get_max_rhs u = with_univ (LeanB.univ_get_max_rhs u) 
  let normalize u   = with_univ (LeanB.univ_normalize u) 

  let eq       u1 u2 = with_bool (LeanB.univ_eq u1 u2)
  let lt       u1 u2 = with_bool (LeanB.univ_lt u1 u2)
  let quick_lt u1 u2 = with_bool (LeanB.univ_quick_lt u1 u2)
  let geq      u1 u2 = with_bool (LeanB.univ_geq u1 u2)

  let to_string u = with_string (LeanB.univ_to_string u)

  let to_string_using u o = with_string (LeanB.univ_to_string_using u o)

  let kind u = to_univ_kind (LeanB.univ_kind u)

  let get_name u = with_name (LeanB.univ_get_name u)

  let instantiate u ln lu = with_univ (LeanB.univ_instantiate u ln lu)
end

(* * List of universes *)

module ListUniv = ListMake (struct
  type t = univ
  type list_t = list_univ

  let mk_nil ()    = with_list_univ LeanB.list_univ_mk_nil
  let mk_cons u lu = with_list_univ (LeanB.list_univ_mk_cons u lu)
  
  let head lu = with_univ (LeanB.list_univ_head lu)
  let tail lu = with_list_univ (LeanB.list_univ_tail lu)

  let is_cons lu = to_bool (LeanB.list_univ_is_cons lu)

  let eq lu1 lu2 = with_bool (LeanB.list_univ_eq lu1 lu2)
end)

(* * Expression *)

module Expr = struct
  let (!) = from_binder_kind
  let mk_var i                  = with_expr (LeanB.expr_mk_var i)
  let mk_sort u                 = with_expr (LeanB.expr_mk_sort u)
  let mk_const n lu             = with_expr (LeanB.expr_mk_const n lu)
  let mk_app f a                = with_expr (LeanB.expr_mk_app f a)
  let mk_lambda n t b k         = with_expr (LeanB.expr_mk_lambda n t b !k)
  let mk_pi n t b k             = with_expr (LeanB.expr_mk_pi n t b !k)
  let mk_macro m args           = with_expr (LeanB.expr_mk_macro m args)
  let mk_local n t              = with_expr (LeanB.expr_mk_local n t)
  let mk_local_ext n pp_n t k   = with_expr (LeanB.expr_mk_local_ext n pp_n t !k)
  let mk_metavar n t            = with_expr (LeanB.expr_mk_metavar n t)

  let macro_def_eq m1 m2        = with_bool (LeanB.macro_def_eq m1 m2)
  let macro_def_to_string m     = with_string (LeanB.macro_def_to_string m)

  let to_string e               = with_string (LeanB.expr_to_string e)
  let get_kind e                = LeanB.expr_get_kind e |> to_expr_kind
                                       
  let eq       u1 u2            = with_bool (LeanB.expr_eq u1 u2)
  let lt       u1 u2            = with_bool (LeanB.expr_lt u1 u2)
  let quick_lt u1 u2            = with_bool (LeanB.expr_quick_lt u1 u2)

  let get_var_idx e             = with_uint (LeanB.expr_get_var_idx e)
  let get_sort_univ e           = with_univ (LeanB.expr_get_sort_univ e)
  let get_const_name e          = with_name (LeanB.expr_get_const_name e)
  let get_const_univ e          = with_list_univ (LeanB.expr_get_const_univs e)
  let get_app_fun e             = with_expr (LeanB.expr_get_app_fun e)
  let get_app_arg e             = with_expr (LeanB.expr_get_app_arg e)
  let get_mlocal_name e         = with_name (LeanB.expr_get_mlocal_name e)
  let get_mlocal_type e         = with_expr (LeanB.expr_get_mlocal_type e)
  let get_local_pp_name e       = with_name (LeanB.expr_get_local_pp_name e)
  let get_local_binder_kind e   = with_int (LeanB.expr_get_local_binder_kind e) |> to_binder_kind
  let get_binding_name e        = with_name (LeanB.expr_get_binding_name e)
  let get_binding_domain e      = with_expr (LeanB.expr_get_binding_domain e)
  let get_binding_body e        = with_expr (LeanB.expr_get_binding_body e)
  let get_binding_binder_kind e = with_int (LeanB.expr_get_binding_binder_kind e) |> to_binder_kind
  let get_macro_def e           = with_macro_def (LeanB.expr_get_macro_def e)
  let get_macro_args e          = with_list_expr (LeanB.expr_get_macro_args e)

  let to_pp_string env ios expr = with_string(LeanB.expr_to_pp_string env ios expr)
end

module ListExpr = ListMake (struct
  type t = expr
  type list_t = list_expr

  let mk_nil ()    = with_list_expr LeanB.list_expr_mk_nil
  let mk_cons u lu = with_list_expr (LeanB.list_expr_mk_cons u lu)
  
  let head lu = with_expr (LeanB.list_expr_head lu)
  let tail lu = with_list_expr (LeanB.list_expr_tail lu)

  let is_cons lu = LeanB.list_expr_is_cons lu |> to_bool

  let eq lu1 lu2 = with_bool (LeanB.list_expr_eq lu1 lu2)
end)
                
(* * Environment *)
module Env =  struct
  let mk_std i = with_env (LeanB.env_mk_std i)
  let mk_hott i = with_env (LeanB.env_mk_hott i)

  let add_univ e n = with_env (LeanB.env_add_univ e n)
  let add e d = with_env (LeanB.env_add e d)
  let replace e d = with_env (LeanB.env_replace e d)

  let trust_level e = LeanB.env_trust_level e
  let proof_irrel e = LeanB.env_proof_irrel e |> to_bool
  let impredicative e = LeanB.env_impredicative e |> to_bool

  let contains_univ e n = LeanB.env_contains_univ e n |> to_bool
  let contains_decl e n = LeanB.env_contains_decl e n |> to_bool

  let get_decl e n = with_decl (LeanB.env_get_decl e n)

  let is_descendant e1 e2 = LeanB.env_is_descendant e1 e2 |> to_bool
                                                    
  let forget e = with_env (LeanB.env_forget e)                        

  (* Inductives *)
  let add_inductive env inductive_decl = with_env(LeanB.env_add_inductive env inductive_decl)
  let is_inductive_type env name = with_inductive_decl(LeanB.env_is_inductive_type env name)
  let is_constructor env name = with_name(LeanB.env_is_constructor env name)
  let is_recursor env name = with_name(LeanB.env_is_recursor env name)

  let get_inductive_type_num_indices env name = with_uint(LeanB.env_get_inductive_type_num_indices env name)
  let get_inductive_type_num_minor_premises env name = with_uint(LeanB.env_get_inductive_type_num_minor_premises env name)
  let get_inductive_type_num_type_formers env name = with_uint(LeanB.env_get_inductive_type_num_type_formers env name)
  let get_inductive_type_has_dep_elim env name = with_bool(LeanB.env_get_inductive_type_has_dep_elim env name)

  (* Modules *)
  let import env ios modules = with_env(LeanB.env_import env ios modules)
  let export env fname = with_unit(LeanB.env_export env fname)
end
                
(* * IO state *)
module Ios = struct
  let mk_std options = with_ios(LeanB.ios_mk_std options)
  let mk_buffered options = with_ios(LeanB.ios_mk_buffered options)

  let is_std ios = LeanB.ios_is_std ios |> to_bool
  let set_options ios options = with_unit(LeanB.ios_set_options ios options) (* FIXME : with_unit *)
  let get_options ios = with_options(LeanB.ios_get_options ios)
  let get_regular ios = with_string(LeanB.ios_get_regular ios)
  let get_diagnostic ios = with_string(LeanB.ios_get_diagnostic ios)
  let reset_regular ios = with_unit(LeanB.ios_reset_regular ios) (* FIXME : with_unit *)
  let reset_diagnostic ios = with_unit(LeanB.ios_reset_diagnostic ios) (* FIXME : with_unit *)
                                      
(* FIXME : exception in input 
  val exception_to_pp_string : env -> ios -> exc -> string *)               
end


(* * Inductive types *)
module InductiveType = struct
 let mk n e list_expr = with_inductive_type(LeanB.inductive_type_mk n e list_expr)
 let get_recursor_name n = with_name(LeanB.get_recursor_name n)

 let get_name itype = with_name(LeanB.inductive_type_get_name itype)
 let get_type itype = with_expr(LeanB.inductive_type_get_type itype)
 let get_constructors itype = with_list_expr(LeanB.inductive_type_get_constructors itype)
end
  
(* * Inductive type list *)
module ListInductiveType = ListMake (struct
  type t = inductive_type
  type list_t = list_inductive_type

 let mk_nil () = with_list_inductive_type(LeanB.list_inductive_type_mk_nil)
 let mk_cons itype list_itype  = with_list_inductive_type(LeanB.list_inductive_type_mk_cons itype list_itype)

 let is_cons list_itype  = LeanB.list_inductive_type_is_cons list_itype |> to_bool
 let eq list_itype list_itype  = with_bool(LeanB.list_inductive_type_eq list_itype list_itype)
 let head list_itype  = with_inductive_type(LeanB.list_inductive_type_head list_itype)
 let tail list_itype  = with_list_inductive_type(LeanB.list_inductive_type_tail list_itype)
end)
  
(* * Inductive declarations *)
module InductiveDecl = struct
 let mk ns i list_itypes = with_inductive_decl(LeanB.inductive_decl_mk ns i list_itypes)
     
 let get_univ_params idecl = with_list_name(LeanB.inductive_decl_get_univ_params idecl)
 let get_num_params idecl = with_uint (LeanB.inductive_decl_get_num_params idecl)
 let get_types idecl = with_list_inductive_type(LeanB.inductive_decl_get_types idecl)
end  

(* * Modules *)
module Module = struct
  let get_std_path  () = with_string(LeanB.get_std_path)
  let get_hott_path () = with_string(LeanB.get_hott_path)
end
                  
(* * Parser *)
module Parse = struct
  let file env ios fname = with_env_and_ios(LeanB.parse_file env ios fname)
  let commands env ios str = with_env_and_ios(LeanB.parse_commands env ios str)
  let expr env ios str = with_expr_and_list_name(LeanB.parse_expr env ios str)
end

(* * Type checker *)

module TypeChecker = struct
end 
