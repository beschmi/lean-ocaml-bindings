open Ctypes

module LeanB = Ffi_bindings.Bindings(Ffi_generated)
module LeanT = Ffi_bindings.Types(Ffi_generated_types)

module F = Format


(* * Strings returned by Lean *)

let to_string ls =
  match coerce LeanB.lean_string string_opt ls with
  | Some s -> s
  | None   -> failwith "to_string: cannot convert NULL"

let to_string_deref ls_p =
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

let deref_exception_ptr = deref_ptr LeanB.exception_del

let deref_name_ptr = deref_ptr LeanB.name_del

let deref_list_name_ptr = deref_ptr LeanB.list_name_del

let deref_options_ptr = deref_ptr LeanB.options_del

let deref_univ_name_ptr = deref_ptr LeanB.univ_del

let with_exn f g =
  let e_p = LeanB.exception_allocate () in
  let lb = to_bool (f e_p) in
  if lb then g ()
  else raise_exception (deref_exception_ptr e_p)

(* * Names *)

type name = LeanB.Name.t

module NameInternal = struct
 
  let mk_anon () =
    let n_p = LeanB.name_allocate () in
    with_exn
      (fun e_p -> LeanB.name_mk_anonymous n_p e_p)
      (fun () -> deref_name_ptr n_p)

  let mk_str n ~str =
    let n_p = LeanB.name_allocate () in
    with_exn
      (fun e_p -> LeanB.name_mk_str n str n_p e_p)
      (fun () -> deref_name_ptr n_p)

  let mk_idx n ~idx =
    let idx = Unsigned.UInt.of_int idx in
    let n_p = LeanB.name_allocate () in
    with_exn
      (fun e_p -> LeanB.name_mk_idx n idx n_p e_p)
      (fun () -> deref_name_ptr n_p)

  let is_str  n = to_bool (LeanB.name_is_str       n)
  let is_anon n = to_bool (LeanB.name_is_anonymous n)
  let is_idx  n = to_bool (LeanB.name_is_idx       n)

  let eq       n1 n2 = to_bool (LeanB.name_eq n1 n2)
  let lt       n1 n2 = to_bool (LeanB.name_lt n1 n2)
  let quick_lt n1 n2 = to_bool (LeanB.name_quick_lt n1 n2)

  let get_prefix n =
    let n_p = LeanB.name_allocate () in
    with_exn
      (fun e_p -> LeanB.name_get_prefix n n_p e_p)
      (fun () -> deref_name_ptr n_p)

  let get_idx n =
    let i_p = allocate uint (Unsigned.UInt.of_int 0) in
    with_exn
      (fun e_p -> LeanB.name_get_idx n i_p e_p)
      (fun () -> Unsigned.UInt.to_int !@i_p)

  let get_str n =
    let s_p = LeanB.string_allocate () in
    with_exn
      (fun e_p -> LeanB.name_get_str n s_p e_p)
      (fun () -> to_string_deref s_p)

  let to_string n =
    let s_p = LeanB.string_allocate () in
    with_exn
      (fun e_p -> LeanB.name_to_string n s_p e_p)
      (fun () -> to_string_deref s_p)

end
 
type list_name = LeanB.List_name.t

module ListNameInternal = struct

  let mk_nil () =
    let ln_p = LeanB.list_name_allocate () in
    with_exn
      (fun e_p -> LeanB.list_name_mk_nil ln_p e_p)
      (fun () -> deref_list_name_ptr ln_p)

  let mk_cons n ln =
    let ln_p = LeanB.list_name_allocate () in
    with_exn
      (fun e_p -> LeanB.list_name_mk_cons n ln ln_p e_p)
      (fun () -> deref_list_name_ptr ln_p)

  let is_cons ln = to_bool (LeanB.list_name_is_cons ln)

  let eq ln1 ln2 = to_bool (LeanB.list_name_eq ln1 ln2)

  let head ln =
    let n_p = LeanB.name_allocate () in
    with_exn
      (fun e_p -> LeanB.list_name_head ln n_p e_p)
      (fun () -> deref_name_ptr n_p)

  let tail ln =
    let ln_p = LeanB.list_name_allocate () in
    with_exn
      (fun e_p -> LeanB.list_name_tail ln ln_p e_p)
      (fun () -> deref_list_name_ptr ln_p)

end

module Name = struct

  open NameInternal
  
  let eq = eq
  let pp = to_string

  type view =
    | Anon
    | Str of string
    | Idx of string * int

  let view n =
    if      is_str n then Str (get_str n)
    else if is_idx n then Idx (get_str (get_prefix n), get_idx n)
    else Anon

  let mk v =
    match v with
    | Anon     -> mk_anon ()
    | Str s    -> mk_str ~str:s @@ mk_anon ()
    | Idx(s,i) -> mk_idx ~idx:i @@ mk_str ~str:s @@ mk_anon ()
 
  open ListNameInternal

  let view_list nl =
    let rec go acc nl =
      if is_cons nl then (
        let n  = head nl in
        let nl = tail nl in
        go (n::acc) nl
      ) else ( List.rev acc )
    in
    go [] nl

  let mk_list ns =
    List.fold_left (fun nl n -> mk_cons n nl) (mk_nil ()) (List.rev ns)

end

(* * Options *)

type options = LeanB.Options.t

module OptionsInternal = struct

  let mk_empty () =
    let o_p = LeanB.options_allocate () in
    with_exn
      (fun e_p -> LeanB.options_mk_empty o_p e_p)
      (fun () -> deref_options_ptr o_p)

  let join o1 o2 =
    let o_p = LeanB.options_allocate () in
    with_exn
      (fun e_p -> LeanB.options_join o1 o2 o_p e_p)
      (fun () -> deref_options_ptr o_p)

  let set_bool o n b =
    let o_p = LeanB.options_allocate () in
    with_exn
      (fun e_p -> LeanB.options_set_bool o n b o_p e_p)
      (fun () -> deref_options_ptr o_p)

  let set_int o n i =
    let o_p = LeanB.options_allocate () in
    with_exn
      (fun e_p -> LeanB.options_set_int o n i o_p e_p)
      (fun () -> deref_options_ptr o_p)

  let set_unsigned o n i =
    let o_p = LeanB.options_allocate () in
    with_exn
      (fun e_p -> LeanB.options_set_unsigned o n i o_p e_p)
      (fun () -> deref_options_ptr o_p)

  let set_double o n d =
    let o_p = LeanB.options_allocate () in
    with_exn
      (fun e_p -> LeanB.options_set_double o n d o_p e_p)
      (fun () -> deref_options_ptr o_p)

  let set_string o n s =
    let o_p = LeanB.options_allocate () in
    with_exn
      (fun e_p -> LeanB.options_set_string o n s o_p e_p)
      (fun () -> deref_options_ptr o_p)


  let get_bool o n =
    let i_p = allocate LeanB.lean_bool 0 in
    with_exn
      (fun e_p -> LeanB.options_get_bool o n i_p e_p)
      (fun () -> to_bool !@i_p)

  let get_int o n =
    let i_p = allocate int 0 in
    with_exn
      (fun e_p -> LeanB.options_get_int o n i_p e_p)
      (fun () -> !@i_p)

  let get_unsigned o n =
    let i_p = allocate uint (Unsigned.UInt.of_int 0) in
    with_exn
      (fun e_p -> LeanB.options_get_unsigned o n i_p e_p)
      (fun () -> Unsigned.UInt.to_int !@i_p)

  let get_double o n =
    let i_p = allocate double 0.0 in
    with_exn
      (fun e_p -> LeanB.options_get_double o n i_p e_p)
      (fun () -> !@i_p)

  let get_string o n =
    let s_p = LeanB.string_allocate () in
    with_exn
      (fun e_p -> LeanB.options_get_string o n s_p e_p)
      (fun () -> to_string_deref s_p)

  let eq o1 o2 = to_bool (LeanB.options_eq o1 o2)
  
  let empty o = to_bool (LeanB.options_empty o)
  
  let contains o n = to_bool (LeanB.options_contains o n)

  let to_string o =
    let s_p = LeanB.string_allocate () in
    with_exn
      (fun e_p -> LeanB.options_to_string o s_p e_p)
      (fun () -> to_string_deref s_p)

end

(* * Universes *)
(* * Expression *)
(* * Environment *)
(* * IO state *)
(* * Inductive types *)
(* * Inductive declarations *)
(* * Modules *)
(* * Parser *)
(* * Type checker *)
