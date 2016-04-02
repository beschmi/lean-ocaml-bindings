open Ctypes

module LeanB = Ffi_bindings.Bindings(Ffi_generated)
module LeanT = Ffi_bindings.Types(Ffi_generated_types)

module F = Format

(* * Conversion *)

(* ** Booleans *)

let conv_bool lb =
  LeanT.Bool.(
    if      lb = true_  then true
    else if lb = false_ then false
    else failwith "Unexpected Lean_bool : not lean_true nor lean_false")

(* ** Strings *)

let conv_string ls =
  match coerce LeanB.lean_string string_opt ls with
  | Some s -> s
  | None   -> failwith "conv_lean_string: cannot convert NULL"

let conv_string_deref ls_p =
  let ls = !@ls_p in
  let s = conv_string ls in
  LeanB.string_del ls;
  s

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

exception Lean_exception of exc_kind * string

let conv_exc_kind c =
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

let raise_exception ex =
  let s = conv_string (LeanB.exception_get_detailed_message ex) in
  let exc = conv_exc_kind (LeanB.exception_get_kind ex) in
  raise (Lean_exception(exc,s))

(* ** Universes *)

type univ_kind =
  | Lean_Univ_Zero
  | Lean_Univ_Succ
  | Lean_Univ_Max
  | Lean_Univ_Imax
  | Lean_Univ_Param
  | Lean_Univ_Global
  | Lean_Univ_Meta

let conv_univ_kind c =
  LeanT.Univ_kind.(
    if      c = univ_zero   then Lean_Univ_Zero
    else if c = univ_succ   then Lean_Univ_Succ
    else if c = univ_max    then Lean_Univ_Max
    else if c = univ_imax   then Lean_Univ_Imax
    else if c = univ_param  then Lean_Univ_Param
    else if c = univ_global then Lean_Univ_Global
    else if c = univ_meta   then Lean_Univ_Meta
    else assert false
  )

(* * Finalizer and withXXX functions *)

let deref_ptr finaliser e_p =
  let e = !@e_p in
  Gc.finalise finaliser e;
  e

let deref_exception_ptr = deref_ptr LeanB.exception_del

let deref_name_ptr = deref_ptr LeanB.name_del

let deref_list_name_ptr = deref_ptr LeanB.list_name_del

let deref_univ_name_ptr = deref_ptr LeanB.univ_del

let with_exn f g =
  let e_p = LeanB.exception_allocate () in
  let lb = conv_bool (f e_p) in
  if lb then g ()
  else raise_exception (deref_exception_ptr e_p)

(* * Lean_name *)

type name = LeanB.Name.t

(* ** creation and deletion *)

let name_mk_anon () =
  let n_p = LeanB.name_allocate () in
  with_exn
    (fun e_p -> LeanB.name_mk_anonymous n_p e_p)
    (fun () -> deref_name_ptr n_p)

let name_mk_str n ~str =
  let n_p = LeanB.name_allocate () in
  with_exn
    (fun e_p -> LeanB.name_mk_str n str n_p e_p)
    (fun () -> deref_name_ptr n_p)

let name_mk_idx n ~idx =
  let idx = Unsigned.UInt.of_int idx in
  let n_p = LeanB.name_allocate () in
  with_exn
    (fun e_p -> LeanB.name_mk_idx n idx n_p e_p)
    (fun () -> deref_name_ptr n_p)

(* ** indicator and comparison *)

let name_is_str  n = conv_bool (LeanB.name_is_str       n)
let name_is_anon n = conv_bool (LeanB.name_is_anonymous n)
let name_is_idx  n = conv_bool (LeanB.name_is_idx       n)

let name_eq       n1 n2 = conv_bool (LeanB.name_eq n1 n2)
let name_lt       n1 n2 = conv_bool (LeanB.name_lt n1 n2)
let name_quick_lt n1 n2 = conv_bool (LeanB.name_quick_lt n1 n2)

(* ** destruction *)

let name_get_idx n =
  let i_p = allocate uint (Unsigned.UInt.of_int 0) in
  let e_p = LeanB.exception_allocate () in
  let lb = conv_bool (LeanB.name_get_idx n i_p e_p) in
  if lb then Unsigned.UInt.to_int !@i_p
  else raise_exception (deref_exception_ptr e_p)

let name_get_str n =
  let s_p = LeanB.string_allocate () in
  let e_p = LeanB.exception_allocate () in
  let lb = conv_bool (LeanB.name_get_str n s_p e_p) in
  if lb then conv_string_deref s_p
  else raise_exception (deref_exception_ptr e_p)

let name_to_string n =
  let s_p = LeanB.string_allocate () in
  let e_p = LeanB.exception_allocate () in
  let lb = conv_bool (LeanB.name_to_string n s_p e_p) in
  if lb then conv_string_deref s_p
  else raise_exception (deref_exception_ptr e_p)

(* * Lean_list_name *)

type list_name = LeanB.List_name.t

let list_name_mk_nil () =
  let ln_p = LeanB.list_name_allocate () in
  with_exn
    (fun e_p -> LeanB.list_name_mk_nil ln_p e_p)
    (fun () -> deref_list_name_ptr ln_p)

let list_name_mk_cons n ln =
  let ln_p = LeanB.list_name_allocate () in
  with_exn
    (fun e_p -> LeanB.list_name_mk_cons n ln ln_p e_p)
    (fun () -> deref_list_name_ptr ln_p)

(* ** Indicator, equality and destructor functions *)

let list_name_is_cons ln = conv_bool (LeanB.list_name_is_cons ln)

let list_name_eq  ln1 ln2 = conv_bool (LeanB.list_name_eq ln1 ln2)

let list_name_head ln =
  let n_p = LeanB.name_allocate () in
  with_exn
    (fun e_p -> LeanB.list_name_head ln n_p e_p)
    (fun () -> deref_name_ptr n_p)

let list_name_tail ln =
  let ln_p = LeanB.list_name_allocate () in
  with_exn
    (fun e_p -> LeanB.list_name_tail ln ln_p e_p)
    (fun () -> deref_list_name_ptr ln_p)

(* * Lean_univ *)



