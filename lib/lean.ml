open Ctypes

module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

module F = Format

(* * Conversion *)

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

exception Lean_exception of exc * string

let conv_exc c =
  T.Lean_exception_kind.(
    if      c = lean_null_exception    then Null_Exception
    else if c = lean_system_exception  then System_Exception
    else if c = lean_out_of_memory     then Out_Of_Memory
    else if c = lean_interrupted       then Interrupted
    else if c = lean_kernel_exception  then Kernel_Exception
    else if c = lean_unifier_exception then Unifier_Exception
    else if c = lean_tactic_exception  then Tactic_Exception
    else if c = lean_parser_exception  then Parser_Exception
    else if c = lean_other_exception   then Other_Exception
    else assert false)

let raise_exception ?(del=true) ex =
  match B.lean_exception_get_detailed_message ex with
  | None   -> assert false
  | Some s ->
     let exc = conv_exc (B.lean_exception_get_kind ex) in
     if del then B.lean_exception_del ex;
     raise (Lean_exception(exc,s))

let conv_bool lb =
  T.Lean_bool.(
    if      lb = lean_true then true
    else if lb = lean_false then false
    else failwith "Unexpected Lean_bool : not lean_true nor lean_false")

(* * Lean_name *)

(* ** creation and deletion *)
let name_del n = B.lean_name_del n
                                 
let name_mk_anonymous () =
  let n_p = B.lean_name_allocate () in
  let e_p = B.lean_exception_allocate () in
  let lb = conv_bool (B.lean_name_mk_anonymous n_p e_p) in
  if lb then !@ n_p else raise_exception (!@ e_p)
      
let name_mk_str n (str : string) =
  let n_p = B.lean_name_allocate () in
  let e_p = B.lean_exception_allocate () in
  let lb = conv_bool (B.lean_name_mk_str n (Some str) n_p e_p) in
  if lb then !@n_p else raise_exception (!@ e_p)

let name_mk_str_of_ano : string -> B.Lean_name.t = 
  name_mk_str (name_mk_anonymous ())

let name_mk_idx n (idx : int) =
  let idx = Unsigned.UInt.of_int idx in
  let n_p = B.lean_name_allocate () in
  let e_p = B.lean_exception_allocate () in
  let lb = conv_bool (B.lean_name_mk_idx n idx n_p e_p) in
  if lb then !@n_p else raise_exception (!@ e_p)

let name_mk_idx_of_ano : int -> B.Lean_name.t =
  name_mk_idx (name_mk_anonymous ())


(* ** indicator and comparison *)

let name_is_str       n = conv_bool (B.lean_name_is_str       n)
let name_is_anonymous n = conv_bool (B.lean_name_is_anonymous n)
let name_is_idx       n = conv_bool (B.lean_name_is_idx       n)

let name_eq       n1 n2 = conv_bool (B.lean_name_eq n1 n2)
let name_lt       n1 n2 = conv_bool (B.lean_name_lt n1 n2)
let name_quick_lt n1 n2 = conv_bool (B.lean_name_quick_lt n1 n2)

(* ** destruction *)

let name_get_idx n =
  let i_p = allocate uint (Unsigned.UInt.of_int 0) in
  let e_p = B.lean_exception_allocate () in
  let lb = conv_bool (B.lean_name_get_idx n i_p e_p) in
  if lb then Unsigned.UInt.to_int (!@ i_p)
  else raise_exception (!@ e_p)

let name_get_str n =
  let s_p = B.lean_string_allocate () in
  let e_p = B.lean_exception_allocate () in
  let lb = conv_bool (B.lean_name_get_str n s_p e_p) in
  if lb then
    match !@ s_p with Some s -> s | None -> assert false
  else raise_exception (!@ e_p)

let name_to_string n =
  let s_p = B.lean_string_allocate () in
  let e_p = B.lean_exception_allocate () in
  let lb = conv_bool (B.lean_name_to_string n s_p e_p) in
  if lb then
    match !@ s_p with Some s -> s | None -> assert false
  else raise_exception (!@ e_p)

(* * Lean_list_name *)
