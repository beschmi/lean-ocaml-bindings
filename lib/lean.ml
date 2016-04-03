open Ctypes

module LI = LeanInternal

type name      = LI.Types.name
type list_name = LI.Types.list_name
type options   = LI.Types.options

module Name = struct

  open LI.Name
  
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
 
  open LI.ListName

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

(* * Universes *)
(* * Expression *)
(* * Environment *)
(* * IO state *)
(* * Inductive types *)
(* * Inductive declarations *)
(* * Modules *)
(* * Parser *)
(* * Type checker *)
