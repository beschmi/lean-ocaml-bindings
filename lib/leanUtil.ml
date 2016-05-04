(** This module provides some utility functions for dealing with the [Lean] API. *)


module F = Format
             
type uint = Unsigned.UInt.t

let uint_of_int = Unsigned.UInt.of_int
let uint_to_int = Unsigned.UInt.to_int

(** Syntactic sugar for function composition *)
let (@<) f g x = f @@ g x

type ('a, 'b) trycatch = Success of 'a | Fail of 'b
                                                   
(* The following function uses the unsound 'Obj' library /!\ it lacks static type checking /!\*)
(** Quick and dirty way to convert a variant type (e.g. [type enum = Foo | Bar]) into
an [int] (e.g. [0 | 1]) as C enums 'naturally' do.
If the input is not of the right type, it will return [-1] *)
let unsafe_int_of_kind x =
  let x = Obj.repr x in
  if (Obj.is_int x) then
    Obj.obj x else
    -1

       
(* ** ----------------------------------------------------------------------- *)
       
let pp_opt pp fmt o =
  match o with
  | Some x -> pp fmt x
  | None    -> F.fprintf fmt "--"

let pp_around before after pp fmt x =
  F.fprintf fmt "%s%a%s" before pp x after

let pp_string fmt s = F.fprintf fmt "%s" s

let pp_int fmt i = F.fprintf fmt "%i" i

let pp_uint fmt ui = F.fprintf fmt "%s" (Unsigned.UInt.to_string ui)

let pp_if c pp1 pp2 fmt x =
  if c then pp1 fmt x else pp2 fmt x

let pp_pair pp1 pp2 fmt (a,b) =
  F.fprintf fmt "(%a,%a)" pp1 a pp2 b
            
let rec pp_list sep pp_elt f l =
  match l with
  | [] -> ()
  | [e] -> pp_elt f e
  | e::l -> F.fprintf f "%a%(%)%a" pp_elt e sep (pp_list sep pp_elt) l

let pp_list_c pe = (pp_list "," pe)
let pp_list_s = pp_list_c pp_string


let fsprintf fmt =
  let buf  = Buffer.create 127 in
  let fbuf = F.formatter_of_buffer buf in
  F.kfprintf
    (fun _ ->
      F.pp_print_flush fbuf ();
      (Buffer.contents buf))
    fbuf fmt
