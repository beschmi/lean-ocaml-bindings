(** This module provides some utility functions for dealing with the [Lean] API. *)

type uint = Unsigned.UInt.t

let uint_of_int = Unsigned.UInt.of_int
let uint_to_int = Unsigned.UInt.to_int

let (@<) f g x = f @@ g x


(* The following function uses the unsound 'Obj' library /!\ it lacks static type checking /!\
it is a quick and dirty way to convert a variant type (e.g. type enum = Foo | Bar) into
an int (e.g. 0 | 1) as C enums 'naturally' do.
If the input is not of the right type, it will return -1 *)
let unsafe_int_of_kind x =
  let x = Obj.repr x in
  if (Obj.is_int x) then
    Obj.obj x else
    -1
