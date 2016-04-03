(* * Types *)

type name = LeanInternal.Types.name
type list_name = LeanInternal.Types.list_name
type options = LeanInternal.Types.options

(* * Names *)

(* type name *)

(* type list_name *)

module Name : sig
  type view =
    | Anon
    | Str of string
    | Idx of string * int
 
  val eq : name -> name -> bool
  val pp : name -> string

  val view : name -> view
  val mk   : view -> name

  val view_list : list_name -> name list
  val mk_list   : name list -> list_name
end
(* * Options *)
(* * Universe *)
(* * List of universes *)
(* * Expression *)
(* * Environment *)
(* * IO state *)
(* * Inductive types *)
(* * Inductive type list *)
(* * Inductive declarations *)
(* * Modules *)
(* * Parser *)
(* * Type checker *)
