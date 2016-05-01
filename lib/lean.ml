(* * Lean (high-level) interface *)

open Ctypes
open LeanUtil

module LI = LeanInternal

(* ** Types *)

type name                = LI.name
type list_name           = LI.list_name
type options             = LI.options
type univ                = LI.univ
type list_univ           = LI.list_univ
type ios                 = LI.ios
type env                 = LI.env
type decl                = LI.decl
type cert_decl           = LI.cert_decl
type expr                = LI.expr
type list_expr           = LI.list_expr
type macro_def           = LI.macro_def
type inductive_type      = LI.ind_type
type list_inductive_type = LI.list_ind_type
type inductive_decl      = LI.ind_decl
type type_checker        = LI.type_checker
type cnstr_seq           = LI.cnstr_seq
type binder_kind         = LI.binder_kind

(* ** Name *)

module Name = struct
  include LI.Name
  module List = LI.ListName
  type view =
    | Anon
    | Str of name * string
    | Idx of name * uint
  let view n =
    if      is_str n then Str(get_prefix n, get_str n)
    else if is_idx n then Idx(get_prefix n, get_idx n)
    else (assert (is_anon n); Anon)
  let mk_str s = mk_anon () |> append_str ~str:s
  let mk_idx s i = mk_str s |> append_idx ~idx:i
end

(* ** Universe *)

module Univ = struct
  include LI.Univ
  module List = LI.ListUniv
  type view =
    | Zero
      (* The zero universe. *)
    | Succ of univ
      (* Successor of the previous universe. *)
    | Max of univ * univ
      (* Maximum of two universes. *)
    | Imax of univ * univ
      (* [IMax(x,y)] denotes [y] if [y] is universe zero, otherwise [Max x y] *)
    | Param of name
      (* Universe parameter with the given name. *)
    | Global of name
      (* Reference to a global universe. *)
    | Meta of name
      (* Meta variable with the given name. *)

  let view u = LI.(
    match kind u with
    | Univ_Zero   -> Zero
    | Univ_Succ   -> Succ (get_pred u)
    | Univ_Max    -> Max(get_max_lhs u,get_max_rhs u)
    | Univ_Imax   -> Imax(get_max_lhs u,get_max_rhs u)
    | Univ_Param  -> Param(get_name u)
    | Univ_Global -> Global(get_name u)
    | Univ_Meta   -> Meta(get_name u))
end

(* ** Local const *)

module LocalConst = struct
  open LI.Expr
  type t = LocalConst of expr
  let to_expr (LocalConst e) = e
  let mk_local_const n t = LocalConst(mk_local n t)
  let mk_local_const_ext bk n pn t =
    LocalConst(LI.Expr.mk_local_ext n pn t bk)
  let binder_kind lc = to_expr lc |> get_local_binder_kind
  let name lc = to_expr lc |> get_mlocal_name
  let ty lc = to_expr lc |> get_mlocal_type
  let pp_name lc = to_expr lc |> get_local_pp_name
end

(* ** Expression *)

module Expr = struct
  include LI.Expr
  module List = LI.ListExpr
  type local_const = LocalConst of expr (* FIXME: add functions for local_const *)
  type view =
    | Var    of uint
    | Sort   of univ
    | Const  of name * list_univ
    | Local  of LocalConst.t
    | Meta   of name * expr
    | App    of expr * expr
    | Lambda of binder_kind * name * expr * expr
    | Pi     of binder_kind * name * expr * expr
    | Macro  of macro_def * list_expr
    | Let    of unit (* FIXME *)

  let view e = LI.(
    match get_kind e with
    | Expr_var    -> Var(get_var_idx e)
    | Expr_sort   -> Sort(get_sort_univ e)
    | Expr_const  -> Const(get_const_name e, get_const_univ e)
    | Expr_local  -> Local(LocalConst.LocalConst(e))
    | Expr_meta   -> Meta(get_mlocal_name e, get_mlocal_type e)
    | Expr_app    -> App(get_app_fun e, get_app_arg e)
    | Expr_lambda -> Lambda(get_binding_binder_kind e,
                            get_binding_name e,
                            get_binding_domain e,
                            get_binding_body e)
    | Expr_pi     -> Pi(get_binding_binder_kind e,
                        get_binding_name e,
                        get_binding_domain e,
                        get_binding_body e)
    | Expr_let    -> Let() (* FIXME: there might be missing functions in the lean api *)
    | Expr_macro  -> Macro(get_macro_def e, get_macro_args e))
end

(* ** Option *)

(* ** IO state *)

(*
module Ios = struct
  open LI.Ios
  let mk ?(options= LI.Options.mk_empty ()) () =
    mk_std options
end

(* ** Environment *)

module Env = struct
  open LI.Env
  let mk ?(filenames = Name.list_mk []) ios =
    let env = mk_std @@ Unsigned.UInt.of_int 1 in (* FIXME which uint here ? *)
    let env = import env ios (Name.list_mk [NStr "init"]) in
    let env = import env ios filenames in
    env
end

(* ** Inductive types *)
(* ** Inductive declarations *)
(* ** Modules *)
(* ** Parser *)
(* ** Type checker *)
(* ** Declaration *)

type decl_view =
  | DeclAxiom of Name.t * Expr.ty
  | DeclConst of Name.t * Expr.ty
  | DeclDef   of Name.t * Expr.ty * Expr.t
  | DeclThm   of Name.t * Expr.ty * Expr.t

module Decl = struct
  let decl_kind_to_string = function
    | LI.Decl_axiom -> "axiom"
    | LI.Decl_const -> "constant"
    | LI.Decl_def   -> "definition"
    | LI.Decl_thm   -> "theorem"

  let has_value decl =
    match LI.Decl.get_kind decl with
    | LI.Decl_def | LI.Decl_thm -> true
    | _ -> false

  let get_opt_value decl =
    if(has_value decl) then Some (LI.Decl.get_value decl)
    else None

  let expr_to_string ?pp e =
    match pp with
      | None -> LI.Expr.to_string e
      | Some(env,ios) -> LI.Expr.to_pp_string env ios e

  let opt_value_to_string ?pp = function
    | Some e -> ((^) " := ") @@ expr_to_string ?pp e
    | None -> ""

  let to_string ?pp decl =
    (LI.Decl.get_kind decl |> decl_kind_to_string) ^ " " ^
      (LI.Decl.get_name decl |> Name.str) ^ " : " ^
        (LI.Decl.get_type decl |> expr_to_string ?pp) ^
          (get_opt_value decl |> opt_value_to_string ?pp)

  let mk_cert_def env ~name ?(univ_params = Name.list_mk []) ?(ty = Expr.ty_prop) value =
    let decl = LI.Decl.mk_def_with
                 env
                 !:name
                 ~univ_params
                 ~ty
                 ~value
                 ~normalized:true (* FIXME : true or false ?? *) in
    LI.Decl.check env decl
end

(* ** EnvParser *)
*)
(*
module type LeanFiles = sig
  val _olean : string list
  val _lean : string list
end
*)

(*
module GetExprParser (LF : LeanFiles) = struct
  type t = LI.expr
  type _1ary = t -> t
  type _2ary = t -> t -> t
  type _nary = t list -> t

  let to_string = LI.Expr.to_string

  let ios = ref @@
    Ios.mk ()

  let env_of_envios (env',ios') =
    ios := ios';
    env'

  let env =
    let env = Env.mk !ios in
    let module N = Name in
    List.fold_left
      (fun env_acc filename ->
        LI.Parse.file env_acc !ios filename |> env_of_envios)
      (LI.Env.import env !ios
                     (List.map (fun s -> NStr s) LF._olean |> N.list_mk))
      LF._lean

  let to_pp_string = LI.Expr.to_pp_string env !ios

  let get_type =
    let ty_chkr = LI.TypeChecker.mk env in
    fst @< LI.TypeChecker.check ty_chkr

  let get_with_univ_params s = LI.Parse.expr env !ios s
  let get s = fst @@ get_with_univ_params s

  let as_nary app =
    let rec go le = function
      | [] -> le
      | x :: xs -> go (LI.Expr.mk_app le x) xs in
    (* FIXME : catch exception thrown when 'x' cannot be "fed" to 'le', i.e.,
       when the length of the initial list is greater than the actual arity of 'app' *)
    go app

  let as_1ary app =
    fun le -> as_nary app [le]
  let as_2ary app =
    fun le1 le2 -> as_nary app [le1; le2]

  let (<@) = LI.Expr.mk_app

  (* Nats and Integers *)
  let lnat_of_posint =
    let nat_zero = get "nat.zero" in
    let nat_succ = get "nat.succ" |> as_1ary in
    let rec go = function
      | n when n < 0 -> invalid_arg "Positive integer expected"
      | 0 -> nat_zero
      | n -> nat_succ (go (n-1)) in
    go

  let lint_of_int =
    let int_of_nat = get "int.of_nat" |> as_1ary
    and neg_succ_of_nat = get "neg_succ_of_nat" |> as_1ary in
    function
    | i when i >= 0 -> int_of_nat @@ lnat_of_posint i
    | i -> neg_succ_of_nat @@ lnat_of_posint (-i -1)

  (* Proof obligation generation *)
  let output_env = ref env
  let added_proof_obligations : expr list ref = ref []
  let names_db = Hashtbl.create 1
  let get_and_incr s =
    let i = try Hashtbl.find names_db s with Not_found -> 0 in
    Hashtbl.replace names_db s (i+1);
    string_of_int (i+1)

  let proof_obligation_name = "proof_obligation"

  let gen_unique_name prefix =
    prefix ^ "_" ^ (get_and_incr prefix)

  let add_proof_obligation
        ?(prefix = "PO")
        ?(name = gen_unique_name prefix)
        ?(univ_params = Name.list_mk [])
        expr =
    if (name = proof_obligation_name) then invalid_arg @@
      "'" ^ name ^ "' name is reserved for the proof obligation wrapper declaration.";
    let checked_proof_obligation_decl =
      Decl.mk_cert_def !output_env ~name ~univ_params expr
    in
    added_proof_obligations := expr :: !added_proof_obligations;
    output_env := LI.Env.add !output_env checked_proof_obligation_decl

  let _no_nl = String.map (function '\n' -> ' ' | c -> c)
  let proof_obligations_to_string () =
    let ios = Ios.mk () in
    fst @@ List.fold_left
      (fun (s,i) po ->
        (s ^ "(" ^ (string_of_int i) ^ ") " ^ (_no_nl @@ LI.Expr.to_pp_string !output_env ios po) ^ "\n",
         i+1))
      ("",1) @@ List.rev !added_proof_obligations

  let _and = get "and" |> as_2ary

  let export_proof_obligations ?univ_params filename =
    let all_POs = List.rev !added_proof_obligations in
    let env = !output_env in
    match all_POs with
    | po1 :: pos ->
       let all_POs_expr = List.fold_left _and po1 pos in
       let all_POs_cert_decl =
         Decl.mk_cert_def env ~name:"proof_obligation" ?univ_params all_POs_expr in
       let env = LI.Env.add env all_POs_cert_decl in
       LI.Env.export env ~olean_file:filename;
    | _ -> ()

end
*)
