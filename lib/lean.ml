
(* * Lean (high-level) interface *)
open Ctypes

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
        go ((view n)::acc) nl
      ) else ( List.rev acc )
    in
    go [] nl

  let mk_list vs =
    List.fold_left (fun nl v -> mk_cons (mk v) nl) (mk_nil ()) (List.rev vs)

end

let (!:) s = Name.mk @@ Name.Str s
let (@<) f g x = f @@ g x
                                 
(* ** Option *)
(* ** Universe *)

module Univ = struct
  open LI.Univ
  let zero = mk_zero ()
  let one = mk_succ zero
  let rec mk = function
    | i when i <= 0 -> zero
    | i -> mk @@ i-1
end
                
(* ** Expression *)

module Expr = struct
  open LI.Expr                                   
  let bruijn = mk_var @< Unsigned.UInt.of_int                                   
  let forall (s, ty) ?(binder_kind=LI.Binder_default) (f : expr -> expr) =
    mk_pi !:s ~ty (f @@ bruijn 0) binder_kind

  let ty_prop = mk_sort Univ.zero
  let ty_type = mk_sort Univ.one
  let (|:) s ty = s,ty
end
                
(* ** IO state *)

module Ios = struct
  open LI.Ios         
  let mk ?(options= LI.Options.mk_empty ()) () =
    mk_std options
end

(* ** Environment *)

module Env = struct
  open LI.Env
  let mk ?(filenames = Name.mk_list []) ios =
    let env = mk_std @@ Unsigned.UInt.of_int 1 in (* FIXME which uint here ? *)
    let env = import env ios (Name.mk_list [Name.Str "init"]) in
    let env = import env ios filenames in
    env
end

(* ** Inductive types *)
(* ** Inductive declarations *)
(* ** Modules *)
(* ** Parser *)
(* ** Type checker *)
(* ** Declaration *)

module Decl = struct
  let decl_kind_to_string = function
    | LI.Decl_axiom -> "axiom"
    | LI.Decl_const -> "constant"
    | LI.Decl_def -> "definition"
    | LI.Decl_thm -> "theorem"

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
    | Some e -> expr_to_string ?pp e
    | None -> ""
                
  let to_string ?pp decl =
    (LI.Decl.get_kind decl |> decl_kind_to_string) ^ " " ^
      (LI.Decl.get_name decl |> Name.pp) ^ " : " ^
        (LI.Decl.get_type decl |> expr_to_string ?pp) ^
          (if has_value decl
           then " :=\n" ^ (decl |> get_opt_value |> opt_value_to_string ?pp)
           else "")
            
  let mk_cert_def env ~name ?(univ_params = Name.mk_list []) ?(ty = Expr.ty_prop) value =
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

module type LeanFiles = sig
  val _olean : string list
  val _lean : string list
end

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
                     (List.map (fun s -> N.Str s) LF._olean |> N.mk_list))
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
        ?(univ_params = Name.mk_list [])
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
