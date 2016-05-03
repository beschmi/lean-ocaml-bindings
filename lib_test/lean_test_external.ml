open OUnit
open Lean_test_util
open LeanUtil

module F  = Format
module LI = LeanInternal

let t_expr =
  "lean_expr: *" >:: fun () ->
  let module E = Lean.Expr in
  let module U = Lean.Univ in
  let module N = Lean.Name in
  let str = N.mk_str in

  (* universe values *)
  let prop   = U.mk_zero () in
  let ty1    = U.mk_succ prop in
  let uparam = U.mk_param (str "uparam") in
  let umax   = U.mk_max uparam ty1 in

  (* var *)
  let bv0 = E.mk_var @@ uint_of_int 0 in
  let bv1 = E.mk_var @@ uint_of_int 1 in
  aeq "bvar/to_string: #0" (E.to_string bv0) "#0"; 
  aeq "bvar/to_string: #1" (E.to_string bv1) "#1" ;
  atrue "bvar/view: #0"
    E.(match view bv0 with Var(u) -> uint_to_int u = 0 | _ -> false);
  atrue "bvar/view: #1"
    E.(match view bv1 with Var(u) -> uint_to_int u = 1 | _ -> false);

  (* sort *)
  let s1 = E.mk_sort prop in
  let s2 = E.mk_sort ty1 in
  aeq "sort/to_string: Prop" (E.to_string s1) "Prop";
  aeq "sort/to_string: Type" (E.to_string s2) "Type" ;
  atrue "sort/view: Prop" E.(match view s1 with Sort(u) -> U.eq u prop | _ -> false);
  atrue "sort/view: Type" E.(match view s2 with Sort(u) -> U.eq u ty1  | _ -> false);

  (* const *)
  let ul0 = U.List.mk_nil () in
  let ul1 = U.List.of_list [prop; ty1; umax] in
  let c1  = E.mk_const (str "c1") ul0 in
  let c2  = E.mk_const (str "c2") ul1 in
  aeq "const/to_string: c1" (E.to_string c1) "c1";
  aeq "const/to_string: c2" (E.to_string c2) "c2.{0 1 (max uparam 1)}";
  atrue "const/view: c1"
    E.(match view c1 with Const(n,ul) -> U.List.eq ul ul0 && N.eq n (str "c1") | _ -> false);
  atrue "const/view: c2"
    E.(match view c2 with Const(n,ul) -> U.List.eq ul ul1 && N.eq n (str "c2") | _ -> false);

  (* meta *)  
  let mv1 = E.mk_metavar (str "mv1") s1 in
  aeq "meta/to_string: mv1" (E.to_string mv1) "?mv1";
  atrue "meta/view: mv1"
    E.(match view mv1 with Meta(n,s) -> E.eq s s1 && N.eq n (str "mv1") | _ -> false);

  (* lambda *)
  let l1 = E.mk_lambda LI.Binder_default (str "x") s1 bv0 in
  let l2 = E.mk_lambda LI.Binder_default (str "x") s2 bv0 in
  let l3 = E.mk_lambda LI.Binder_default (str "y") s2 l2 in
  aeq "lambda/to_string: l1" (E.to_string l1) "fun (x : Prop), x";
  aeq "lambda/to_string: l2" (E.to_string l2) "fun (x : Type), x";
  aeq "lambda/to_string: l3" (E.to_string l3) "fun (y : Type) (x : Type), x";
  atrue "lambda/view: l1"
    E.(match view l1 with
       | Lambda(LI.Binder_default,n,t,e) -> N.eq n (str "x") && E.eq t s1 && E.eq e bv0
       | _ -> false);
  atrue "lambda/view: l2"
    E.(match view l2 with
       | Lambda(LI.Binder_default,n,t,e) -> N.eq n (str "x") && E.eq t s2 && E.eq e bv0
       | _ -> false);
  atrue "lambda/view: l3"
    E.(match view l3 with
       | Lambda(LI.Binder_default,n,t,e) -> N.eq n (str "y") && E.eq t s2 && E.eq e l2
       | _ -> false);

  (* pi *)
  let p1 = E.mk_pi LI.Binder_default (str "x") s1 bv0 in
  let p2 = E.mk_pi LI.Binder_default (str "x") s2 bv0 in
  let p3 = E.mk_pi LI.Binder_default (str "y") s2 p2 in
  aeq "pi/to_string: p1" (E.to_string p1) "Pi (x : Prop), x";
  aeq "pi/to_string: p2" (E.to_string p2) "Pi (x : Type), x";
  aeq "pi/to_string: p3" (E.to_string p3) "Type -> (Pi (x : Type), x)";
  atrue "pi/view: p1"
    E.(match view p1 with
       | Pi(LI.Binder_default,n,t,e) -> N.eq n (str "x") && E.eq t s1 && E.eq e bv0
       | _ -> false);
  atrue "pi/view: p2"
    E.(match view p2 with
       | Pi(LI.Binder_default,n,t,e) -> N.eq n (str "x") && E.eq t s2 && E.eq e bv0
       | _ -> false);
  atrue "pi/view: p3"
    E.(match view p3 with
       | Pi(LI.Binder_default,n,t,e) -> N.eq n (str "y") && E.eq t s2 && E.eq e p2
       | _ -> false);

  (* app *)
  let a1 = E.mk_app l1 l1 in
  aeq "app/to_string: a1" (E.to_string a1) "(fun (x : Prop), x) (fun (x : Prop), x)";
  atrue "app/view: a1"
    E.(match view a1 with
       | App(e1,e2) -> E.eq e1 l1 && E.eq e2 l1
       | _ -> false);

  (* FIXME: write pretty-printing function that recursively prints view of expression *)

  (* macro *)
  (* FIXME: where to get a macrodef? *)
  ()

let t_env =
  "lean_env: *" >:: fun () ->
  let module E = Lean.Expr in
  let module Ev = LI.Env in
  let module D = LI.Decl in
  let module I = LI.Ios in
  let module O = LI.Options in
  let module U = Lean.Univ in
  let module N = Lean.Name in
  let str = N.mk_str in
  let ul_nil = U.List.mk_nil () in
  let nl_nil = N.List.mk_nil () in
  
  let opt = O.mk_empty () in
  let ios = I.mk_buffered opt in
  let env = Ev.mk_std Ev.trust_high in
  let env = Ev.import env ios (N.List.of_list [str "init"]) in
  (* let env = Ev.import env ios (N.List.of_list [str "aprelude"])  in *)
  let (env,ios) = LI.Parse.file env ios "lib_test/aprelude.lean" in
  let d  = Ev.get_decl env (str "e1") in
  let e1 = D.get_value d in
  (* mini-api *)
  let cmul = E.mk_const (str "grp" |> N.append_str ~str:"mul") ul_nil in
  let mk_mul a b = E.mk_app (E.mk_app cmul a) b in
  let e2 = mk_mul e1 e1 in
  let ty_G = E.mk_const (str "G") ul_nil in
  let def =
    D.mk_def_with env (str "e2") ~univ_params:nl_nil ~ty:ty_G ~value:e2 ~normalized:true
  in
  let cdef = D.check env def in
  let env = Ev.add env cdef in

  let d  = Ev.get_decl env (str "e2") in
  let e2 = D.get_value d in

  F.printf "\ne = %a\n" E.pp_debug e2;
  F.printf "\ne = %a\n" E.pp e2
