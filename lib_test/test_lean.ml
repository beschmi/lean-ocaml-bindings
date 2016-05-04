open OUnit
open Lean
open LeanUtil
open Lean_test_util

module LI = LeanInternal
module L  = Lean
module F  = Format

(* * Tests: internal *)

let t_internal_name_anon =
  "lean_name: anon" >:: fun () ->
  let open LI.Name in
  for i = 1 to num_loops do
    let n = mk_anon () in
    assert_bool "" (not (is_str n));
    assert_bool "" (not (is_idx n));
    assert_bool "" (is_anon n);
    assert_bool "" (eq n n);
    assert_bool "" (not (lt n n));
    assert_bool "" (not (quick_lt n n));
    let n2 = mk_anon () in
    assert_bool ""  (eq n n2);
    aeq "" (to_string n) "[anonymous]"
  done

let t_internal_name_str =
  "lean_name: str" >:: fun () ->
  let open LI.Name in
  for i = 1 to num_loops do
    let na = mk_anon () in
    let ns = append_str na "foo" in
    aeq "" (get_str ns) "foo";
    assert_bool "" (is_str ns);
    assert_bool "" (not @@ is_idx ns);
    assert_bool "" (not @@ is_anon ns);
    assert_bool "" (eq ns ns);
    assert_bool "" (not @@ lt ns ns);
    assert_bool "" (not @@ quick_lt ns ns);
    let ns2 = append_str na "foo" in
    let ns3 = append_str na "bar" in
    assert_bool ""  (eq ns ns2);
    assert_bool ""  (not (eq ns ns3));
    assert_equal (to_string ns) "foo"
  done

let t_internal_name_idx =
  "lean_name: idx" >:: fun () ->
  let open LI.Name in
  for i = 1 to num_loops do
    let ns = append_str ~str:"foo" @@ mk_anon () in
    let ni = append_idx ~idx:(uint_of_int 7) ns in
    assert_bool "a1" (not (is_str ni));
    assert_bool "a2" (is_idx ni);
    assert_bool "a3" (not (is_anon ni));
    assert_bool "a4" (eq ni ni);
    assert_bool "a5" (not (lt ni ni));
    assert_bool "a6" (not (quick_lt ni ni));
    assert_equal (get_idx ni) (uint_of_int 7);
    let ns = append_str ~str:"bar" @@ mk_anon ()  in
    let ni2 = append_idx ~idx:(uint_of_int 7) ns in
    let ni3 = append_idx ~idx:(uint_of_int 5) ns in
    assert_bool "a7"  (not (eq ni ni2));
    assert_bool "a8"  (not (eq ni ni3));
    assert_equal (to_string ni) "foo.7"
  done

let t_internal_name_mixed =
  "lean_name: idx" >:: fun () ->
  let open LI.Name in
  for i = 1 to num_loops do
    let n =
      mk_anon ()
      |> append_str ~str:"a"
      |> append_str ~str:"b"
      |> append_idx ~idx:(uint_of_int 1)
      |> append_str ~str:"c"
    in
    assert_equal (get_str n) "c";
    assert_equal (get_prefix n |> get_idx) (uint_of_int 1);
    assert_equal (get_prefix n |> get_prefix |> get_str) "b";
    assert_equal (to_string n) "a.b.1.c"
  done

let t_internal_list_name =
  "lean_list_name: *" >:: fun () ->
  let open LI.Name in
  let open LI.ListName in
  let is_cons l =
    not (view l = Nil) in
  let head l = match view l with
    | Cons(hd, _) -> hd | _ -> assert false in
  let tail l = match view l with
    | Cons(_,tl) -> tl | _ -> assert false in
  let lnil = mk_nil () in
  let na =
    mk_anon ()
    |> append_str ~str:"foo"
    |> append_idx ~idx:(uint_of_int 7) 
  in
  let ln1 = mk_cons na lnil in
  let ln2 = mk_cons na ln1 in
  let ln2' = mk_cons na ln1 in
  assert_bool "a1" (not (is_cons lnil));
  assert_bool "a2" (is_cons ln1);
  assert_bool "a3" (is_cons ln2);
  assert_bool "a4" (eq lnil lnil);
  assert_bool "a5" (eq ln1 ln1);
  assert_bool "a6" (eq ln2 ln2);
  assert_bool "a7" (eq ln2 ln2');
  assert_bool "a8" (not (eq ln1 ln2));
  assert_bool "a9" (not (eq lnil ln2));
  let na' = head ln2 in
  let ln1' = tail ln2 in
  assert_bool "a10" (LI.Name.eq na na');
  assert_bool "a11" (eq ln1 ln1')


let t_internal_options =
  "lean_options: *" >:: fun () ->
  let open LI.Options in
  let oe        = mk_empty () in
  let n_bool   = L.Name.mk_str "o_bool"   in
  let n_int    = L.Name.mk_str "o_int"    in
  let n_uint   = L.Name.mk_str "o_uint"   in
  let n_double = L.Name.mk_str "o_double" in
  let n_string = L.Name.mk_str "o_string" in
  let v_bool   = true in
  let v_int    = 42 in
  let v_uint   = Unsigned.UInt.of_int 99 in
  let v_double = 18.9 in
  let v_string = "bar" in
  (* set flags *)
  let o1 = set_bool     oe n_bool   v_bool   in
  let o1 = set_int      o1 n_int    v_int    in
  let o1 = set_double   o1 n_double v_double in
  let o1 = set_uint o1 n_uint   v_uint   in
  let o2 = set_string   oe n_string v_string in
  let o3 = join o1 o2 in
  aeq "a1" v_bool   (get_bool   o1 n_bool);
  aeq "a2" v_int    (get_int    o1 n_int);
  aeq "a3" v_double (get_double o1 n_double);
  aeq "a4" v_uint   (get_uint   o1 n_uint);
  aeq "a5" v_string (get_string o2 n_string);
  (* o3 has all entries *)
  aeq "a6"  v_bool   (get_bool   o3 n_bool);
  aeq "a7"  v_int    (get_int    o3 n_int);
  aeq "a8"  v_double (get_double o3 n_double);
  aeq "a9"  v_uint   (get_uint   o3 n_uint);
  aeq "a10" v_string (get_string o3 n_string);
  (* empty and contains *)
  aeq "a11" true  (empty oe);
  aeq "a12" true  (contains o2 n_string);
  aeq "a13" false (contains o1 n_string);
  let s = "⟨o_bool ↦ true, o_int ↦ 42, o_double ↦ 18.9, o_uint ↦ 99, o_string ↦ \"bar\"⟩" in
  aeq "a14" s (to_string o3)

let t_internal_univ =
  "lean_univ: internal" >:: fun () ->
  let open LI.Univ in
  let open LI in
  let u0 = mk_zero () in
  let u1 = mk_param (L.Name.mk_str "u1") in
  let u2 = mk_param (L.Name.mk_str "u2") in
  aeq "1" true        (eq (mk_zero ()) u0);
  aeq "2" Univ_Succ   (kind (mk_succ u1));
  aeq "3" Univ_Max    (kind (mk_max u1 u2));
  aeq "4" Univ_Imax   (kind (mk_imax u1 u2));
  aeq "5" Univ_Param  (kind (mk_param (L.Name.mk_str "hello")));
  aeq "6" Univ_Global (kind (mk_global (L.Name.mk_str "hello")));
  aeq "7" Univ_Meta   (kind (mk_meta (L.Name.mk_str "hello")));
  aeq "8" Univ_Param  (kind (normalize (mk_max u1 u1)))

let t_internal_list_univ =
  "lean_list_univ: internal" >:: fun () ->
    let open LI.ListUniv in
    let ul_nil = mk_nil () in
    aeq "1" true (eq (mk_nil ()) ul_nil)
    (* FIXME: add more tests *)

let t_internal_expr =
  "lean_expr: internal" >:: fun () ->
    let open LI.Expr in
    let uzero = uint_of_int 0 in
    let e0 = mk_var uzero in
    let e1 = mk_const (Name.mk_anon ()) (LI.ListUniv.mk_nil ()) in
    aeq "e0 <> e1" false (eq e0 e1);
    aeq "e0 -> #0" "#0" (to_string e0);
    aeq "e1 -> [anonymous]" "[anonymous]" (to_string e1)

(* * Tests: high-level interface *)

let t_name =
  "lean_name: *" >:: fun () ->
    let na = Name.mk_anon () in
    assert_equal ~msg:"a1" (Name.view na) Name.Anon;
    let ni = Name.mk_idx "foo" (uint_of_int 5) in
    assert_bool "a2" (match Name.view ni with Name.Idx(_, u) -> u = uint_of_int 5 | _ -> false);
    let ns = Name.mk_str "hello" in
    assert_bool "a3" (match Name.view ns with Name.Str(_, u) -> u = "hello" | _ -> false)

let t_list_name =
  let open Name in
  "lean_list_name: *" >:: fun () ->
    let n1 = mk_idx "foo" (uint_of_int 5) in
    let n2 = mk_str "hello" in
    let n3 = mk_anon () in
    let ns1 = [ n1; n2; n3 ] in
    let nl1 = List.of_list ns1 in
    let ns2 = List.to_list nl1 in
    let ns3 = [ n2; n3 ] in
    let nl2  = List.of_list ns3 in
    assert_bool "a1" (List.eq nl1 (Name.List.of_list ns2));
    assert_bool "a2" (match List.view nl2 with List.Cons(n,_) -> eq n2 n | _ -> false)

let t_univ =
  let open Univ in
  "lean_univ: *" >:: fun () ->
    let n1 = Name.mk_str "p1" in
    let n2 = Name.mk_str "p2" in
    let uz = mk_zero () in
    let up1 = mk_param n1 in
    let up2 = mk_param n2 in
    let us  = mk_succ up1 in
    let um  = mk_max up1 up2 in
    let ui  = mk_imax up1 up2 in
    let ug  = mk_global n1 in
    let ume  = mk_meta n1 in
    assert_bool "a1" (match view uz  with Zero -> true | _ -> false);
    assert_bool "a2" (match view us  with Succ(u) -> eq up1 u | _ -> false);
    assert_bool "a3" (match view um  with Max(u1,u2) -> eq up1 u1 && eq up2 u2 | _ -> false);
    assert_bool "a4" (match view ui  with Imax(u1,u2) -> eq up1 u1 && eq up2 u2 | _ -> false);
    assert_bool "a5" (match view up1 with Param(n) -> Name.eq n n1 | _ -> false);
    assert_bool "a6" (match view ug  with Global(n) -> Name.eq n n1 | _ -> false);
    assert_bool "a7" (match view ume with Meta(n) -> Name.eq n n1 | _ -> false);
    ()
(*
let t_internal_parse =
  "lean_parse: internal" >:: fun () ->
    let (!!) = Unsigned.UInt.of_int in
    let open LI in
    let env = Env.mk_std !!1 in (* with !!0 it may loop at import ... *)
    let options = Options.mk_empty () in
    let ios = Ios.mk_std options in
    let (!:) str =
      Name.mk_str (Name.mk_anon ()) ~str in
*)

let t_internal_parse =
  "lean_parse: expr" >:: fun () ->
  let env = LI.Env.(mk_std trust_high) in
  let options = LI.Options.mk_empty () in
  let ios = LI.Ios.mk_std options in
  let string_of_local e = LI.(
    (Expr.to_pp_string env ios e) ^ " : " ^ (Expr.to_pp_string env ios @@ Expr.get_mlocal_type e))
  in
  let env =
    let module LN = LI.ListName in
    LI.Env.import env ios (LN.of_list [L.Name.mk_str "init"(*; !:"data/fin"*)])  in
    (* 'groups.lean' requires 'data.fin' to be imported ;
          both importing it with 'Env.import' or with its own 'import data.fin' line in the .lean file work
          (the latter option seems to be a little bit faster though) *)
  let (env,ios) = LI.Parse.commands env ios "constant (q : ℕ)" in
  let (env,ios) = LI.Parse.file env ios "lib_test/groups.lean" in
  let (env,ios) = LI.Parse.commands env ios "example {x : 𝓕} : ⟦x⟧ = ⟦x⟧ := rfl" in
  LI.Env.export env ~olean_file:"export_test.olean";
  let open LI.Expr in
  let uzero = Unsigned.UInt.of_int 0 in
  let e0 = mk_var uzero in
  let univ0 = Univ.mk_zero () in
  let prop_sort = mk_sort (univ0) in
  let p = mk_local (Name.mk_str "p") prop_sort in
  let e1 = mk_const (Name.mk_anon ()) (Univ.List.of_list [Univ.mk_zero ()]) in
  let pp_string = to_pp_string env ios in
  let t_pp_print = t_print @< pp_string in
  t_start_scope "Expressions" (fun () ->
    t_start_scope "Sorts" (fun () ->
      string_of_local p |> t_print;
    );
    t_start_scope "Decls" LI.(fun () ->
      let q_decl = Env.get_decl env (L.Name.mk_str "q") in
      aeq "Decl.get_kind q_decl = Decl_const" (Decl.get_kind q_decl) Decl_const;
      Decl.get_type q_decl |> Expr.to_string |> t_print;
      let eq_decl = Env.get_decl env (L.Name.mk_str "eq") in
      Expr.to_string @@ Decl.get_type eq_decl |> t_print
    );
    t_start_scope "Eqs" LI.(fun () ->
        let eq_app,univ_params = Parse.expr env ios "eq" in
        let univ_params = ListName.to_list univ_params in
        t_start_scope "eq univ_params" (fun () ->
          List.iter (t_print @< Name.to_string) univ_params
        );
        let univs = List.map Univ.mk_param univ_params |> ListUniv.of_list in
        let univ = Univ.instantiate univ0 (ListName.of_list univ_params) univs in
        Univ.to_string univ |> t_print;
        let sort = Expr.mk_sort univ in
        let p' = Expr.mk_local (L.Name.mk_str "p'") sort in
        let env = List.fold_left Env.add_univ env univ_params in
        Expr.to_string eq_app |> t_print;
        let (<@) = Expr.mk_app in
        let eq_test =  eq_app <@ p' <@ p' in
        pp_string eq_test |> t_print;
        let ty_chkr = TypeChecker.mk env in
        (*let eq_test_type,_ = TypeChecker.check ty_chkr eq_test in (* fails here *)
        Expr.to_string eq_test_type |> t_print*)ignore ty_chkr;
    );
    aeq "e0 <> e1" false (eq e0 e1);
    aeq "e0 -> #0" "#0" (pp_string e0);
    aeq "e1 -> [anonymous]" "[anonymous]" (pp_string e1);
    t_start_scope "sorry" LI.(fun () ->
      let sorry_decl = Env.get_decl env @@ L.Name.mk_str "sorry" in
      let sorry = Parse.expr env ios "sorry" |> fst in
      (* Decl.get_kind sorry_decl |> int_of_kind |> string_of_int |> t_print; *)
      Decl.get_type sorry_decl |> t_pp_print;
      (sorry_decl, sorry) |> snd |> t_pp_print;
    );(*
    t_start_scope "New def" (fun () ->
      let open Env in
      t_print @@ to_pp_string @@ mk_Eq
        (mk_GExp mk_GGen (mk_FPlus (mk_FNat (-1)) (mk_FNat 4)))
        (mk_GExp mk_GGen (mk_FPlus (mk_FNat 4) (mk_FNat (-1))));
    );*)
  )
  (*
  t_start_scope "Proof obligation" (fun () ->
    let open LeanEnv in
    let nat = get "nat" in
    let forall_n_eq_n_n =
      let open L.Expr in
      mk_forall ("n" |: nat) (fun n -> view @@ lean_eq ~ty:nat n n) in
    LeanEnv.add_proof_obligation @@ L.Expr.mk forall_n_eq_n_n;
    LeanEnv.add_proof_obligation (mk_GGen |=| mk_GGen);
    LeanEnv.proof_obligations_to_string () |> t_long_print;
    LeanEnv.export_proof_obligations "foo.olean";
  )*)

(* *** LEAN EXPRESSIONS *** *)
(*
module ImportLeanDefs (LF : L.LeanFiles) = struct
  include L.GetExprParser(LF)
  let mk_Eq   = get "eq"        |> as_2ary
  let mk_GExp = get "expr.Exp"  |> as_2ary
  let mk_GGen = get "expr.Ggen"
  let mk_FNat : int -> t =
    let nat_zero = get "nat.zero" in
    let nat_succ = get "nat.succ" |> as_1ary in
    let neg_succ_of_nat = get "neg_succ_of_nat" |> as_1ary in
    let rec lint_of_int = function
      | 0 -> nat_zero
      | n when n < 0 -> neg_succ_of_nat @@ lint_of_int (-n - 1)
      | n -> nat_succ @@ lint_of_int (n-1) in
    let t_of_lint = get "expr.Fint" |> as_1ary in
    fun n -> t_of_lint @@ lint_of_int n
  let mk_FPlus = get "expr.Fop2 expr.fop2.Fadd" |> as_2ary

  let lean_eq e1 ?(ty = get_type e1) e2 =
    let eq = get "@eq.{1}" |> as_nary in
    eq [ty; e1; e2]
  let (|=|) = lean_eq
end
*)
(* Now that the functor is defined,
the module only needs to be instantiated with a call to the functor,
e.g., *)

(*
module LeanEnv =
  ImportLeanDefs(
      struct
        let _olean = ["data/nat"]
        let _lean = ["../autognp-lean/expr.lean"]
      end)
*)

(* * Test suite *)

let _ =
  let suite = "lean" >::: [
    t_internal_name_anon;
    t_internal_name_str;
    t_internal_name_idx;
    t_internal_name_mixed;
    t_internal_list_name;
    t_internal_options;
    t_internal_univ;
    t_internal_list_univ;
    (* t_internal_parse; disabled: print garbage *)
    t_name;
    t_list_name;
    t_univ;
    Lean_test_external.t_expr;
    Lean_test_external.t_env;
    ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()
