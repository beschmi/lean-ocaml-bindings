open OUnit
open Lean

module LI = LeanInternal
module L  = Lean
module F  = Format

(* * Tests: internal *)
              (* The following function uses the unsound Obj.library (avoids static type checking) ;
it is a quick and dirty way to convert a variant type (e.g. type enum = Foo | Bar) into
an int (e.g. 0 | 1) as C enums 'naturally' do.
If the input is not of the right type, it will return -1 *)
let int_of_kind x =
  let x = Obj.repr x in
  if (Obj.is_int x) then
    Obj.obj x else
    -1
              
(* *** LEAN EXPRESSIONS *** *)
            
module ImportLeanDefs (LF : L.LeanFiles) = struct
  module ExprParser = L.GetExprParser(LF)                               
  open ExprParser 
  let get_with_univ_params,get,as_1ary,as_2ary,as_nary = get_with_univ_params,get,as_1ary,as_2ary,as_nary

  let to_string = to_string
  let to_pp_string = to_pp_string
                         
  let mk_Eq   = get "eq"        |> as_2ary
  let mk_GExp = get "expr.Exp"  |> as_2ary
  let mk_GGen = get "expr.Ggen"

  let mk_FNat : int -> ExprParser.t =
    let nat_zero = get "nat.zero" in
    let nat_succ = get "nat.succ" |> as_1ary in
    let neg_succ_of_nat = get "neg_succ_of_nat" |> as_1ary in
    let rec lint_of_int = function
      | 0 -> nat_zero
      | n when n < 0 -> neg_succ_of_nat @@ lint_of_int (-n - 1)
      | n -> nat_succ @@ lint_of_int (n-1) in
    let t_of_lint = get "expr.Fint" |> as_1ary in
    fun n -> t_of_lint @@ lint_of_int n
                                    
  let mk_FPlus = "expr.Fop2" <@ "expr.fop2.Fadd" |> as_2ary
end
(* Now that the functor is defined,
the module only needs to be instanciated with a call to the functor,
e.g., *)

module LeanDefs =
  ImportLeanDefs(
      struct
        let _olean = ["data/nat"]
        let _lean = ["expr.lean"]
      end)
        
              
let num_loops = 1000

let aeq m a b = assert_equal ~msg:m a b

let (@<) f g x = f @@ g x
                    
let t_shift,
    t_print,
    t_unshift  =
  let n = ref 1 in
  let nl () = "\n" ^ (String.make n.contents '*') ^ " " in
  let (!) f = f () in
  let open' ?(name="Test") () =
    print_string (!nl ^ name ^ ":");
    incr n
  and print' s =
    print_string(!nl ^ s)
  and close' () =
    decr n;
    print_string !nl
  in
  (open', print', close')

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
    let ns = mk_str na "foo" in
    aeq "" (get_str ns) "foo";
    assert_bool "" (is_str ns);
    assert_bool "" (not @@ is_idx ns);
    assert_bool "" (not @@ is_anon ns);
    assert_bool "" (eq ns ns);
    assert_bool "" (not @@ lt ns ns);
    assert_bool "" (not @@ quick_lt ns ns);
    let ns2 = mk_str na "foo" in
    let ns3 = mk_str na "bar" in
    assert_bool ""  (eq ns ns2);
    assert_bool ""  (not (eq ns ns3));
    assert_equal (to_string ns) "foo"
  done

let t_internal_name_idx =
  "lean_name: idx" >:: fun () ->
  let open LI.Name in
  for i = 1 to num_loops do
    let na = mk_anon () in
    let ns = mk_str ~str:"foo" na in
    let ni = mk_idx ~idx:7 ns in
    assert_bool "a1" (not (is_str ni));
    assert_bool "a2" (is_idx ni);
    assert_bool "a3" (not (is_anon ni));
    assert_bool "a4" (eq ni ni);
    assert_bool "a5" (not (lt ni ni));
    assert_bool "a6" (not (quick_lt ni ni));
    assert_equal (get_idx ni) 7;
    let na = mk_anon () in
    let ns = mk_str ~str:"bar" na in
    let ni2 = mk_idx ~idx:7 ns in
    let ni3 = mk_idx ns 5 in
    assert_bool "a7"  (not (eq ni ni2));
    assert_bool "a8"  (not (eq ni ni3));
    assert_equal (to_string ni) "foo.7"
  done

let t_internal_list_name =
  "lean_list_name: *" >:: fun () ->
  let open LI.Name in
  let open LI.ListName in
  let lnil = mk_nil () in
  let na = mk_idx ~idx:7 @@ mk_str ~str:"foo" @@ mk_anon () in
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
  let n_bool   = L.Name.(mk (Str "o_bool"))   in
  let n_int    = L.Name.(mk (Str "o_int"))    in
  let n_uint   = L.Name.(mk (Str "o_uint"))   in
  let n_double = L.Name.(mk (Str "o_double")) in
  let n_string = L.Name.(mk (Str "o_string")) in
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
  let u0 = mk_zero () in
  aeq "1" true (eq (mk_zero ()) u0)
  (* FIXME: add more tests *)

let t_internal_list_univ = 
  "lean_list_univ: internal" >:: fun () ->
  let open LI.ListUniv in
  let ul_nil = mk_nil () in
  aeq "1" true (eq (mk_nil ()) ul_nil)
  (* FIXME: add more tests *)

(* * Tests: high-level interface *)

let t_name =
  "lean_name: *" >:: fun () ->
  let n = Name.mk (Name.Idx("hello",5)) in
  assert_equal ~msg:"a1" (Name.view n) (Name.Idx("hello",5));
  let n = Name.mk (Name.Str "hello") in
  assert_equal ~msg:"a2" (Name.view n) (Name.Str "hello");
  let n = Name.mk Name.Anon in
  assert_equal ~msg:"a3" (Name.view n) Name.Anon

let t_list_name =
  "lean_name: *" >:: fun () ->
  let n1 = Name.mk (Name.Idx("hello",5)) in
  let n2 = Name.mk (Name.Str "hello") in
  let n3 = Name.mk Name.Anon in
  let ns = [ n1; n2; n3 ] in
  let nl = LI.ListName.of_list ns in
  let ns1 = List.map Name.view ns in
  let ns2 = (Name.view_list nl) in
  assert_equal ~msg:"a1" ns1 ns2

let t_internal_expr =
  "lean_expr: internal" >:: fun () ->
    let open LI.Expr in
    let uzero = Unsigned.UInt.of_int 0 in
    let e0 = mk_var uzero in
    let e1 = mk_const (Name.mk Name.Anon) (LI.ListUniv.mk_nil ()) in
    aeq "e0 <> e1" false (eq e0 e1);
    aeq "e0 -> #0" "#0" (to_string e0);
    aeq "e1 -> [anonymous]" "[anonymous]" (to_string e1)

let t_internal_parse =
  "lean_parse: internal" >:: fun () ->
    let (!!) = Unsigned.UInt.of_int in
    let open LI in
    let env = Env.mk_std !!1 in (* with !!0 it may loop at import ... *)
    let options = Options.mk_empty () in
    let ios = Ios.mk_std options in
    let (!:) str =
      Name.mk_str (Name.mk_anon ()) ~str in
    let env =
      let module LN = ListName in
      Env.import env ios (LN.of_list [!:"init"(*; !:"data/fin"*)])  in
    (* 'groups.lean' requires 'data.fin' to be imported ;
          both importing it with 'Env.import' or with its own 'import data.fin' line in the .lean file work 
          (the latter option seems to be a little bit faster though) *)
    let (env,ios) =
      Parse.commands env ios "constant (q : ℕ)" in
    let (env,ios) =
      Parse.file env ios "lib_test/groups.lean" in
    let (env,ios) =
      Parse.commands env ios "example {x : 𝓕} : ⟦x⟧ = ⟦x⟧ := rfl" in
    Env.export env ~olean_file:"export_test.olean";
    
    let string_of_local e =
      let open LI in
      (Expr.to_pp_string env ios e) ^ " : " ^ (Expr.to_pp_string env ios @@ Expr.get_mlocal_type e) in
    let open LI.Expr in
    let uzero = Unsigned.UInt.of_int 0 in
    let e0 = mk_var uzero in
    let univ0 = Univ.mk_zero () in
    let prop_sort = mk_sort (univ0) in
    let p = mk_local !:"p" prop_sort in
    let e1 = mk_const (L.Name.mk L.Name.Anon) (ListUniv.of_list [Univ.mk_zero ()]) in
    let pp_string = to_pp_string env ios in
    let t_pp_print = t_print @< pp_string in

    t_shift ~name:"Expressions" (); (
      t_shift ~name:"Sorts" (); (
        string_of_local p |> t_print;
      ); t_unshift ();
      t_shift ~name:"Decls" (); (
        let q_decl = Env.get_decl env !:"q" in
        aeq "Decl.get_kind q_decl = Decl_const" (Decl.get_kind q_decl) Decl_const;
        Decl.get_type q_decl |> Expr.to_string |> t_print;
        let eq_decl = Env.get_decl env !:"eq" in
        Expr.to_string @@ Decl.get_type eq_decl |> t_print
      ); t_unshift ();
      t_shift ~name:"Eqs" (); (
        let eq_app,univ_params = Parse.expr env ios "eq" in
        let univ_params = ListName.to_list univ_params in
        t_shift ~name:"eq univ_params" (); (
          List.iter (t_print @< Name.to_string) univ_params );
        t_unshift ();
        let univs = List.map Univ.mk_param univ_params |> ListUniv.of_list in
        let univ = Univ.instantiate univ0 (ListName.of_list univ_params) univs in
        Univ.to_string univ |> t_print;
        let sort = Expr.mk_sort univ in
        let p' = Expr.mk_local !:"p'" sort in
        let env = List.fold_left Env.add_univ env univ_params in
        Expr.to_string eq_app |> t_print;
        let (<@) = Expr.mk_app in
        let eq_test =  eq_app <@ p' <@ p' in
        pp_string eq_test |> t_print;
        let ty_chkr = TypeChecker.mk env in
        (*let eq_test_type,_ = TypeChecker.check ty_chkr eq_test in (* fails here *)
        Expr.to_string eq_test_type |> t_print*)ignore ty_chkr;
      ); t_unshift ();
      aeq "e0 <> e1" false (eq e0 e1);
      aeq "e0 -> #0" "#0" (pp_string e0);
      aeq "e1 -> [anonymous]" "[anonymous]" (pp_string e1);
      t_shift ~name:"sorry" (); (
        let sorry_decl = Env.get_decl env @@ L.Name.mk @@ L.Name.Str "sorry" in
        let sorry = Parse.expr env ios "sorry" |> fst in
        Decl.get_kind sorry_decl |> int_of_kind |> string_of_int |> t_print;
        Decl.get_type sorry_decl |> t_pp_print;
        (sorry_decl, sorry) |> snd |> t_pp_print;
      ); t_unshift () ;
      t_shift ~name:"New def" (); (
        let open LeanDefs in
        t_print @@ to_pp_string @@ mk_Eq
          (mk_GExp mk_GGen (mk_FPlus (mk_FNat (-1)) (mk_FNat 4)))
          (mk_GExp mk_GGen (mk_FPlus (mk_FNat 4) (mk_FNat (-1))));
      ); t_unshift (); 
    ); t_unshift ();

    t_shift ~name:"Investigating forall" (); (
      let expr,_ = Parse.expr env ios "forall (n : nat), eq n n" in
      expr |> Expr.get_kind |> int_of_kind |> string_of_int |> t_print;
    ); t_unshift ();

    t_shift ~name:"Generating proof obligation" (); (
      let open LeanDefs in
    (* Generate a .lean file proof obligation of:
        forall n : nat, g ^ n = g ^ n *)
      let nat_ty = get "nat" in
      let n = !: "n" in
      let eq,univ_params = get_with_univ_params "@eq.{1}" in
      let eq = as_nary eq in      
      let (!!!) = Expr.mk_var @< Unsigned.UInt.of_int in
      let trivial_eq : expr = eq @@ nat_ty :: !!!0 :: [!!!0] in
      let prop = Expr.mk_pi n ~ty:nat_ty trivial_eq LI.Binder_default in
      (*      let eq_univ_params = (Decl.get_univ_params (Env.get_decl env !: "eq")) in*)
      t_shift ~name:"eq_univ_params" (); (
        List.iter (t_print @< Name.to_string) (ListName.to_list univ_params);
      ); t_unshift ();
      
      let proof_obligation = Decl.mk_def_with env !:"proof_obligation"
                                              ~univ_params:univ_params
                                              ~ty:prop_sort
                                              ~value:prop
                                              ~normalized:false in
      L.Decl.to_string proof_obligation |> t_print;
      let env = Env.add env (Decl.check env proof_obligation) in
      Env.export env ~olean_file:"foo.olean";
    ); t_unshift()
    
    
let _ =
  let suite = "lean" >::: [
        t_internal_name_anon;
        t_internal_name_str;
        t_internal_name_idx;
        t_internal_list_name;
        t_internal_options;
        t_internal_univ;
        t_internal_list_univ;
        t_internal_expr;
        t_name;
        t_list_name;
        t_internal_parse
      ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()
