open OUnit
open Lean

module LI = LeanInternal
module L  = Lean
module F  = Format

(* * Tests: internal *)

let num_loops = 1000

let aeq m a b = assert_equal ~msg:m a b

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
  let nl = Name.mk_list ns in
  let ns1 = List.map Name.view ns in
  let ns2 = List.map Name.view (Name.view_list nl) in
  assert_equal ~msg:"a1" ns1 ns2

let t_internal_expr =
  "lean_expr: internal" >::
    fun () ->
    let open LI.Expr in
    let uzero = Unsigned.UInt.of_int 0 in
    let e0 = mk_var uzero in
    let e1 = mk_const (Name.mk Name.Anon) (LI.ListUniv.mk_nil ()) in
    aeq "e0 <> e1" false (eq e0 e1);
    aeq "e0 -> #0" "#0" (to_string e0);
    aeq "e1 -> [anonymous]" "[anonymous]" (to_string e1)
    
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
      ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()
