open OUnit
open Lean

module F = Format

(* * Test cases *)

let num_loops = 1000

let t_internal_name_anon =
  "lean_name: anon" >:: fun () ->
  let open NameInternal in
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
    assert_equal (to_string n) "[anonymous]"
  done

let t_internal_name_str =
  "lean_name: str" >:: fun () ->
  let open NameInternal in
  for i = 1 to num_loops do
    let na = mk_anon () in
    let ns = mk_str na "foo" in
    assert_equal (get_str ns) "foo";
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
  let open NameInternal in
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
  let open NameInternal in
  let open ListNameInternal in
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
  assert_bool "a10" (NameInternal.eq na na');
  assert_bool "a11" (eq ln1 ln1')

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
   
let _ =
  let suite = "lean" >::: [
        t_internal_name_anon;
        t_internal_name_str;
        t_internal_name_idx;
        t_internal_list_name;
        t_name;
        t_list_name;
      ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()
