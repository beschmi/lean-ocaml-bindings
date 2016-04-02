open OUnit
open Lean

module F = Format

(* * Test cases *)

let num_loops = 1000

let t_name_anon =
  "lean_name: anon" >:: fun () ->
  for i = 1 to num_loops do 
    let n = name_mk_anon () in
    assert_bool "" (not (name_is_str n));
    assert_bool "" (not (name_is_idx n));
    assert_bool "" (name_is_anon n);
    assert_bool "" (name_eq n n);
    assert_bool "" (not (name_lt n n));
    assert_bool "" (not (name_quick_lt n n));
    let n2 = name_mk_anon () in
    assert_bool ""  (name_eq n n2);
    assert_equal (name_to_string n) "[anonymous]"
  done

let t_name_str =
  "lean_name: str" >:: fun () ->
  for i = 1 to num_loops do
    let na = name_mk_anon () in
    let ns = name_mk_str na "foo" in
    assert_equal (name_get_str ns) "foo";
    assert_bool "" (name_is_str ns);
    assert_bool "" (not @@ name_is_idx ns);
    assert_bool "" (not @@ name_is_anon ns);
    assert_bool "" (name_eq ns ns);
    assert_bool "" (not @@ name_lt ns ns);
    assert_bool "" (not @@ name_quick_lt ns ns);
    let ns2 = name_mk_str na "foo" in
    let ns3 = name_mk_str na "bar" in
    assert_bool ""  (name_eq ns ns2);
    assert_bool ""  (not (name_eq ns ns3));
    assert_equal (name_to_string ns) "foo"
  done

let t_name_idx =
  "lean_name: idx" >:: fun () ->
  for i = 1 to num_loops do
    let na = name_mk_anon () in
    let ns = name_mk_str ~str:"foo" na in
    let ni = name_mk_idx ~idx:7 ns in
    assert_bool "a1" (not (name_is_str ni));
    assert_bool "a2" (name_is_idx ni);
    assert_bool "a3" (not (name_is_anon ni));
    assert_bool "a4" (name_eq ni ni);
    assert_bool "a5" (not (name_lt ni ni));
    assert_bool "a6" (not (name_quick_lt ni ni));
    assert_equal (name_get_idx ni) 7;
    let ni2 = name_mk_idx ns 7 in
    let ni3 = name_mk_idx ns 5 in
    assert_bool "a7"  (name_eq ni ni2);
    assert_bool "a8"  (not (name_eq ni ni3));
    assert_equal (name_to_string ni) "foo.7"
  done

let t_list_name =
  "lean_list_name: *" >:: fun () ->
   let lnil = list_name_mk_nil () in
   let na = name_mk_idx ~idx:7 @@ name_mk_str ~str:"foo" @@ name_mk_anon () in
   let ln1 = list_name_mk_cons na lnil in
   let ln2 = list_name_mk_cons na ln1 in
   let ln2' = list_name_mk_cons na ln1 in
   assert_bool "a1"  (not (list_name_is_cons lnil));
   assert_bool "a2"  (list_name_is_cons ln1);
   assert_bool "a3"  (list_name_is_cons ln2);
   assert_bool "a4"  (list_name_eq lnil lnil);
   assert_bool "a5"  (list_name_eq ln1 ln1);
   assert_bool "a6"  (list_name_eq ln2 ln2);
   assert_bool "a7"  (list_name_eq ln2 ln2');
   assert_bool "a8"  (not (list_name_eq ln1 ln2));
   assert_bool "a9"  (not (list_name_eq lnil ln2));
   let na' = list_name_head ln2 in
   let ln1' = list_name_tail ln2 in
   assert_bool "a10" (name_eq na na');
   assert_bool "a11" (list_name_eq ln1 ln1')
   
let _ =
  let suite = "lean" >::: [
        t_name_anon;
        t_name_str;
        t_name_idx;
        t_list_name;
      ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()
