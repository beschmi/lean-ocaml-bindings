open OUnit
open Lean

module F = Format

(* Test cases *)

let t_name_anon =
  "lean_name: anon" >:: fun () ->
  let n = name_mk_anonymous () in
  assert_bool "" (not (name_is_str n));
  assert_bool "" (not (name_is_idx n));
  assert_bool "" (name_is_anonymous n);
  assert_bool "" (name_eq n n);
  assert_bool "" (not (name_lt n n));
  assert_bool "" (not (name_quick_lt n n));
  let n2 = name_mk_anonymous () in
  assert_bool ""  (name_eq n n2);
  assert_equal (name_to_string n) "[anonymous]"

let t_name_str =
  "lean_name: str" >:: fun () ->
  let na = name_mk_anonymous () in
  let ns = name_mk_str na "foo" in
  assert_equal (name_get_str ns) "foo";
  assert_bool "" (name_is_str ns);
  assert_bool "" (not @@ name_is_idx ns);
  assert_bool "" (not @@ name_is_anonymous ns);
  assert_bool "" (name_eq ns ns);
  assert_bool "" (not @@ name_lt ns ns);
  assert_bool "" (not @@ name_quick_lt ns ns);
  let ns2 = name_mk_str na "foo" in
  let ns3 = name_mk_str na "bar" in
  assert_bool ""  (name_eq ns ns2);
  assert_bool ""  (not (name_eq ns ns3));
  assert_equal (name_to_string ns) "foo"

let t_name_idx =
  "lean_name: idx" >:: fun () ->
  let na = name_mk_anonymous () in
  let ns = name_mk_str na "foo" in
  let ni = name_mk_idx ns 7 in
  assert_bool "" (not (name_is_str ni));
  assert_bool "" (name_is_idx ni);
  assert_bool "" (not (name_is_anonymous ni));
  assert_bool "" (name_eq ni ni);
  assert_bool "" (not (name_lt ni ni));
  assert_bool "" (not (name_quick_lt ni ni));
  assert_equal (name_get_idx ni) 7;
  let ni2 = name_mk_idx ns 7 in
  let ni3 = name_mk_idx ns 5 in
  assert_bool ""  (name_eq ni ni2);
  assert_bool ""  (not (name_eq ni ni3));
  assert_equal (name_to_string ni) "foo.7"

let _ =
  let suite = "lean" >::: [
        t_name_anon;
        t_name_str;
        t_name_idx
      ]
  in
  (* print_string "Starting testing"; *)
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite(*; 
  print_string "Test ended without exceptions"; print_newline(); *)
                                              
