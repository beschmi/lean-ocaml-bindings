(* * Utility functions *)
open OUnit

let num_loops = 1

let aeq m a b = assert_equal ~msg:m a b
let atrue m a = assert_equal ~msg:m true a

let (t_shift, t_print, t_unshift) =
  let n = ref 1 in
  let names = ref [] in
  let prefix s = "\n" ^ (String.make !n '*') ^ " " ^ s in
  let open' ?(name="Test") () =
    print_string @@ prefix ("<" ^ name ^ ">");
    names := name :: !names;
    incr n
  in
  let print' s =
    print_string @@ prefix s
  in
  let close' () =
    decr n;
    let name = List.hd !names in
    names := List.tl !names;
    print_string @@ prefix ("</" ^ name ^ ">")
  in
  (open', print', close')

let t_start_scope s f =
  t_shift ~name:s (); f (); t_unshift ()

let t_long_print s =
  let tmp = ref "" in
  let t_char_print = function
    | '\n' -> t_print !tmp; tmp := ""
    | c    -> tmp := !tmp ^ String.make 1 c
  in
  String.iter t_char_print s;
  if !tmp <> "" then
    t_print !tmp
