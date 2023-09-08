let run_test ?eq_test:(eq_test : 'a -> 'a -> bool = ( = ))
             (exp : 'a) (f : unit -> 'a) (show : 'a -> string) : bool =
  let v = f () in
  if eq_test v exp then true
  else begin
      print_endline ("Unexpected result: expected " ^ show exp ^
                     " but got " ^ show v);
      false
    end

let print_list (show : 'a -> string) (lst : 'a list) : string =
  "[" ^ String.concat "; " (List.map show lst) ^ "]"

let remove_duplicates_tests (_ : unit) : bool =
  let v = [
    run_test
      []
      (fun _ -> remove_duplicates [])
      (print_list string_of_int);
    run_test
      [1; 3; 2]
      (fun _ -> remove_duplicates [1; 1; 3; 1; 1; 2; 2; 3; 1])
      (print_list string_of_int);
    run_test
      ["a"; "c"; "b"; "d"]
      (fun _ -> remove_duplicates ["a"; "c"; "c"; "b"; "d"; "b"])
      (print_list (fun x -> x));
    run_test
      [1; 2; 3]
      (fun _ -> remove_duplicates [1; 2; 3])
      (print_list string_of_int)
  ] in List.for_all (fun x -> x) v

let test_remove_duplicates (_ : unit) : unit =
  if remove_duplicates_tests () then print_endline "All tests passed"

let prefix_sum_tests _ =
  let v = [
    run_test
      []
      (fun _ -> prefix_sum [])
      (print_list string_of_int);
    run_test
      [2]
      (fun _ -> prefix_sum [2])
      (print_list string_of_int);
    run_test
      [1; 3; 6; 10; 15]
      (fun _ -> prefix_sum [1; 2; 3; 4; 5])
      (print_list string_of_int);
    run_test
      [1; 4; 10; 20; 35]
      (fun _ -> prefix_sum [1; 3; 6; 10; 15])
      (print_list string_of_int);
  ] in List.for_all (fun x -> x) v

let test_prefix_sum (_ : unit) : unit =
  if prefix_sum_tests () then print_endline "All tests passed"

let suffix_sum_tests _ =
  let v = [
    run_test
      []
      (fun _ -> suffix_sum [])
      (print_list string_of_int);
    run_test
      [2]
      (fun _ -> suffix_sum [2])
      (print_list string_of_int);
    run_test
      [15; 14; 12; 9; 5]
      (fun _ -> suffix_sum [1; 2; 3; 4; 5])
      (print_list string_of_int);
    run_test
      [55; 40; 26; 14; 5]
      (fun _ -> suffix_sum [15; 14; 12; 9; 5])
      (print_list string_of_int);
  ] in List.for_all (fun x -> x) v

let test_suffix_sum (_ : unit) : unit =
  if suffix_sum_tests () then print_endline "All tests passed"

let table_to_str : (int * int * int) list list -> string =
  print_list
    (print_list
       (fun (x, y, fxy) -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^
                           ", " ^ string_of_int fxy ^ ")"))

let table_tests _ =
  let v = [
    run_test
      []
      (fun _ -> table ( + ) [] [1; 2; 3])
      table_to_str;
    run_test
      [[]; []; []]
      (fun _ -> table ( + ) [1; 2; 3] [])
      table_to_str;
    run_test
      [[(1, 5, 15); (1, 6, 16); (1, 7, 17); (1, 8, 18)];
       [(2, 5, 25); (2, 6, 26); (2, 7, 27); (2, 8, 28)];
       [(3, 5, 35); (3, 6, 36); (3, 7, 37); (3, 8, 38)]]
      (fun _ -> table (fun x y -> 10 * x + y) [1; 2; 3] [5; 6; 7; 8])
      table_to_str;
  ] in List.for_all (fun x -> x) v

let test_table (_ : unit) : unit =
  if table_tests () then print_endline "All tests passed"

let binaries_tests _ =
  let v = [
    run_test
      [""]
      (fun _ -> binaries 0)
      (print_list (fun x -> x));
    run_test
      ["0"; "1"]
      (fun _ -> binaries 1)
      (print_list (fun x -> x));
    run_test
      ["000"; "001"; "010"; "011"; "100"; "101"; "110"; "111"]
      (fun _ -> binaries 3)
      (print_list (fun x -> x));
    run_test
      ["00000"; "00001"; "00010"; "00011"; "00100"; "00101"; "00110"; "00111";
       "01000"; "01001"; "01010"; "01011"; "01100"; "01101"; "01110"; "01111";
       "10000"; "10001"; "10010"; "10011"; "10100"; "10101"; "10110"; "10111";
       "11000"; "11001"; "11010"; "11011"; "11100"; "11101"; "11110"; "11111"]
      (fun _ -> binaries 5)
      (print_list (fun x -> x));
  ] in List.for_all (fun x -> x) v

let test_binaries (_ : unit) : unit =
  if binaries_tests () then print_endline "All tests passed"

let rec tree_to_str : bstree -> string = function
  | Lf -> "Lf"
  | Br (i, l, r) -> "Br (" ^ string_of_int i ^ ", " ^ tree_to_str l ^ ", " ^
                    tree_to_str r ^ ")"

let without_tests _ =
  let v = [
    run_test
      Lf
      (fun _ -> without Lf 1)
      tree_to_str;
    run_test
      Lf
      (fun _ -> without (Br (1, Lf, Lf)) 1)
      tree_to_str;
    run_test
      (insert_all Lf [2; 1; 3])
      (fun _ -> without (insert_all Lf [2; 1; 3]) 5)
      tree_to_str;
    run_test
      (Br (3, Br (1, Lf, Br (2, Lf, Lf)), Br (4, Lf, Br (6, Lf, Lf))))
      (fun _ -> without (insert_all Lf [3; 1; 2; 5; 4; 6]) 5)
      tree_to_str;
  ] in List.for_all (fun x -> x) v

let test_without (_ : unit) : unit =
  if without_tests () then print_endline "All tests passed"

let flatten_tests _ =
  let v = [
    run_test
      []
      (fun _ -> flatten Lf)
      (print_list string_of_int);
    run_test
      [1]
      (fun _ -> flatten (Br (1, Lf, Lf)))
      (print_list string_of_int);
    run_test
      [1; 2; 3; 4; 5; 6]
      (fun _ -> flatten (insert_all Lf [4; 3; 1; 6; 5; 2]))
      (print_list string_of_int);
    run_test
      [1; 2; 5; 6]
      (fun _ -> flatten (insert_all Lf [1; 6; 5; 2]))
      (print_list string_of_int);
    run_test
      [1; 2; 3; 4]
      (fun _ -> flatten (insert_all Lf [1; 2; 3; 4]))
      (print_list string_of_int);
  ] in List.for_all (fun x -> x) v

let test_flatten (_ : unit) : unit =
  if flatten_tests () then print_endline "All tests passed"

let compare_contents (a : 'a list) (b : 'a list) : bool =
  List.sort compare a = List.sort compare b

let free_vars_tests _ =
  let v = [
    run_test
      []
      (fun _ -> free_vars (Plus (Num 1, Num 2)))
      (print_list (fun x -> x));
    run_test
      ["x"; "z"]
      (fun _ -> free_vars (Negate (Plus (Times (Var "x", Var "z"), Var "x"))))
      (print_list (fun x -> x))
      ~eq_test:compare_contents;
  ] in List.for_all (fun x -> x) v

let test_free_vars (_ : unit) : unit =
  if free_vars_tests () then print_endline "All tests passed"

let rec arith_to_str : arith -> string = function
  | Plus (e1, e2) -> "Plus (" ^ arith_to_str e1 ^ ", " ^ arith_to_str e2 ^ ")"
  | Times (e1, e2) ->
    "Times (" ^ arith_to_str e1 ^ ", " ^ arith_to_str e2 ^ ")"
  | Negate e1 -> "Negate (" ^ arith_to_str e1 ^ ")"
  | Var s -> "Var (" ^ s ^ ")"
  | Num i -> "Num (" ^ string_of_int i ^ ")"

let subst_tests _ =
  let v = [
    run_test
      (Negate (Plus (Times (Num 4, Var "z"), Num 4)))
      (fun _ ->
        subst "x" (Num 4) (Negate (Plus (Times (Var "x", Var "z"), Var "x"))))
      arith_to_str;
    run_test
      (Negate (Plus (Times (Var "x", Var "z"), Var "x")))
      (fun _ ->
        subst "y" (Num 4) (Negate (Plus (Times (Var "x", Var "z"), Var "x"))))
      arith_to_str;
  ] in List.for_all (fun x -> x) v

let test_subst (_ : unit) : unit =
  if subst_tests () then print_endline "All tests passed"

let test_all (_ : unit) : unit =
  let v = [
    remove_duplicates_tests ();
    prefix_sum_tests ();
    suffix_sum_tests ();
    table_tests ();
    binaries_tests ();
    without_tests();
    flatten_tests();
    free_vars_tests();
    subst_tests();
  ] in if List.for_all (fun x -> x) v then print_endline "All tests passed"
