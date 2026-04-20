open OUnit2
open Quizcaml.Quiztest

let tests =
  "test suite"
  >::: [
         ( "A guess and an answer are not compared correctly" >:: fun _ ->
           assert_equal (correctness "right" "right") "1";
           assert_equal (correctness "right" "wrong") "0";
           assert_equal (correctness "right" "RIGHT") "1";
           assert_equal (correctness "right" "   right   ") "1";
           assert_equal (correctness "homemade" "home made") "1";
           assert_equal (correctness "homemade" "home mede") "1";
           assert_equal (correctness "homemade" "home mole") "0" );
         ( "The random test list was not generated correctly" >:: fun _ ->
           let rlist1 = ran_list [] 20 10 in
           let rlist2 = ran_list [] 20 20 in
           assert_equal (List.length rlist1) 10;
           assert_equal (List.length rlist2) 20;
           assert_equal (List.for_all (fun x -> x >= 0 && x < 20) rlist1) true;
           assert_equal (List.for_all (fun x -> x >= 0 && x < 20) rlist2) true;
           (*This assert below is to check if each element of the list is
             unique. If each element of the list is unique and contains a number
             from 0 to 19, then it should always add up to 190. The chance of a
             list where each element isn't unique adding up to 190 is highly
             unlikely.*)
           assert_equal (List.fold_left ( + ) 0 rlist2) 190 );
         ( "A score list is not graded correctly" >:: fun _ ->
           assert_equal (grader [ "0"; "1"; "1"; "0"; "1" ] 5) [ "3"; "60" ];
           assert_equal (grader [ "1"; "1"; "1"; "1"; "1" ] 5) [ "5"; "100" ] );
       ]

let _ = run_test_tt_main tests