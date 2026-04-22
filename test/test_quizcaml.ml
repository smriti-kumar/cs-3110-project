open OUnit2
open Quizcaml.Matching_game_logic
open Quizcaml.Flashcards

(*Helper method to initialize game from beginning in test suite. Doesn't impact
  field values in user version*)
let help_start () : unit =
  begin
    num_corr := 0;
    num_inc := 0;
    (*Manually set empty array values for now- fill based on each test*)
    for i = 0 to 9 do
      game_arr.(i) <- ("", "")
    done
  end

let matching_tests =
  "matching game tests"
  >::: [
         (*Correct guess*)
         ( "check_guess - guess is correct" >:: fun _ ->
           help_start ();
           (*Manually set word_assn...assume one word left in game*)
           word_assn := [| (10, "Urja Saha") |];
           def_assn := [| ("a", "CS3110 student :)") |];
           (*Assume Rest of terms in game_arr are ireelevant*)
           game_arr.(7) <- ("Urja Saha", "CS3110 student :)");
           let guess = "10 a" in
           (*Guess is correct*)
           assert_bool "guess is correct" (check_guess guess);
           (*"Scoring" metrics are correct*)
           assert_equal ~printer:(fun x -> string_of_int x) 1 !num_corr;
           assert_equal ~printer:(fun x -> string_of_int x) 0 !num_inc );
         (*Correct guess removes elements from word_assn and def_assn*)
         ( "correct assn check" >:: fun _ ->
           help_start ();
           (*Manually set word_assn...assume one word left in game*)
           word_assn := [| (10, "Urja Saha"); (1, "Piglet") |];
           def_assn := [| ("a", "CS3110 student :)"); ("d", "Tiny pig") |];
           (*Assume Rest of terms in game_arr are ireelevant*)
           game_arr.(7) <- ("Urja Saha", "CS3110 student :)");
           game_arr.(0) <- ("Piglet", "Tiny pig");
           let guess = "10 a" in
           (*Correct guess*)
           (*Guess is correct*)
           assert_bool "guess is correct" (check_guess guess);
           (*New assn values*)
           let expected_word_assn : (int * string) array =
             [| (1, "Piglet") |]
           in
           let expected_def_assn : (string * string) array =
             [| ("d", "Tiny pig") |]
           in
           assert_equal
             ~printer:(fun arr ->
               Array.fold_left
                 (fun acc (num, elem) ->
                   acc ^ string_of_int num ^ " = " ^ elem ^ " ; ")
                 "" arr)
             expected_word_assn !word_assn;
           assert_equal
             ~printer:(fun arr ->
               Array.fold_left
                 (fun acc (str, elem) -> acc ^ str ^ " = " ^ elem ^ " ; ")
                 "" arr)
             expected_def_assn !def_assn );
         (*Incorrect guess*)
         ( "check_guess - guess is incorrect" >:: fun _ ->
           help_start ();
           (*Manually set word_assn...assume one word left in game*)
           word_assn := [| (10, "Urja Saha"); (1, "Piglet") |];
           def_assn := [| ("a", "CS3110 student :)"); ("d", "Tiny pig") |];
           (*Assume Rest of terms in game_arr are ireelevant*)
           game_arr.(7) <- ("Urja Saha", "CS3110 student :)");
           game_arr.(0) <- ("Piglet", "Tiny pig");
           let guess = "10 d" in
           (*Guess is correct*)
           assert_bool "guess is incorrect" (not (check_guess guess));
           (*"Scoring" metrics are correct*)
           assert_equal ~printer:(fun x -> string_of_int x) 0 !num_corr;
           assert_equal ~printer:(fun x -> string_of_int x) 1 !num_inc );
         (*Incorrect guess doesn't remove elements from word_assn and def_assn*)
         ( "correct assn check" >:: fun _ ->
           help_start ();
           (*Manually set word_assn...assume one word left in game*)
           word_assn := [| (10, "Urja Saha"); (1, "Piglet") |];
           def_assn := [| ("a", "CS3110 student :)"); ("d", "Tiny pig") |];
           (*Assume Rest of terms in game_arr are ireelevant*)
           game_arr.(7) <- ("Urja Saha", "CS3110 student :)");
           game_arr.(0) <- ("Piglet", "Tiny pig");
           let guess = "10 d" in
           (*Correct guess*)
           (*Guess is correct*)
           assert_bool "guess is incorrect" (not (check_guess guess));
           (*New assn values*)
           let expected_word_assn : (int * string) array =
             [| (10, "Urja Saha"); (1, "Piglet") |]
           in
           let expected_def_assn : (string * string) array =
             [| ("a", "CS3110 student :)"); ("d", "Tiny pig") |]
           in
           assert_equal
             ~printer:(fun arr ->
               Array.fold_left
                 (fun acc (num, elem) ->
                   acc ^ string_of_int num ^ " = " ^ elem ^ " ; ")
                 "" arr)
             expected_word_assn !word_assn;
           assert_equal
             ~printer:(fun arr ->
               Array.fold_left
                 (fun acc (str, elem) -> acc ^ str ^ " = " ^ elem ^ " ; ")
                 "" arr)
             expected_def_assn !def_assn );
       ]

let flashcard_tests =
  "test suite"
  >::: [
         ( "[read_cards filename] creates the list of flashcards by reading \
            the CSV file at filename, and if the format is not valid, it \
            returns None "
         >:: fun _ ->
           let print_card_list_opt (x : card_list option) : string =
             match x with
             | None -> "None"
             | Some lst ->
                 let card_to_string (t, d) = "(" ^ t ^ ", " ^ d ^ ")" in
                 "Some ["
                 ^ String.concat "; " (List.map card_to_string lst)
                 ^ "]"
           in
           let filename = "../data/small_test.csv" in
           let expected =
             Some
               [
                 ("Apple", "Red fruit");
                 ("Orange", "Orange fruit");
                 ("Lettuce", "Green vegetable");
                 ("Carrot", "Orange vegetable");
                 ("Mango", "Yellow fruit");
               ]
           in
           let loaded = read_cards filename in
           assert_equal ~printer:print_card_list_opt expected loaded );
         ( "[add_card_from_input curr term def] adds a flashcard (term, def) \
            to the curr list of flashcads "
         >:: fun _ ->
           let print_card_list (lst : card_list) : string =
             let card_to_string (t, d) = "(" ^ t ^ ", " ^ d ^ ")" in
             "[" ^ String.concat "; " (List.map card_to_string lst) ^ "]"
           in
           let curr =
             [
               ("Apple", "Red fruit");
               ("Orange", "Orange fruit");
               ("Lettuce", "Green vegetable");
               ("Carrot", "Orange vegetable");
               ("Mango", "Yellow fruit");
             ]
           in
           let modified = add_card_from_input curr "Strawberries" "Red fruit" in
           let expected =
             [
               ("Strawberries", "Red fruit");
               ("Apple", "Red fruit");
               ("Orange", "Orange fruit");
               ("Lettuce", "Green vegetable");
               ("Carrot", "Orange vegetable");
               ("Mango", "Yellow fruit");
             ]
           in
           assert_equal ~printer:print_card_list expected modified );
         ( "[remove_card_from_input curr rem_term] removes all flashcards\n\
           \            with  term rem_term from the curr list of flashcards"
         >:: fun _ ->
           let print_card_list (lst : card_list) : string =
             let card_to_string (t, d) = "(" ^ t ^ ", " ^ d ^ ")" in
             "[" ^ String.concat "; " (List.map card_to_string lst) ^ "]"
           in
           let curr =
             [
               ("Apple", "Red fruit");
               ("Orange", "Orange fruit");
               ("Lettuce", "Green vegetable");
               ("Carrot", "Orange vegetable");
               ("Mango", "Yellow fruit");
             ]
           in
           let modified = remove_card_from_input curr "Lettuce" in
           let expected =
             [
               ("Apple", "Red fruit");
               ("Orange", "Orange fruit");
               ("Carrot", "Orange vegetable");
               ("Mango", "Yellow fruit");
             ]
           in
           assert_equal ~printer:print_card_list expected modified );
       ]

let tests = "test suite" >::: [ matching_tests; flashcard_tests ]
let _ = run_test_tt_main tests
