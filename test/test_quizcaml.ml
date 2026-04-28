open OUnit2
open Quizcaml.Quiztest
open Quizcaml.Flashcard_review
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

(* We might have to change this because data shouldn't be hard coded (Check-in
   2)*)
let small_test_list =
  [
    ("Apple", "Red fruit");
    ("Orange", "Orange fruit");
    ("Lettuce", "Green vegetable");
    ("Carrot", "Orange vegetable");
    ("Mango", "Yellow fruit");
    ("Grape", "Purple fruit");
    ("Rainbow Chard", "Rainbow fruit");
    ("Watermelon", "Green fruit");
    ("Potato", "Yellow vegetable");
    ("Eggplant", "Purple vegetable");
  ]

let test_gen_tests =
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

let flashcard_review_tests =
  "flashcard review test suite"
  >::: [
         ( "string_to_words splits correctly" >:: fun _ ->
           assert_equal
             ~printer:(fun lst -> "[" ^ String.concat "; " lst ^ "]")
             [ "hello"; "world" ]
             (string_to_words "hello world") );
         ( "string_to_words handles single word" >:: fun _ ->
           assert_equal
             ~printer:(fun lst -> "[" ^ String.concat "; " lst ^ "]")
             [ "hello" ] (string_to_words "hello") );
         ( "wrap_string with basic wrapping" >:: fun _ ->
           assert_equal
             ~printer:(fun lst -> "[" ^ String.concat "; " lst ^ "]")
             [ " hello"; "world" ]
             (wrap_string "hello world" 6) );
         ( "wrap_string with no wrapping needed" >:: fun _ ->
           assert_equal
             ~printer:(fun lst -> "[" ^ String.concat "; " lst ^ "]")
             [ " hello world" ]
             (wrap_string "hello world" 20) );
         ( "center_string centers properly" >:: fun _ ->
           assert_equal "  hi  " (center_string "hi" 6) );
         ( "stat_to_csv produces 6 comma-separated fields" >:: fun _ ->
           let row = stat_to_csv 1 (("term1", "def1"), true, false, Low) in
           let fields = String.split_on_char ',' row in
           assert_equal ~printer:string_of_int 6 (List.length fields) );
         ( "stat_to_csv places session number as first field" >:: fun _ ->
           let row = stat_to_csv 3 (("term1", "def1"), true, true, High) in
           let first = List.nth (String.split_on_char ',' row) 0 in
           assert_equal ~printer:(fun s -> s) "3" first );
         ( "csv_to_stat correctly assigns a Low confidence stat" >:: fun _ ->
           let stat = (("term1", "def1"), true, false, Low) in
           let row = stat_to_csv 1 stat in
           let card, flipped, known, conf = csv_to_stat row in
           assert_equal ("term1", "def1") card;
           assert_equal true flipped;
           assert_equal false known;
           assert_equal Low conf );
         ( "csv_to_stat correctly assigns a High confidence known stat"
         >:: fun _ ->
           let stat = (("term2", "def2"), true, true, High) in
           let row = stat_to_csv 2 stat in
           let card, flipped, known, conf = csv_to_stat row in
           assert_equal ("term2", "def2") card;
           assert_equal true flipped;
           assert_equal true known;
           assert_equal High conf );
         ( "csv_to_stat correctly assigns a (card, false, false, Low) stat"
         >:: fun _ ->
           let stat = (("term3", "def3"), false, false, Low) in
           let row = stat_to_csv 1 stat in
           let card, flipped, known, conf = csv_to_stat row in
           assert_equal ("term3", "def3") card;
           assert_equal false flipped;
           assert_equal false known;
           assert_equal Low conf );
         ( "csv_to_stat correctly converts term and definition with spaces"
         >:: fun _ ->
           let card = ("a long term name", "a long definition here") in
           let stat = (card, true, true, Medium) in
           let row = stat_to_csv 1 stat in
           let card', _, _, _ = csv_to_stat row in
           assert_equal card card' );
         ( "get_session extracts session number from csv row" >:: fun _ ->
           assert_equal ~printer:string_of_int 5
             (get_session "5,\"term\",\"def\",true,false,Low") );
         ( "get_session parses session 1 correctly" >:: fun _ ->
           assert_equal ~printer:string_of_int 1
             (get_session "1,\"t\",\"d\",false,false,Low") );
         ( "stats_to_csv produces one row for one stat" >:: fun _ ->
           let csv = stats_to_csv [ (("term1", "def1"), true, false, Low) ] 1 in
           let lines = String.split_on_char '\n' csv in
           assert_equal ~printer:string_of_int 1 (List.length lines) );
         ( "stats_to_csv produces three rows for three stats" >:: fun _ ->
           let stats =
             [
               (("term1", "def1"), true, false, Low);
               (("term2", "def2"), true, false, High);
               (("term3", "def3"), true, true, Medium);
             ]
           in
           let csv = stats_to_csv stats 1 in
           let lines = String.split_on_char '\n' csv in
           assert_equal ~printer:string_of_int 3 (List.length lines) );
         ( "optimal_order places (card, false, false, Low) first" >:: fun _ ->
           let stats =
             [
               (("term1", "def1"), true, true, High);
               (("term2", "def2"), false, false, Low);
             ]
           in
           let order = optimal_order stats in
           assert_equal
             ~printer:(fun (t, _) -> t)
             ("term2", "def2") (List.nth order 0) );
         ( "optimal_order places unknown low before unknown medium" >:: fun _ ->
           let stats =
             [
               (("term2", "def2"), true, false, Medium);
               (("term1", "def1"), true, false, Low);
             ]
           in
           let order = optimal_order stats in
           assert_equal
             ~printer:(fun (t, _) -> t)
             ("term1", "def1") (List.nth order 0) );
         ( "optimal_order places unknown low before known low" >:: fun _ ->
           let stats =
             [
               (("term2", "def2"), true, true, Low);
               (("term1", "def1"), true, false, Low);
             ]
           in
           let order = optimal_order stats in
           assert_equal
             ~printer:(fun (t, _) -> t)
             ("term1", "def1") (List.nth order 0) );
         ( "optimal_order places known high last" >:: fun _ ->
           let stats =
             [
               (("term1", "def1"), true, true, High);
               (("term2", "def2"), true, false, Low);
               (("term3", "def3"), true, true, Low);
             ]
           in
           let order = optimal_order stats in
           let last = List.nth order (List.length order - 1) in
           assert_equal ~printer:(fun (t, _) -> t) ("term1", "def1") last );
         ( "optimal_order preserves all cards" >:: fun _ ->
           let stats =
             [
               (("term1", "def1"), true, false, Low);
               (("term2", "def2"), true, true, High);
               (("term3", "def3"), false, false, Low);
               (("term1", "def1"), true, false, Medium);
             ]
           in
           let order = optimal_order stats in
           assert_equal ~printer:string_of_int (List.length stats)
             (List.length order) );
         ( "optimal_order returns empty list when input is empty" >:: fun _ ->
           assert_equal [] (optimal_order []) );
         ( "optimal_order returns a single card for input of size 1" >:: fun _ ->
           let stats = [ (("term1", "def1"), true, false, Low) ] in
           assert_equal [ ("term1", "def1") ] (optimal_order stats) );
         ( "read_last_row returns last session number" >:: fun _ ->
           let dir =
             let folder = "data" in
             if not (Sys.file_exists folder) then
               ignore (Sys.command "mkdir -p data");
             folder
           in
           let file =
             Filename.temp_file ~temp_dir:dir "test_read_last_row" ".csv"
           in
           let oc = open_out file in
           output_string oc
             "session,term,definition,flipped,known,confidence\n\
              1,\"a\",\"b\",true,false,Low\n\
              2,\"c\",\"d\",false,true,High\n";
           close_out oc;

           assert_equal ~printer:string_of_int 2 (read_last_row file) );
         ( "load_last only loads last session rows" >:: fun _ ->
           let dir =
             let folder = "data" in
             if not (Sys.file_exists folder) then
               ignore (Sys.command "mkdir -p data");
             folder
           in
           let file =
             Filename.temp_file ~temp_dir:dir "test_load_last" ".csv"
           in
           let oc = open_out file in
           output_string oc
             "session,term,definition,flipped,known,confidence\n\
              1,\"a\",\"b\",true,false,Low\n\
              2,\"c\",\"d\",false,true,High\n\
              2,\"e\",\"f\",true,false,Medium\n";
           close_out oc;

           let result = load_last file in
           assert_equal ~printer:string_of_int 2 (List.length result) );
       ]

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
         (*Check that sample_terms is set to flashcards passed in*)
         ( "check sample_terms" >:: fun _ ->
           help_start ();
           let sample_flashcards : (string * string) list =
             [
               ("1", "one");
               ("2", "two");
               ("3", "green three");
               ("4", "four");
               ("5", "five");
               ("6", "six");
               ("7", "seven");
               ("8", "eight");
               ("9", "nine");
               ("10", "ten");
             ]
           in
           (*attemp updating terms_arr*)
           set_flashcards_value sample_flashcards;
           assert_equal
             ~printer:(fun lst ->
               List.fold_left
                 (fun acc (word, def) -> acc ^ word ^ ": " ^ def ^ "; ")
                 "" lst)
             !sample_terms sample_flashcards );
         (*Test assign print with word values*)
         ( "check assign_print words" >:: fun _ ->
           help_start ();
           (*Make word_assn and def_assn to original value*)
           word_assn := Array.make 10 (0, "");
           def_assn := Array.make 10 ("z", "");
           (*define elements for word_arr*)
           let sample_words : string array =
             [| "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10" |]
           in
           for i = 0 to 9 do
             word_arr.(i) <- sample_words.(i)
           done;
           assign_print ();
           let expected : (int * string) array =
             [|
               (1, "1");
               (2, "2");
               (3, "3");
               (4, "4");
               (5, "5");
               (6, "6");
               (7, "7");
               (8, "8");
               (9, "9");
               (10, "10");
             |]
           in
           assert_equal
             ~printer:(fun lst ->
               Array.fold_left
                 (fun acc (num, word) ->
                   acc ^ string_of_int num ^ ": " ^ word ^ "; ")
                 "" lst)
             expected !word_assn );
       ]

let flashcard_tests =
  "flashcard test suite"
  >::: [
         ( "[read_cards filename] creates the list of flashcards by reading \
            the CSV file at filename, and if the format is not valid, it \
            returns None "
         >:: fun _ ->
           let print_card_list_opt (x : (string * string) list option) : string
               =
             match x with
             | None -> "None"
             | Some lst ->
                 let card_to_string (t, d) = "(" ^ t ^ ", " ^ d ^ ")" in
                 "Some ["
                 ^ String.concat "; " (List.map card_to_string lst)
                 ^ "]"
           in
           let filename = "../data/small_test.csv" in
           let expected = Some small_test_list in
           let loaded = read_cards filename in
           assert_equal ~printer:print_card_list_opt expected loaded;
           let file_name = "../data/invalid_file.csv" in
           let expected = None in
           let loaded = read_cards file_name in
           assert_equal ~printer:print_card_list_opt expected loaded );
         ( "[add_card_from_input curr term def] adds a flashcard (term, def) \
            to the curr list of flashcads "
         >:: fun _ ->
           let print_card_list (lst : (string * string) list) : string =
             let card_to_string (t, d) = "(" ^ t ^ ", " ^ d ^ ")" in
             "[" ^ String.concat "; " (List.map card_to_string lst) ^ "]"
           in
           let curr = small_test_list in
           let modified = add_card_from_input curr "Strawberries" "Red fruit" in
           let expected = ("Strawberries", "Red fruit") :: small_test_list in
           assert_equal ~printer:print_card_list expected modified );
         ( "[remove_card_from_input curr rem_term] removes all flashcards \
            with  term rem_term from the curr list of flashcards"
         >:: fun _ ->
           let print_card_list (lst : (string * string) list) : string =
             let card_to_string (t, d) = "(" ^ t ^ ", " ^ d ^ ")" in
             "[" ^ String.concat "; " (List.map card_to_string lst) ^ "]"
           in
           let curr = small_test_list in
           let modified = remove_card_from_input curr "Lettuce" in
           let expected =
             [
               ("Apple", "Red fruit");
               ("Orange", "Orange fruit");
               ("Carrot", "Orange vegetable");
               ("Mango", "Yellow fruit");
               ("Grape", "Purple fruit");
               ("Rainbow Chard", "Rainbow fruit");
               ("Watermelon", "Green fruit");
               ("Potato", "Yellow vegetable");
               ("Eggplant", "Purple vegetable");
             ]
           in
           assert_equal ~printer:print_card_list expected modified );
       ]

let tests =
  "test suite"
  >::: [
         matching_tests; flashcard_tests; flashcard_review_tests; test_gen_tests;
       ]

let _ = run_test_tt_main tests
