open OUnit2
open Quizcaml.Flashcards

let tests =
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

let _ = run_test_tt_main tests
