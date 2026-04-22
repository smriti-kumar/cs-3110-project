open Quizcaml.Flashcard_review
open Quizcaml.Matching_game_logic
open Quizcaml.Flashcards
open Quizcaml.Quiztest

(* matching game frontend *)

(*Print word assn on LHS and def Assn on RHS*)
let print_match_choices () : unit =
  for i = 0 to Array.length !word_assn - 1 do
    Printf.printf "%d) %-20s %-4s %s) %s\n\n"
      (fst !word_assn.(i))
      (snd !word_assn.(i))
      " "
      (fst !def_assn.(i))
      (snd !def_assn.(i))
  done

(*Give feedback on guesses*)
let guess_feedback (guess : string) () : unit =
  let corr : bool = check_guess guess in
  if corr then print_endline "Correct" else print_endline "Incorrect";
  print_endline ("\nYou have " ^ string_of_int !num_corr ^ " correct guesses\n");
  print_endline ("\nYou have " ^ string_of_int !num_inc ^ " incorrect guesses\n");
  print_endline "\nTerms left to match: \n\n"

(*Loop through the game until all pairs are correctly matched*)
let round_loop () : unit =
  while Array.length !word_assn > 0 do
    print_match_choices ();
    print_endline "\n\n Guess: ";
    let guess : string = read_line () in
    guess_feedback guess ()
  done;
  (*All pairs are matched*)
  print_endline "\n\nCongratulations on finishing all matches!\n";
  print_endline
    ("Final results: \n Correct matches: " ^ string_of_int !num_corr
   ^ "\n Incorrect matches: " ^ string_of_int !num_inc)

(*Startup matching*)
let start_matching (flashcards : (string * string) list) :
    (string * string) list =
  begin
    print_endline
      "\n\n\
       Match each word on the left to a definition on the right\n\n\
      \ Enter a number on the left, then a space, followed by a letter on the \
       right ";
    start_game_logic flashcards;
    round_loop ();
    flashcards
  end

(* flashcard review frontend *)

let clear () =
  if Sys.os_type = "Unix" then ignore (Sys.command "clear")
  else ignore (Sys.command "cls")

let rec print_dash (n : int) =
  if n == 1 then print_string "-"
  else (
    print_string "-";
    print_dash (n - 1))

let print_flashcard (text : string) =
  let width = 50 in
  let wrapped = wrap_string text (width - 6) in
  print_dash width;
  print_endline "";
  print_endline ("|" ^ String.make (width - 2) ' ' ^ "|");
  print_endline ("|" ^ String.make (width - 2) ' ' ^ "|");
  List.iter
    (fun line ->
      print_string "| ";
      print_string (center_string line (width - 4));
      print_endline " |")
    wrapped;
  print_endline ("|" ^ String.make (width - 2) ' ' ^ "|");
  print_endline ("|" ^ String.make (width - 2) ' ' ^ "|");
  print_dash width;
  print_endline ""

let show_term (card : flashcard) =
  clear ();
  print_endline "Term:";
  print_flashcard (fst card)

let show_definition (card : flashcard) =
  clear ();
  print_endline "Definition:";
  print_flashcard (snd card)

let print_stats (stats : review_stats list) =
  let percent =
    List.fold_left
      (fun acc (_, _, known, _) -> if known then acc + 1 else acc)
      0 stats
    * 100 / List.length stats
  in
  if percent == 100 then print_string "Congrats! Perfect session. "
  else if percent >= 70 then print_string "Nice work! "
  else print_string "Keep practicing! ";
  print_endline (string_of_int percent ^ "% of cards known.");
  List.iter
    (fun ((term, def), flipped, known, conf) ->
      let known_status = if known then "Known" else "Unknown" in
      let flipped_status = if flipped then "Flipped" else "Unflipped" in
      let confidence =
        match conf with
        | High -> "High"
        | Medium -> "Medium"
        | Low -> "Low"
      in
      print_endline ("Term: " ^ term ^ ", Definition: " ^ def ^ " - " ^ known_status ^ ", " ^ flipped_status ^ ", Confidence: " ^ confidence))
    stats

let review_card (card : flashcard) =
  show_definition card;
  print_string "Press s to skip, press any other key to flip: ";
  flush stdout;
  let skip = String.lowercase_ascii (read_line ()) in
  print_endline "";
  if skip = "s" then (card, false, false, Low)
  else (
    show_term card;
    print_string "Did you know this? (y/n): ";
    flush stdout;
    let right = String.lowercase_ascii (read_line ()) in
    let correct = right = "y" in
    print_string "Rate your confidence (l for low/m for medium/h for high): ";
    let conf = String.lowercase_ascii (read_line ()) in
    print_endline "";
    let confidence =
      match conf with
      | "h" -> High
      | "m" -> Medium
      | _ -> Low
    in
    (card, true, correct, confidence))

let review_session (cards : flashcard list) (name : string) =
  let filename = "flashcard_review_stats/" ^ name ^ "_flashcard_stats.csv" in
  let file_exists = Sys.file_exists filename in
  let rec review_session_helper (acc : review_stats list) cards =
    match cards with
    | [] ->
        print_stats (List.rev acc);
        let oc =
          open_out_gen [ Open_creat; Open_text; Open_append ] 0o666 filename
        in
        if file_exists then
          output_string oc
            (stats_to_csv (List.rev acc) (read_last_row filename + 1) ^ "\n")
        else
          output_string oc
            ("session,term,definition,flipped,known,confidence" ^ "\n"
            ^ stats_to_csv (List.rev acc) 1
            ^ "\n");
        close_out oc
    | h :: t -> review_session_helper (review_card h :: acc) t
  in
  if file_exists then
    review_session_helper [] (optimal_order (load_last filename))
  else review_session_helper [] cards;
  cards

(* test generation frontend *)

(*In order to play the test activity the Mula library must be installed.*)
let test_question (td : string * string) (num : int) : string list =
  match td with
  | i, j ->
      let () =
        print_endline "";
        print_endline ("Question " ^ string_of_int num ^ ": " ^ j);
        print_endline "";
        print_endline "Enter a guess: "
      in
      let guess = read_line () in
      [ guess; i; correctness guess i ]

let rec test_activity_loop (tdlist : (string * string) list) (rlist : int list)
    (acc : 'a list) (num : int) (count : int) : string list list =
  if num = count then acc
  else
    let question = List.nth tdlist (List.nth rlist num) in
    let gar = test_question question (num + 1) in
    test_activity_loop tdlist rlist (gar :: acc) (num + 1) count

let rec valid_count (size : int) : int =
  let input = read_line () in
  if
    String.for_all
      (fun x ->
        List.exists
          (fun y -> x = y)
          [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ])
      input
    = false
  then (
    print_endline
      ("Please enter a valid integer from 1 to " ^ string_of_int size ^ "!");
    valid_count size)
  else if int_of_string input > 0 && int_of_string input <= size then
    int_of_string input
  else (
    print_endline
      ("Please enter a valid integer from 1 to " ^ string_of_int size ^ "!");
    valid_count size)

let rec results_loop (num : int) (count : int) (scantron : string list list) =
  if num >= count then print_endline "Good Effort!"
  else (
    print_endline "";
    print_endline ("QUESTION " ^ string_of_int (count - num) ^ ":");
    print_endline
      ("YOUR ANSWER: "
      ^ List.nth (List.nth scantron 0) num
      ^ ", CORRECT ANSWER: "
      ^ List.nth (List.nth scantron 1) num
      ^ ", "
      ^ speak (List.nth (List.nth scantron 2) num));
    print_endline "";
    results_loop (num + 1) count scantron)

let test_activity (tdlist : (string * string) list) =
  let () =
    print_endline "";
    print_endline
      "Welcome to the test activity! In this activity, you will be\n\
       given a test consisting of random definitions from the set. To get a\n\
       question right, you must input the correct term to the given definition\n\
       from a set. Once all questions have been answered, you will be able to\n\
       see your grade, as well as what questions you got right and wrong.";
    print_endline "";
    print_endline
      ("Enter how many questions (from 1 to "
      ^ string_of_int (List.length tdlist)
      ^ ") the test should have to begin: ")
  in
  let count = valid_count (List.length tdlist) in
  let rlist = ran_list [] (List.length tdlist) count in
  let scantron =
    BatList.transpose (test_activity_loop tdlist rlist [] 0 count)
  in
  let grade = grader (List.nth scantron 2) count in
  let () = results_loop 0 count scantron in
  let () =
    print_endline "";
    print_endline
      ("RESULTS: " ^ List.nth grade 0 ^ "/" ^ string_of_int count ^ ", "
     ^ List.nth grade 1 ^ "%")
  in
  print_endline "";
  tdlist

(* flashcards frontend *)

(* As of now, this removes all cards in the list with that term. Can be changed
   depending on how we want to handle duplicates.*)

let add_card (curr : card_list) : card_list =
  print_string "Please enter the term for the card you want to add: ";
  let term = read_line () in
  print_string "Please enter the definition for the card you want to add: ";
  let def = read_line () in
  add_card_from_input curr term def

let remove_card (curr : card_list) : card_list =
  print_string "Please enter the term of the card you want to remove ";
  let rem_term = String.trim (read_line ()) in
  remove_card_from_input curr rem_term

let rec starter () : card_list =
  print_endline
    "Please choose one of the following options and type your choice below:";
  print_endline "(1) I have an existing CSV file I would like to upload";
  print_endline
    "(2) I don't have a starter set but would like to manually add cards";
  let choice = ref None in
  let take_input () =
    try
      print_string "Your choice: ";
      flush stdout;
      let input = read_int () in
      if input = 1 || input = 2 then choice := Some input
      else print_endline "\nThat is not a valid choice. Please try again\n"
    with Failure _ ->
      print_endline "\nThat is not a valid choice. Please try again\n"
  in
  while !choice = None do
    take_input ()
  done;

  if !choice = Some 1 then
    match upload_cards () with
    | None -> starter ()
    | Some x -> x
  else add_card []

let run () =
  print_endline "\nWelcome to QuizCaml!\n";
  let caml_cards = ref (starter ()) in
  while true do
    print_endline
      "\n\
      \ Please choose one of the following games/actions and enter your choice \
       below: ";
    print_endline "(1) Add a card";
    print_endline "(2) Remove a card";
    print_endline "(3) Matching";
    print_endline "(4) Testing";
    print_endline "(5) Flashcard Review";
    print_endline "(6) Quit";
    let choice = ref None in
    let take_input () =
      try
        print_string "Your choice: ";
        flush stdout;
        let input = read_int () in
        if input = 6 then exit 0
        else if input = 1 || input = 2 || input = 3 || input = 4 || input = 5
        then choice := Some input
        else print_endline "\nThat is not a valid choice. Please try again\n"
      with Failure _ ->
        print_endline "\nThat is not a valid choice. Please try again\n"
    in
    while !choice = None do
      take_input ()
    done;
    if !choice = Some 1 then caml_cards := add_card !caml_cards
    else if !choice = Some 2 then caml_cards := remove_card !caml_cards
    else if !choice = Some 3 then caml_cards := start_matching !caml_cards
    else if !choice = Some 4 then
      caml_cards := test_activity !caml_cards
    else if !choice = Some 5 then (
      print_endline "Name the set you would like to review: ";
      let input_name = read_line () in
      caml_cards := review_session !caml_cards input_name)
  done

(* main driver *)

let () = run ()
