(*frontend code*)

open Matching_game_logic

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
      "\n\nMatch each word on the left to a definition on the right\n\n";
    start_game_logic flashcards;
    round_loop ();
    flashcards
  end
