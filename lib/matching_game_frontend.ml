(*frontend code*)

open Matching_game_logic

(*Print word assn on LHS and def Assn on RHS*)
let print_match_choices () : unit =
  for i = 0 to 9 do
    Printf.printf "%d) %-20s %-4s %s) %s\n\n"
      (fst word_assn.(i))
      (snd word_assn.(i))
      " "
      (fst def_assn.(i))
      (snd def_assn.(i))
  done

(*Give feedback on guesses*)
let guess_feedback (guess : string) () : unit =
  let corr : bool = check_guess guess in
  if corr then print_endline "yay" else print_endline "incorrect"

(*Startup matching*)
let start_matching () : unit =
  begin
    print_endline
      "\n\nMatch each word on the left to a definition on the right\n\n";
    start_game_logic ();
    print_match_choices ();
    print_endline "\n\n Guess: \n\n";
    let guess : string = read_line () in
    guess_feedback guess ()
  end
