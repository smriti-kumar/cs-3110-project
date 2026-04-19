type term = string
type definition = string
type flashcard = term * definition

type confidence =
  | Low
  | Medium
  | High

(* flashcard, confidence level, number of reviews - default flashcard, Low, 0 *)
type flashcard_record = flashcard * confidence * int

(* flashcard, flipped, known, confidence level - default flashcard, false,
   false, Low *)
type review_stats = flashcard * bool * bool * confidence

let clear () =
  if Sys.os_type = "Unix" then ignore (Sys.command "clear")
  else ignore (Sys.command "cls")

let rec print_dash (n : int) =
  if n == 1 then print_string "-"
  else (
    print_string "-";
    print_dash (n - 1))

let string_to_words (s : string) : string list = String.split_on_char ' ' s

let wrap_string (s : string) (width : int) : string list =
  let words = string_to_words s in
  let rec wrap acc curr_line remaining =
    match remaining with
    | [] -> acc @ [ curr_line ]
    | h :: t ->
        if String.length curr_line + String.length h + 1 <= width then
          wrap acc (curr_line ^ " " ^ h) t
        else wrap (curr_line :: acc) h t
  in
  wrap [] "" words

let center_string (s : string) (width : int) : string =
  let padding = (width - String.length s) / 2 in
  let left = String.make padding ' ' in
  let right = String.make (width - padding - String.length s) ' ' in
  left ^ s ^ right

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
      let status = if known then "Known" else "Unknown" in
      let confidence =
        match conf with
        | High -> "High"
        | Medium -> "Medium"
        | Low -> "Low"
      in
      print_endline (term ^ " - " ^ status ^ " (Confidence: " ^ confidence ^ ")"))
    stats

let rec optimal_order (stats : review_stats list) =
  List.map
    (fun (card, _, _, _) -> card)
    (List.filter (fun (_, _, _, conf) -> conf = Low) stats
    @ List.filter (fun (_, _, _, conf) -> conf = Medium) stats
    @ List.filter (fun (_, _, _, conf) -> conf = High) stats)

let rec review_session (acc : review_stats list) (cards : flashcard list) =
  match cards with
  | [] -> print_stats (List.rev acc)
  | h :: t -> review_session (review_card h :: acc) t

(* Functionalities to implement: - In review_session or review_card, each time
   they don't know something or skip something add to end of list so that it has
   to be reviewed again - This requires using a hashmap for review_stats so that
   we can easily update card stats - Also at the end of each review_session
   export results to a csv along with id of what session this was - Also
   requires tracking in a hash map of how many times this set has been reviewed
   - If this isn't the first review session, need to load previous review stats
   from the csv just for the previous session and put cards in the most optimal
   order - If this isn't the first review session, compare performance against
   previous sessions *)
