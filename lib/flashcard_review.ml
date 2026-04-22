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

let rec optimal_order (stats : review_stats list) =
  List.map
    (fun (card, _, _, _) -> card)
    (List.filter (fun (_, flipped, _, _) -> not flipped) stats
    @ List.filter
        (fun (_, flipped, known, conf) -> flipped && (not known) && conf = Low)
        stats
    @ List.filter
        (fun (_, flipped, known, conf) ->
          flipped && (not known) && conf = Medium)
        stats
    @ List.filter
        (fun (_, flipped, known, conf) -> flipped && (not known) && conf = High)
        stats
    @ List.filter
        (fun (_, flipped, known, conf) -> flipped && known && conf = Low)
        stats
    @ List.filter
        (fun (_, flipped, known, conf) -> flipped && known && conf = Medium)
        stats
    @ List.filter
        (fun (_, flipped, known, conf) -> flipped && known && conf = High)
        stats)

let stat_to_csv (session_num : int) (stat : review_stats) =
  let (term, def), flipped, known, conf = stat in
  let escape s = "\"" ^ String.escaped s ^ "\"" in
  String.concat ","
    [
      string_of_int session_num;
      escape term;
      escape def;
      string_of_bool flipped;
      string_of_bool known;
      (match conf with
      | High -> "High"
      | Medium -> "Medium"
      | Low -> "Low");
    ]

let csv_to_stat (line : string) =
  match String.split_on_char ',' line with
  | [ session; term; def; flipped; known; conf ] ->
      let unescape s = Scanf.unescaped (String.sub s 1 (String.length s - 2)) in
      let card = (unescape term, unescape def) in
      let flipped = bool_of_string flipped in
      let known = bool_of_string known in
      let confidence =
        match conf with
        | "High" -> High
        | "Medium" -> Medium
        | _ -> Low
      in
      (card, flipped, known, confidence)
  | _ -> failwith "Invalid CSV line format"

let get_session (s : string) =
  match String.split_on_char ',' s with
  | h :: _ -> int_of_string h
  | [] -> 0

let read_last_row (filename : string) =
  let ic = open_in filename in
  let rec read_last last =
    try
      let line = input_line ic in
      read_last (Some line)
    with End_of_file -> last
  in
  let last_line = read_last None in
  close_in ic;
  match last_line with
  | None -> 0
  | Some line -> get_session line

let stats_to_csv (stats : review_stats list) (session_num : int) =
  let rows = List.map (stat_to_csv session_num) stats in
  String.concat "\n" rows

let load_last (filename : string) =
  let ic = open_in filename in
  let _ = input_line ic in
  let rec load_all acc =
    try
      let line = input_line ic in
      load_all (line :: acc)
    with End_of_file -> List.rev acc
  in
  let lines = load_all [] in
  close_in ic;
  List.map csv_to_stat
    (List.filter (fun s -> get_session s = read_last_row filename) lines)

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
  else review_session_helper [] cards
