type confidence =
  | Low
  | Medium
  | High

type flashcard_record = (string * string) * confidence * int
type review_stats = (string * string) * bool * bool * confidence

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

let rec optimal_order (stats : review_stats list) : (string * string) list =
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

let stat_to_csv (session_num : int) (stat : review_stats) : string =
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

let csv_to_stat (line : string) : review_stats =
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

let get_session (s : string) : int =
  match String.split_on_char ',' s with
  | h :: _ -> int_of_string h
  | [] -> 0

let read_last_row (filename : string) : int =
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

let stats_to_csv (stats : review_stats list) (session_num : int) : string =
  let rows = List.map (stat_to_csv session_num) stats in
  String.concat "\n" rows

let load_last (filename : string) : review_stats list =
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

let load_all (filename : string) : (int * review_stats) list =
  let ic = open_in filename in
  let _ = input_line ic in
  let rec load acc =
    try
      let line = input_line ic in
      let session = get_session line in
      let stat = csv_to_stat line in
      load ((session, stat) :: acc)
    with End_of_file -> List.rev acc
  in
  let result = load [] in
  close_in ic;
  result

let group_sessions (history : (int * review_stats) list) :
    (int * review_stats list) list =
  let map = Hashtbl.create 10 in
  List.iter
    (fun (session, stat) ->
      let curr_val =
        match Hashtbl.find_opt map session with
        | Some lst -> lst
        | None -> []
      in
      Hashtbl.replace map session (stat :: curr_val))
    history;
  List.sort
    (fun (a, _) (b, _) -> compare a b)
    (Hashtbl.fold (fun key value acc -> (key, value) :: acc) map [])

let session_known (stats : review_stats list) : float =
  let total = List.length stats in
  let known =
    List.fold_left (fun acc (_, _, k, _) -> if k then acc + 1 else acc) 0 stats
  in
  if total = 0 then 0.0 else float_of_int known *. 100. /. float_of_int total

let session_confidence (stats : review_stats list) : float * float * float =
  let total = List.length stats in
  let low, med, high =
    List.fold_left
      (fun (l, m, h) (_, _, _, conf) ->
        match conf with
        | Low -> (l + 1, m, h)
        | Medium -> (l, m + 1, h)
        | High -> (l, m, h + 1))
      (0, 0, 0) stats
  in
  if total = 0 then (0.0, 0.0, 0.0)
  else
    ( float_of_int low *. 100. /. float_of_int total,
      float_of_int med *. 100. /. float_of_int total,
      float_of_int high *. 100. /. float_of_int total )
