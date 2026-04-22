type card = string * string
type card_list = card list

(* Cards are added by trimming starting/ending whitespaces*)
let read_cards (filename : string) : card_list option =
  let data = Csv.load filename in
  let rec make_cards rows =
    match rows with
    | [] -> Some []
    | h :: t -> (
        match h with
        | [ term; def ] -> (
            match make_cards t with
            | Some c -> Some ((String.trim term, String.trim def) :: c)
            | None -> None)
        | _ ->
            (* maybe instead of failwith, redo the question (switch return type
               to option)*)
            print_endline
              "\n\
               The provided CSV file has one or more rows that don't contain \
               exactly 2 rows. Please fix the file!\n";
            None)
  in
  make_cards data

let upload_cards () : card_list option =
  print_endline
    "Please upload a two column CSV file with the entries in the first column \
     representing the terms and the corresponding entries in the second column \
     representing that definitions. Enter the path to the file below: ";
  try
    let filename = read_line () in
    read_cards filename
  with
  | Sys_error e ->
      (* maybe redo question instead of error*)
      print_endline
        "\n\
         The file could not be found or was not accessible. Please check the \
         path of the file!\n";
      None
  | Csv.Failure (_, _, _) ->
      print_endline
        "\n\
         The data in the file doesn't correspond to the CSV format so please \
         double check the formatting of this file!\n";
      None

let add_card_from_input curr term def : card_list =
  (String.trim term, String.trim def) :: curr

let add_card (curr : card_list) : card_list =
  print_string "Please enter the term for the card you want to add: ";
  let term = read_line () in
  print_string "Please enter the definition for the card you want to add: ";
  let def = read_line () in
  add_card_from_input curr term def

let remove_card_from_input curr rem_term : card_list =
  let rec remove_term (lst : (string * string) list) acc =
    match lst with
    | [] -> acc
    | (term, def) :: t ->
        if term = rem_term then remove_term t acc
        else remove_term t ((term, def) :: acc)
  in
  List.rev (remove_term curr [])

(* As of now, this removes all cards in the list with that term. Can be changed
   depending on how we want to handle duplicates.*)
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
    else if !choice = Some 3 then
      caml_cards := Matching_game_frontend.start_matching !caml_cards
  done
