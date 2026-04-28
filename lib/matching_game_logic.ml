(*Matching game backend*)
(*Randomly choose terms + defs *)
(*Give correct/inc*)
(*Assumes for each word, there is only 1 def*)

let sample_terms : (string * string) list ref =
  ref
    [
      ("Term1", "Def1");
      ("Term2", "Def2");
      ("Term3", "Def3");
      ("Term4", "Def4");
      ("Term5", "Def5");
      ("Term6", "Def6");
      ("Term7", "Def7");
      ("Term8", "Def8");
      ("Term9", "Def9");
      ("Term10", "Def10");
      ("Term11", "Def11");
      ("Term12", "Def12");
      ("Term13", "Def13");
      ("Term14", "Def14");
      ("Term15", "Def15");
    ]

(*Set the value of cards used*)
let set_flashcards_value (flashcards_list : (string * string) list) : unit =
  begin
    sample_terms := flashcards_list
  end

(*Hold all 10 word+def pairs chosen for the game*)
let game_arr : (string * string) array = Array.make 10 ("", "")

(*Hold words*)
let word_arr : string array = Array.make 10 ""

(*Hold all defs*)
let def_arr : string array = Array.make 10 ""

(*Hold number-word associations*)
let word_assn : (int * string) array ref = ref (Array.make 10 (0, ""))

(*Hold char-def associations*)
let def_assn : (string * string) array ref = ref (Array.make 10 ("z", ""))

(*Hold number of incorrect matching attempts*)
let num_inc : int ref = ref 0

(*Hold number of correct matching attempts*)
let num_corr : int ref = ref 0

(*In case of repeated matching games with same user, reset everything to initial
  state*)
let reset_game () : unit =
  begin
    num_inc := 0;
    num_corr := 0;
    word_assn := Array.make 10 (0, "");
    def_assn := Array.make 10 ("z", "");
    for i = 0 to 9 do
      game_arr.(i) <- ("", "");
      word_arr.(i) <- "";
      def_arr.(i) <- ""
    done
  end

(*Randomly choose 10 word-def pairs*)

(*Associate each element in word_arr and def_arr with a character*)
let assign_print () : unit =
  begin
    (*assign words to a number from [1,10]*)
    let all_nums : int array = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |] in
    for i = 0 to 9 do
      !word_assn.(i) <- (all_nums.(i), word_arr.(i))
    done;

    (*assign words to a number from [1,10]*)
    let all_strs : string array =
      [| "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" |]
    in
    Array.shuffle ~rand:Random.int def_arr;
    for i = 0 to 9 do
      (*Shuffle order of elements in def_arr*)
      !def_assn.(i) <- (all_strs.(i), def_arr.(i))
    done
  end

(*Update arrays when correct matches found*)
let update_arrs (word_num : int) (def_str : string) : unit =
  begin
    (*For each word in word_assn, if it is not the matched word, include it in
      def_assn*)
    let word_assn_list : (int * string) list = Array.to_list !word_assn in
    let def_assn_list : (string * string) list = Array.to_list !def_assn in
    let word_assn_2_lst : (int * string) list =
      List.filter (fun (num, wd) -> num <> word_num) word_assn_list
    in
    let def_assn_2_lst : (string * string) list =
      List.filter (fun (str, df) -> str <> def_str) def_assn_list
    in
    word_assn := Array.of_list word_assn_2_lst;
    def_assn := Array.of_list def_assn_2_lst
  end

(*Check if a guess is correct and update remaining match choices + score accordingly*)
(*As of now, requires the guess is valid*)
let check_guess (guess : string) : bool =
  let guess_info : string list = String.split_on_char ' ' guess in
  let word_num : int = int_of_string (List.nth guess_info 0) in
  let def_str : string = List.nth guess_info 1 in
  (*find word guessed from word_assn array*)
  let word_val : string =
    snd
      (Option.get (Array.find_opt (fun (num, wd) -> num = word_num) !word_assn))
  in
  (*find def guessed from def_assn based on associated string*)
  (*find index for that guess in def_assn*)
  let def_str_index : int =
    Option.get (Array.find_index (fun (c, def) -> c = def_str) !def_assn)
  in
  let def_val : string = snd !def_assn.(def_str_index) in
  (*Check correctness*)
  let corr : bool =
    Array.for_all
      (fun (word, def) ->
        begin if word = word_val then
          begin if def = def_val then begin
            (*Increase number of correct guesses*)
            num_corr := !num_corr + 1;
            (*Update word_assn and def_assn to not include matches already
              correctly found*)
            update_arrs word_num def_str;
            true
          end
          else begin
            (*increase count of incorrect guesses*)
            num_inc := !num_inc + 1;
            false
          end
          end
        else true
        end)
      game_arr
  in
  corr

let start_game_logic (flashcards_list : (string * string) list) : unit =
  begin
    reset_game ();
    set_flashcards_value flashcards_list;
    let terms_arr : (string * string) array = Array.of_list !sample_terms in
    (*Initialize random*)
    let () = Random.self_init () in
    Array.shuffle ~rand:Random.int terms_arr;
    (*From the shuffles array, choose 1st 10 pairs*)
    for i = 0 to 9 do
      let game_term : string * string = terms_arr.(i) in
      game_arr.(i) <- game_term
    done;

    (*Make word_arr*)
    for i = 0 to 9 do
      let word_term : string = fst terms_arr.(i) in
      word_arr.(i) <- word_term
    done;

    (*Make def_arr*)
    for i = 0 to 9 do
      let def_term : string = snd terms_arr.(i) in
      def_arr.(i) <- def_term
    done;
    assign_print ()
  end
