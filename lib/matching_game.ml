(*Matching game*)
(*Randomly choose terms + defs *)
(*Give correct/inc*)

let sample_terms : (string * string) list =
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

(*Hold all chosen words used for the game*)
let game_arr : (string * string) array = Array.make 10 ("", "")

(*Hold words*)
let word_arr : string array = Array.make 10 ""

(*Hold all defs*)
let def_arr : string array = Array.make 10 ""

(*Array.make ideal_length default_elem*)
(*Array.iter function array*)
(*Array.init function_taking_index array*)
(*Array.map2 function array1 array2*)

(*Randomly choose 10 word-def pairs*)
let start_game () : unit =
  let terms_arr : (string * string) array = Array.of_list sample_terms in
  (*Initialize random*)
  let () = Random.self_init () in
  Array.shuffle ~rand:Random.int terms_arr;
  (*From the shuffles array, choose 1st 10 pairs*)
  for i = 0 to 9 do
    let game_term : string * string = terms_arr.(i) in
    game_arr.(i) <- game_term
  done

(*Startup matching*)
let start_matching () : unit =
  begin
    print_endline "Match each word on the left to a definition on the right";
    start_game ();
    (*Just make sure pairs were correctly selected*)
    (*Print all pairs*)
    print_endline
      (Array.fold_left
         (fun acc (term, def) -> acc ^ term ^ " = " ^ def ^ ";  ")
         "" game_arr)
  end
