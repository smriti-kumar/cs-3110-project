open OUnit2
open Quizcaml.Flashcard_review

let tests =
  "quizcaml test suite"
  >::: [
         ( "string_to_words splits correctly" >:: fun _ ->
           assert_equal
             ~printer:(fun lst -> "[" ^ String.concat "; " lst ^ "]")
             [ "hello"; "world" ]
             (string_to_words "hello world") );
         ( "string_to_words handles single word" >:: fun _ ->
           assert_equal
             ~printer:(fun lst -> "[" ^ String.concat "; " lst ^ "]")
             [ "hello" ] (string_to_words "hello") );
         ( "wrap_string with basic wrapping" >:: fun _ ->
           assert_equal
             ~printer:(fun lst -> "[" ^ String.concat "; " lst ^ "]")
             [ " hello"; "world" ]
             (wrap_string "hello world" 6) );
         ( "wrap_string with no wrapping needed" >:: fun _ ->
           assert_equal
             ~printer:(fun lst -> "[" ^ String.concat "; " lst ^ "]")
             [ " hello world" ]
             (wrap_string "hello world" 20) );
         ( "center_string centers properly" >:: fun _ ->
           assert_equal "  hi  " (center_string "hi" 6) );
         ( "stat_to_csv produces 6 comma-separated fields" >:: fun _ ->
           let row = stat_to_csv 1 (("term1", "def1"), true, false, Low) in
           let fields = String.split_on_char ',' row in
           assert_equal ~printer:string_of_int 6 (List.length fields) );
         ( "stat_to_csv places session number as first field" >:: fun _ ->
           let row = stat_to_csv 3 (("term1", "def1"), true, true, High) in
           let first = List.nth (String.split_on_char ',' row) 0 in
           assert_equal ~printer:(fun s -> s) "3" first );
         ( "csv_to_stat correctly assigns a Low confidence stat" >:: fun _ ->
           let stat = (("term1", "def1"), true, false, Low) in
           let row = stat_to_csv 1 stat in
           let card, flipped, known, conf = csv_to_stat row in
           assert_equal ("term1", "def1") card;
           assert_equal true flipped;
           assert_equal false known;
           assert_equal Low conf );
         ( "csv_to_stat correctly assigns a High confidence known stat"
         >:: fun _ ->
           let stat = (("term2", "def2"), true, true, High) in
           let row = stat_to_csv 2 stat in
           let card, flipped, known, conf = csv_to_stat row in
           assert_equal ("term2", "def2") card;
           assert_equal true flipped;
           assert_equal true known;
           assert_equal High conf );
         ( "csv_to_stat correctly assigns a (card, false, false, Low) stat"
         >:: fun _ ->
           let stat = (("term3", "def3"), false, false, Low) in
           let row = stat_to_csv 1 stat in
           let card, flipped, known, conf = csv_to_stat row in
           assert_equal ("term3", "def3") card;
           assert_equal false flipped;
           assert_equal false known;
           assert_equal Low conf );
         ( "csv_to_stat correctly converts term and definition with spaces"
         >:: fun _ ->
           let card = ("a long term name", "a long definition here") in
           let stat = (card, true, true, Medium) in
           let row = stat_to_csv 1 stat in
           let card', _, _, _ = csv_to_stat row in
           assert_equal card card' );
         ( "get_session extracts session number from csv row" >:: fun _ ->
           assert_equal ~printer:string_of_int 5
             (get_session "5,\"term\",\"def\",true,false,Low") );
         ( "get_session parses session 1 correctly" >:: fun _ ->
           assert_equal ~printer:string_of_int 1
             (get_session "1,\"t\",\"d\",false,false,Low") );
         ( "stats_to_csv produces one row for one stat" >:: fun _ ->
           let csv = stats_to_csv [ (("term1", "def1"), true, false, Low) ] 1 in
           let lines = String.split_on_char '\n' csv in
           assert_equal ~printer:string_of_int 1 (List.length lines) );
         ( "stats_to_csv produces three rows for three stats" >:: fun _ ->
           let stats =
             [
               (("term1", "def1"), true, false, Low);
               (("term2", "def2"), true, false, High);
               (("term3", "def3"), true, true, Medium);
             ]
           in
           let csv = stats_to_csv stats 1 in
           let lines = String.split_on_char '\n' csv in
           assert_equal ~printer:string_of_int 3 (List.length lines) );
         ( "optimal_order places (card, false, false, Low) first" >:: fun _ ->
           let stats =
             [
               (("term1", "def1"), true, true, High);
               (("term2", "def2"), false, false, Low);
             ]
           in
           let order = optimal_order stats in
           assert_equal
             ~printer:(fun (t, _) -> t)
             ("term2", "def2") (List.nth order 0) );
         ( "optimal_order places unknown low before unknown medium" >:: fun _ ->
           let stats =
             [
               (("term2", "def2"), true, false, Medium);
               (("term1", "def1"), true, false, Low);
             ]
           in
           let order = optimal_order stats in
           assert_equal
             ~printer:(fun (t, _) -> t)
             ("term1", "def1") (List.nth order 0) );
         ( "optimal_order places unknown low before known low" >:: fun _ ->
           let stats =
             [
               (("term2", "def2"), true, true, Low);
               (("term1", "def1"), true, false, Low);
             ]
           in
           let order = optimal_order stats in
           assert_equal
             ~printer:(fun (t, _) -> t)
             ("term1", "def1") (List.nth order 0) );
         ( "optimal_order places known high last" >:: fun _ ->
           let stats =
             [
               (("term1", "def1"), true, true, High);
               (("term2", "def2"), true, false, Low);
               (("term3", "def3"), true, true, Low);
             ]
           in
           let order = optimal_order stats in
           let last = List.nth order (List.length order - 1) in
           assert_equal ~printer:(fun (t, _) -> t) ("term1", "def1") last );
         ( "optimal_order preserves all cards" >:: fun _ ->
           let stats =
             [
               (("term1", "def1"), true, false, Low);
               (("term2", "def2"), true, true, High);
               (("term3", "def3"), false, false, Low);
               (("term1", "def1"), true, false, Medium);
             ]
           in
           let order = optimal_order stats in
           assert_equal ~printer:string_of_int (List.length stats)
             (List.length order) );
         ( "optimal_order returns empty list when input is empty" >:: fun _ ->
           assert_equal [] (optimal_order []) );
         ( "optimal_order returns a single card for input of size 1" >:: fun _ ->
           let stats = [ (("term1", "def1"), true, false, Low) ] in
           assert_equal [ ("term1", "def1") ] (optimal_order stats) );
         ( "read_last_row returns last session number" >:: fun _ ->
           let dir =
             let folder = "data" in
             if not (Sys.file_exists folder) then
               ignore (Sys.command "mkdir -p data");
             folder
           in
           let file =
             Filename.temp_file ~temp_dir:dir "test_read_last_row" ".csv"
           in
           let oc = open_out file in
           output_string oc
             "session,term,definition,flipped,known,confidence\n\
              1,\"a\",\"b\",true,false,Low\n\
              2,\"c\",\"d\",false,true,High\n";
           close_out oc;

           assert_equal ~printer:string_of_int 2 (read_last_row file) );
         ( "load_last only loads last session rows" >:: fun _ ->
           let dir =
             let folder = "data" in
             if not (Sys.file_exists folder) then
               ignore (Sys.command "mkdir -p data");
             folder
           in
           let file =
             Filename.temp_file ~temp_dir:dir "test_load_last" ".csv"
           in
           let oc = open_out file in
           output_string oc
             "session,term,definition,flipped,known,confidence\n\
              1,\"a\",\"b\",true,false,Low\n\
              2,\"c\",\"d\",false,true,High\n\
              2,\"e\",\"f\",true,false,Medium\n";
           close_out oc;

           let result = load_last file in
           assert_equal ~printer:string_of_int 2 (List.length result) );
       ]

let _ = run_test_tt_main tests
