open Quizcaml.Flashcard_review
open Quizcaml.Matching_game_frontend
open Quizcaml.Flashcards
open At.Testing

let () = run ()

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
  print_endline ""
