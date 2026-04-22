(*Helper function that reads the result of get_distance and determines if an
  input is within the acceptable margin of error*)
let in_bounds (s1 : string) (s2 : string) : bool =
  match Mula.Strings.Dem.get_distance ~k:2 s1 s2 with
  | None -> false
  | Some x -> true

(*Determines whether a guess is correct or not. A guess is correct if it it is
  two or less character edits away from the answer ignoring capitalization and
  leading/trailing white space. Correct guesses are marked as "1" and incorrect
  guesses are marked as "0"*)
let correctness (guess : string) (answer : string) : string =
  let gs = String.trim (String.lowercase_ascii guess) in
  let ans = String.trim (String.lowercase_ascii answer) in
  if gs = ans then "1" else if in_bounds gs ans then "1" else "0"

(*Intializes the random number generator.*)
let () = Random.self_init ()

(*Creates a list of integers from 0 to size with no repeats. Progress is the
  size of the outputted random list, or how many numbers will be in the list.
  Progress has the invariant that it cannot be greater than size.*)
let rec ran_list (acc : int list) (size : int) (progress : int) : int list =
  match progress with
  | 0 -> acc
  | _ ->
      let num = Random.int size in
      if List.for_all (fun x -> x != num) acc then
        ran_list (num :: acc) size (progress - 1)
      else ran_list acc size progress

(*Takes in a list of "0"s and "1"s and outputs the corresponding grade. The
  output is a string list where the first element is the number of questions
  they got correct, and the second is their percent grade*)
let grader (scores : string list) (count : int) : string list =
  let cqs = List.fold_left ( + ) 0 (List.map int_of_string scores) in
  let perc = int_of_float (float_of_int cqs /. float_of_int count *. 100.) in
  [ string_of_int cqs; string_of_int perc ]

let speak (s : string) : string = if s = "1" then "RIGHT" else "WRONG"

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
  print_endline "";
  tdlist
