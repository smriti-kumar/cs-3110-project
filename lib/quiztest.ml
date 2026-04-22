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

let speak (s : string) : string = if s = "1" then "RIGHT" else "WRONG
