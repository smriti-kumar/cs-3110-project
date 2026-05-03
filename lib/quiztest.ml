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

(*Takes in a number and randomly outputs either a 1 or a 2 according to that
  number*)
let rec ran_num (seed : int) : int = (Random.int (seed + 2) mod 2) + 1

(*Takes in a list of "0"s and "1"s and outputs the corresponding grade,
  according to the logic that "0"s are questions they got wrong and "1"s are
  questions they got right. The output is a string list where the first element
  is the number of questions they got correct as a string, and the second is
  their percent grade as a string*)
let grader (scores : string list) (count : int) : string list =
  let cqs = List.fold_left ( + ) 0 (List.map int_of_string scores) in
  let perc = int_of_float (float_of_int cqs /. float_of_int count *. 100.) in
  [ string_of_int cqs; string_of_int perc ]

let speak (s : string) : string = if s = "1" then "RIGHT" else "WRONG"

let access (p : string * string) (mode : int) : string =
  match p with
  | i, j -> if mode = 1 then i else j

let accesstwo (p : string * string) (mode : int) : string list =
  match p with
  | i, j -> if mode = 1 then [ i; j ] else [ j; i ]

let rec mcq_set (acc : string list) (key : int list) (keychain : int list)
    (tdlist : (string * string) list) (mode : int) (n : int) : string list =
  if n >= 4 then acc
  else
    let choice =
      access (List.nth tdlist (List.nth key (List.nth keychain n))) mode
    in
    mcq_set (choice :: acc) key keychain tdlist mode (n + 1)

let mcq_builder (answer : int) (tdlist : (string * string) list) (mode : int) :
    string list * string list =
  let key = ran_list [ answer ] (List.length tdlist) 3 in
  let keychain = ran_list [] 4 4 in
  ( mcq_set [] key keychain tdlist mode 0,
    accesstwo (List.nth tdlist answer) mode )
