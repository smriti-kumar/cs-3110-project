(** [correctness guess answer] ouputs a "0" or a "1 depending" on whether
    [guess] is "correct" or not. [guess] is "correct" if it it is two or less
    edits away from [answer] ignoring capitalization and leading/trailing white
    space. Returns "1" if [guess] is correct, and "0" if it is not*)
val correctness : string -> string -> string

(**[ran_list acc size progress]* outputs a list of integers from 0 to [size] in
   a random order with no repeating numbers. [progress] is how many numbers will
   be in the outputted int list (the size of the list). Requires that [progress]
   is less than or equal to [size]*)
val ran_list : int list -> int -> int -> int list

(**[ran_num seed] randomly outputs either a 1 or 2. Requires that [seed] >= 0*)
val ran_num : int -> int

(**[grader scores count] outputs the grade corresponding to [scores]. [scores]
   is an int list of "0"s and "1s", and it is graded according to the logic that
   "0"s are questions they got wrong and "1"s are questions they got right out
   of a total of [count] questions. The output is a string list where the first
   element is the number of questions they got correct as a string, and the
   second is their percent grade as a string**)
val grader : string list -> int -> string list

(**[speak s] takes in a string [s] and outputs "RIGHT" if [s] = "1" and "WRONG"
   otherwise*)
val speak : string -> string

(**[mcq_builder answer tdlist mode] outputs the set of answers for a multiple
   choice question with [answer] being the position of the correct answer in the
   tdlist. Requires 0 =< [answer] < List.length [tdlist]. [mode] determines
   whether the term or definition in a pair is considered an answer.*)
val mcq_builder :
  int -> (string * string) list -> int -> string list * string list
