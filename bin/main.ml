open Quizcaml.Flashcard_review

(* example flashcard set for testing purposes only: *)
let example = [("1 + 1", "2"); ("2 + 2", "4"); ("3 + 3", "6")]
let result = review_session [] example