(* Confidence level assigned to a flashcard during flashcard review mode.
   Default low if card is skipped or confidence level is not selected. *)
type confidence =
  | Low
  | Medium
  | High

(* Flashcard record used for cross-session tracking of flashcard statistics.
   Contains (term, definition), confidence level, and number of times reviewed.
   Default is (term, definition), Low, 0. *)
type flashcard_record = (string * string) * confidence * int

(* Review statistics for a flashcard during a review session. Contains (term,
   definition), whether the card was flipped, whether the user marked it as
   known, and the confidence level. Default is (term, definition), false, false,
   Low. *)
type review_stats = (string * string) * bool * bool * confidence

(** [string_to_words s] splits [s] into a list of words using spaces as
    delimiters. *)
val string_to_words : string -> string list

(** [wrap_string s width] breaks [s] into a list of strings so that each line
    has length at most [width]. If a word has to be split up since including
    that word would exceed the width, that word is moved to the next line. *)
val wrap_string : string -> int -> string list

(** [center_string s width] returns a new string of length [width] where [s] is
    centered in the given [width] by adding spaces on both sides. *)
val center_string : string -> int -> string

(** [optimal_order stats] returns a reordered list of (term, definition) pairs
    based on their review priority given the stats from the most recent
    flashcard review session. Cards are prioritized in the following order to
    display to users: not flipped, flipped unknown low confidence, flipped
    unknown medium confidence, flipped unknown high confidence, flipped known
    low confidence, flipped known medium confidence, flipped known high
    confidence. *)
val optimal_order : review_stats list -> (string * string) list

(** [stat_to_csv session_num stat] converts a single [review_stats] value into a
    string that is CSV-formatted, and also adds the session number as the first
    value. *)
val stat_to_csv : int -> review_stats -> string

(** [csv_to_stat line] parses a CSV row into a [review_stats] value. Raises
    [Failure] if the input format is invalid. *)
val csv_to_stat : string -> review_stats

(** [get_session s] extracts the session number from a CSV row string and
    returns the first comma separated value parsed as an integer. *)
val get_session : string -> int

(** [read_last_row filename] returns the session number of the last row in the
    CSV file [filename] and returns 0 if the file is empty (indicates that the
    flashcard review mode has not yet been played on this set). Requires that
    the session number is the first value in the last row. *)
val read_last_row : string -> int

(** [stats_to_csv stats session_num] converts a list of [review_stats] into a
    multi-line CSV string for a given session. *)
val stats_to_csv : review_stats list -> int -> string

(** [load_last filename] loads all [review_stats] entries corresponding to the
    most recent session in the CSV file. *)
val load_last : string -> review_stats list

(** [load_all filename] loads all review history from the CSV file, returning a
    list of (session_number, review_stats) pairs. Each pair represents a review
    session and its corresponding statistics. *)
val load_all : string -> (int * review_stats) list

(** [group_sessions history] groups review statistics by session number. Returns
    a list of (session_number, review_stats list) sorted by session. *)
val group_sessions : (int * review_stats) list -> (int * review_stats list) list

(** [session_known stats] returns the percentage (0. to 100.) of cards marked as
    known in the given session. Returns 0. for an empty list. *)
val session_known : review_stats list -> float

(** [session_confidence stats] returns a tuple of percentages (low, medium,
    high) representing the distribution of confidence levels in the session;
    that is, the percent of cards in that session that were marked with low,
    medium, and high confidence. Returns (0., 0., 0.) for an empty list. *)
val session_confidence : review_stats list -> float * float * float
