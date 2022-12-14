open Framework

(** The type [responses] represents the variety of generated customer responses.
    Each are based off of the player's input of price and recipe. *)
type responses =
  | Sour
  | Bland
  | JustRight
  | Expensive
  | Cheap
  | JustAlright

val customer_responses : t -> responses list -> responses list
(** [customer_responses t r] Returns a list of responses based off of the
    player's input of price and recipe.

    Invariant: The response [JustAlright] will always be one of the elements.

    Requires: [t] is a valid state and [r] a valid response list, the respomse d
    list cannot be empty. *)

val generate : responses list -> int -> string list
(** [generate f i] Returns a string list of customer feedbacks based off the
    given response list that depend on the player's input of price and recipe.
    Requires: [t] is a valid state and [r] is a valid int *)

val print_feedback : string list -> unit
(** [customer_responses s] Returns unit and prints the string list it was
    provided. Requires: [s] is a valid string list. *)
