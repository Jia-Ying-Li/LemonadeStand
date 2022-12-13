(* Satisfaction Value: composed of sweet, sour, water amount and cost. *)
open Framework

(* Customer Responses *)
type responses =
  | Sour
  | Bland
  | JustRight
  | Expensive
  | Cheap
  | JustAlright

val customer_responses : t -> responses list -> responses list
val generate : responses list -> int -> string list
val print_feedback : string list -> int -> unit
