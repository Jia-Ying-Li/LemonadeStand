(* Satisfaction Value: composed of sweet, sour, water amount and cost. *)
type ratio

(* Customer Responses *)
type responses

val generate : responses list -> int -> string list
