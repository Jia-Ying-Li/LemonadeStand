(* Satisfaction Value: composed of sweet, sour, water amount and cost. *)
type ratio = {
  sour : float;
  sweet : float;
  water : float;
  cost : float;
}

(* Customer Responses *)
type responses =
  | Sour
  | Bland
  | JustRight
  | Expensive
  | Cheap
  | JustAlright

val customer_responses : ratio -> responses list -> responses list
val generate : responses list -> int -> string list
