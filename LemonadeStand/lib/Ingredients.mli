type options
(** The type [options] represents the different unit of ingredients you can
    purchase. *)

type bounds
(** The type [bounds] represents *)

type cup_contains

val purchase_option0 : options
val purchase_option1 : options
val purchase_option2 : options
val purchase_option3 : options
val get_lemon_total_cost : options -> float
val get_cup_total_cost : options -> float
val get_sugar_total_cost : options -> float
val get_lemon_amt : options -> float
val get_cup_amt : options -> int
val get_sugar_amt : options -> float
val set_cup : float -> float -> float -> cup_contains