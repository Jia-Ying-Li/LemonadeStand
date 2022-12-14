type options
(** The type [options] represents the different unit of ingredients you can
    purchase. *)

type bounds
(** The type [bounds] represents *)

type cup_contains

val set_bounds_lemons : bounds
val set_bounds_sugar : bounds
val set_bounds_water : bounds
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

(* val get_lemon_total_cost : float val get_cup_total_cost : float val
   get_sugar_total_cost : float val get_lemon_amt : float val get_cup_amt : int
   val get_sugar_amt : float *)
val set_cup : float -> float -> float -> cup_contains