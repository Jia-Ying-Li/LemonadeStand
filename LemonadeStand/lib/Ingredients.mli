type options
type bounds
type cup_contains

val set_bounds_lemons : bounds
val set_bounds_sugar : bounds
val set_bounds_water : bounds
val purchase_option1 : options
val purchase_option2 : options
val purchase_option3 : options
(* val get_lemon_total_cost : options -> float val get_cup_total_cost : options
   -> float val get_sugar_total_cost : options -> float val get_lemon_amt :
   options -> float val get_cup_amt : options -> int val get_sugar_amt : options
   -> float *)

val get_lemon_total_cost : float
val get_cup_total_cost : float
val get_sugar_total_cost : float
val get_lemon_amt : float
val get_cup_amt : int
val get_sugar_amt : float
val init_cup : cup_contains
val get_cup : cup_contains