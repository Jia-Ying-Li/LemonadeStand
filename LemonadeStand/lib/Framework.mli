type stage
type t

val adjust_state : t -> t
val start_state : t
val init_state : t
val get_wallet : t -> float
val get_days_left : t -> int
val get_lemon_count : t -> float
val get_cup_count : t -> int
val get_sugar_count : t -> float
val add_water : t -> float -> t
val serve : t -> t
val buy_lemon : t -> float -> float -> t
val buy_cup : t -> int -> float -> t
val buy_sugar : t -> float -> float -> t
val add_sugar : t -> float -> t
val add_lemons : t -> float -> t
val sell : t -> t
