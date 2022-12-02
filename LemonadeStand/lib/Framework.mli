type t

val init_state : t
val get_wallet : t -> float
val get_days_left : t -> int
val get_lemon_count : t -> int
val get_cup_count : t -> int
val get_sugar_count : t -> int
val add_water : t -> int -> t
val serve : t -> t
val buy_lemon : t -> int -> float -> t
val buy_cup : t -> int -> float -> t
val buy_sugar : t -> int -> float -> t
val add_sugar : t -> int -> t
val add_lemons : t -> int -> t
val sell : t -> t
