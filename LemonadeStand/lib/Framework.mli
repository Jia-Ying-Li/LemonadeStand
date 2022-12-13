type stage =
  | Start
  | Purchasing
  | Adjusting
  | Feedback
  | Gameover

type t = {
  state : stage;
  days_left : int;
  wallet : float;
  lemon_count : float;
  cup_count : int;
  sugar_count : float;
  cup_lemon : float;
  cup_sugar : float;
  cup_water : float;
  price : float;
}

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
val add_cost : t -> float -> t
val get_cup_sugar_count : t -> float
val get_cup_lemon_count : t -> float
val get_cup_water_count : t -> float
val get_price : t -> float
val next_state : t -> t
val cup_sell : t -> float -> float -> int -> int -> int
val profit : t -> float
val return_state : t -> t

type ratio = {
  sour : float;
  sweet : float;
  water : float;
  cost : float;
}

val response_ratio : t -> ratio
