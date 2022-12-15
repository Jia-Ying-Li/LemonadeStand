(** The type [stage] represents the stage the player is in. Invariant: The
    player must start at the [Start] stage and remain at a stage throughout *)
type stage =
  | Start
  | Purchasing
  | Adjusting
  | Feedback
  | Gameover

type t
(** The abstract type [t] represents the game state the player is in, each state
    contains information about the player's current status. *)

val init_state : t
(** [init_state] is the [Start] stage of the game and the player can enter any
    key to proceed to the next stage. *)

val purchasing_state : t
(** [purchasing_state] is the initial [Purchasing] stage of the game where
    players are allowed to purchase their ingredients. The player can enter
    "end" to proceed to the next stage. *)

val adjusting_state : t
(** [adjusting_state] is the initial [Adjusting] stage of the game where players
    are allowed to adjust their recipe. The player can enter "end" to proceed to
    the next stage. *)

val feedback_state : t
(** [feedback_state] is the initial [Feedback] stage of the game where players
    get feedback on their recipe. The player can enter "end" to proceed to the
    next stage. *)

val get_wallet : t -> float
(** [get_wallet t] Returns the wallet of the player's current state. Requires:
    [t] is a valid state. *)

val get_days_left : t -> int
(** [get_days_left t] Returns the days remaining of the player's current state.
    Requires: [t] is a valid state. *)

val get_lemon_count : t -> float
(** [get_lemon_count t] Returns the inventory lemon count of the player's
    current state. Requires: [t] is a valid state. *)

val get_cup_count : t -> int
(** [get_cup_count t] Returns the inventory cup count of the player's current
    state. Requires: [t] is a valid state. *)

val get_sugar_count : t -> float
(** [get_sugar_count t] Returns the inventory sugar count of the player's
    current state. Requires: [t] is a valid state. *)

val get_cup_sugar_count : t -> float
(** [get_cup_sugar_count t] Returns the player's input of their recipe sugar
    count. Requires: [t] is a valid state. *)

val get_cup_lemon_count : t -> float
(** [get_cup_lemon_count t] Returns the player's input of their recipe lemon
    count. Requires: [t] is a valid state. *)

val get_cup_water_count : t -> float
(** [get_cup_water_count t] Returns the player's input of their recipe water
    count. Requires: [t] is a valid state. *)

val get_price : t -> float
(** [get_price t] Returns the player's input of their set lemonade price.
    Requires: [t] is a valid state. *)

val buy_lemon : t -> float -> float -> t
(** [buy_lemon t n c] Returns the altered state after the player processed the
    [purchase lemon] command. Requires: [t] is a valid state, [n] is a valid
    float and [t] is a valid float. *)

val buy_cup : t -> int -> float -> t
(** [buy_cup t n c] Returns the altered state after the player processed the
    [purchase cup] command. Requires: [t] is a valid state, [n] is a valid float
    and [t] is a valid float. *)

val buy_sugar : t -> float -> float -> t
(** [buy_sugar t n c] Returns the altered state after the player processed the
    [purchase sugar] command. Requires: [t] is a valid state, [n] is a valid
    float and [t] is a valid float. *)

val add_sugar : t -> float -> t
(** [add_sugar t n] Returns the altered state after the player processed the
    [add sugar] command. Requires: [t] is a valid state and [n] is a valid
    float. *)

val add_lemons : t -> float -> t
(** [add_lemons t n] Returns the altered state after the player processed the
    [add lemon] command. Requires: [t] is a valid state and [n] is a valid
    float.*)

val add_water : t -> float -> t
(** [add_water t n] Returns the altered state after the player processed the
    [add water] command. Requires: [t] is a valid state and [n] is a valid
    float.*)

val add_cost : t -> float -> t
(** [add_cost t n] Returns the altered state after the player processed the
    [add price] command. Requires: [t] is a valid state and [n] is a valid
    float.*)

val serve : t -> t
(** [serve t] Returns the altered state after the player processed the [serve]
    command. Requires: [t] is a valid state.*)

val return_state : t -> t
(** [return_state t] Returns the player's current state. Requires: [t] is a
    valid state.*)

val cup_sell : t -> float -> float -> int -> int -> int
(** [cup_sell t l s c acc] Returns the number of lemonade the player can create
    strictly from just their inventory and recipe. Requires: [t] is a valid
    state, [l] is a valid float, [s] is a valid float, [c] is a valid int and
    [acc] is a valid int.*)

val profit : t -> float
(** [profit t] Returns the player's profit after selling their lemonade.
    Calculation are based off the player's recipe and the cost they set.
    Requires: [t] is a valid state.*)

val cup_ready : t -> float
(** [cup_ready t] Returns the number of lemonade the player's will sell.
    Calculation are based off the player's recipe. Requires: [t] is a valid
    state.*)

val test_set : float -> float -> float -> float -> t
(** [test_set l s w p] Returns the state created using the given parameter.
    Requires: [t] is a valid state, [l] is a valid float, [s] is a valid float,
    [w] is a valid float and [p] is a valid float.*)
