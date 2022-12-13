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

let start_state =
  {
    state = Start;
    days_left = 10;
    wallet = 30.0;
    lemon_count = 0.;
    cup_count = 0;
    sugar_count = 0.;
    cup_lemon = 0.;
    cup_sugar = 0.;
    cup_water = 0.;
    price = 0.;
  }

let init_state =
  {
    state = Purchasing;
    days_left = 10;
    wallet = 30.0;
    lemon_count = 0.;
    cup_count = 0;
    sugar_count = 0.;
    cup_lemon = 0.;
    cup_sugar = 0.;
    cup_water = 0.;
    price = 0.;
  }

let get_wallet state = state.wallet
let get_days_left state = state.days_left
let get_lemon_count state = state.lemon_count
let get_cup_count state = state.cup_count
let get_sugar_count state = state.sugar_count
let get_cup_lemon_count state = state.cup_lemon
let get_cup_water_count state = state.cup_water
let get_cup_sugar_count state = state.cup_sugar
let get_price state = state.price

let adjust_state state =
  {
    state = Adjusting;
    days_left = state.days_left;
    wallet = state.wallet;
    lemon_count = state.lemon_count;
    cup_count = state.cup_count;
    sugar_count = state.sugar_count;
    cup_lemon = 0.;
    cup_sugar = 0.;
    cup_water = 0.;
    price = 0.;
  }

let add_water state count =
  {
    state = Purchasing;
    days_left = state.days_left;
    wallet = state.wallet;
    lemon_count = state.lemon_count;
    cup_count = state.cup_count;
    sugar_count = state.sugar_count;
    cup_lemon = state.cup_lemon;
    cup_sugar = state.cup_sugar;
    cup_water = count;
    price = 0.;
  }

let buy_lemon state count cost =
  {
    state = Purchasing;
    days_left = state.days_left;
    wallet = state.wallet -. cost;
    lemon_count = state.lemon_count +. count;
    cup_count = state.cup_count;
    sugar_count = state.sugar_count;
    cup_lemon = state.cup_lemon;
    cup_sugar = state.cup_sugar;
    cup_water = state.cup_water;
    price = 0.;
  }

let buy_cup state count cost =
  {
    state = Purchasing;
    days_left = state.days_left;
    wallet = state.wallet -. cost;
    lemon_count = state.lemon_count;
    cup_count = state.cup_count + count;
    sugar_count = state.sugar_count;
    cup_lemon = state.cup_lemon;
    cup_sugar = state.cup_sugar;
    cup_water = state.cup_water;
    price = 0.;
  }

let buy_sugar state count cost =
  {
    state = Purchasing;
    days_left = state.days_left;
    wallet = state.wallet -. cost;
    lemon_count = state.lemon_count;
    cup_count = state.cup_count;
    sugar_count = state.sugar_count +. count;
    cup_lemon = state.cup_lemon;
    cup_sugar = state.cup_sugar;
    cup_water = state.cup_water;
    price = 0.;
  }

let add_lemons state count =
  {
    state = Adjusting;
    days_left = state.days_left;
    wallet = state.wallet;
    lemon_count = state.lemon_count;
    cup_count = state.cup_count;
    sugar_count = state.sugar_count;
    cup_lemon = count;
    cup_sugar = state.cup_sugar;
    cup_water = state.cup_water;
    price = 0.;
  }

let add_sugar state count =
  {
    state = Adjusting;
    days_left = state.days_left;
    wallet = state.wallet;
    lemon_count = state.lemon_count;
    cup_count = state.cup_count;
    sugar_count = state.sugar_count;
    cup_lemon = state.cup_lemon;
    cup_sugar = count;
    cup_water = state.cup_water;
    price = 0.;
  }

let compare_to_optimal ingr = 0

let serve state i =
  let (sell_count : float) =
    min (float_of_int state.cup_count) (min state.lemon_count state.sugar_count)
  in
  {
    state = Feedback;
    days_left = state.days_left - 1;
    wallet = state.wallet +. (sell_count *. i);
    lemon_count = state.lemon_count -. sell_count;
    cup_count = state.cup_count - int_of_float sell_count;
    sugar_count = state.sugar_count -. sell_count;
    cup_lemon = state.cup_lemon;
    cup_sugar = state.cup_sugar;
    cup_water = state.cup_water;
    price = i;
  }
(* let serve state = { state = Feedback; days_left = state.days_left - 1; wallet
   = state.wallet +. 5.0; lemon_count = state.lemon_count; cup_count =
   state.cup_count; sugar_count = state.sugar_count; } *)

let frat_party state = { state with cup_count = 0 }
let fridge_broke state = { state with lemon_count = 0. }
let rat_infestation state = { state with sugar_count = 0. }
let bank_robbery state = { state with wallet = state.wallet -. 20.0 }