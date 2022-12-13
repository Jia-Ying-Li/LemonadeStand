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

let return_state state =
  {
    state = Adjusting;
    days_left = state.days_left;
    wallet = state.wallet;
    lemon_count = state.lemon_count;
    cup_count = state.cup_count;
    sugar_count = state.sugar_count;
    cup_lemon = state.cup_lemon;
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

let add_cost state p =
  {
    state = Adjusting;
    days_left = state.days_left;
    wallet = state.wallet;
    lemon_count = state.lemon_count;
    cup_count = state.cup_count;
    sugar_count = state.sugar_count;
    cup_lemon = state.cup_lemon;
    cup_sugar = state.cup_sugar;
    cup_water = state.cup_water;
    price = p;
  }

let rec cup_sell state lemon sugar cup acc =
  if
    lemon -. get_cup_lemon_count state >= 0.
    && sugar -. get_cup_sugar_count state >= 0.
    && cup > 0
  then
    cup_sell state
      (lemon -. get_cup_lemon_count state)
      (sugar -. get_cup_sugar_count state)
      (cup - 1) (acc + 1)
  else acc

let profit state =
  float_of_int
    (cup_sell state state.lemon_count state.sugar_count state.cup_count 0)
  *. get_price state

let serve state =
  let sell_count =
    float_of_int
      (cup_sell state state.lemon_count state.sugar_count state.cup_count 0)
  in
  {
    state = Feedback;
    days_left = state.days_left - 1;
    wallet = state.wallet +. profit state;
    lemon_count = state.lemon_count -. (state.cup_lemon *. sell_count);
    cup_count = state.cup_count - int_of_float sell_count;
    sugar_count = state.sugar_count -. (state.cup_sugar *. sell_count);
    cup_lemon = state.cup_lemon;
    cup_sugar = state.cup_sugar;
    cup_water = state.cup_water;
    price = state.price;
  }

(* let serve state = { state = Feedback; days_left = state.days_left - 1; wallet
   = state.wallet +. 5.0; lemon_count = state.lemon_count; cup_count =
   state.cup_count; sugar_count = state.sugar_count; } *)
type ratio = {
  sour : float;
  sweet : float;
  water : float;
  cost : float;
}

let set_lemon = 4.
let set_sugar = 4.
let set_water = 1.
let set_cost = 3.

let response_ratio state =
  {
    sour = state.cup_lemon /. set_lemon;
    sweet = state.cup_sugar /. set_sugar;
    water = state.cup_water /. set_water;
    cost = state.price /. set_cost;
  }

let next_state state = { state with days_left = state.days_left - 1 }
let frat_party state = { state with cup_count = 0 }
let fridge_broke state = { state with lemon_count = 0. }
let rat_infestation state = { state with sugar_count = 0. }
let bank_robbery state = { state with wallet = state.wallet -. 20.0 }