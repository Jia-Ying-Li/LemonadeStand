type t = {
  days_left : int;
  wallet : float;
  lemon_count : int;
  cup_count : int;
  sugar_count : int;
}

let init_state =
  {
    days_left = 10;
    wallet = 30.0;
    lemon_count = 0;
    cup_count = 0;
    sugar_count = 0;
  }

let get_wallet state = state.wallet
let get_days_left state = state.days_left
let get_lemon_count state = state.lemon_count
let get_cup_count state = state.cup_count
let get_sugar_count state = state.sugar_count

let buy_lemon state count cost =
  {
    days_left = state.days_left;
    wallet = state.wallet -. cost;
    lemon_count = state.lemon_count + count;
    cup_count = state.cup_count;
    sugar_count = state.sugar_count;
  }

let buy_cup state count cost =
  {
    days_left = state.days_left;
    wallet = state.wallet -. cost;
    lemon_count = state.lemon_count;
    cup_count = state.cup_count + count;
    sugar_count = state.sugar_count;
  }

let buy_sugar state count cost =
  {
    days_left = state.days_left;
    wallet = state.wallet -. cost;
    lemon_count = state.lemon_count;
    cup_count = state.cup_count;
    sugar_count = state.sugar_count + count;
  }
