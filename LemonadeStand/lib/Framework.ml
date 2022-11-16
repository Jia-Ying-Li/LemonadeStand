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

let sell state =
  let sell_count =
    min state.cup_count (min state.lemon_count state.sugar_count)
  in
  {
    days_left = state.days_left - 1;
    wallet = state.wallet +. float_of_int (sell_count * 4);
    lemon_count = state.lemon_count - sell_count;
    cup_count = state.cup_count - sell_count;
    sugar_count = state.sugar_count - sell_count;
  }

let frat_party state = { state with cup_count = 0 }
let fridge_broke state = { state with lemon_count = 0 }
let rat_infestation state = { state with sugar_count = 0 }
let bank_robbery state = { state with wallet = state.wallet -. 20.0 }
