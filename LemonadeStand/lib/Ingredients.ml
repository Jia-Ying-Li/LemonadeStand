type cup_price = {
  amt : int;
  total_cost : float;
  unit_price : float;
}

type price = {
  amt : float;
  total_cost : float;
  unit_price : float;
}

type options = {
  lemon : price;
  cup : cup_price;
  sugar : price;
}

type bounds = {
  upper_bound : float;
  lower_bound : float;
  optimal : float;
}

type cup_contains = {
  lemon : float;
  sugar : float;
  water : float;
}

let set_bounds_lemons = { upper_bound = 2.; lower_bound = 1.; optimal = 1.5 }
let set_bounds_sugar = { upper_bound = 2.25; lower_bound = 1.5; optimal = 2. }
let set_bounds_water = { upper_bound = 0.75; lower_bound = 1.25; optimal = 1. }

let purchase_option1 =
  {
    lemon = { amt = 2.; total_cost = 1.5; unit_price = 2.0 /. 1.5 };
    cup = { amt = 3; total_cost = 2.5; unit_price = 3.0 /. 2.5 };
    sugar = { amt = 6.; total_cost = 2.0; unit_price = 6. /. 2.0 };
  }

let purchase_option2 =
  {
    lemon = { amt = 5.; total_cost = 3.; unit_price = 5. /. 3. };
    cup = { amt = 7; total_cost = 5.; unit_price = 7.0 /. 5.0 };
    sugar = { amt = 10.; total_cost = 3.; unit_price = 10.0 /. 3.0 };
  }

let purchase_option3 =
  {
    lemon = { amt = 10.; total_cost = 5.; unit_price = 10.0 /. 5. };
    cup = { amt = 10; total_cost = 6.; unit_price = 10. /. 6.0 };
    sugar = { amt = 15.; total_cost = 4.; unit_price = 3.0 /. 2.0 };
  }

(* let get_lemon_total_cost (option : options) = option.lemon.total_cost let
   get_lemon_amt (option : options) = option.lemon.amt let get_cup_total_cost
   (option : options) = option.cup.total_cost let get_cup_amt (option : options)
   = option.cup.amt let get_sugar_total_cost (option : options) =
   option.sugar.total_cost let get_sugar_amt (option : options) =
   option.sugar.amt *)

let get_lemon_total_cost = purchase_option1.lemon.total_cost
let get_lemon_amt = purchase_option1.lemon.amt
let get_cup_total_cost = purchase_option1.cup.total_cost
let get_cup_amt = purchase_option1.cup.amt
let get_sugar_total_cost = purchase_option1.sugar.total_cost
let get_sugar_amt = purchase_option1.sugar.amt

let set_cup (i : float) (j : float) (k : float) =
  { lemon = i; sugar = j; water = k }
