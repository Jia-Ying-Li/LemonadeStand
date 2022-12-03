type price = {
  amt : float;
  total_cost : float;
  unit_price : float;
}

type options = {
  lemon : price;
  cup : price;
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

let set_bounds_lemons = { upper_bound = 1.; lower_bound = 2.; optimal = 1.5 }
let set_bounds_sugar = { upper_bound = 2.25; lower_bound = 1.5; optimal = 2. }
let set_bounds_water = { upper_bound = 0.75; lower_bound = 1.25; optimal = 1. }

let purchase_options =
  {
    lemon = { amt = 10.; total_cost = 2.0; unit_price = 10.0 /. 2.0 };
    cup = { amt = 10.; total_cost = 2.0; unit_price = 10.0 /. 2.0 };
    sugar =
      { amt = 10.; total_cost = 2.0; unit_price = 10.0 /. 2.0 }
      (* lemon = [ { amt = 10; total_cost = 2.0; unit_price = 10.0 /. 2.0 }; {
         amt = 20; total_cost = 3.75; unit_price = 20.0 /. 3.75 }; ]; cups = [ {
         amt = 10; total_cost = 2.0; unit_price = 10.0 /. 2.0 }; { amt = 20;
         total_cost = 3.75; unit_price = 20.0 /. 3.75 }; ]; sugar = [ { amt =
         10; total_cost = 2.0; unit_price = 10.0 /. 2.0 }; { amt = 20;
         total_cost = 3.75; unit_price = 20.0 /. 3.75 }; ]; *);
  }

let get_lemon_total_cost = purchase_options.lemon.total_cost
let get_lemon_amt = purchase_options.lemon.amt
let get_cup_total_cost = purchase_options.cup.total_cost
let get_cup_amt = purchase_options.cup.amt
let get_sugar_total_cost = purchase_options.sugar.total_cost
let get_sugar_amt = purchase_options.sugar.amt
let init_cup = { lemon = 0.; sugar = 0.; water = 0. }

let get_cup =
  { lemon = init_cup.lemon; sugar = init_cup.sugar; water = init_cup.water }
