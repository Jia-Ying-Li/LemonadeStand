type price = {
  amt : int;
  total_cost : float;
  unit_price : float;
}

type options = {
  lemon : price;
  cup : price;
  sugar : price;
}

let purchase_options =
  {
    lemon = { amt = 10; total_cost = 2.0; unit_price = 10.0 /. 2.0 };
    cup = { amt = 10; total_cost = 2.0; unit_price = 10.0 /. 2.0 };
    sugar =
      { amt = 10; total_cost = 2.0; unit_price = 10.0 /. 2.0 }
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
