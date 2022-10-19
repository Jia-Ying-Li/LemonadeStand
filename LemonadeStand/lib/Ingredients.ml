open Framework

type price = {
  amt : int;
  total_cost : float;
  unit_price : float;
}

type option = {
  lemon : price list;
  cups : price list;
  sugar : price list;
}

type ingredient_owned = {
  balance : float;
  lemon : int;
  cups : int;
  sugar : int;
}

let init_shelf s =
  { balance = Framework.get_start_amt s; lemon = 0; cups = 0; sugar = 0 }

let options_to_buy =
  {
    lemon =
      [
        { amt = 10; total_cost = 2.0; unit_price = 10.0 /. 2.0 };
        { amt = 20; total_cost = 3.75; unit_price = 20.0 /. 3.75 };
      ];
    cups =
      [
        { amt = 10; total_cost = 2.0; unit_price = 10.0 /. 2.0 };
        { amt = 20; total_cost = 3.75; unit_price = 20.0 /. 3.75 };
      ];
    sugar =
      [
        { amt = 10; total_cost = 2.0; unit_price = 10.0 /. 2.0 };
        { amt = 20; total_cost = 3.75; unit_price = 20.0 /. 3.75 };
      ];
  }
