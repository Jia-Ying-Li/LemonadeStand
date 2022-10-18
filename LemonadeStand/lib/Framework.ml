type t = {
  days : int;
  start_amt : float;
  wallet : float;
}

let init_struct = { days = 10; start_amt = 30.0; wallet = 30.0 }
let get_start_amt t = t.start_amt