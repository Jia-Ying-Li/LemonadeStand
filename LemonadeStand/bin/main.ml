open LemonadeStand
open Framework
open Ingredients
open Input

let handle_purchase state params =
  match params with
  | [] -> state
  | ingredient :: t ->
      if ingredient = "lemon" then
        Framework.buy_lemon state Ingredients.get_lemon_amt
          Ingredients.get_lemon_total_cost
      else if ingredient = "cup" then
        Framework.buy_cup state Ingredients.get_cup_amt
          Ingredients.get_cup_total_cost
      else if ingredient = "sugar" then
        Framework.buy_sugar state Ingredients.get_sugar_amt
          Ingredients.get_sugar_total_cost
      else Framework.init_state

let handle_sell state params = Framework.sell state

let handle_input state input =
  match Input.parse input with
  | Quit ->
      print_endline "Game ended";
      Framework.init_state
  | Purchase params -> handle_purchase state params
  | Sell params -> handle_sell state params

(* let print_purchase_options purchase_options = Printf.printf "Lemon: %i\n"
   purchase_options.amt; *)

let rec play_game new_state =
  print_endline "";
  Printf.printf "Days left: %i\n" (Framework.get_days_left new_state);
  Printf.printf "Money left: %f\n" (Framework.get_wallet new_state);
  Printf.printf "Lemons left: %i\n" (Framework.get_lemon_count new_state);
  Printf.printf "Cups left: %i\n" (Framework.get_cup_count new_state);
  Printf.printf "Sugars left: %i\n" (Framework.get_sugar_count new_state);
  print_endline "";

  print_endline "Lemon";
  Printf.printf "price: %f\n" Ingredients.get_lemon_total_cost;
  Printf.printf "amount: %i\n" Ingredients.get_lemon_amt;
  print_endline "";

  print_endline "Cup";
  Printf.printf "price: %f\n" Ingredients.get_cup_total_cost;
  Printf.printf "amount: %i\n" Ingredients.get_cup_amt;
  print_endline "";

  print_endline "Sugar";
  Printf.printf "price: %f\n" Ingredients.get_sugar_total_cost;
  Printf.printf "amount: %i\n" Ingredients.get_sugar_amt;
  print_endline "";
  print_endline
    "What would you like to purchase? You can purchase either 1) lemon 2) cup \
     3) sugar\n";
  print_endline "Commands:";
  print_endline "purchase <ingredient>";
  print_endline "sell";
  print_endline "quit";
  print_string "> ";

  match read_line () with
  | input -> play_game (handle_input new_state input)

let start_game = play_game Framework.init_state

let main () =
  print_endline "\n";
  print_endline "Welcome to the Lemonade Stand game.";
  print_endline "Enter any key to start.";
  print_string "> ";
  match read_line () with
  | random_input -> start_game

let () = main ()
