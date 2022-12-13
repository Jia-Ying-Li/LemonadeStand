open LemonadeStand
open Framework
open Ingredients
open Input
open Customers

exception GameEnded

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

let check_add state =
  if
    Framework.get_cup_count state = 0
    || Framework.get_lemon_count state < 2.
    || Framework.get_sugar_count state < 2.25
  then false
  else true

let handle_add state params =
  match params with
  | [] -> state
  | ingredient :: t ->
      if ingredient = "lemon" then
        let () =
          print_string "How many? Please input a value between 1 and 2 >  "
        in
        let i = read_float () in
        Framework.add_lemons state i
      else if ingredient = "sugar" then
        let () =
          print_string
            "How many tablespoons? Please input an number between 1.5 and 2.25 \
             >  "
        in
        let i = read_float () in
        Framework.add_sugar state i
      else if ingredient = "water" then
        let () =
          print_string
            "How many cups? Please input an number between 0.75 and 1.25 >  "
        in
        let i = read_float () in
        Framework.add_water state i
      else if ingredient = "price" then
        let () = print_string "set the price >  " in
        let i = read_float () in
        Framework.serve state i
      else Framework.init_state

let handle_set_price state =
  let () = print_string "Set the price of your lemonade >   " in
  let i = read_float () in
  Framework.serve state i

let lst_resp = [ JustAlright; Cheap ]

(* Make sure to decrement day and If typed in wrong command, next command no
   longer works *)
let rec handle_feedback state =
  Printf.printf "Feedback";
  print_endline "";

  Customers.print_feedback (generate lst_resp 10) 10;
  print_endline "";

  print_endline "Commands:";
  print_endline "[next]";
  print_endline "[quit]";

  print_endline "";
  print_string "> ";

  try
    match read_line () with
    | input -> (
        match Input.parse input with
        | Next -> purchase_game state
        | Quit ->
            print_endline "Game Ended";
            raise GameEnded
        | _ -> raise CommandNotFound)
  with CommandNotFound -> (
    print_endline "Invalid Command, Please Input in the correct format";
    print_string "> ";
    match read_line () with
    | input -> handle_feedback state)
(* Using the number of cups that can be made, multiply that number to the cost
   input. Using a formula, determine how many sales there will be base on the
   deviation from the optimal creation. Using the number of sale, display
   customer response *)

(* let handle_sell state params = Framework.sell state *)

and adjust_stage state input =
  match Input.parse input with
  | Serve -> handle_feedback state
  | Add params -> handle_add state params
  | _ -> failwith "Impossible"

and adjust_game new_state =
  print_endline "";
  Printf.printf "Days left: %i\n" (Framework.get_days_left new_state);
  Printf.printf "Money left: %f\n" (Framework.get_wallet new_state);
  Printf.printf "Lemons left: %f\n" (Framework.get_lemon_count new_state);
  Printf.printf "Cups left: %i\n" (Framework.get_cup_count new_state);
  Printf.printf "Sugars left: %f\n" (Framework.get_sugar_count new_state);
  print_endline "";
  Printf.printf "Lemons per cup: %f\n" (Framework.get_cup_lemon_count new_state);
  Printf.printf "Sugar per cup: %f\n" (Framework.get_cup_sugar_count new_state);
  Printf.printf "Water per cup: %f\n" (Framework.get_cup_water_count new_state);
  print_endline "";
  Printf.printf "Price of lemonade: %f\n" (Framework.get_price new_state);

  if
    Framework.get_cup_water_count new_state > 0.
    && Framework.get_cup_lemon_count new_state > 0.
    && Framework.get_cup_sugar_count new_state > 0.
  then
    print_endline
      "set the price of your lemonade using the function [add price]"
  else
    print_endline
      "Add ingredients to your jug of lemonade! Use the command <add \
       [ingredient]> to add 1)lemon 2)sugar or 3)water";

  print_endline "Commands:";
  print_endline "[add]";
  print_endline "[quit]";

  print_endline "";
  print_string "> ";
  (* Error Handling, make sure serve only works when all the inputs are done *)
  match read_line () with
  | input -> (
      match Input.parse input with
      | _ -> adjust_game (adjust_stage new_state input))
(*ignore (match read_line () with | input -> adjust_game (adjust_stage new_state
  input)); new_state*)

and purchase_stage state input =
  try
    match Input.parse input with
    | End -> adjust_game state
    | Quit ->
        print_endline "Game Ended";
        raise GameEnded
    | Purchase params -> handle_purchase state params
    | _ -> raise CommandNotFound
    (* | Sell params -> handle_sell state params *)
  with
  | CommandNotFound -> (
      print_endline "Invalid Command, Please Input in the correct format";
      print_string "> ";
      match read_line () with
      | input -> purchase_stage state input)
  | InvalidParameter -> (
      print_endline "Invalid Parameter, Please Input in a Valid Ingredient";
      print_string "> ";
      match read_line () with
      | input -> purchase_stage state input)
  | Empty -> (
      print_endline "Please Input a Valid Command ";
      print_string "> ";
      match read_line () with
      | input -> purchase_stage state input)

(* let print_purchase_options purchase_options = Printf.printf "Lemon: %i\n"
   purchase_options.amt; *)

and purchase_game new_state =
  print_endline "";
  Printf.printf "Days left: %i\n" (Framework.get_days_left new_state);
  Printf.printf "Money left: %f\n" (Framework.get_wallet new_state);
  Printf.printf "Lemons left: %f\n" (Framework.get_lemon_count new_state);
  Printf.printf "Cups left: %i\n" (Framework.get_cup_count new_state);
  Printf.printf "Sugars left: %f\n" (Framework.get_sugar_count new_state);
  print_endline "";

  print_endline "Lemon";
  Printf.printf "price: %f\n" Ingredients.get_lemon_total_cost;
  Printf.printf "amount: %f\n" Ingredients.get_lemon_amt;
  print_endline "";

  print_endline "Cup";
  Printf.printf "price: %f\n" Ingredients.get_cup_total_cost;
  Printf.printf "amount: %i\n" Ingredients.get_cup_amt;
  print_endline "";

  print_endline "Sugar";
  Printf.printf "price: %f\n" Ingredients.get_sugar_total_cost;
  Printf.printf "amount: %f\n" Ingredients.get_sugar_amt;
  print_endline "";
  print_endline
    "Would you like to purchase ingredients? You can purchase either 1) lemon \
     2) cup 3) sugar\n";

  print_endline "Commands:";
  print_endline "[purchase <ingredient>]";
  print_endline "[end]";
  (* print_endline "sell"; *)
  print_endline "[quit]";
  print_string "> ";

  match read_line () with
  | input -> purchase_game (purchase_stage new_state input)
(*match read_line () with | input -> purchase_game (purchase_stage new_state
  input)*)

let rec init_game state =
  print_endline "\n";
  print_endline "Welcome to the Lemonade Stand game.";
  print_endline "Enter any key to start.";
  print_string "> ";
  match read_line () with
  | random_input -> purchase_game Framework.init_state

let begin_game = init_game Framework.start_state
(*let () = main ()*)
