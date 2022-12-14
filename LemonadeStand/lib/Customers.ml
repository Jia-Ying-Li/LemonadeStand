open Random
open Framework

(* Let's say a perfect cup of lemonade needs 4 tsp of sugar, 4 tsp of squeezed
   lemon and 1 cup of water for each cup of lemonade. Player's selected
   ingredient will be divided by the above values to create a ratio.

   Prep Phase (Range):

   (Lemon): 0 tsp - 5 tsp

   (Sugar): 0 tsp - 5 tsp

   (Water): 0.5 cup - 2 cups

   (Cost): $0.05 - $10.00 **)

type responses =
  | Sour
  | Bland
  | JustRight
  | Expensive
  | Cheap
  | JustAlright

(* Inspired by StackOverflow, Author: vukung
   https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)
let read_lines filename =
  let rand_count = Random.int 999 in
  let f = open_in filename in
  let rec loop () count =
    try
      let next = input_line f in
      if count = 0 then (
        close_in f;
        next)
      else loop () (count - 1)
    with End_of_file ->
      close_in f;
      ""
  in
  loop () rand_count

(* Sour: greater than 20% of the perfect amount of squeezed lemon or the ratio
   of squeezed lemon is 20% greater than that of sugar *)
(* Bland: The ratio of squeezed lemon and sugar to water amount is less than 5*)
(* JustRight: The ratio of squeezed lemon and suga to water is betwee n 7.5 and
   8.5 *)
(* Expensive: The ratio of squeezed lemon and sugar to cost is less than 1.75 *)
(* Cheap: If the cost is less than $1.50 *)
(* JustAlright: To catch all the cases*)

let customer_responses state lst =
  let sour =
    if
      get_cup_lemon_count state > 4.25
      || get_cup_lemon_count state /. get_cup_sugar_count state > 1.5
    then Sour :: lst
    else lst
  in
  let bland =
    if
      (get_cup_lemon_count state +. get_cup_sugar_count state)
      /. get_cup_water_count state
      < 5.
    then Bland :: sour
    else sour
  in
  let justright =
    if
      (get_cup_lemon_count state +. get_cup_sugar_count state)
      /. get_cup_water_count state
      > 7.5
      && (get_cup_lemon_count state +. get_cup_sugar_count state)
         /. get_cup_water_count state
         < 8.5
      || get_cup_lemon_count state = 4.
         && get_cup_sugar_count state = 4.
         && get_cup_water_count state = 1.
    then JustRight :: bland
    else bland
  in
  let expensive =
    if
      (get_cup_lemon_count state +. get_cup_sugar_count state)
      /. get_cup_water_count state
      < 1.75
      && get_price state > 2.
      || get_price state > 6.
    then Expensive :: justright
    else justright
  in
  let cheap =
    if get_price state <= 1.5 then Cheap :: expensive else expensive
  in
  JustAlright :: cheap

(* Accumulator: Number of responses generated Max: Number of people who
   purchased lemonade less than 10 else 10 *)
let rec generate lst acc =
  let name = read_lines "lib/CustomerNames.txt" in
  match (List.nth lst (Random.int (List.length lst)), acc) with
  | _, 0 -> []
  | Sour, acc ->
      List.nth
        [
          name ^ " thinks the lemonade is too sour";
          name ^ " thinks the lemonade could use more sugar";
          name ^ " will not come back next time";
        ]
        (Random.int 2)
      :: generate lst (acc - 1)
  | Bland, acc ->
      List.nth
        [
          name ^ " thinks the lemonade is bland";
          name ^ " thinks the lemonade could use more sugar";
          name ^ " thinks the lemonade could use more squeenzed lemon";
          name ^ " did not enjoy this lemonade";
        ]
        (Random.int 3)
      :: generate lst (acc - 1)
  | JustRight, acc ->
      List.nth
        [
          name ^ " thinks the lemonade is just right";
          name ^ " thinks the lemonade is the perfect mixture of ingredients";
          name ^ " loved the lemonade";
          name ^ " enjoyed this lemonade";
        ]
        (Random.int 3)
      :: generate lst (acc - 1)
  | Expensive, acc ->
      List.nth
        [
          name ^ " thinks the lemonade is too expensive";
          name ^ " thinks the lemonade is not worth the cost";
          name ^ " thinks the lemonade could be cheaper";
        ]
        (Random.int 2)
      :: generate lst (acc - 1)
  | Cheap, acc ->
      List.nth
        [
          name ^ " thinks the lemonade is at a good price";
          name ^ " thinks the lemonade is well worth the cost";
          name ^ " thinks the lemonade was a good purchase";
        ]
        (Random.int 2)
      :: generate lst (acc - 1)
  | JustAlright, acc ->
      List.nth
        [
          name ^ " thinks the lemonade is just alright at best";
          name ^ " thinks the lemonade is just okay";
          name ^ " thinks the lemonade could be better";
          name ^ " thinks the lemonade is nothing special";
        ]
        (Random.int 3)
      :: generate lst (acc - 1)

let rec print_feedback lst acc =
  match lst with
  | [] -> ()
  | h :: t ->
      print_endline h;
      print_feedback t (acc - 1)
