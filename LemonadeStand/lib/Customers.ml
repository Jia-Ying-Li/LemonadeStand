open Random
open Recipe

(* Let's say a perfect cup of lemonade needs 4 tsp of sugar, 4 tsp of squeezed
   lemon and 1 cup of water for each cup of lemonade. Player's selected
   ingredient will be divided by the above values to create a ratio.

   Prep Phase (Range):

   (Lemon): 0 tsp - 5 tsp

   (Sugar): 0 tsp - 5 tsp

   (Water): 0.5 cup - 2 cups

   (Cost): $0.05 - $10.00 **)

type ratio = {
  sour : float;
  sweet : float;
  water : float;
  cost : float;
}

let set_lemon = 4.
let set_sugar = 4.
let set_water = 1.
let set_cost = 3.

let response_ratio =
  {
    sour = Recipe.get_lemon_input /. set_lemon;
    sweet = Recipe.get_sugar_input /. set_sugar;
    water = Recipe.get_water_input /. set_water;
    cost = Recipe.get_cost_input /. set_cost;
  }

type responses =
  | Sour
  | Bland
  | JustRight
  | Expensive
  | Cheap
  | JustAlright

(* Textfile containing names, inspired by StackOverflow
   (https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml) *)
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

(* Let's say each lemon yields 3 tsp of lemon juice *)
(* Sour: greater than 20% of the perfect amount of squeezed lemon or the ratio
   of squeezed lemon is 20% greater than that of sugar *)
(* Bland: The ratio of squeezed lemon and sugar to water amount is less than 5*)
(* JustRight: The ratio of squeezed lemon and suga to water is betwee n 7.5 and
   8.5 *)
(* Expensive: The ratio of squeezed lemon and sugar to cost is less than 1.75 *)
(* Cheap: If the cost is less than $1.50 *)
(* JustAlright: To catch all the cases*)

let customer_responses s lst =
  let sour =
    if s.sour > 1.2 || s.sour /. s.sweet > 1.2 then Sour :: lst else lst
  in
  let bland =
    if (s.sour +. s.sweet) /. s.water < 5. then Bland :: sour else sour
  in
  let justright =
    if
      (s.sour +. s.sweet) /. s.water > 7.5
      && (s.sour +. s.sweet) /. s.water < 8.5
    then JustRight :: bland
    else bland
  in
  let expensive =
    if (s.sour +. s.sweet) /. s.cost < 1.75 then Expensive :: justright
    else justright
  in
  let cheap = if s.cost < 1.5 then Cheap :: expensive else expensive in
  JustAlright :: cheap

(* Accumulator: Number of responses generated Max: Number of people who
   purchased lemonade less than 10 else 10 *)
let rec generate lst acc =
  let name = read_lines "lib/CustomerNames.txt" in
  match (List.nth lst (Random.int (List.length lst - 1)), acc) with
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

(* Adapted from CS3110 Assignments *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  pp_elts lst

let rec print_feedback lst acc =
  match lst with
  | [] -> ()
  | h :: t ->
      print_endline h;
      print_feedback t (acc - 1)
