open Input
(* Let's say a perfect cup of lemonade needs 4 tsp of sugar and 4 tsp of
   squeezed lemon for each cup of lemonade. Player's selected ingredient will be
   divided by the above values to create a ratio.

   Prep Phase (Range):

   (Lemon): 0 tsp - 5 tsp

   (Sugar): 0 tsp - 5 tsp

   (Water): 0.5 cup - 2 cups

   (Cost): $0.05 - $10.00 **)

(* val comapre_to_ideal : string -> feedback *)

(* Hard coded for testing, let's say these are the inputs *)

let get_lemon_input = 4.
let get_sugar_input = 4.
let get_water_input = 1.
let get_cost_input = 3.
