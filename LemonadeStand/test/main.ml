open OUnit2
open LemonadeStand
open Framework
open Ingredients
open Input
open Customers

(********************************************************************
  Test Plan
 ********************************************************************)
(** Framework.ml: Advika Todo

    Ingredients.ml:

    Input.ml:

    Customer.ml: *)

let print_responses r =
  match r with
  | Sour -> "Sour"
  | Bland -> "Bland"
  | JustRight -> "JustRight"
  | Expensive -> "Expensive"
  | Cheap -> "Cheap"
  | JustAlright -> "JustAlright"

(* Adapted from CS3110 Assignments *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let rec pp_list pp_elt lst =
  match lst with
  | [] -> ""
  | [ h ] -> pp_elt h
  | h :: t -> pp_elt h ^ ", " ^ pp_list pp_elt t

let print_lst pp_elt lst = "[" ^ pp_list pp_elt lst ^ "]"

(*****************************************************************)
(* Framework.ml *)
(*****************************************************************)

(* val init_state : t val purchasing_state : t val get_wallet : t -> float val
   get_days_left : t -> int val get_lemon_count : t -> float val get_cup_count :
   t -> int val get_sugar_count : t -> float val get_cup_sugar_count : t ->
   float val get_cup_lemon_count : t -> float val get_cup_water_count : t ->
   float val get_price : t -> float val buy_lemon : t -> float -> float -> t val
   buy_cup : t -> int -> float -> t val buy_sugar : t -> float -> float -> t val
   add_sugar : t -> float -> t val add_lemons : t -> float -> t val add_water :
   t -> float -> t val add_cost : t -> float -> t val serve : t -> t val
   return_state : t -> t val cup_sell : t -> float -> float -> int -> int -> int
   val profit : t -> float val cup_ready : t -> float val test_set : float ->
   float -> float -> float -> t *)

(*****************************************************************)
(* Ingredients.ml *)
(*****************************************************************)
(* val purchase_option0 : options val purchase_option1 : options val
   purchase_option2 : options val purchase_option3 : options val
   get_lemon_total_cost : options -> float val get_cup_total_cost : options ->
   float val get_sugar_total_cost : options -> float val get_lemon_amt : options
   -> float val get_cup_amt : options -> int val get_sugar_amt : options ->
   float val set_cup : float -> float -> float -> cup_contains *)
(*****************************************************************)
(* Input.ml *)
(*****************************************************************)
let input_tests =
  [
    (* Double Verb *)
    ( "Testing parse, Parse Purchase" >:: fun _ ->
      assert_equal (Purchase [ "lemon" ]) (parse "purchase lemon") );
    ( "Testing parse, Parse Purchase (All Uppercase)" >:: fun _ ->
      assert_equal (Purchase [ "lemon" ]) (parse "purchase LEMON") );
    (* Parse Verb *)
    ("Testing parse, Parse End" >:: fun _ -> assert_equal End (parse "end"));
    ( "Testing parse, Parse End (All Uppercase)" >:: fun _ ->
      assert_equal End (parse "END") );
    ("Testing parse, Parse Next" >:: fun _ -> assert_equal Next (parse "next"));
    ( "Testing parse, Parse Next (All Uppercase)" >:: fun _ ->
      assert_equal Next (parse "NEXT") );
    ("Testing parse, Parse Quit" >:: fun _ -> assert_equal Quit (parse "quit"));
    ( "Testing parse, Parse Quit (All Uppercase)" >:: fun _ ->
      assert_equal Quit (parse "QUIT") );
    ( "Testing parse, Parse Serve" >:: fun _ ->
      assert_equal Serve (parse "serve") );
    ( "Testing parse, Parse Serve (All Uppercase)" >:: fun _ ->
      assert_equal Serve (parse "SERVE") );
    (* Parse Excepetions *)
    ( "Testing parse, Parse CommandNotFound" >:: fun _ ->
      assert_raises CommandNotFound (fun () -> parse "command") );
    ( "Testing parse, Parse InvalidParameter" >:: fun _ ->
      assert_raises InvalidParameter (fun () ->
          parse "purchase someingredients") );
    ( "Testing parse, Parse Empty" >:: fun _ ->
      assert_raises Empty (fun () -> parse "") );
  ]

(*****************************************************************)
(* Customer.ml *)
(*****************************************************************)
(* Fulfilling both side of OR statement for sour *)
let ratioSourI = Framework.test_set 5. 0.5 0.5 2.

(* Short circuit of OR statement for sour *)
let ratioSourII = Framework.test_set 5. 4. 0.5 3.
let ratioBland = Framework.test_set 1. 1. 2. 2.
let ratioJustRight = Framework.test_set 4. 4. 1. 2.
let ratioExpensive = Framework.test_set 3. 3. 1. 10.
let ratioCheap = Framework.test_set 3. 3. 1.0 0.5
let ratioJustAlright = Framework.test_set 3. 3. 1. 2.
let ratioMore = Framework.test_set 3. 4. 2. 10.

let customer_tests =
  [
    ( "Testing customer_responses, Fulfilling both side of OR statement for sour"
    >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists
        ~printer:(print_lst print_responses)
        [ Sour; JustAlright ]
        (customer_responses ratioSourI []) );
    ( "Testing customer_responses, Short circuit of OR statement for sour"
    >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists
        ~printer:(print_lst print_responses)
        [ Sour; JustAlright ]
        (customer_responses ratioSourII []) );
    ( "Testing customer_responses, Bland Response" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists
        ~printer:(print_lst print_responses)
        [ Bland; JustAlright ]
        (customer_responses ratioBland []) );
    ( "Testing customer_responses, JustRight Response" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists
        ~printer:(print_lst print_responses)
        [ JustRight; JustAlright ]
        (customer_responses ratioJustRight []) );
    ( "Testing customer_responses, Expensive Response" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists
        ~printer:(print_lst print_responses)
        [ Expensive; JustAlright ]
        (customer_responses ratioExpensive []) );
    ( "Testing customer_responses, Cheap Response" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists
        ~printer:(print_lst print_responses)
        [ Cheap; JustAlright ]
        (customer_responses ratioCheap []) );
    ( "Testing customer_responses, JustAlright Response" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists
        ~printer:(print_lst print_responses)
        [ JustAlright ]
        (customer_responses ratioJustAlright []) );
    ( "Testing customer_responses, Multiple Response" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists
        ~printer:(print_lst print_responses)
        [ JustAlright; Expensive; Bland ]
        (customer_responses ratioMore []) );
    ( "Testing generate, Length of Zero" >:: fun _ ->
      assert_equal 0
        (List.length (generate (customer_responses ratioJustAlright []) 0)) );
    ( "Testing generate, Length of One" >:: fun _ ->
      assert_equal 1
        (List.length (generate (customer_responses ratioJustAlright []) 1)) );
    ( "Testing generate, Length of Many" >:: fun _ ->
      assert_equal 10
        (List.length (generate (customer_responses ratioJustAlright []) 10)) );
  ]

(*****************************************************************)
(* Test Cases *)
(*****************************************************************)
let tests = "test suite" >::: List.flatten [ customer_tests; input_tests ]
let _ = run_test_tt_main tests