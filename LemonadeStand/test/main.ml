open OUnit2
open LemonadeStand
open Framework
open Ingredients
open Input
open Customers

(********************************************************************
  Test Plan
 ********************************************************************)
(** Our final project involves a lot of player input therefore, a lot of our
    main.ml functions that output unit can’t be tested through OUnit but rather
    through manual testing. However, the functions inside our other modules
    (Framework, Ingredients, Customer, Input) are less reliant on player input
    and can be tested through OUnit and this in a way guarantees the functions
    in main.ml to be working as main.ml is heavily reliant on the functions
    created in these other modules. One exception is Customer.ml which contains
    functions that have a random component to it therefore it’s unknown what the
    output may be. Since we can’t pinpoint exactly what the output may be, to
    get around this, we decide to test the property of the output in these
    functions. For example, one of the functions outputs a string list, we may
    not know what it contains but it’s certain what the length will be and
    that’s something we can test. Most of our test cases are developed using
    glass box testing. We will test edge cases and some common cases to make
    sure our intention for these functions follow through in the test cases.
    This testing approach demonstrates the correctness of the system because we
    will attempt to test every property of these functions and all the possible
    outputs using our knowledge of how the functions are implemented. *)

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

let test_set_test (l : float) (s : float) (w : float) (p : float)
    (expected_output : Framework.t) : test =
  "Testing profit" >:: fun _ -> assert_equal expected_output (test_set l s w p)

let test_set_tests = [ [ test_set_test 0. 0. 0. 0. Framework.adjusting_state ] ]

let profit_test (t : Framework.t) (expected_output : float) : test =
  "Testing profit" >:: fun _ -> assert_equal expected_output (cup_ready t)

let profit_tests = [ [ profit_test Framework.adjusting_state 0. ] ]

let cup_ready_test (t : Framework.t) (expected_output : float) : test =
  "Testing cup_ready" >:: fun _ -> assert_equal expected_output (cup_ready t)

let cup_ready_tests = [ [ cup_ready_test Framework.adjusting_state 0. ] ]

let return_state_test (t : Framework.t) (expected_output : Framework.t) : test =
  "Testing return_state" >:: fun _ ->
  assert_equal expected_output (return_state t)

let return_state_tests =
  [ [ return_state_test Framework.adjusting_state Framework.adjusting_state ] ]

let add_cost_test (t : Framework.t) (cost : float)
    (expected_output : Framework.t) : test =
  "Testing add_cost" >:: fun _ ->
  assert_equal expected_output (add_sugar t cost)

let add_cost_tests =
  [ [ add_cost_test Framework.adjusting_state 0. Framework.adjusting_state ] ]

let add_sugar_test (t : Framework.t) (count : float)
    (expected_output : Framework.t) : test =
  "Testing add_sugar" >:: fun _ ->
  assert_equal expected_output (add_sugar t count)

let add_sugar_tests =
  [ [ add_sugar_test Framework.adjusting_state 0. Framework.adjusting_state ] ]

let add_lemons_test (t : Framework.t) (count : float)
    (expected_output : Framework.t) : test =
  "Testing add_lemons" >:: fun _ ->
  assert_equal expected_output (add_lemons t count)

let add_lemons_tests =
  [ [ add_lemons_test Framework.adjusting_state 0. Framework.adjusting_state ] ]

let add_water_test (t : Framework.t) (count : float)
    (expected_output : Framework.t) : test =
  "Testing add_water" >:: fun _ ->
  assert_equal expected_output (add_water t count)

let add_water_tests =
  [ [ add_water_test Framework.adjusting_state 0. Framework.adjusting_state ] ]

let buy_lemon_test (t : Framework.t) (count : float) (cost : float)
    (expected_output : Framework.t) : test =
  "Testing buy_lemon" >:: fun _ ->
  assert_equal expected_output (buy_lemon t count cost)

let buy_lemon_tests =
  [
    [
      buy_lemon_test Framework.purchasing_state 0. 0. Framework.purchasing_state;
    ];
  ]

let buy_sugar_test (t : Framework.t) (count : float) (cost : float)
    (expected_output : Framework.t) : test =
  "Testing buy_sugar" >:: fun _ ->
  assert_equal expected_output (buy_sugar t count cost)

let buy_sugar_tests =
  [
    [
      buy_sugar_test Framework.purchasing_state 0. 0. Framework.purchasing_state;
    ];
  ]

let buy_cup_test (t : Framework.t) (count : int) (cost : float)
    (expected_output : Framework.t) : test =
  "Testing buy_cup" >:: fun _ ->
  assert_equal expected_output (buy_cup t count cost)

let buy_cup_tests =
  [
    [ buy_cup_test Framework.purchasing_state 0 0. Framework.purchasing_state ];
  ]

let get_wallet_test (t : Framework.t) (expected_output : float) : test =
  "Testing get_wallet" >:: fun _ -> assert_equal expected_output (get_wallet t)

let get_wallet_tests = [ [ get_wallet_test Framework.init_state 30. ] ]

let get_days_left_test (t : Framework.t) (expected_output : int) : test =
  "Testing get_days_left" >:: fun _ ->
  assert_equal expected_output (get_days_left t)

let get_days_left_tests = [ [ get_days_left_test Framework.init_state 10 ] ]

let get_lemon_count_test (t : Framework.t) (expected_output : float) : test =
  "Testing get_lemon_count" >:: fun _ ->
  assert_equal expected_output (get_lemon_count t)

let get_lemon_count_tests = [ [ get_lemon_count_test Framework.init_state 0. ] ]

let get_sugar_count_test (t : Framework.t) (expected_output : float) : test =
  "Testing get_sugar_count" >:: fun _ ->
  assert_equal expected_output (get_sugar_count t)

let get_sugar_count_tests = [ [ get_sugar_count_test Framework.init_state 0. ] ]

let get_cup_count_test (t : Framework.t) (expected_output : int) : test =
  "Testing get_cup_count" >:: fun _ ->
  assert_equal expected_output (get_cup_count t)

let get_cup_count_tests = [ [ get_cup_count_test Framework.init_state 0 ] ]

let get_price_test (t : Framework.t) (expected_output : float) : test =
  "Testing get_price" >:: fun _ -> assert_equal expected_output (get_price t)

let get_price_tests = [ [ get_price_test Framework.init_state 0. ] ]

let get_cup_sugar_count_test (t : Framework.t) (expected_output : float) : test
    =
  "Testing get_cup_sugar_count" >:: fun _ ->
  assert_equal expected_output (get_cup_sugar_count t)

let get_cup_sugar_count_tests =
  [ [ get_sugar_count_test Framework.init_state 0. ] ]

let get_cup_lemon_count_test (t : Framework.t) (expected_output : float) : test
    =
  "Testing get_cup_lemon_count" >:: fun _ ->
  assert_equal expected_output (get_cup_lemon_count t)

let get_cup_lemon_count_tests =
  [ [ get_cup_lemon_count_test Framework.init_state 0. ] ]

let get_cup_water_count_test (t : Framework.t) (expected_output : float) : test
    =
  "Testing get_cup_water_count" >:: fun _ ->
  assert_equal expected_output (get_cup_water_count t)

let get_cup_water_count_tests =
  [ [ get_cup_water_count_test Framework.init_state 0. ] ]

let framework_tests =
  List.flatten
    (List.flatten
       [
         List.map
           (fun x -> List.flatten x)
           [
             get_wallet_tests;
             get_days_left_tests;
             get_lemon_count_tests;
             get_cup_count_tests;
             get_sugar_count_tests;
             get_cup_sugar_count_tests;
             get_cup_lemon_count_tests;
             get_cup_water_count_tests;
             get_price_tests;
             buy_lemon_tests;
             buy_cup_tests;
             buy_sugar_tests;
             add_sugar_tests;
             add_lemons_tests;
             add_water_tests;
             add_cost_tests;
             return_state_tests;
             cup_ready_tests;
             profit_tests;
             test_set_tests;
           ];
       ])

(*****************************************************************)
(* Ingredients.ml *)
(*****************************************************************)

let get_lemon_total_cost_test (option : Ingredients.options)
    (expected_output : float) : test =
  "Testing get_lemon_total_cost" >:: fun _ ->
  assert_equal expected_output (get_lemon_total_cost option)

let get_lemon_total_cost_tests =
  [ [ get_lemon_total_cost_test Ingredients.purchase_option0 0. ] ]

let get_sugar_total_cost_test (option : Ingredients.options)
    (expected_output : float) : test =
  "Testing get_sugar_total_cost" >:: fun _ ->
  assert_equal expected_output (get_sugar_total_cost option)

let get_sugar_total_cost_tests =
  [ [ get_sugar_total_cost_test Ingredients.purchase_option0 0. ] ]

let get_cup_total_cost_test (option : Ingredients.options)
    (expected_output : float) : test =
  "Testing get_cup_total_cost" >:: fun _ ->
  assert_equal expected_output (get_cup_total_cost Ingredients.purchase_option0)

let get_cup_total_cost_tests =
  [ [ get_cup_total_cost_test Ingredients.purchase_option0 0. ] ]

let get_lemon_amt_test (option : Ingredients.options) (expected_output : float)
    : test =
  "Testing get_lemon_amt" >:: fun _ ->
  assert_equal expected_output (get_lemon_amt option)

let get_lemon_amt_tests =
  [ [ get_lemon_amt_test Ingredients.purchase_option0 0. ] ]

let get_sugar_amt_test (option : Ingredients.options) (expected_output : float)
    : test =
  "Testing get_sugar_amt" >:: fun _ ->
  assert_equal expected_output (get_sugar_amt option)

let get_sugar_amt_tests =
  [ [ get_sugar_amt_test Ingredients.purchase_option0 0. ] ]

let get_cup_amt_test (option : Ingredients.options) (expected_output : int) :
    test =
  "Testing get_cup_amt" >:: fun _ ->
  assert_equal expected_output (get_cup_amt Ingredients.purchase_option0)

let get_cup_amt_tests = [ [ get_cup_amt_test Ingredients.purchase_option0 0 ] ]

let ingredients_tests =
  List.flatten
    (List.flatten
       [
         List.map
           (fun x -> List.flatten x)
           [
             get_lemon_total_cost_tests;
             get_sugar_total_cost_tests;
             get_cup_total_cost_tests;
             get_lemon_amt_tests;
             get_lemon_amt_tests;
             get_lemon_amt_tests;
           ];
       ])

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
let tests =
  "test suite"
  >::: List.flatten
         [ customer_tests; input_tests; ingredients_tests; framework_tests ]

let _ = run_test_tt_main tests