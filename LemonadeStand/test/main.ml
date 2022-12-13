open OUnit2
open LemonadeStand
open Framework
open Ingredients
open Input
open Customers

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
  "[" ^ pp_elts lst ^ "]"

(*****************************************************************)
(* Framework.ml *)
(*****************************************************************)
(* Advika Todo *)

(*****************************************************************)
(* Ingredients.ml *)
(*****************************************************************)
(* Advika Todo *)

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

let test_state =
  {
    state = Adjusting;
    days_left = 10;
    wallet = 30.0;
    lemon_count = 30.;
    cup_count = 30;
    sugar_count = 30.;
    cup_lemon = 0.;
    cup_sugar = 0.;
    cup_water = 0.;
    price = 0.;
  }

let ratioSourI =
  {
    test_state with
    cup_lemon = 5.;
    cup_sugar = 0.;
    cup_water = 0.5;
    price = 1.5;
  }

(* Short circuit of OR statement for sour *)
let ratioSourII =
  {
    test_state with
    cup_lemon = 5.;
    cup_sugar = 5.;
    cup_water = 0.5;
    price = 1.5;
  }

let ratioBland =
  {
    test_state with
    cup_lemon = 1.;
    cup_sugar = 1.;
    cup_water = 2.;
    price = 1.5;
  }

let ratioJustRight =
  {
    test_state with
    cup_lemon = 4.;
    cup_sugar = 4.;
    cup_water = 1.;
    price = 1.5;
  }

let ratioExpensive =
  {
    test_state with
    cup_lemon = 3.;
    cup_sugar = 3.;
    cup_water = 1.;
    price = 10.;
  }

let ratioCheap =
  {
    test_state with
    cup_lemon = 3.;
    cup_sugar = 3.;
    cup_water = 1.;
    price = 0.5;
  }

let ratioJustAlright =
  { test_state with cup_lemon = 3.; cup_sugar = 3.; cup_water = 1.; price = 2. }

let ratioMore =
  {
    test_state with
    cup_lemon = 3.;
    cup_sugar = 4.;
    cup_water = 2.;
    price = 10.;
  }

let customer_tests =
  [
    ( "Testing customer_responses, Fulfilling both side of OR statement for sour"
    >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list print_responses)
        [ Sour; JustAlright ]
        (customer_responses ratioSourI []) );
    ( "Testing customer_responses, Short circuit of OR statement for sour"
    >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list print_responses)
        [ Sour; JustAlright ]
        (customer_responses ratioSourII []) );
    ( "Testing customer_responses, Bland Response" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list print_responses)
        [ Bland; JustAlright ]
        (customer_responses ratioBland []) );
    ( "Testing customer_responses, JustRight Response" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list print_responses)
        [ JustRight; JustAlright ]
        (customer_responses ratioJustRight []) );
    ( "Testing customer_responses, Expensive Response" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list print_responses)
        [ Expensive; JustAlright ]
        (customer_responses ratioExpensive []) );
    ( "Testing customer_responses, Cheap Response" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list print_responses)
        [ Cheap; JustAlright ]
        (customer_responses ratioCheap []) );
    ( "Testing customer_responses, JustAlright Response" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list print_responses)
        [ JustAlright ]
        (customer_responses ratioJustAlright []) );
    ( "Testing customer_responses, Multiple Response" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list print_responses)
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