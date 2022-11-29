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

(*****************************************************************)
(* Ingredients.ml *)
(*****************************************************************)

(*****************************************************************)
(* Input.ml *)
(*****************************************************************)

(*****************************************************************)
(* Customer.ml *)
(*****************************************************************)
(* Fulfilling both side of OR statement for sour *)
let ratioSourI : ratio = { sour = 5.; sweet = 0.; water = 0.5; cost = 1.5 }

(* Short circuit of OR statement for sour *)
let ratioSourII = { sour = 5.; sweet = 5.; water = 0.5; cost = 1.5 }

let customer_tests =
  [
    ( "Testing customer_responses, Fulfilling both side of OR statement for sour"
    >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list print_responses)
        [ Sour; JustAlright ]
        (customer_responses ratioSourI []) );
    ( "Testing customer_responses, Fulfilling both side of OR statement for sour"
    >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list print_responses)
        [ Sour; JustAlright ]
        (customer_responses ratioSourII []) );
  ]

(*****************************************************************)
(* Test Cases *)
(*****************************************************************)
let tests = "test suite" >::: List.flatten [ customer_tests ]
let _ = run_test_tt_main tests