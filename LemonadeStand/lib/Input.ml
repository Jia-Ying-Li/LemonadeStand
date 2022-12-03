(* type input = {
  ingredient : Ingredients.option;
  amt : int;
} *)

type command =
  | Purchase of string list
  | Add of string list
  | Serve of string list
  (* | Sell of string list *)
  | End 
  | Quit


exception CommandNotFound
(** Raised when a command does not include Purchase or Done command *)
exception InvalidParameter
(** Raised when a command does not have necessary parameter *)
exception Empty
(** Raised when the input string is empty *)

let parse str =
  let arr = List.filter (fun s -> s <> "") (String.split_on_char ' ' str) in

  match arr with
  | [] -> raise Empty
  | verb :: t ->
      if verb = "quit" then Quit
      else if verb = "purchase" then Purchase t
      else if verb = "end" then End
      else if verb = "add" then Add t
      else if verb = "serve" then Serve t
      (* else if verb = "sell" then Sell t *)
      else raise CommandNotFound
