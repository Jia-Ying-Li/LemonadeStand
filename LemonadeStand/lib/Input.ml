(* type input = { ingredient : Ingredients.option; amt : int; } *)

(* Raised when a command does not include Purchase or Done command *)
exception CommandNotFound

(* Raised when a command does not have necessary parameter *)
exception InvalidParameter

(* Raised when the input string is empty *)
exception Empty

type command =
  | Purchase of string list
  | Add of string list
  | Serve
  (* | Sell of string list *)
  | End
  | Quit

let parse str =
  let arr = List.filter (fun s -> s <> "") (String.split_on_char ' ' str) in

  match arr with
  | [] -> raise Empty
  | [ verb ] ->
      if verb = "quit" then Quit
      else if verb = "serve" then Serve
      else if verb = "end" then End
      else raise CommandNotFound
  | verb :: t -> (
      match verb with
      | "purchase" ->
          if t <> [ " lemon" ] && t <> [ " sugar" ] && t <> [ " cup" ] then
            raise CommandNotFound
          else Purchase t
      | "add" -> Add t
      | _ -> raise CommandNotFound)
(* else if verb = "sell" then Sell t *)
