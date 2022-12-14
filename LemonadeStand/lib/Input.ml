exception CommandNotFound
exception InvalidParameter
exception Empty
exception EndofGame

type command =
  | Purchase of string list
  | Add of string list
  | Serve
  | End
  | Next
  | Quit

let parse str =
  let arr =
    List.map
      (fun s -> String.lowercase_ascii s)
      (List.filter (fun s -> s <> "") (String.split_on_char ' ' str))
  in

  match arr with
  | [] -> raise Empty
  | [ verb ] ->
      if verb = "quit" then Quit
      else if verb = "serve" then Serve
      else if verb = "end" then End
      else if verb = "next" then Next
      else raise CommandNotFound
  | verb :: t -> (
      match verb with
      | "purchase" ->
          if t <> [ "lemon" ] && t <> [ "sugar" ] && t <> [ "cup" ] then
            raise InvalidParameter
          else Purchase t
      | "add" -> Add t
      | _ -> raise Empty)
(* else if verb = "sell" then Sell t *)
