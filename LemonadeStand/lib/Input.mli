type command =
  | Purchase of string list
  | Add of string list
  | Serve
  (* | Sell of string list *)
  | End
  | Quit

exception CommandNotFound
(** Raised when a command does not include Purchase or Done command *)
exception InvalidParameter
(** Raised when a command does not have necessary parameter *)
exception Empty
(** Raised when the input string is empty *)

val parse : string -> command
