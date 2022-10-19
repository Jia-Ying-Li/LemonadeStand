type input 

type command =
  | Purchase of input

exception CommandNotFound
(** Raised when a command does not include Purchase or Done command *)
exception MissingParameter
(** Raised when a command does not have necessary parameter *)
exception InvalidType
(** Raised when a command does not have parameters with the correct type *)


(* val parse : string -> command *)