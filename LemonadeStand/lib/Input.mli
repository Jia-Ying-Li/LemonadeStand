(** The type [command] represents a player command that is decomposed into a
    verb and possibly an object phrase. Invariant: the [object_phrase] carried
    by [Purchase] and [Add] must not be empty. *)
type command =
  | Purchase of string list
  | Add of string list
  | Serve
  | End
  | Next
  | Quit

exception CommandNotFound
(** Raised when a command does not include any of the command key words *)

exception InvalidParameter
(** Raised when a command does not have a valid parameter *)

exception Empty
(** Raised when the input string is empty *)

exception EndofGame
(** Raised when the player reaches the end of the game *)

val parse : string -> command
(** [parse str] will parse a player's input into a [command]. The first word
    (indicated by the seperation of spaces) of a player's input becomes the
    verb. The rest of the input, if any, become the object phrase. Examples:

    - [parse "    purchase   lemon   "] is ["purchase"; "lemon"]
    - [parse "end"] is [End].

    Requires: The input should contain only alphabets (A-Z, a-z) and space
    characters.

    Raises: [CommandNotFound] if the input does not include any of the command
    key words.

    Raises: [InvalidParameter] if the input consisting of a verb followed by an
    object phase (ex. Purchase t and Add t) does not contain a valid object
    phrase, t.

    Raises: [Empty] if the input is the empty string or contains only spaces. *)
