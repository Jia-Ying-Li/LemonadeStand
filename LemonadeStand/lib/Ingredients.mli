type options
(** The type [options] represents the different unit of ingredients you can
    purchase. *)

val purchase_option0 : options
(** [purchase_option0] Returns one of the purchasing options for the player to
    choose from. *)

val purchase_option1 : options
(** [purchase_option1] Returns one of the purchasing options for the player to
    choose from, with the highest unit price per item. *)

val purchase_option2 : options
(** [purchase_option2] Returns one of the purchasing options for the player to
    choose from. *)

val purchase_option3 : options
(** [purchase_option3] Returns one of the purchasing options for the player to
    choose from, with the lowest unit price per item. *)

val get_lemon_total_cost : options -> float
(** [get_lemon_total_cost options] Returns the cost of the lemons of the
    player's desired option. Requires: [options] to be a valid option choice
    from either purchase_option0, purchase_option1, purchase_option2,
    purchase_option3 *)

val get_cup_total_cost : options -> float
(** [get_cup_total_cost options] Returns the cost of the cup of the player's
    desired option. Requires: [options] to be a valid option choice from either
    purchase_option0, purchase_option1, purchase_option2, purchase_option3 *)

val get_sugar_total_cost : options -> float
(** [get_sugar_total_cost options] Returns the cost of the sugar of the player's
    desired option. Requires: [options] to be a valid option choice from either
    purchase_option0, purchase_option1, purchase_option2, purchase_option3 *)

val get_lemon_amt : options -> float
(** [get_lemon_amt options] Returns the lemon count of the player's input of the
    desired option they want to purchase. Requires: [options] to be a valid
    option choice from either purchase_option0, purchase_option1,
    purchase_option2, purchase_option3 *)

val get_cup_amt : options -> int
(** [get_cup_amt options] Returns the cup count of the player's input of the
    desired option they want to purchase. Requires: [options] to be a valid
    option choice from either purchase_option0, purchase_option1,
    purchase_option2, purchase_option3 *)

val get_sugar_amt : options -> float
(** [get_sugar_amt options] Returns the sugar count of the player's input of the
    desired option they want to purchase. Requires: [options] to be a valid
    option choice from either purchase_option0, purchase_option1,
    purchase_option2, purchase_option3 *)
