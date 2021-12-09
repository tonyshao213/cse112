(* $Id: etc.mli,v 1.6 2021-04-28 06:47:17-07 - - $ *)

(*
* Main program and system access.
*)

val warn : string list -> unit

val die : string list -> unit

val syntax_error : Lexing.position -> string list -> unit

val parse_failed : bool ref

val usage_exit : unit -> unit

val read_number : unit -> float

val int_of_round_float : float -> int

