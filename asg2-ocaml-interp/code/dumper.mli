(* Generated by: ocamlopt -g -i dumper.ml *)
(* Date: Mon Apr 26 10:46:07 PDT 2021 *)
(* 
Partner: Tony Shao (toshao@ucsc.edu)
Partner: Brendan Teo (bteo1@ucsc.edu) 
*)

val quote : string -> string
val join : string -> string -> string -> string list -> string
val string_of_option : ('a -> string) -> 'a option -> string
val string_of_ctor : string -> string list -> string
val string_of_list : ('a -> string) -> 'a list -> string
val string_of_printable : Absyn.printable -> string
val string_of_memref : Absyn.memref -> string
val string_of_expr : Absyn.expr -> string
val string_of_relexpr : Absyn.relexpr -> string
val string_of_stmt : Absyn.stmt -> string
val dump_progline : int * string option * Absyn.stmt option -> unit
val dump_program : Absyn.program -> unit
