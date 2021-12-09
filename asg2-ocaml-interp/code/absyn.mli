(* $Id: absyn.mli,v 1.8 2020-10-21 18:41:46-07 - - $ *)

(*
* Abstract syntax definitions for MB.
*)

(* 
Partner: Tony Shao (toshao@ucsc.edu)
Partner: Brendan Teo (bteo1@ucsc.edu) 
*)


type linenr    = int
type ident     = string
type label     = string
type oper      = string
type number    = float

type memref    = Arrayref of ident * expr
               | Variable of ident

and  expr      = Number of number 
               | Memref of memref
               | Unary of oper * expr
               | Binary of oper * expr * expr

type relexpr   = Relexpr of oper * expr * expr

type printable = Printexpr of expr
               | String of string

type stmt      = Dim of ident * expr
               | Let of memref * expr
               | Goto of label
               | If of relexpr * label
               | Print of printable list
               | Input of memref list

type progline  = linenr * label option * stmt option

type program   = progline list


