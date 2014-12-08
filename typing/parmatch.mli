(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Detection of partial matches and unused match cases. *)
open Asttypes
open Typedtree
open Types

val pretty_const : constant -> string
val top_pretty : Format.formatter -> pattern -> unit
val pretty_pat : pattern -> unit
val pretty_line : pattern list -> unit
val pretty_matrix : pattern list list -> unit

val omega : pattern
val omegas : int -> pattern list
val omega_list : 'a list -> pattern list
val normalize_pat : pattern -> pattern

(*  Extract intervals from first column *)
val inters_pss : pattern list list -> (constant * constant) list

(* Split interval p c1 c2 inters 
   returns disjoint interval patterns according to division 'inter' *)
val split_interval :
   pattern -> constant -> constant ->  (constant * constant) list ->
   pattern list

val all_record_args :
    (Longident.t loc * label_description * pattern) list ->
    (Longident.t loc * label_description * pattern) list
val const_compare : constant -> constant -> int
val const_interv_compare : constant * constant -> constant * constant -> int
val next_constant : constant -> constant option
val prev_constant : constant -> constant option

val le_pat : pattern -> pattern -> bool
val le_pats : pattern list -> pattern list -> bool
val compat : pattern -> pattern -> bool
val compats : pattern list -> pattern list -> bool
exception Empty
val lub : pattern -> pattern -> pattern
val lubs : pattern list -> pattern list -> pattern list

val get_mins : ('a -> 'a -> bool) -> 'a list -> 'a list

(* Those two functions recombine one pattern and its arguments:
   For instance:
     (_,_)::p1::p2::rem -> (p1, p2)::rem
   The second one will replace mutable arguments by '_'
*)
val set_args : pattern -> pattern list -> pattern list
val set_args_erase_mutable : pattern -> pattern list -> pattern list

val pat_of_constr : pattern -> constructor_description -> pattern
val complete_constrs :
    pattern -> constructor_tag list -> constructor_description  list

val pressure_variants: Env.t -> pattern list -> unit
val check_partial: Location.t -> case list -> partial
val check_partial_gadt:
    ((string, constructor_description) Hashtbl.t ->
     (string, label_description) Hashtbl.t ->
     Parsetree.pattern -> pattern option) ->
    Location.t -> case list -> partial
val check_unused: Env.t -> case list -> unit

(* Irrefutability tests *)
val irrefutable : pattern -> bool
val fluid : pattern -> bool
