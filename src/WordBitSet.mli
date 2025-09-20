(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers bit sets that fit within one word of memory, that is,
   one OCaml value of type [int]. These bit sets can store integer values in
   the semi-open interval [\[0, bound)], where [bound] is [Sys.word_size - 1],
   that is, usually 63. *)

(**[bound] is [Sys.word_size - 1], that is, usually 63. *)
val bound: int

(**A bit set represents a set of integers and is itself an integer value. *)
include API.SET
  with type elt = int
   and type t = private int

(* [iter_delta] and [fold_delta] are slightly generalized variants of [iter]
   and [fold]. They add the constant [delta] on the fly to each set element
   before presenting this set element to the user function [f]. *)

val iter_delta: int -> (elt -> unit) -> t -> unit
val fold_delta: int -> (elt -> 'b -> 'b) -> t -> 'b -> 'b

val shift : t -> int -> t * t
(**[shift s delta] shifts the elements of the set [s] up by [delta],
   where [0 <= delta <= bound] must hold.
   The result is returned as pair of sets [(l, r)]:
     [l = { x + delta | x ∈ set, x + delta < bound }]
   and
     [r = { x + delta - bound | x ∈ set, x + delta >= bound }] *)
