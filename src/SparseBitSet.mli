(******************************************************************************)
(*                                                                            *)
(*                                  Bitsets                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers bitsets whose elements are not subject to a bound.
   These bit sets can store arbitrary nonnegative integer values. Their
   internal representation is sparse: a bit set is represented as a linked
   list of words. *)

(**A bit set represents a set of nonnegative integers. *)
include API.SET
  with type elt = int

(**The type [view] offers a view of a set as a list of words.

   The constructor [N] represents the empty set.

   The constructor [C (o, word, s)] represents the disjoint union of
   the set [word], whose inhabitants must be shifted up by [o], and
   the set [s].

   The offset [o] is always a multiple of
   [WordBitSet.bound]. *)
type view =
  | N
  | C of offset * word * t

and offset =
  int

and word =
  WordBitSet.t

(**The function [view] offers a view of a sparse bit set as a list of words. *)
val view : t -> view

(**/**)
(**[check] is used only during testing. *)
val check : t -> unit
