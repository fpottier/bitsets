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

(* We select an implementation of bit sets based on the value of [n], which
   is chosen by the user. *)

(* The functor [Make] must take a dummy argument [()] in order to indicate
   that it is not an applicative functor. Otherwise, we get a cryptic type
   error message: "This expression creates fresh types. It is not allowed
   inside applicative functors." *)

module Make (N : sig
  val n: int
end) ()
= struct

  (* An [if] construct in the module language would be welcome. *)

  module type SET =
    API.SET with type elt = int

  include (val
    if N.n <= WordBitSet.bound then
      (module WordBitSet : SET)
    else if N.n <= DWordBitSet.bound then
      (module DWordBitSet : SET)
    else if N.n <= QWordBitSet.bound then
      (module QWordBitSet : SET)
    else
      (module SparseBitSet : SET)
    : SET)

end
