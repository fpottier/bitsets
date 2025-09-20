(******************************************************************************)
(*                                                                            *)
(*                                  Skeleton                                  *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

include Set.Make(Int)

let is_singleton s =
  cardinal s = 1

let nonempty s =
  not (is_empty s)

(* [qs s1 s2] determines whether [s1] and [s2] are suitable arguments
   for [quick_subset]. They must be both nonempty and the condition
   [s1 ⊆ s2 ⋁ s1 ∩ s2 = ∅] must hold. *)
let qs s1 s2 =
  nonempty s1 &&
  nonempty s2 &&
  (subset s1 s2 || disjoint s1 s2)

let rev_elements s =
  List.rev (elements s)

let minimum =
  min_elt

let compare_minimum s1 s2 =
  match is_empty s1, is_empty s2 with
  | true, true ->
      0
  | true, false ->
      -1
  | false, true ->
      +1
  | false, false ->
      Int.compare (minimum s1) (minimum s2)

let sorted_union ss =
  List.fold_left union empty ss
