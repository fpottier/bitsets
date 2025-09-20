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

let maximum =
  max_elt

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

(*
let print_set f s =
  Printf.fprintf f "[";
  iter (fun i -> Printf.fprintf f "%d; " i) s;
  Printf.fprintf f "]"

let print_sets f ss =
  List.iter (print_set f) ss
 *)

(* [is_slndos ss] determines whether [ss] is a sorted list of disjoint
   non-overlapping sets, that is, a suitable argument for the function
   [sorted_union]. *)

let rec nonoverlapping1 s1 ss =
  match ss with [] -> true | s :: ss -> disjoint s1 s && nonoverlapping1 s ss

let nonoverlapping ss =
  match ss with [] -> true | s :: ss -> nonoverlapping1 s ss

let is_slndos ss =
  List.for_all nonempty ss &&
  List.sort compare_minimum ss = ss &&
  nonoverlapping ss

let sorted_union ss =
  assert (is_slndos ss);
  List.fold_left union empty ss

let above x s =
  let _, _, s = split x s in
  s

(* [eup s1 s2] is the precondition of [extract_unique_prefix s1 s2]. *)

let eup s1 s2 =
  nonempty s1 &&
  nonempty s2 &&
  minimum s1 < minimum s2

let extract_unique_prefix s1 s2 =
  let x2 = minimum s2 in
  let head1, _, _ = split x2 s1 in
  assert (nonempty head1);
  let tail1 = diff s1 head1 in
  head1, tail1

(* [esp s1 s2] is the precondition of [extract_shared_prefix s1 s2]. *)

let esp s1 s2 =
  nonempty s1 &&
  nonempty s2 &&
  minimum s1 = minimum s2

let rec shared_prefix s1 s2 =
  let x = minimum s1 in
  let s1 = remove x s1
  and s2 = remove x s2 in
  if esp s1 s2 then
    add x (shared_prefix s1 s2)
  else
    singleton x

let extract_shared_prefix s1 s2 =
  let head = shared_prefix s1 s2 in
  head, (diff s1 head, diff s2 head)
