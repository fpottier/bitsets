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

module W =
  WordBitSet

let bound =
  2 * W.bound

type elt =
  int

(* A bit set is represented as a pair of words. *)

type t =
  | D of W.t * W.t

(* -------------------------------------------------------------------------- *)

(* Construction. *)

let empty =
  D (W.empty, W.empty)

(* The smart constructor [construct hi lo] produces a result that is
   equivalent to [D (hi, lo)]. In the special case where [hi] and [lo] are
   both empty, it produces [empty], thereby avoiding memory allocation.

   Furthermore, because [empty] is the sole empty set, [is_empty] can be
   implemented in the form of a physical equality test. (We assume that
   the OCaml compiler does not perform unsharing!) *)

(* Because [is_empty] is supposedly fast, one might wish to add a fast path
   to many (most) functions, where the case of an empty set receives special
   treatment. However, this would make the code longer, and it is not clear
   whether it would be beneficial overall, so it is not done. *)

let[@inline] construct hi lo =
  if W.is_empty hi && W.is_empty lo then
    empty
  else
    D (hi, lo)

let check s =
  let D (hi, lo) = s in
  if W.is_empty hi && W.is_empty lo then
    assert (s == empty)

let singleton i =
  if i < W.bound then
    D (W.empty, W.singleton i)
  else
    D (W.singleton (i - W.bound), W.empty)

let add i s =
  let D (hi, lo) = s in
  if i < W.bound then
    let lo' = W.add i lo in
    if lo == lo' then s else D (hi, lo')
  else
    let hi' = W.add (i - W.bound) hi in
    if hi == hi' then s else D (hi', lo)

let remove i s =
  let D (hi, lo) = s in
  if i < W.bound then
    let lo' = W.remove i lo in
    if lo == lo' then s else construct hi lo'
  else
    let hi' = W.remove (i - W.bound) hi in
    if hi == hi' then s else construct hi' lo

let union s1 s2 =
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  let hi = W.union hi1 hi2
  and lo = W.union lo1 lo2 in
  if hi == hi2 && lo == lo2 then s2 else D (hi, lo)

let inter s1 s2 =
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  construct (W.inter hi1 hi2) (W.inter lo1 lo2)

let diff s1 s2 =
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  construct (W.diff hi1 hi2) (W.diff lo1 lo2)

let above x s =
  let D (hi, lo) = s in
  if x < W.bound then
    construct hi (W.above x lo)
  else
    construct (W.above (x - W.bound) hi) W.empty

(* -------------------------------------------------------------------------- *)

(* Cardinality. *)

let[@inline] is_empty s =
  s == empty

let is_singleton s =
  let D (hi, lo) = s in
  W.is_empty hi && W.is_singleton lo ||
  W.is_singleton hi && W.is_empty lo

let cardinal s =
  let D (hi, lo) = s in
  W.cardinal hi + W.cardinal lo

(* -------------------------------------------------------------------------- *)

(* Tests. *)

let mem i s =
  let D (hi, lo) = s in
  if i < W.bound then
    W.mem i lo
  else
    W.mem (i - W.bound) hi

let equal s1 s2 =
  (s1 == s2) ||
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  W.equal hi1 hi2 &&
  W.equal lo1 lo2

let compare s1 s2 =
  if s1 == s2 then 0 else
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  let c = W.compare hi1 hi2 in
  if c = 0 then W.compare lo1 lo2
  else c

let disjoint s1 s2 =
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  W.disjoint hi1 hi2 && W.disjoint lo1 lo2

let subset s1 s2 =
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  W.subset hi1 hi2 && W.subset lo1 lo2

let[@inline] quick_subset s1 s2 =
  not (disjoint s1 s2)

(* -------------------------------------------------------------------------- *)

(* Extraction. *)

let minimum s =
  let D (hi, lo) = s in
  if W.is_empty lo then
    W.minimum hi + W.bound
  else
    W.minimum lo

let maximum s =
  let D (hi, lo) = s in
  if W.is_empty hi then
    W.maximum lo
  else
    W.maximum hi + W.bound

let choose =
  minimum

(* -------------------------------------------------------------------------- *)

(* Iteration. *)

let iter yield s =
  let D (hi, lo) = s in
  W.iter yield lo;
  W.iter_delta W.bound yield hi

let fold yield s accu =
  let D (hi, lo) = s in
  let accu = W.fold yield lo accu in
  let accu = W.fold_delta W.bound yield hi accu in
  accu

let elements s =
  (* Note: the list is produced in decreasing order. *)
  fold (fun tl hd -> tl :: hd) s []

(* -------------------------------------------------------------------------- *)

(* Decomposition. *)

let compare_minimum s1 s2 =
  match is_empty s1, is_empty s2 with
  | true, true  ->  0
  | true, false -> -1
  | false, true -> +1
  | false, false ->
      let D (hi1, lo1) = s1
      and D (hi2, lo2) = s2 in
      match W.is_empty lo1, W.is_empty lo2 with
      | true , true  -> W.compare_minimum hi1 hi2
      | false, false -> W.compare_minimum lo1 lo2
      | true , false -> +1
      | false, true  -> -1

let[@inline] sorted_union xs =
  List.fold_left union empty xs

let extract_unique_prefix s1 s2 =
  assert (not (is_empty s2));
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  if W.is_empty lo2 then
    (* [lo1] is entirely part of the unique prefix; [hi1] must be split. *)
    let hi1a, hi1b = W.extract_unique_prefix hi1 hi2 in
    construct hi1a lo1, construct hi1b W.empty
  else
    (* [lo1] must be split; [hi1] is entirely outside of the unique prefix. *)
    let lo1a, lo1b = W.extract_unique_prefix lo1 lo2 in
    construct W.empty lo1a, construct hi1 lo1b

let extract_shared_prefix s1 s2 =
  let D (hi1, lo1) = s1
  and D (hi2, lo2) = s2 in
  if W.equal lo1 lo2 then
    (* [lo1] is entirely part of the shared prefix. *)
    let hi, (hi1, hi2) = W.extract_shared_prefix hi1 hi2 in
    construct hi lo1, (construct hi1 W.empty, construct hi2 W.empty)
  else
    (* The shared prefix is a fragment of [lo]. *)
    let lo, (lo1, lo2) = W.extract_shared_prefix lo1 lo2 in
    construct W.empty lo, (construct hi1 lo1, construct hi2 lo2)
