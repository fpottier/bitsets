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
  4 * W.bound

let quarter =
  1 * W.bound

let middle =
  2 * W.bound

let quarter3 =
  3 * W.bound

type elt =
  int

(* A bit set is represented as a quadruple of words. *)

(* In principle, we could and should implement this data structure as a pair
   of two bit sets of type [DWordBitSet.t]. However, we wish to avoid nesting
   heap-allocated pairs, so we have to manually copy and adapt the code in
   [DWordBitSet]. *)

type t =
  | Q of W.t * W.t * W.t * W.t

let empty =
  Q (W.empty, W.empty, W.empty, W.empty)

let[@inline] construct hhi hlo lhi llo =
  if W.is_empty hhi && W.is_empty hlo &&
     W.is_empty lhi && W.is_empty llo then
    empty
  else
    Q (hhi, hlo, lhi, llo)

let check s =
  let Q (hhi, hlo, lhi, llo) = s in
  if W.is_empty hhi && W.is_empty hlo &&
     W.is_empty lhi && W.is_empty llo then
        assert (s == empty)

let is_empty s =
  s == empty

let singleton i =
  if i < middle then
    if i < quarter then
      Q (W.empty, W.empty, W.empty, W.singleton i)
    else
      let i = i - quarter in
      Q (W.empty, W.empty, W.singleton i, W.empty)
  else
    let i = i - middle in
    if i < quarter then
      Q (W.empty, W.singleton i, W.empty, W.empty)
    else
      let i = i - quarter in
      Q (W.singleton i, W.empty, W.empty, W.empty)

let add i s =
  let Q (hhi, hlo, lhi, llo) = s in
  if i < middle then
    if i < quarter then
      let llo' = W.add i llo in
      if llo == llo' then s else Q (hhi, hlo, lhi, llo')
    else
      let i = i - quarter in
      let lhi' = W.add i lhi in
      if lhi == lhi' then s else Q (hhi, hlo, lhi', llo)
  else
    let i = i - middle in
    if i < quarter then
      let hlo' = W.add i hlo in
      if hlo == hlo' then s else Q (hhi, hlo', lhi, llo)
    else
      let i = i - quarter in
      let hhi' = W.add i hhi in
      if hhi == hhi' then s else Q (hhi', hlo, lhi, llo)

let remove i s =
  let Q (hhi, hlo, lhi, llo) = s in
  if i < middle then
    if i < quarter then
      let llo' = W.remove i llo in
      if llo == llo' then s else construct hhi hlo lhi llo'
    else
      let i = i - quarter in
      let lhi' = W.remove i lhi in
      if lhi == lhi' then s else construct hhi hlo lhi' llo
  else
    let i = i - middle in
    if i < quarter then
      let hlo' = W.remove i hlo in
      if hlo == hlo' then s else construct hhi hlo' lhi llo
    else
      let i = i - quarter in
      let hhi' = W.remove i hhi in
      if hhi == hhi' then s else construct hhi' hlo lhi llo

let fold f s accu =
  let Q (hhi, hlo, lhi, llo) = s in
  let accu = W.fold f llo accu in
  let accu = W.fold_delta quarter f lhi accu in
  let accu = W.fold_delta middle f hlo accu in
  let accu = W.fold_delta quarter3 f hhi accu in
  accu

let iter f s =
  let Q (hhi, hlo, lhi, llo) = s in
  W.iter f llo;
  W.iter_delta quarter f lhi;
  W.iter_delta middle f hlo;
  W.iter_delta quarter3 f hhi

let is_singleton s =
  let Q (hhi, hlo, lhi, llo) = s in
  W.is_singleton hhi && W.is_empty hlo && W.is_empty lhi && W.is_empty llo ||
  W.is_empty hhi && W.is_singleton hlo && W.is_empty lhi && W.is_empty llo ||
  W.is_empty hhi && W.is_empty hlo && W.is_singleton lhi && W.is_empty llo ||
  W.is_empty hhi && W.is_empty hlo && W.is_empty lhi && W.is_singleton llo

let cardinal s =
  let Q (hhi, hlo, lhi, llo) = s in
  W.cardinal hhi + W.cardinal hlo + W.cardinal lhi + W.cardinal llo

let elements s =
  fold (fun tl hd -> tl :: hd) s []

let subset s1 s2 =
  let Q (hhi1, hlo1, lhi1, llo1) = s1
  and Q (hhi2, hlo2, lhi2, llo2) = s2 in
  W.subset hhi1 hhi2 && W.subset hlo1 hlo2 &&
  W.subset lhi1 lhi2 && W.subset llo1 llo2

let mem i s =
  let Q (hhi, hlo, lhi, llo) = s in
  if i < middle then
    if i < quarter then
      W.mem i llo
    else
      let i = i - quarter in
      W.mem i lhi
  else
    let i = i - middle in
    if i < quarter then
      W.mem i hlo
    else
      let i = i - quarter in
      W.mem i hhi

let union s1 s2 =
  let Q (hhi1, hlo1, lhi1, llo1) = s1
  and Q (hhi2, hlo2, lhi2, llo2) = s2 in
  let hhi = W.union hhi1 hhi2
  and hlo = W.union hlo1 hlo2
  and lhi = W.union lhi1 lhi2
  and llo = W.union llo1 llo2 in
  if hhi == hhi2 && hlo == hlo2 && lhi == lhi2 && llo == llo2 then s2
  else Q (hhi, hlo, lhi, llo)

let inter s1 s2 =
  let Q (hhi1, hlo1, lhi1, llo1) = s1
  and Q (hhi2, hlo2, lhi2, llo2) = s2 in
  construct
    (W.inter hhi1 hhi2) (W.inter hlo1 hlo2)
    (W.inter lhi1 lhi2) (W.inter llo1 llo2)

let diff s1 s2 =
  let Q (hhi1, hlo1, lhi1, llo1) = s1
  and Q (hhi2, hlo2, lhi2, llo2) = s2 in
  construct
    (W.diff hhi1 hhi2) (W.diff hlo1 hlo2)
    (W.diff lhi1 lhi2) (W.diff llo1 llo2)

let above x s =
  let Q (hhi, hlo, lhi, llo) = s in
  if x < quarter then
    construct hhi hlo lhi (W.above x llo)
  else if x < middle then
    construct hhi hlo (W.above (x - quarter) lhi) W.empty
  else if x < quarter3 then
    construct hhi (W.above (x - middle) hlo) W.empty W.empty
  else
    construct (W.above (x - quarter3) hhi) W.empty W.empty W.empty

let minimum s =
  let Q (hhi, hlo, lhi, llo) = s in
  if not (W.is_empty llo) then
    W.minimum llo
  else if not (W.is_empty lhi) then
    W.minimum lhi + quarter
  else if not (W.is_empty hlo) then
    W.minimum hlo + middle
  else
    W.minimum hhi + quarter3

let maximum s =
  let Q (hhi, hlo, lhi, llo) = s in
  if not (W.is_empty hhi) then
    W.maximum hhi + quarter3
  else if not (W.is_empty hlo) then
    W.maximum hlo + middle
  else if not (W.is_empty lhi) then
    W.maximum lhi + quarter
  else
    W.maximum llo

let choose =
  minimum

let compare s1 s2 =
  if s1 == s2 then 0 else
  let Q (hhi1, hlo1, lhi1, llo1) = s1
  and Q (hhi2, hlo2, lhi2, llo2) = s2 in
  let c = W.compare hhi1 hhi2 in if c <> 0 then c else
  let c = W.compare hlo1 hlo2 in if c <> 0 then c else
  let c = W.compare lhi1 lhi2 in if c <> 0 then c else
  let c = compare llo1 llo2 in c

let equal s1 s2 =
  (s1 == s2) ||
  let Q (hhi1, hlo1, lhi1, llo1) = s1
  and Q (hhi2, hlo2, lhi2, llo2) = s2 in
  W.equal hhi1 hhi2 &&
  W.equal hlo1 hlo2 &&
  W.equal lhi1 lhi2 &&
  W.equal llo1 llo2

let disjoint s1 s2 =
  let Q (hhi1, hlo1, lhi1, llo1) = s1
  and Q (hhi2, hlo2, lhi2, llo2) = s2 in
  W.disjoint hhi1 hhi2 && W.disjoint hlo1 hlo2 &&
  W.disjoint lhi1 lhi2 && W.disjoint llo1 llo2

let[@inline] quick_subset s1 s2 =
  not (disjoint s1 s2)

let compare_minimum s1 s2 =
  match is_empty s1, is_empty s2 with
  | true, true  ->  0
  | true, false -> -1
  | false, true -> +1
  | false, false ->
      let Q (hhi1, hlo1, lhi1, llo1) = s1
      and Q (hhi2, hlo2, lhi2, llo2) = s2 in
      match W.is_empty llo1, W.is_empty llo2 with
      | false, false -> W.compare_minimum llo1 llo2
      | true , false -> 1
      | false, true  -> -1
      | true , true  ->
        match W.is_empty lhi1, W.is_empty lhi2 with
        | false, false -> W.compare_minimum lhi1 lhi2
        | true , false -> 1
        | false, true  -> -1
        | true , true  ->
          match W.is_empty hlo1, W.is_empty hlo2 with
          | false, false -> W.compare_minimum hlo1 hlo2
          | true , false -> 1
          | false, true  -> -1
          | true , true  ->
            W.compare_minimum hhi1 hhi2

let sorted_union xs =
  List.fold_left union empty xs

let extract_unique_prefix s1 s2 =
  assert (not (is_empty s2));
  let Q (hhi1, hlo1, lhi1, llo1) = s1
  and Q (hhi2, hlo2, lhi2, llo2) = s2 in
  if not (W.is_empty llo2) then (
    let p, r = W.extract_unique_prefix llo1 llo2 in
    construct W.empty W.empty W.empty p,
    construct hhi1 hlo1 lhi1 r
  ) else if not (W.is_empty lhi2) then (
    let p, r = W.extract_unique_prefix lhi1 lhi2 in
    construct W.empty W.empty p llo1,
    construct hhi1 hlo1 r W.empty
  ) else if not (W.is_empty hlo2) then (
    let p, r = W.extract_unique_prefix hlo1 hlo2 in
    construct W.empty p lhi1 llo1,
    construct hhi1 r W.empty W.empty
  ) else (
    let p, r = W.extract_unique_prefix hhi1 hhi2 in
    construct p hlo1 lhi1 llo1,
    construct r W.empty W.empty W.empty
  )

let extract_shared_prefix s1 s2 =
  let Q (hhi1, hlo1, lhi1, llo1) = s1
  and Q (hhi2, hlo2, lhi2, llo2) = s2 in
  if not (W.equal llo1 llo2) then
    let ll, (llo1, llo2) = W.extract_shared_prefix llo1 llo2 in
    construct W.empty W.empty W.empty ll,
    (construct hhi1 hlo1 lhi1 llo1,
     construct hhi2 hlo2 lhi2 llo2)
  else if not (W.equal lhi1 lhi2) then
    let lh, (lhi1, lhi2) = W.extract_shared_prefix lhi1 lhi2 in
    construct W.empty W.empty lh llo1,
    (construct hhi1 hlo1 lhi1 W.empty,
     construct hhi2 hlo2 lhi2 W.empty)
  else if not (W.equal hlo1 hlo2) then
    let hl, (hlo1, hlo2) = W.extract_shared_prefix hlo1 hlo2 in
    construct W.empty hl lhi1 llo1,
    (construct hhi1 hlo1 W.empty W.empty,
     construct hhi2 hlo2 W.empty W.empty)
  else if not (W.equal hhi1 hhi2) then
    let hh, (hhi1, hhi2) = W.extract_shared_prefix hhi1 hhi2 in
    construct hh hlo1 lhi1 llo1,
    (construct hhi1 W.empty W.empty W.empty,
     construct hhi2 W.empty W.empty W.empty)
  else (* TODO useless branch? *)
    s1, (empty, empty)
