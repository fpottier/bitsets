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
  | E
  | Q of W.t * W.t * W.t * W.t

let construct hhi hlo lhi llo =
  if W.is_empty hhi && W.is_empty hlo && W.is_empty lhi && W.is_empty llo then
    E
  else
    Q (hhi, hlo, lhi, llo)

let check s =
  match s with
  | E -> ()
  | Q (hhi, hlo, lhi, llo) ->
      if W.is_empty hhi && W.is_empty hlo && W.is_empty lhi && W.is_empty llo then
        assert false

let empty =
  E

let is_empty s =
  match s with
  | E ->
      true
  | Q (_, _, _, _) ->
      false

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
  match s with
  | E ->
      singleton i
  | Q (hhi, hlo, lhi, llo) ->
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
  match s with
  | E ->
      s
  | Q (hhi, hlo, lhi, llo) ->
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
  match s with
  | E ->
      accu
  | Q (hhi, hlo, lhi, llo) ->
      let accu = W.fold f llo accu in
      let accu = W.fold_delta quarter f lhi accu in
      let accu = W.fold_delta middle f hlo accu in
      let accu = W.fold_delta quarter3 f hhi accu in
      accu

let iter f s =
  match s with
  | E ->
      ()
  | Q (hhi, hlo, lhi, llo) ->
      W.iter f llo;
      W.iter_delta quarter f lhi;
      W.iter_delta middle f hlo;
      W.iter_delta quarter3 f hhi

let is_singleton s =
  match s with
  | E ->
      false
  | Q (hhi, hlo, lhi, llo) ->
      W.is_singleton hhi && W.is_empty hlo && W.is_empty lhi && W.is_empty llo ||
      W.is_empty hhi && W.is_singleton hlo && W.is_empty lhi && W.is_empty llo ||
      W.is_empty hhi && W.is_empty hlo && W.is_singleton lhi && W.is_empty llo ||
      W.is_empty hhi && W.is_empty hlo && W.is_empty lhi && W.is_singleton llo

let cardinal s =
  match s with
  | E ->
      0
  | Q (hhi, hlo, lhi, llo) ->
      W.cardinal hhi + W.cardinal hlo + W.cardinal lhi + W.cardinal llo

let elements s =
  fold (fun tl hd -> tl :: hd) s []

let subset s1 s2 =
  match s1, s2 with
  | E, _ ->
      true
  | Q (_, _, _, _), E ->
      false
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
      W.subset hhi1 hhi2 && W.subset hlo1 hlo2 &&
      W.subset lhi1 lhi2 && W.subset llo1 llo2

let mem i s =
  match s with
  | E ->
      false
  | Q (hhi, hlo, lhi, llo) ->
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
  match s1, s2 with
  | E, s
  | s, E ->
      s
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
      let hhi = W.union hhi1 hhi2
      and hlo = W.union hlo1 hlo2
      and lhi = W.union lhi1 lhi2
      and llo = W.union llo1 llo2 in
      if hhi == hhi2 && hlo == hlo2 && lhi == lhi2 && llo == llo2 then s2
      else Q (hhi, hlo, lhi, llo)

let inter s1 s2 =
  match s1, s2 with
  | E, _
  | _, E ->
      E
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
      construct
        (W.inter hhi1 hhi2) (W.inter hlo1 hlo2)
        (W.inter lhi1 lhi2) (W.inter llo1 llo2)

let diff s1 s2 =
  match s1, s2 with
  | E, _
  | _, E ->
      s1
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
      construct
        (W.diff hhi1 hhi2) (W.diff hlo1 hlo2)
        (W.diff lhi1 lhi2) (W.diff llo1 llo2)

let above x s =
  match s with
  | E -> E
  | Q (hhi, hlo, lhi, llo) ->
      if x < quarter then
        construct hhi hlo lhi (W.above x llo)
      else if x < middle then
        construct hhi hlo (W.above (x - quarter) lhi) W.empty
      else if x < quarter3 then
        construct hhi (W.above (x - middle) hlo) W.empty W.empty
      else
        construct (W.above (x - quarter3) hhi) W.empty W.empty W.empty

let minimum s =
  match s with
  | E ->
      raise Not_found
  | Q (hhi, hlo, lhi, llo) ->
      if not (W.is_empty llo) then
        W.minimum llo
      else if not (W.is_empty lhi) then
        W.minimum lhi + quarter
      else if not (W.is_empty hlo) then
        W.minimum hlo + middle
      else
        W.minimum hhi + quarter3

let maximum s =
  match s with
  | E ->
      raise Not_found
  | Q (hhi, hlo, lhi, llo) ->
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
  if s1 == s2 then 0
  else match s1, s2 with
  | E  , E   -> 0
  | Q _, E   -> 1
  | E  , Q _ -> -1
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
    begin match W.compare hhi1 hhi2 with
      | 0 ->
        begin match W.compare hlo1 hlo2 with
          | 0 ->
            begin match W.compare lhi1 lhi2 with
              | 0 -> W.compare llo1 llo2
              | n -> n
            end
          | n -> n
        end
      | n -> n
    end

let equal s1 s2 =
  (s1 == s2) ||
  match s1, s2 with
  | E , E -> true
  | Q _, E | E , Q _ -> false
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
    W.equal hhi1 hhi2 &&
    W.equal hlo1 hlo2 &&
    W.equal lhi1 lhi2 &&
    W.equal llo1 llo2

let disjoint s1 s2 =
  match s1, s2 with
  | E, _
  | _, E ->
      true
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
      W.disjoint hhi1 hhi2 && W.disjoint hlo1 hlo2 &&
      W.disjoint lhi1 lhi2 && W.disjoint llo1 llo2

let quick_subset s1 s2 =
  match s1, s2 with
  | E, _ | _, E -> false
  | Q (hh1, hl1, lh1, ll1), Q (hh2, hl2, lh2, ll2) ->
    W.quick_subset ll1 ll2 ||
    W.quick_subset lh1 lh2 ||
    W.quick_subset hl1 hl2 ||
    W.quick_subset hh1 hh2

let compare_minimum s1 s2 =
  match s1, s2 with
  | E, E -> 0
  | E, _ -> -1
  | _, E -> 1
  | Q (hh1, hl1, lh1, ll1), Q (hh2, hl2, lh2, ll2) ->
    match W.is_empty ll1, W.is_empty ll2 with
    | false, false -> W.compare_minimum ll1 ll2
    | true , false -> 1
    | false, true  -> -1
    | true , true  ->
      match W.is_empty lh1, W.is_empty lh2 with
      | false, false -> W.compare_minimum lh1 lh2
      | true , false -> 1
      | false, true  -> -1
      | true , true  ->
        match W.is_empty hl1, W.is_empty hl2 with
        | false, false -> W.compare_minimum hl1 hl2
        | true , false -> 1
        | false, true  -> -1
        | true , true  ->
          W.compare_minimum hh1 hh2

let sorted_union xs =
  List.fold_left union empty xs

let extract_unique_prefix s1 s2 =
  assert (not (is_empty s2));
  match s1, s2 with
  | E, _ -> E, E
  | _, E -> assert false
  | Q (hh1, hl1, lh1, ll1), Q (hh2, hl2, lh2, ll2) ->
    if not (W.is_empty ll2) then (
      let p, r = W.extract_unique_prefix ll1 ll2 in
      construct W.empty W.empty W.empty p,
      construct hh1 hl1 lh1 r
    ) else if not (W.is_empty lh2) then (
      let p, r = W.extract_unique_prefix lh1 lh2 in
      construct W.empty W.empty p ll1,
      construct hh1 hl1 r W.empty
    ) else if not (W.is_empty hl2) then (
      let p, r = W.extract_unique_prefix hl1 hl2 in
      construct W.empty p lh1 ll1,
      construct hh1 r W.empty W.empty
    ) else (
      let p, r = W.extract_unique_prefix hh1 hh2 in
      construct p hl1 lh1 ll1,
      construct r W.empty W.empty W.empty
    )

let extract_shared_prefix s1 s2 =
  match s1, s2 with
  | _, E | E, _ -> E, (s1, s2)
  | Q (hh1, hl1, lh1, ll1), Q (hh2, hl2, lh2, ll2) ->
    if not (W.equal ll1 ll2) then
      let ll, (ll1, ll2) = W.extract_shared_prefix ll1 ll2 in
      construct W.empty W.empty W.empty ll,
      (construct hh1 hl1 lh1 ll1,
       construct hh2 hl2 lh2 ll2)
    else if not (W.equal lh1 lh2) then
      let lh, (lh1, lh2) = W.extract_shared_prefix lh1 lh2 in
      construct W.empty W.empty lh ll1,
      (construct hh1 hl1 lh1 W.empty,
       construct hh2 hl2 lh2 W.empty)
    else if not (W.equal hl1 hl2) then
      let hl, (hl1, hl2) = W.extract_shared_prefix hl1 hl2 in
      construct W.empty hl lh1 ll1,
      (construct hh1 hl1 W.empty W.empty,
       construct hh2 hl2 W.empty W.empty)
    else if not (W.equal hh1 hh2) then
      let hh, (hh1, hh2) = W.extract_shared_prefix hh1 hh2 in
      construct hh hl1 lh1 ll1,
      (construct hh1 W.empty W.empty W.empty,
       construct hh2 W.empty W.empty W.empty)
    else
      s1, (E, E)
