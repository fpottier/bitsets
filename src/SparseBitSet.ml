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

(* This data structure implements sets of integers (of unbounded magnitude). *)

module W =
  WordBitSet

(* A sparse bit set is a linked list pairs of an index and a bit set. The list
   is sorted by order of increasing indices. *)

type offset =
  int (* a multiple of [W.bound] *)

let compare_offsets (o1 : offset) (o2 : offset) =
  compare o1 o2

type word =
  W.t

type t =
  | N
  | C of offset * word * t

let check_offset (o : offset) =
  assert (0 <= o);
  assert (o mod W.bound = 0)

let rec check o s =
  match s with
  | N -> ()
  | C (o', w, s) ->
      check_offset o';
      assert (o < o');
      assert (not (W.is_empty w));
      check o' s

let check s =
  match s with
  | N -> ()
  | C (o, w, s) ->
      check_offset o;
      assert (not (W.is_empty w));
      check o s

type elt =
  int

let empty =
  N

let is_empty = function
  | N ->
      true
  | C _ ->
      false

let rec add base offset s =
  match s with
  | N ->
      (* Insert at end. *)
      C (base, W.singleton offset, N)
  | C (addr, ss, qs) ->
      if base < addr then
        (* Insert in front. *)
        C (base, W.singleton offset, s)
      else if base = addr then
        (* Found appropriate cell, update bit field. *)
        let ss' = W.add offset ss in
        if W.equal ss' ss then s else C (addr, ss', qs)
      else
        (* Not there yet, continue. *)
        let qs' = add base offset qs in
        if qs == qs' then s else C (addr, ss, qs')

let add i s =
  let offset = i mod W.bound in
  let base = i - offset in
  add base offset s

let singleton i =
  (* This is [add i N], specialised. *)
  let offset = i mod W.bound in
  let base = i - offset in
  C (base, W.singleton offset, N)

let rec remove base offset s =
  match s with
  | N ->
      N
  | C (addr, ss, qs) ->
      if base < addr then
        s
      else if base = addr then
        (* Found appropriate cell, update bit field. *)
        let ss' = W.remove offset ss in
        if W.is_empty ss' then
          qs
        else if W.equal ss' ss then s else C (addr, ss', qs)
      else
        (* Not there yet, continue. *)
        let qs' = remove base offset qs in
        if qs == qs' then s else C (addr, ss, qs')

let remove i s =
  let offset = i mod W.bound in
  let base = i - offset in
  remove base offset s

let rec mem base offset s =
  match s with
  | N ->
      false
  | C (addr, ss, qs) ->
      if base < addr then
        false
      else if base = addr then
        W.mem offset ss
      else
        mem base offset qs

let mem i s =
  let offset = i mod W.bound in
  let base = i - offset in
  mem base offset s

let rec fold f s accu =
  match s with
  | N ->
      accu
  | C (addr, ss, qs) ->
      let accu = W.fold_delta addr f ss accu in
      fold f qs accu

let rec iter f s =
  match s with
  | N ->
      ()
  | C (addr, ss, qs) ->
      W.iter_delta addr f ss;
      iter f qs

let is_singleton s =
  match s with
  | C (_, ss, N) ->
      W.is_singleton ss
  | C (_, _, C _)
  | N ->
      false

let rec cardinal accu s =
  match s with
  | C (_, ss, qs) ->
      let accu = accu + W.cardinal ss in
      cardinal accu qs
  | N ->
      accu

let cardinal s =
  cardinal 0 s

let elements s =
  fold (fun tl hd -> tl :: hd) s []

let rec subset s1 s2 =
  match s1, s2 with
  | N, _ ->
      true
  | _, N ->
      false
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then
        false
      else if addr1 = addr2 then
        W.subset ss1 ss2 && subset qs1 qs2
      else
        subset s1 qs2

(* [union] preserves sharing (if possible) between its second argument
   and its result. *)

let rec union s1 s2 =
  match s1, s2 with
  | N, s
  | s, N ->
      s
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then
        C (addr1, ss1, union qs1 s2)
      else if addr1 > addr2 then
        let s = union s1 qs2 in
        if s == qs2 then s2 else C (addr2, ss2, s)
      else
        let ss = W.union ss1 ss2 in
        let s = union qs1 qs2 in
        if W.equal ss ss2 && s == qs2 then s2 else C (addr1, ss, s)

(* [inter] arbitrarily attempts to preserve sharing between its first
   argument and its result. *)

let rec inter s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
      N
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then
        inter qs1 s2
      else if addr1 > addr2 then
        inter s1 qs2
      else
        let ss = W.inter ss1 ss2 in
        let s = inter qs1 qs2 in
        if W.is_empty ss then
          s
        else
          if W.equal ss ss1 && s == qs1 then s1 else C (addr1, ss, s)

let minimum s =
  match s with
  | N ->
      raise Not_found
  | C (addr, ss, _) ->
      addr + W.minimum ss

let rec maximum1 o w s =
  match s with
  | N ->
      o + W.maximum w
  | C (o, w, s) ->
      maximum1 o w s

let maximum s =
  match s with
  | N ->
      raise Not_found
  | C (o, w, s) ->
      maximum1 o w s

let choose =
  minimum

let rec compare x y =
  if x == y then 0 else
    match x, y with
    | C (a1, ss1, qs1), C (a2, ss2, qs2) ->
      begin match compare_offsets a1 a2 with
        | 0 -> begin match W.compare ss1 ss2 with
            | 0 -> compare qs1 qs2
            | n -> n
          end
        | n -> n
      end
    | N, N -> 0
    | C _, N -> 1
    | N, C _ -> -1

let rec equal x y =
  (x == y) ||
  match x, y with
  | C (a1, ss1, qs1), C (a2, ss2, qs2) ->
    a1 = a2 &&
    W.equal ss1 ss2 &&
    equal qs1 qs2
  | N, N -> true
  | C _, N | N, C _ -> false

let rec disjoint s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
      true
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 = addr2 then
        W.disjoint ss1 ss2 && disjoint qs1 qs2
      else if addr1 < addr2 then
        disjoint qs1 s2
      else
        disjoint s1 qs2

let rec quick_subset a1 ss1 = function
  | N ->
      false
  | C (a2, ss2, qs2) ->
      if a1 = a2 then
        W.quick_subset ss1 ss2
      else
        (a1 > a2 && quick_subset a1 ss1 qs2)

let quick_subset s1 s2 =
  match s1 with
  | N ->
      false
  | C (a1, ss1, _) ->
      (* We know that, by construction, [ss1] is not empty. It suffices to
         test whether [s2] has elements in common with [ss1] at address [a1]. *)
      quick_subset a1 ss1 s2

let compare_minimum s1 s2 =
  match s1, s2 with
  | N, N ->
      0
  | N, _ ->
      -1
  | _, N ->
      1
  | C (addr1, ss1, _), C (addr2, ss2, _) ->
      match Int.compare addr1 addr2 with
      | 0 -> W.compare_minimum ss1 ss2
      | n -> n

let sorted_union xs =
  (* It is important to start folding from the right end of the list. Since
     elements are sorted, by starting from the right end, we only prepend
     elements. This makes the algorithm linear in the number of items.
     Starting from the left end would make it quadratic, revisiting the
     prefix of a list that gets longer and longer as elements are added. *)
  List.fold_right union xs empty

let rec extract_unique_prefix addr2 ss2 = function
  | N ->
      N, N
  | C (addr1, ss1, qs1) as self ->
    if addr1 < addr2 then
      let prefix, suffix = extract_unique_prefix addr2 ss2 qs1 in
      C (addr1, ss1, prefix), suffix
    else if addr1 > addr2 || W.equal ss1 ss2 then
      N, self
    else
      (* l and r have the same address, and
         l has some prefix that is not part of r (lsb l < lsb r)*)
      let ss0, ss1 = W.extract_unique_prefix ss1 ss2 in
      if W.is_empty ss0 then
        N, self
      else if W.is_empty ss1 then
        (C (addr1, ss0, N), qs1)
      else
        (C (addr1, ss0, N), C (addr1, ss1, qs1))

let extract_unique_prefix l r =
  match l, r with
  | N, _ ->
      N, N
  | _, N ->
      invalid_arg "extract_unique_prefix"
  | l, C (addr2, ss2, _) ->
      extract_unique_prefix addr2 ss2 l

let rec extract_shared_prefix s1 s2 =
  match s1, s2 with
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2)
    when addr1 = addr2 ->
      if W.equal ss1 ss2 then
        let common, rest = extract_shared_prefix qs1 qs2 in
        (C (addr1, ss1, common), rest)
      else
        let common, (ss1, ss2) = W.extract_shared_prefix ss1 ss2 in
        let common = if W.is_empty common then N else C (addr1, common, N) in
        let qs1 = if W.is_empty ss1 then qs1 else C (addr1, ss1, qs1) in
        let qs2 = if W.is_empty ss2 then qs2 else C (addr2, ss2, qs2) in
        common, (qs1, qs2)
  | (l, r) ->
      N, (l, r)

let rec diff s1 s2 =
  match s1, s2 with
  | N, _ | _, N ->
      s1
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then (
        let qs1' = diff qs1 s2 in
        if qs1' == qs1 then
          s1
        else
          C (addr1, ss1, qs1')
      )
      else if addr1 > addr2 then
        diff s1 qs2
      else
        let ss = W.diff ss1 ss2 in
        if W.is_empty ss then
          diff qs1 qs2
        else
          let qs1' = diff qs1 qs2 in
          if W.equal ss ss1 && qs1' == qs1 then
            s1
          else
            C (addr1, ss, qs1')

let above elt s =
  let offset = elt mod W.bound in
  let base = elt - offset in
  let rec loop = function
    | N ->
        N
    | C (base', ss, qs) as s ->
        if base < base' then
          (* Stop now. *)
          s
        else if base = base' then
          (* Found appropriate cell, split bit field. *)
          let ss' = W.above offset ss in
          if W.is_empty ss' then
            qs
          else if W.equal ss ss' then
            s
          else
            C (base', ss', qs)
        else
          (* Not there yet, continue. *)
          loop qs
  in
  loop s

exception Found of elt

let rec find_first f s =
  match s with
  | N ->
      None
  | C (base, ss, qs) ->
      W.iter_delta base (fun elt -> if f elt then raise (Found elt)) ss;
      find_first f qs

let find_first_opt f qs =
  try
    find_first f qs
  with Found elt ->
    Some elt

type view = t =
  | N
  | C of offset * word * t

let[@inline] view x = x
