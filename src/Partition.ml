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

module[@inline] Make (Set : sig
  type t
  val is_empty : t -> bool
  val compare_minimum : t -> t -> int
  val big_union : t list -> t
  val extract_unique_prefix : t -> t -> t * t
  val extract_shared_prefix : t -> t -> t * (t * t)
end) = struct

type set =
  Set.t

type sets =
  set list

let[@inline] nonempty (s : set) =
  not (Set.is_empty s)

module H =
  LeftistHeap.Make(struct
    type t = Set.t
    let compare = Set.compare_minimum
  end)
open H

let[@inline] insert_if_nonempty s k heap : 'v heap =
  if nonempty s then H.insert s k heap else heap

(* In the following, we choose an implementation of [IntSet] based on the
   value of [n], which is (at most) the length of the list [ss]. Thus, if the
   list [ss] is short enough, [IntSet] is [WordMini], which is very efficient
   (a set is just one word). Otherwise, [IntSet] is [SparseMini]. *)

(* We cannot use any of the modules [...BitSet] because that would introduce a
   cyclic dependency; these modules use [Partition.Make]. *)

module Run (N : sig val n : int end) () = struct

  module IntSet =
    BoundedMini.Make(N)()

  type key =
    IntSet.t

  type part =
    set * key

  type parts =
    part list

  let[@inline] cons_if_nonempty s k parts : parts =
    if nonempty s then (s, k) :: parts else parts

  (* [loop] grows the list [parts] by extracting pairs [(s, k)] out of
     the priority queue [heap]. A better comment would be desirable! *)

  let rec loop (parts : parts) (heap : key heap) : parts =
    match pop2 heap with
    | Head (s1, k1, s2, k2, heap) ->
        let sp, s1 = Set.extract_unique_prefix s1 s2 in
        let sc, (s1, s2) = Set.extract_shared_prefix s1 s2 in
        let parts = cons_if_nonempty sp k1 parts in
        let heap =
          (* [insert_if_nonempty sc (IntSet.union k1 k2)] *)
          if nonempty sc then insert sc (IntSet.union k1 k2) heap
          else heap
        in
        let heap = insert_if_nonempty s1 k1 heap in
        let heap = insert_if_nonempty s2 k2 heap in
        loop parts heap
    | Tail (k, v) ->
        (k, v) :: parts
    | Done ->
        parts

  (* [prepare ss] builds a heap of pairs [(s, i)] where [s] ranges
     over the nonempty sets in the list [ss] and where [i] is a
     singleton set that contains a unique integer index. *)

  let prepare (ss : sets) : key heap =
    let c = ref 0 in
    List.fold_left (fun h (s : set) ->
      (* Any empty sets in the list [ss] are ignored on the fly. *)
      if nonempty s then
        let i = !c in
        c := i + 1;
        insert s (IntSet.singleton i) h
      else
        h
    ) empty ss

  (* [union_parts] expects a list of pairs [(s, k)] where [s] is a set and [k]
     is a key. It finds all sets that have a common key and fuses them. The
     result is a list of sets. *)

  let by_key (_, k1) (_, k2) =
    IntSet.compare k1 k2

  let rec merge accu ss k parts : sets =
    match parts with
    | [] ->
        (* [merge] intentionally uses an accumulator [ss] of type [set list],
           as opposed to just [set]. Indeed, it can be more efficient to use
           [Set.big_union] just once at the end, as opposed to repeatedly
           using [Set.union] along the way. *)
        Set.big_union ss :: accu
    | (s, k') :: parts ->
        if IntSet.equal k k' then
          merge accu (s :: ss) k parts
        else
          merge (Set.big_union ss :: accu) [s] k' parts

  let union_parts (parts : parts) : sets =
    (* Sort the pairs [(s, k)] by key. *)
    match List.sort by_key parts with
    | [] ->
        []
    | (s1, k1) :: parts ->
        (* Merge the sets that have the same key. *)
        merge [] [s1] k1 parts

  let partition ss =
    ss
    |> prepare
    |> loop []
    |> union_parts

end (* Run *)

(* [count_nonempty ss] counts the nonempty sets in the list [ss]. *)

let rec count_nonempty accu (ss : sets) : int =
  match ss with
  | [] ->
      accu
  | s :: ss ->
      let accu = if nonempty s then accu + 1 else accu in
      count_nonempty accu ss

let[@inline] count_nonempty (ss : sets) : int =
  count_nonempty 0 ss

let partition (ss : sets) : sets =
  let module R = Run(struct let n = count_nonempty ss end)() in
  R.partition ss

end (* Make *)
