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

open Monolith

(* [R] is the reference implementation. *)
module R = Reference

(* [C] is the candidate implementation. *)
module Make (C : sig
  val bound : int
  include Bitsets.API.SET with type elt = int
  val check : t -> unit
  val name : string
end) = struct

let () =
  dprintf "          open %s;;\n" C.name

(* -------------------------------------------------------------------------- *)

(* Wrap the candidate implementation [C] with extra tests of the sharing
   guarantees. *)

module C = struct
  include C

  let add x s =
    let s' = add x s in
    if equal s s' then assert (s == s');
    s'

  let remove x s =
    let s' = remove x s in
    if equal s s' then assert (s == s');
    s'

  let union s1 s2 =
    let s' = union s1 s2 in
    if equal s' s2 then assert (s2 == s');
    s'

  let inter s1 s2 =
    let s' = inter s1 s2 in
    if equal s' s2 then assert (s2 == s');
    s'

  let diff s1 s2 =
    let s' = diff s1 s2 in
    if equal s' s1 then assert (s1 == s');
    s'

  let above x s =
    let s' = above x s in
    if equal s s' then assert (s == s');
    s'

  (* To make [partition] deterministic, we sort its result with respect to the
     lexicographic ordering on sets. *)

  (* It should be possible to implement [lexico] efficiently on bit sets.
     Let's not worry about this, for now, and use a naive implementation. *)

  let rec lexico s1 s2 =
    if is_empty s1 then -1 else
    if is_empty s2 then +1 else
    let c = compare_minimum s1 s2 in
    if c <> 0 then c else
    let x = minimum s1 in
    assert (x = minimum s2);
    lexico (remove x s1) (remove x s2)

  let sorted_partition xs =
    let ys = partition xs in
    List.sort lexico ys

  let () = dprintf {|
          let rec lexico s1 s2 =
            if is_empty s1 then -1 else
            if is_empty s2 then +1 else
            let c = compare_minimum s1 s2 in
            if c <> 0 then c else
            let x = minimum s1 in
            assert (x = minimum s2);
            lexico (remove x s1) (remove x s2)

          let sorted_partition xs =
            let ys = partition xs in
            List.sort lexico ys
|}

end

(* -------------------------------------------------------------------------- *)

(* The abstract type [t]. *)

let check _model =
  C.check, constant "check"

let t =
  declare_abstract_type ~check ()

(* The type [elt]. *)

let elt =
  lt C.bound

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  (* Construction. *)

  let spec = t in
  declare "empty" spec R.empty C.empty;

  let spec = elt ^> t in
  declare "singleton" spec R.singleton C.singleton;

  let spec = elt ^> t ^> t in
  declare "add" spec R.add C.add;

  let spec = elt ^> t ^> t in
  declare "remove" spec R.remove C.remove;

  let spec = t ^> t ^> t in
  declare "union" spec R.union C.union;

  let spec = t ^> t ^> t in
  declare "inter" spec R.inter C.inter;

  let spec = t ^> t ^> t in
  declare "diff" spec R.diff C.diff;

  let spec = elt ^> t ^> t in
  declare "above" spec R.above C.above;

  (* Cardinality. *)

  let spec = t ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = t ^> bool in
  declare "is_singleton" spec R.is_singleton C.is_singleton;

  let spec = t ^> int in
  declare "cardinal" spec R.cardinal C.cardinal;

  (* Tests. *)

  let spec = elt ^> t ^> bool in
  declare "mem" spec R.mem C.mem;

  let spec = t ^> t ^> bool in
  declare "equal" spec R.equal C.equal;

  (* [compare] is not tested. It would be difficult to test, because its
     specification states that it implements "a total order". *)

  let spec = t ^> t ^> bool in
  declare "disjoint" spec R.disjoint C.disjoint;

  let spec = t ^> t ^> bool in
  declare "subset" spec R.subset C.subset;

  let spec = t ^>> fun s1 -> (R.qs s1 % t) ^> bool in
  declare "quick_subset" spec R.quick_subset C.quick_subset;

  (* Extraction. *)

  (* We test that [choose] produces the minimum element of the set. This is
     not guaranteed by the specification of [choose], but testing a stronger
     specification is easier. *)

  let spec = t ^!> elt in
  declare "minimum" spec R.minimum C.minimum;
  declare "maximum" spec R.maximum C.maximum;
  declare "choose" spec R.choose C.choose;

  (* Iteration. *)

  let spec = iter (t ^> list elt) in
  declare "iter" spec R.iter C.iter;

  let spec = foldr (t ^> list elt) in
  declare "fold" spec R.fold C.fold;

  (* We test that [elements] produces a list that is sorted in decreasing
     order. This is not guaranteed by the specification of [choose], but
     testing a stronger specification is easier. *)

  let spec = t ^> list elt in
  declare "elements" spec R.rev_elements C.elements;

  let spec = list elt ^> t in
  declare "of_list" spec R.of_list C.of_list;

  (* We test [find_first_opt] with the fixed predicate [div7]. *)

          let div7 x = x mod 7 = 0 in
  dprintf {|
          let div7 x = x mod 7 = 0;;
|};
  let spec = t ^> option elt in
  declare "find_first_opt div7" spec (R.find_first_opt div7) (C.find_first_opt div7);

  (* Decomposition. *)

  let spec = t ^> t ^> comparison in
  declare "compare_minimum" spec R.compare_minimum C.compare_minimum;

  let spec = list t ^> t in
  declare "big_union" spec R.big_union C.big_union;

  let spec = t ^> R.nonempty % t ^> t *** t in
  declare "extract_unique_prefix" spec
    R.extract_unique_prefix
    C.extract_unique_prefix;

  let spec = t ^> t ^> t *** (t *** t) in
  declare "extract_shared_prefix" spec
    R.extract_shared_prefix
    C.extract_shared_prefix;

  let spec = list t ^> list t in
  declare "sorted_partition" spec R.sorted_partition C.sorted_partition;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 16 in
  main fuel

end (* Make *)
