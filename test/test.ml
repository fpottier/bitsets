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

open Monolith

(* This is the reference implementation. *)
module R = Reference

(* This is the candidate implementation. *)
module C = Bitsets.WordBitSet

(* -------------------------------------------------------------------------- *)

          let comparison_eq c1 c2 =
            c1 < 0 && c2 < 0 || c1 = 0 && c2 = 0 || c1 > 0 && c2 > 0

let () =
  dprintf {|
          let comparison_eq c1 c2 =\n\
            c1 < 0 && c2 < 0 || c1 = 0 && c2 = 0 || c1 > 0 && c2 > 0;;
|}

let comparison : (int, int) spec =
  let equal : (int -> int -> bool) code =
    comparison_eq, constant "comparison_eq"
  in
  deconstructible ~equal Print.int

(* -------------------------------------------------------------------------- *)

(* The abstract type [t]. *)

let t =
  declare_abstract_type ()

(* The type [elt]. *)

let elt =
  lt C.bound

(* The function [prepare], which we define on both sides (reference and
   candidate), transforms a list of sets into a a sorted list of disjoint
   non-overlapping sets. Such a list forms a suitable argument for the
   function [sorted_union], which we wish to test. *)

(* Ideally, we should print the source code of [prepare] on the candidate
   side, but this is a pain. Let's not do it. Thus, if a test fails, the
   test scenario will show a call to [prepare], but the definition of
   [prepare] will not be printed, so this scenario will not be easily
   replayable. Never mind, for now. *)

module RP = Prepare.Make(R)
module CP = Prepare.Make(C)

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

  let spec = (R.nonempty % t) ^>> fun s1 -> (R.qs s1 % t) ^> bool in
  declare "quick_subset" spec R.subset C.quick_subset;

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

  (* Decomposition. *)

  let spec = t ^> t ^> comparison in
  declare "compare_minimum" spec R.compare_minimum C.compare_minimum;

  let spec = list t ^> t in
  declare "(Fun.compose sorted_union prepare)"
    spec
    (Fun.compose R.sorted_union RP.prepare)
    (Fun.compose C.sorted_union CP.prepare);

  (* TODO *)

  let spec = elt ^> t ^> t in
  declare "above" spec R.above C.above;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  dprintf "          open Bitsets;;\n"

let () =
  let fuel = 16 in
  main fuel
