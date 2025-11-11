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

(* We use integer keys and integer values. *)
module Key = struct
  include Int
  let show k = Printf.sprintf "%d" k
end
module Val = Key

(* This is the candidate implementation. *)
module C = struct

  include Bitsets.LeftistHeap.Make(Key)

  (* To test [pop2], we define [pop2'], a wrapper whose type does not involve
     the type ['a pop2]. Another approach would be to tell Monolith about the
     type ['a pop2] by using the combinator [map_into]. That would be a slightly
     more elaborate way of achieving the same effect. *)

  let pop2' q =
    match pop2 q with
    | Head (k1, v1, k2, v2, q) ->
        [(k1, v1); (k2, v2)], q
    | Tail (k, v) ->
        [(k, v)], empty
    | Done ->
        [], empty

end

(* This is the reference implementation. *)
module R = Reference.Make(Key)(Val)

let () =
  dprintf "          open Bitsets.LeftistHeap.Make(Int);;\n"

(* -------------------------------------------------------------------------- *)

(* The abstract type [t]. *)

(* This type is equipped with a well-formedness check,
   which ignores the model (the reference side). *)

let check _model =
  C.check,
  constant "check"

let t =
  declare_abstract_type ~check ()

(* -------------------------------------------------------------------------- *)

(* Random keys and values. *)

let key =
  lt 16

let value =
  lt 16

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = t in
  declare "empty" spec R.empty C.empty;

  let spec = value ^> key ^> t in
  declare "singleton" spec R.singleton C.singleton;

  let spec = value ^> key ^> t ^> t in
  declare "insert" spec R.insert C.insert;

  let spec = t ^> t ^> t in
  declare "merge" spec R.merge C.merge;

  let spec = t ^> nondet (option ((key *** value) *** t)) in
  declare "pop" spec R.pop C.pop;

  let spec = t ^> nondet (list (key *** value) *** t) in
  declare "pop2'" spec R.pop2' C.pop2';

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 32 in
  main fuel
