(******************************************************************************)
(*                                                                            *)
(*                                  IntPQueue                                 *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

open Monolith

(* This is the candidate implementation. *)
module C = struct

  include Bitsets.LeftistHeap.Make(Int)

  (* To test [pop2], we define [pop2'], a wrapper whose type is simpler. *)

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
module R = struct

  include Reference.Make(Int)

  let format =
    PPrint.utf8format

  let show (k, v) =
    Printf.sprintf "(%d, %d)" k v

  let rec remove (k, v) kvs =
    match kvs with
    | [] ->
        raise Not_found
    | (k', v') :: kvs ->
        if k = k && v = v' then kvs else (k', v') :: remove (k, v) kvs

  (* Because [pop2] is non-deterministic, our reference implementation
     of [pop2'] must validate and simulate the result of the candidate
     implementation. *)

  let pop2' (q : 'a heap) (result : (int * 'a) list * 'a C.heap) =
    let kvs, _cq = result in
    match kvs with
    | [kv1; kv2] ->
        begin match remove kv1 (remove kv2 q) with
        | q ->
            Monolith.Valid ([kv1; kv2], q)
        | exception Not_found ->
            let cause _doc =
              format "(* candidate returns [%s; %s]; at least one of these pairs does not exist *)"
                (show kv1) (show kv2)
            in
            Monolith.Invalid cause
        end
    | [kv1] ->
        begin match remove kv1 q with
        | q ->
            if q = [] then
              Monolith.Valid ([kv1], q)
            else
              let cause _doc =
                format "(* candidate returns only one element; there are more *)"
              in
              Monolith.Invalid cause
        | exception Not_found ->
            let cause _doc =
              format "(* candidate returns [%s]; this pair does not exist *)"
                (show kv1)
            in
            Monolith.Invalid cause
        end
    | [] ->
        if q = [] then
          Monolith.Valid ([], q)
        else
          let cause _doc =
            format "(* candidate returns zero element; there are some *)"
          in
          Monolith.Invalid cause
    | _ ->
        (* [pop2'] never returns more than two elements *)
        assert false

end

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

  let spec = t ^> nondet (list (key *** value) *** t) in
  declare "pop2'" spec R.pop2' C.pop2';

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 32 in
  main fuel
