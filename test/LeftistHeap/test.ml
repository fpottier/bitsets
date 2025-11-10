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

let format = PPrint.utf8format
open Monolith
let valid x = Valid x
let invalid f = Invalid f

(* We use integer keys and values. *)
module Key = Int
module Val = Int
module KVPair = struct
  type t = Key.t * Val.t
  let show (k, v) =
    Printf.sprintf "(%d, %d)" k v
  let equal (k, v) (k', v') =
    Key.equal k k' && Val.equal v v'
  let leq (k, _) (k', _) =
    Key.compare k k' <= 0
end

(* This is the candidate implementation. *)
module C = struct

  include Bitsets.LeftistHeap.Make(Key)

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

  include Reference.Make(Key)

  exception Absent of KVPair.t

  (* [remove kv kvs] removes the key-value pair [kv] from the list [kvs].
     If [kv] does not appear in the list [kvs], [Absent kv] is raised. *)

  let rec remove kv kvs =
    match kvs with
    | [] ->
        raise (Absent kv)
    | kv' :: kvs ->
        if KVPair.equal kv kv' then kvs else kv' :: remove kv kvs

  (* [check_minimal kv kvs] checks that the key-value pair [kv] is minimal
     with respect to the list [kvs], that is, every key in the list is greater
     than or equal to the key in the pair [kv]. If this is not the case,
     [NotMinimal kv] is raised. *)

  let is_minimal kv kvs =
    List.for_all (KVPair.leq kv) kvs

  exception NotMinimal of KVPair.t

  let check_minimal kv kvs =
    if not (is_minimal kv kvs) then
      raise (NotMinimal kv)

  let remove_minimal kv kvs =
    let kvs' = remove kv kvs in (* may raise [Absent] *)
    check_minimal kv kvs;       (* may raise [NotMinimal] *)
    kvs'

  (* [handle action] runs [action()] and handles the exception [Absent]
     and [NotMinimal] by returning an [Invalid] diagnostic. *)

  let handle (action : unit -> 'a diagnostic) : 'a diagnostic =
    try
      action()
    with
    | Absent kv ->
        invalid @@ fun _doc ->
        format "(* candidate returns %s, which does not exist *)"
          (KVPair.show kv)
    | NotMinimal kv ->
        invalid @@ fun _doc ->
        format "(* candidate returns %s, which is not minimal *)"
          (KVPair.show kv)

  (* [pop] is non-deterministic: when two key-value pairs have the same key,
     either of them can be returned. Therefore our reference implementation of
     [pop] must validate and simulate the choice that is made by the candidate
     implementation. We check that the key-value pair [kv] chosen by the
     candidate exists in the queue on the reference side and is minimal. *)

  let pop (q : 'a t) (result : ((int * 'a) * 'a C.t) option) =
    match result with
    | Some (kv, _) ->
        handle @@ fun () ->
        let q = remove_minimal kv q in
        valid (Some (kv, q))
    | None ->
        if q = [] then
          valid None
        else
          invalid @@ fun _doc ->
          format "(* candidate returns None, yet queue is nonempty *)"

  (* Testing [pop2'] is analogous. *)

  let pop2' (q : 'a heap) (result : (int * 'a) list * 'a C.heap) =
    let kvs, _cq = result in
    match kvs with
    | [kv1; kv2] ->
        handle @@ fun () ->
        let q = remove_minimal kv1 q in
        let q = remove_minimal kv2 q in
        valid (kvs, q)
    | [kv1] ->
        handle @@ fun () ->
        let q = remove_minimal kv1 q in
        if q = [] then
          valid (kvs, q)
        else
          invalid @@ fun _doc ->
          format "(* candidate returns only one element; there are more *)"
    | [] ->
        if q = [] then
          valid (kvs, q)
        else
          invalid @@ fun _doc ->
          format "(* candidate returns zero element; there are some *)"
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
