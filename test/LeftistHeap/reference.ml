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

(* This is a reference implementation of an immutable priority queue.
   It uses an unsorted list of key-value pairs. *)

(* One could use a sorted list and exploit the fact that the list is sorted to
   speed up [remove] and [is_minimal]. In terms of efficiency, this would not
   make much of a difference. It is preferable to make this implementation as
   simple as possible. *)

module Make
(Key : sig
  type t
  val compare: t -> t -> int
  val show: t -> string
end)
(Val : sig
  type t
  val compare: t -> t -> int
  val show: t -> string
end) = struct

  module KeyValPair = struct
    type t =
      Key.t * Val.t
    (* [equal] requires equality of keys and equality of values. *)
    let equal (k, v) (k', v') =
      Key.compare k k' = 0 && Val.compare v v'= 0
    let show (k, v) =
      Printf.sprintf "(%s, %s)" (Key.show k) (Val.show v)
    (* [leq] compares just the keys, not the values. *)
    let leq (k, _) (k', _) =
      Key.compare k k' <= 0
  end

  type t =
    (Key.t * Val.t) list

  let empty : t =
    []

  let singleton k v : t =
    [(k, v)]

  let merge q1 q2 : t =
    q1 @ q2

  let insert k v q : t =
    (k, v) :: q

  (* [remove kv kvs] removes the key-value pair [kv] from the list [kvs].
     If [kv] does not appear in the list [kvs], [Absent kv] is raised. *)

  exception Absent of KeyValPair.t

  let rec remove kv kvs =
    match kvs with
    | [] ->
        raise (Absent kv)
    | kv' :: kvs ->
        if KeyValPair.equal kv kv' then kvs else kv' :: remove kv kvs

  (* [is_minimal kv kvs] tests whether the key-value pair [kv] is minimal with
     respect to the list [kvs], that is, whether every key-value pair [kv'] in
     the list satisfies [leq kv kv']. *)

  let is_minimal kv kvs =
    List.for_all (KeyValPair.leq kv) kvs

  (* [check_minimal kv kvs] checks that the key-value pair [kv] is minimal
     with respect to the list [kvs]. If this is not the case, [NotMinimal kv]
     is raised. *)

  exception NotMinimal of KeyValPair.t

  let check_minimal kv kvs =
    if not (is_minimal kv kvs) then
      raise (NotMinimal kv)

  (* [remove_minimal kv kvs] checks that [kv] is a minimal element of the list
     [kvs] and returns this list deprived of this element. *)

  let remove_minimal kv kvs =
    let kvs' = remove kv kvs in (* may raise [Absent] *)
    check_minimal kv kvs;       (* may raise [NotMinimal] *)
    kvs'

  (* [handle action] runs [action()] and handles the exceptions [Absent] and
     [NotMinimal] by returning an [Invalid] diagnostic. *)

  let format = PPrint.utf8format
  open Monolith
  let valid x = Valid x
  let invalid f = Invalid f

  let handle (action : unit -> 'a diagnostic) : 'a diagnostic =
    try
      action()
    with
    | Absent kv ->
        invalid @@ fun _doc ->
        format "(* candidate returns %s, which does not exist *)"
          (KeyValPair.show kv)
    | NotMinimal kv ->
        invalid @@ fun _doc ->
        format "(* candidate returns %s, which is not minimal *)"
          (KeyValPair.show kv)

  (* [pop] is non-deterministic: if the queue contains several minimal key-value
     pairs, then any of them can be returned. In such a situation, the candidate
     implementation of [pop] makes a choice. The reference implementation of
     [pop] cannot also make a choice: if it did, the two choices might be
     different, and the candidate and the reference might become out of sync.
     Instead, the reference implementation of [pop] must validate and simulate
     the choice that is made by the candidate implementation. In addition to the
     reference queue [q], as an extra argument, it receives the result returned
     by [pop] on the candidate side. *)

  let pop (q : t) (result : ((Key.t * Val.t) * _) option)
  : (((Key.t * Val.t) * _) option) diagnostic =
    match result with
    | Some (kv, _cq) ->
        (* The candidate has extracted the key-value pair [kv] and has returned
           a candidate queue [_cq] that we cannot inspect, as it is an abstract
           data structure. Fortunately, there is no need to inspect it. *)
        (* Check that the key-value pair [kv] chosen by the candidate is a
           minimal element of the queue [q], and return [q] minus [kv]. *)
        handle @@ fun () ->
        let q = remove_minimal kv q in
        valid (Some (kv, q))
    | None ->
        (* The candidate has returned [None]. Check that the reference queue [q]
           is empty; if it isn't, fail. *)
        if q = [] then
          valid None
        else
          invalid @@ fun _doc ->
          format "(* candidate returns None, yet queue is nonempty *)"

  (* Testing [pop2'] is analogous. *)

  let pop2' (q : t) (result : (Key.t * Val.t) list * _)
  : ((Key.t * Val.t) list * t) diagnostic =
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
