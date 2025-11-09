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

(* This reference implementation uses a sorted list of key-value pairs. *)

module Make (Key : sig
  type t
  val compare: t -> t -> int
end) = struct

  type key =
    Key.t

  type 'a heap =
    (key * 'a) list

  type 'a t =
    'a heap

  let compare_pairs (k1, _) (k2, _) =
    Key.compare k1 k2

  let sort kvs =
    List.sort compare_pairs kvs

  let empty =
    []

  let singleton k v =
    [(k, v)]

  let merge q1 q2 =
    sort (q1 @ q2)

  let insert k v q =
    sort ((k, v) :: q)

  type 'a pop2 =
    | Head of key * 'a * key * 'a * 'a heap
    | Tail of key * 'a
    | Done

  let pop2 q =
    match q with
    | (k1, v1) :: (k2, v2) :: q ->
        Head (k1, v1, k2, v2, q)
    | [(k, v)] ->
        Tail (k, v)
    | [] ->
        Done

end
