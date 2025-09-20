module Make (X : sig
  type elt = int
  type t
  val is_empty : t -> bool
  val above : elt -> t -> t
  val minimum : t -> elt
  val maximum : t -> elt
end) = struct
open X

let nonempty s =
  not (is_empty s)

(* [cut x ss] cuts (shrinks) the sets in the list [ss] so that their
   elements lie above [x] and these sets are pairwise disjoint (that
   is, non-overlapping). The sets in [ss] must be nonempty, and the
   list [ss] must be sorted by minimum. *)

let rec cut x ss =
  match ss with
  | [] ->
      []
  | s :: ss ->
      above x s ::
      let x = max x (maximum s) in
      cut x ss

(* [cut ss] cuts (shrinks) the sets in the list [ss] so that these sets are
   pairwise disjoint (that is, non-overlapping). The sets in [ss] must be
   nonempty, and the list [ss] must be sorted by minimum. *)

let cut ss =
  match ss with
  | [] ->
      []
  | s :: ss ->
      let x = maximum s in
      s :: cut x ss

(* [prepare] accepts an arbitrary list of sets [ss] and turns it into a
   sorted list of non-overlapping sets, which forms a suitable argument
   for [sorted_union]. *)

let prepare ss =
  ss
  |> List.filter nonempty
  |> List.sort (fun s1 s2 -> Int.compare (minimum s1) (minimum s2))
  |> cut
  |> List.filter nonempty

end (* Make *)
