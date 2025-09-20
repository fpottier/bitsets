(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**The signature [SET] describes the operations that must be offered by
   an implementation of sets. *)
module type SET = sig

  (**The elements of a set have type [elt].

     We assume that the elements of a set are equipped with a total order.
     This order is mentioned in the specification of several operations on
     sets. *)
  type elt

  (**The type of sets. *)
  type t

  (** {1 Construction} *)

  (**[empty] is the empty set. *)
  val empty: t

  (**[singleton x] is a singleton set containing just the element [x]. *)
  val singleton: elt -> t

  (**[add x s] is the union of the sets [singleton x] and [s]. *)
  val add: elt -> t -> t

  (**[remove x s] is the set [s] deprived of the element [x]. *)
  val remove: elt -> t -> t

  (**[union s1 s2] is the union of the sets [s1] and [s2]. If the union is
     mathematically equal to [s2], then [union s1 s2] returns [s2] itself. *)
  val union: t -> t -> t

  (**[inter s1 s2] is the intersection of the sets [s1] and [s2]. *)
  val inter: t -> t -> t

  (** [diff s1 s2] is the set difference of the sets [s1] and [s2]. *)
  val diff: t -> t -> t

  (** [above x s] is the set of the elements of [s] that are greater
      than [x]. *)
  val above: elt -> t -> t

  (** {1 Cardinality} *)

  (**[is_empty s] determines whether the [s] is empty. *)
  val is_empty: t -> bool

  (**[is_singleton s] tests whether [s] is a singleton set. *)
  val is_singleton: t -> bool

  (**[cardinal s] is the cardinal of the set [s]. *)
  val cardinal: t -> int

  (** {1 Tests} *)

  (**[mem x s] determines whether [x] is a member of the set [s]. *)
  val mem: elt -> t -> bool

  (**[equal s1 s2] determines whether the sets [s1] and [s2] are equal. *)
  val equal: t -> t -> bool

  (**[compare] is a total order on sets. *)
  val compare: t -> t -> int

  (**[disjoint s1 s2] determines whether the sets [s1] and [s2] are
     disjoint, that is, whether their intersection is empty. *)
  val disjoint: t -> t -> bool

  (**[subset s1 s2] determines whether the set [s1] is a subset of the set
     [s2]. *)
  val subset: t -> t -> bool

  (**[quick_subset s1 s2] is a fast test for the set inclusion [s1 ⊆ s2].

     The sets [s1] and [s2] must be nonempty.

     It must be the case that either [s1] is a subset of [s2] or [s1] and [s2]
     are disjoint: that is, [s1 ⊆ s2 ⋁ s1 ∩ s2 = ∅] must hold.

     Under this hypothesis, [quick_subset s1 s2] can be implemented simply
     by picking an arbitrary element of [s1] (if there is one) and testing
     whether it is a member of [s2]. *)
  val quick_subset: t -> t -> bool

  (** {1 Extraction} *)

  (** [minimum s] returns the minimum element of the set [s].
      If the set [s] is empty, the exception [Not_found] is raised. *)
  val minimum: t -> elt

  (** [maximum s] returns the maximum element of the set [s].
      If the set [s] is empty, the exception [Not_found] is raised. *)
  val maximum: t -> elt

  (**If the set [s] is nonempty, then [choose s] returns an arbitrary
     element of this set. Otherwise, the exception [Not_found] is raised. *)
  val choose: t -> elt

  (** {1 Iteration} *)

  (**[iter yield s] enumerates the elements of the set [s], in increasing
     order, by presenting them to the function [yield]. That is, [yield x]
     is invoked in turn for each element [x] of the set [s], in increasing
     order. *)
  val iter: (elt -> unit) -> t -> unit

  (**[fold yield s b] enumerates the elements of the set [s], in increasing
     order, by presenting them to the function [yield]. That is, [yield x b]
     is invoked in turn for each element [x] of the set [s], in increasing
     order, where [b] is the current (loop-carried) state. The final state
     is returned. *)
  val fold: (elt -> 'b -> 'b) -> t -> 'b -> 'b

  (**[elements s] is a list of all elements in the set [s],
     in an unspecified order. *)
  val elements: t -> elt list

  (** {1 Decomposition} *)

  (**[compare_minimum], a total order on sets, is defined as follows:

     - The empty set is less than any nonemptyset.
     - If the sets [s1] and [s2] are nonempty, then [compare_minimum s1 s2]
       is [compare (minimum s1) (minimum s2)]. *)
  val compare_minimum : t -> t -> int

  (**[sorted_union ss] computes the union of the sets in the list [ss]. Every
     set in the list [ss] must be nonempty. The intervals that underlie these
     sets must be ordered and nonoverlapping: that is, if [s1] and [s2] are
     two adjacent sets in the list [ss], then they must satisfy the condition
     [maximum s1 < minimum s2]. *)
  val sorted_union : t list -> t

  (**[extract_unique_prefix s1 s2] requires the sets [s1] and [s2] to be
     nonempty. Furthermore, it requires [compare_minimum s1 s2 < 0], that is,
     [minimum s1 < minimum s2]. It splits [s1] into two disjoint subsets
     [head1] and [tail1] such that [head1] is exactly the subset of [s1] whose
     elements are less than [minimum s2]. Thus, [head1] is guaranteed to be
     nonempty, whereas [tail1] may be empty. *)
  val extract_unique_prefix : t -> t -> t * t

  (**[extract_shared_prefix s1 s2] requires the sets [s1] and [s2] to be
     nonempty. Furthermore, it requires [compare_minimum s1 s2 = 0], that is,
     [minimum s1 = minimum s2]. It splits [s1] and [s2] into three subsets
     [head], [tail1], and [tail2], as follows:

     - [s1] is [head U tail1] and [s2] is [head U tail2].
       This implies that [head] is a subset of both [s1] and [s2].
     - An element of [head] is less than any element of [tail1] or [tail2].
     - [head] is maximal with respect to the previous two properties.

     In summary,
     [head] is the maximal shared prefix of the sets [s1] and [s2]. *)
  val extract_shared_prefix : t -> t -> t * (t * t)

end (* SET *)
