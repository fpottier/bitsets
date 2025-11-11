Suppose that we have written (or someone has given us) an implementation
of (immutable) priority queues. The signature of this module might look
like this ([LeftistHeap.mli](https://github.com/fpottier/bitsets/blob/main/src/LeftistHeap.mli)):

```ocaml
module Make (Key : sig
  type t
  val compare: t -> t -> int
end) : sig

  (**A key. *)
  type key = Key.t

  (**An immutable priority queue. *)
  type 'a t

  (**[empty] is the empty queue. *)
  val empty : 'a t

  (**[singleton k v] is a singleton queue containing the key-value
     pair [(k, v)]. *)
  val singleton : key -> 'a -> 'a t

  (**[merge q1 q2] merges the queues [q1] and [q2]. The result is a
     new queue. *)
  val merge : 'a t -> 'a t -> 'a t

  (**[pop q] extracts a key-value pair whose key is minimal out of the queue
     [q]. [None] is returned only in the case where [q] is empty. *)
  val pop: 'a t -> ((key * 'a) * 'a t) option

end
```

The implementation of this module might be based on Chris Okasaki's
leftist heaps, a simple and beautiful data structure
([LeftistHeap.ml](https://github.com/fpottier/bitsets/blob/main/src/LeftistHeap.ml)).
But this does not matter. Let's see how to test this implementation
as a black box
using [Monolith](https://gitlab.inria.fr/fpottier/monolith/)
([paper](https://cambium.inria.fr/~fpottier/publis/pottier-monolith-2021.pdf)).

Monolith expects us to provide a reference implementation of a priority queue.
This reference implementation does not need to be very efficient; what matters
is that it should be simple and correct. The simplest possible approach is to
use an unsorted list of key-value pairs, along the following lines
([reference.ml](https://github.com/fpottier/bitsets/blob/b27da071b117bf70916207aee93c86694e757f09/test/LeftistHeap/reference.ml#L46))
:

```ocaml
  type t =
    (Key.t * Val.t) list

  let empty : t =
    []

  let singleton k v : t =
    [(k, v)]

  let merge q1 q2 : t =
    q1 @ q2

  (* missing [pop], for now *)
```

Here comes the interesting part. `pop` is a non-deterministic operation: if a
priority queue contains several minimal key-value pairs, then any of them can
be legally returned by `pop`.

In such a situation, the candidate implementation of `pop` makes a choice. It
would not make sense for the reference implementation of `pop` to also make a
choice: because the two choices might be different, the candidate and the
reference could become out of sync.

Instead, we want the reference implementation of `pop` to check that the
choice made by the candidate implementation is legal and to obey this choice,
that is, to make the same choice.

Therefore, the reference implementation of `pop` cannot have type
`t -> ((Key.t * Val.t) * t) option`,
as one might expect.
Instead, it must take two arguments,
namely the queue on the candidate side
and the result returned by `pop` on the candidate side.
It is expected to return a diagnostic,
that is,
an indication of whether this candidate result
is valid or invalid.

The reference implementation of `pop` can be written as follows:

```ocaml
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
```

The auxiliary function `remove_minimal kv kvs`
([link](https://github.com/fpottier/bitsets/blob/6b3c466701e294177a1c1a3c182ea607404f93e4/test/LeftistHeap/reference.ml#L93))
checks that the key-value pair `kv` is a minimal element of the list `kvs` and
returns this list deprived of this element. It fails if `kv` is not in the
list or not minimal.

The auxiliary function `handle`
([link](https://github.com/fpottier/bitsets/blob/6b3c466701e294177a1c1a3c182ea607404f93e4/test/LeftistHeap/reference.ml#L106))
handles these failures and returns an `invalid` diagnostic in these cases.

The reference implementation is now complete.
There remains to tell Monolith about the operations
that we want to test.
For each operation,
we must provide a specification
(that is, roughly, a type),
a reference implementation,
and a candidate implementation.
This is done as follows:

```ocaml
  let spec = t in
  declare "empty" spec R.empty C.empty;

  let spec = value ^> key ^> t in
  declare "singleton" spec R.singleton C.singleton;

  let spec = t ^> t ^> t in
  declare "merge" spec R.merge C.merge;

  let spec = t ^> nondet (option ((key *** value) *** t)) in
  declare "pop" spec R.pop C.pop;
```

In the case of `pop`, we have used the combinator `nondet`
([documentation](https://cambium.inria.fr/~fpottier/monolith/doc/monolith/Monolith/#spec:fun:nondet))
to tell Monolith that `pop` is a non-deterministic operation,
whose reference implementation is written in the unusual style
that we have shown above.

There remains to run the test,
which runs forever
and prints the problematic scenarios
that it detects.

It is worth noting that the result of `pop` is a composite value: it is partly
concrete, partly abstract, as it is an (optional) pair of a key-value pair
(concrete data that can be observed) and a priority queue (abstract data that
cannot be directly observed). This does not create any problem: Monolith is
able to automatically decompose this composite value and submit the priority
queue component to further testing.

To illustrate this, let us intentionally introduce the following bug in the
candidate implementation:

```ocaml
  let pop q =
    match pop q with
    | Some (kv, _q') ->
        Some (kv, q) (* return the original queue, unchanged *)
    | None ->
        None
```

Then, the test immediately fails and prints the following scenario:

```
(* @03: Failure in an observation: candidate and reference disagree. *)
          open Bitsets.LeftistHeap.Make(Int);;
          #require "monolith";;
          module Sup = Monolith.Support;;
(* @01 *) let x0 = singleton 6 11;;
(* @02 *) let (Some ((_, _), x1)) = pop x0;;
(* @03 *) let observed = pop x1;;
          (* candidate returns (6, 11), which does not exist *)
```

As you can see, the priority queue returned by the first `pop` operation is
extracted: it is named `x1` and it is submitted to a second `pop` operation,
where an observable problem appears.
