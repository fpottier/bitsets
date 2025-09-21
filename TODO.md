# To Do

# API

* The specification of `sorted_union` is more restrictive
  than it could be. (The sets need not be disjoint.)
  Also, perhaps we should offer an iterated-union function
  which sorts internally.

* In `SET`, check whether `add`, `remove`, `inter`
  and other functions
  should offer a physical equality guarantee.
  Also, re-read the code and check which allocations could be avoided
  by re-using an existing object.

* In `SET`, maybe expose `singleton_minimum`, which is very efficient.

* In `SET`, maybe expose `below`, which is symmetric with `above`.

* In `SET`, add a printer for sets.

# WordBitSet

* Use intrinsics -- but not inside Menhir.
  See `caml_z.c` in the library `zarith`.
  See the library `ocaml_intrinsics`.
  [link](https://github.com/janestreet/ocaml_intrinsics_kernel/blob/master/src/int.mli)

# Tests

* Move `comparison` into Monolith.

* Find a way of testing `compare`.

# Extensions

* Add a type of dense bit sets of fixed-but-unlimited size,
  represented as an immutable array.
