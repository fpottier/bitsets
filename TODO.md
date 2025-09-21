# To Do

# API

* In `SET`, check whether `add`, `remove`, `inter`
  should offer a physical equality guarantee.

* In `SET`, maybe expose `singleton_minimum`, which is very efficient.

* In `SET`, maybe expose `below`, which is symmetric with `above`.

* In `SET`, add a printer for sets.

# WordBitSet

* Use intrinsics -- but not inside Menhir.
  See `caml_z.c` in the library `zarith`.
  See the library `ocaml_intrinsics`.

# Tests

* Move `comparison` into Monolith.

# Extensions

* Add a type of dense bit sets of fixed-but-unlimited size,
  represented as an immutable array.
