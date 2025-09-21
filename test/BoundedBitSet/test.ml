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

(* This test is not super-interesting, as the implementation of BoundedBitSet
   is almost trivial; it simply selects an appropriate implementation, based
   on the value of [n]. *)

(* Plus, at the moment, only one (random) choice of [n] is made at the start,
   then the test runs forever with this value of [n]. This could be remedied
   by choosing [n] inside Monolith's prologue -- as done in Sek -- but I am
   lazy. *)

(* Choose a random bound. *)
let n = Random.int 1024

open Monolith

let () =
  dprintf "          module N = struct\n";
  dprintf "            let n = %d\n" n;
  dprintf "          end\n"

module B = struct
  module N = struct let n = n end
  include Bitsets.BoundedBitSet.Make(N)()
  let name = Printf.sprintf "Bitsets.BoundedBitSet.Make(N)"
  let check = ignore
end

module T =
  Setup.Make(B)
