(* This file is part of mlexn.

   mlexn is free software: you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   mlexn is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   mlexn.  If not, see <http://www.gnu.org/licenses/>. *)

(** Complex number of expansions components. Real and imaginary expansions are
    sorted in order of {b increasing} magnitude, except that any of the component
    may be zero. *)
type cexpansion = { re : Exn.expansion; im : Exn.expansion }

(** {2 Conversion functions} *)

(** Create an expansion of length one from a complex. *)
val of_complex : Complex.t -> cexpansion

(** Convert an expansion back to a complex. The expansion parts are compressed,
    see {!val:compress}, then the most significant component is returned. *)
val to_complex : cexpansion -> Complex.t

(** {2 Complex expansions} *)

(** Add a single complex to an expansion. *)
val grow_expansion : cexpansion -> Complex.t -> cexpansion

(** Add an expansion to another one. *)
val expansion_sum : cexpansion -> cexpansion -> cexpansion

(** [expansion_diff x y] subtracts [y] to [x]. *)
val expansion_diff : cexpansion -> cexpansion -> cexpansion

(** Multiply an expansion by a complex value. *)
val scale_expansion : cexpansion -> Complex.t -> cexpansion

(** Multiply two expansions. *)
val expansion_product : cexpansion -> cexpansion -> cexpansion

(** {2 Cleanup fonctions} *)

(** Eliminate zeros (see {!val:Exn.zero_elimination}). *)
val zero_elimination : cexpansion -> cexpansion

(** Find a compact form for an expansion (see {!val:Exn.compress}). *)
val compress : cexpansion -> cexpansion
