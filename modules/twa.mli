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

(** Algorithms for triple_word arithmetic come from:

{v  Algorithms for triple-word arithmetic - Nicolas Fabiano, Jean-Michel Muller,
Joris Picot. IEEE Transactions on Computers, Institute of Electrical and
Electronics Engineers, 2019, 68 (11), pp.1573-1583. v}

    see {{:https://dx.doi.org/10.1109/TC.2019.2918451}https://dx.doi.org/10.1109/TC.2019.2918451}. *)

(** Nonoverlapping expansion of three float components sorted in order of {b
    decreasing} magnitude, such as {e a op b = x + y + z} ([(x, y, z)]). *)
type triple_word = float * float * float

(** {2 Conversion functions} *)

(** Create a triple-word from a float. *)
val of_float : float  -> triple_word

(** Convert a triple-word back to a float. *)
val to_float : triple_word -> float

(** Convert to a string with component separator [sep] (default is [" "]).
    Components are printed in order of {b decreasing} magnitude. *)
val to_string : ?sep:string -> triple_word -> string
