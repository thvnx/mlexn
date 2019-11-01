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

(** Error-free transformation type ({!type:error_free_transformation})
    reprensents a nonoverlapping expansion of length 2 such as: {e a op b = hi +
    lo}, for summation and product {e op}. *)
type error_free_transformation = { hi : float; lo : float }

val to_float : error_free_transformation -> float

val check_fpclass : float -> unit
val fast_two_sum : ?wa:bool -> float -> float -> error_free_transformation
val two_sum : ?wa:bool -> float -> float -> error_free_transformation
val split : ?wa:bool -> float -> error_free_transformation
val two_product : ?wa:bool -> float -> float -> error_free_transformation
val print_error_free_transformation : ?endline:bool -> ?sep:string -> error_free_transformation -> unit
