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


(** Return value of an error-free transformation (EFT). *)
type error_free_float = {high : float; low : float}

val check_fpclass : float -> unit
val fast_two_sum : ?wa:bool -> float -> float -> error_free_float
val two_sum : ?wa:bool -> float -> float -> error_free_float
val split : ?wa:bool -> float -> error_free_float
val two_product : ?wa:bool -> float -> float -> error_free_float
