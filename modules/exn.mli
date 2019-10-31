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

type expansion = float list

val of_float : float -> expansion
val to_float : expansion -> float

val grow_expansion : ?acc:expansion -> expansion -> float -> expansion
val expansion_sum : expansion -> expansion -> expansion
val fast_expansion_sum : expansion -> expansion -> expansion
val scale_expansion : expansion -> float -> expansion
val zero_elimination : expansion -> expansion
val expansion_product : expansion -> expansion -> expansion
val compress : expansion -> expansion
val print_expansion : ?ze:bool -> ?endl:bool -> ?sep:string -> expansion -> unit
