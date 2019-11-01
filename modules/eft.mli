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
    reprensents a nonoverlapping expansion of length 2 such as:
    {e a op b = hi + lo}, for summation and product {e op}s. *)
type error_free_transformation = { hi : float; lo : float }

(** {2 Conversion functions} *)

(** Convert an {!type:error_free_transformation}  back to a float. *)
val to_float : error_free_transformation -> float

(** Convert an {!type:error_free_transformation} to a string. Default members
    separator [sep] is [" "]. *)
val to_string : ?sep:string -> error_free_transformation -> string

(** {2 Error-free transformations} *)

(** Let {e a} and {e b} be float numbers such that {e |a| >= |b|}. Then
    [fast_two_sum a b] will produce a nonoverlapping expansion
    ([{ hi = x; lo = y}]) such that {e a + b = x + y}, where {e x} is an
    approximation to {e a + b} and {e y} represents the roundoff error in the
    calculation of {e x}. [wa] stands for {i with assertions} and is [false]
    by default. *)
val fast_two_sum : ?wa:bool -> float -> float -> error_free_transformation

(** Let {e a} and {e b} be float numbers. Then [two_sum a b] will produce a
    nonoverlapping expansion ([{ hi = x; lo = y}]) such that {e a + b = x + y},
    where {e x} is an approximation to {e a + b} and {e y} represents the
    roundoff error in the calculation of {e x}. [wa] stands for
    {i with assertions} and is [false] by default. *)
val two_sum : ?wa:bool -> float -> float -> error_free_transformation

(** Let {e a} be a float number. Then [split a] will produce a value {e a_hi}
    and a nonoverlapping value {e a_lo} ([{ hi = a_hi; lo = a_lo}]) such that
    {e |a_hi| >= |a_lo|} and {e a = a_hi + a_lo}. [wa] stands for
    {i with assertions} and is [false] by default. *)
val split : ?wa:bool -> float -> error_free_transformation

(** Let {e a} and {e b} be float numbers. Then [two_product a b] will produce a
    nonoverlapping expansion ([{ hi = x; lo = y}]) such that {e ab = x+y}, where
    {e x} is an approximation to {e ab} and {e y} represents the roundoff error
    in the calculation of {e x}. [wa] stands for    {i with assertions} and is
    [false] by default.*)
val two_product : ?wa:bool -> float -> float -> error_free_transformation

(**/**)
val check_fpclass : float -> unit
