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

(** Nonoverlapping expansion of four float components sorted in order of {b
    decreasing} magnitude, such as {e a op b = m + n + o + p}
    ([(m, n, o, p)]). *)
type quadruple_word = float * float * float * float

(** {2 Conversion functions} *)

(** Create a quadruple-word from a float. *)
val of_float : float  -> quadruple_word

(** Convert a quadruple-word back to a float. *)
val to_float : quadruple_word -> float

(** Convert to a string with component separator [sep] (default is [" "]).
    Components are printed in order of {b decreasing} magnitude. *)
val to_string : ?sep:string -> quadruple_word -> string

(** {2 quadruple_word functions} *)

(** Add a quadruple_word with a float. *)
val add_float : quadruple_word -> float -> quadruple_word

(** Add two quadruple_word numbers. [add_fast] is faster than {!val:add}, but
    less accurate. *)
val add_fast : quadruple_word -> quadruple_word -> quadruple_word

(** Add two quadruple_word numbers. *)
val add : quadruple_word -> quadruple_word -> quadruple_word

(** Subtract two quadruple_word numbers. [sub_fast] is faster than {!val:sub},
    but less accurate. *)
val sub_fast : quadruple_word -> quadruple_word -> quadruple_word

(** Subtract two quadruple_word numbers. *)
val sub : quadruple_word -> quadruple_word -> quadruple_word

(** Multiply a quadruple_word with a float. *)
val mul_float : quadruple_word -> float -> quadruple_word

(** Multiply a quadruple_word with a float using fma-based error-free
    transformation. Faster if hardware fma is available. *)
val mul_float_fma : quadruple_word -> float -> quadruple_word

(**/**)
val renormalize : float list -> quadruple_word
