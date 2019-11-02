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

(** Algorithms for double_word arithmetic come from:

{v Tight and rigourous error bounds for basic building blocks of double-word
arithmetic - Mioara Joldes, Jean-Michel Muller, Valentina Popescu, ACM
Transactions on Mathematical Software, Association for Computing Machinery,
2017, 44 (2), pp.1 - 27. v}

    see {{:https://doi.org/10.1145/3121432}https://doi.org/10.1145/3121432}. *)

(** Nonoverlapping expansion of two float components sorted in order of {b
    decreasing} magnitude, such as {e a op b = x + y} ([(x, y)]). *)
type double_word = float * float

(** {2 Conversion functions} *)

(** Create a double-word from a float. *)
val of_float : float  -> double_word

(** Convert a double-word back to a float. *)
val to_float : double_word -> float

(** Convert to a string with component separator [sep] (default is [" "]).
    Components are printed in order of {b decreasing} magnitude. *)
val to_string : ?sep:string -> double_word -> string

(** {2 double_word functions} *)

(** Addition of a double_word and a float. *)
val add_float : double_word -> float -> double_word

(** Addition of two double_word. *)
val add : double_word -> double_word -> double_word

(** Multiplication of a double_word and a float. *)
val mul_float : double_word -> float -> double_word

(** Multiplication of a double_word and a float. [mul_float_fast] is faster than
    [mul_float] but it is less accurate. *)
val mul_float_fast : double_word -> float -> double_word

(** Multiplication of a double_word and a float. Accurate and fast if hardware
    fma is available. *)
val mul_float_fma : double_word -> float -> double_word

(** Multiplication of two double_word. *)
val mul : double_word -> double_word -> double_word

(** Multiplication of two double_word. More accurate than [mul] and faster is
    hardware fma is available. *)
val mul_fma : double_word -> double_word -> double_word

(** Division of a double_word and a float. *)
val div_float : double_word -> float -> double_word

(** Division of two double_word. *)
val div : double_word -> double_word -> double_word

(** Division of two double_word. More accurate than [mul] and faster is hardware
    fma is available. *)
val div_fma : double_word -> double_word -> double_word

(**/**)
val sloppy_add : double_word -> double_word -> double_word
val mul_fma1 : double_word -> double_word -> double_word
val div_float1 : double_word -> float -> double_word
val div_float2 : double_word -> float -> double_word
val div1 : double_word -> double_word -> double_word
