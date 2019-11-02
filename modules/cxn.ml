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

type cexpansion = { re : Exn.expansion; im : Exn.expansion }

let grow_expansion e (b : Complex.t) =
  let r = Exn.grow_expansion e.re b.re in
  let i = Exn.grow_expansion e.im b.im in
  { re = r; im = i }

let expansion_sum e f =
  let r = Exn.expansion_sum e.re f.re in
  let i = Exn.expansion_sum e.im f.im in
  { re = r; im = i }
