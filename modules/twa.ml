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

type triple_word = float * float * float

let of_float f = (f, 0., 0.)
let to_float w = match w with (x, y, z) -> x +. y +. z

let to_string ?sep:(sep = " ") w =
  match w with (x, y, z) -> Printf.sprintf "%h%s%h%s%h" x sep y sep z

let vecsum e =
  let rec eft ?acc:(acc = []) s e =
    match e with
    | h::t -> let (s, e) = Eft.two_sum h s in eft ~acc:(e::acc) s t
    | []   -> List.rev (s::acc)
  in
  eft (List.hd e) (List.tl e)

let vecsum_err_branch e =
  let rec yy ?acc:(acc = []) eps e =
    match e with
    | h::t -> let (r, eps) = Eft.two_sum eps h in
      if eps <> 0. then yy ~acc:(r::acc) eps t
      else yy ~acc:acc r t
    | []   -> List.rev (eps::acc)
  in
  let e = List.rev e in
  yy (List.hd e) (List.tl e)
