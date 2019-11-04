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

type quadruple_word = float * float * float * float

let of_float f = (f, 0., 0., 0.)
let to_float w = match w with (m, n, o, p) -> m +. n +. o +. p

let to_string ?sep:(sep = " ") w =
  match w with (m, n, o, p) -> Printf.sprintf "%h%s%h%s%h%s%h"
                                 m sep n sep o sep p

(* The input is a five-term expansion with limited overlapping bits, with
   head list being the most significant component. *)
let renormalize a =
  let rec tt ?acc:(acc = []) s a =
    match a with
    | h::t -> let (s, e) = Eft.fast_two_sum h s in tt ~acc:(e::acc) s t
    | []   -> List.rev (s::acc)
  in
  let rec bb ?acc:(acc = []) s b =
    match b with
    | h::t -> let (s, e) = Eft.fast_two_sum s h in
      if e <> 0. then
        bb ~acc:(s::acc) e t
      else
        bb ~acc:acc s t
    | [] -> acc
  in
  let a = List.rev a in
  let t = tt (List.hd a) (List.tl a) in
  let b = bb (List.hd t) (List.tl t) in
  ((if List.length b > 0 then List.nth b 0 else 0.),
   (if List.length b > 1 then List.nth b 1 else 0.),
   (if List.length b > 2 then List.nth b 2 else 0.),
   (if List.length b > 3 then List.nth b 3 else 0.))

let add_float q f =
  let rec qwa ?acc:(acc = []) q f =
    match q with
    | h::t -> let (f, e) = Eft.two_sum f h in qwa ~acc:(f::acc) t e
    | []   -> List.rev (f::acc)
  in
  match q with
    (a0, a1, a2, a3) -> renormalize (qwa [a0; a1; a2; a3] f)
