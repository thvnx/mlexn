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

let horner p x =
  let rec eval a p x =
    match p with
    | h::t -> eval (a *. x +. h) t x
    | []   -> a
  in
  eval (List.hd p) (List.tl p) x

let exn_horner p x =
  let rec eval a p x =
    match p with
    | h::t -> eval (Exn.grow_expansion (Exn.scale_expansion a x) h) t x
    | []   -> a
  in
  eval (Exn.of_float (List.hd p)) (List.tl p) x

let _ =
  let p = [2.373046875000000e-01;
           -4.192382812500000e+00;
           3.467285156250000e+01;
           -1.781982421875000e+02;
           6.370019531250000e+02;
           -1.679423828125000e+03;
           3.378095703125000e+03;
           -5.288271484375000e+03;
           6.511538085937500e+03;
           -6.327524414062500e+03;
           4.836465820312500e+03;
           -2.877295898437500e+03;
           1.306113281250000e+03;
           -4.373437500000000e+02;
           1.018750000000000e+02;
           -1.475000000000000e+01;
           1.000000000000000e+00] in
  let x = 0.750001 in
  Printf.printf "%h\n" (horner p x);
  let r = (exn_horner p x) in
  Exn.print_expansion ~ze:false r;
  Exn.print_expansion (Exn.compress r)
