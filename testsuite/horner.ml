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
    let a = Exn.compress a in (* speeds up evaluation 300x *)
    match p with
    | h::t -> eval (Exn.grow_expansion (Exn.scale_expansion a x) h) t x
    | []   -> a
  in
  eval (Exn.of_float (List.hd p)) (List.tl p) x

let _ =
  let read_lines ch =
    let data = ref [] in
    try
      while true; do
        data := Float.of_string (input_line ch) :: !data
      done; !data
    with End_of_file ->
      close_in ch;
      List.rev !data
  in
  let p = [2.373046875e-1; -4.1923828125; 3.46728515625e1; -1.781982421875e2;
           6.37001953125e2; -1.679423828125e3; 3.378095703125e3;
           -5.288271484375e3; 6.5115380859375e3; -6.3275244140625e3;
           4.8364658203125e3; -2.8772958984375e3; 1.30611328125e3; -4.3734375e2;
           1.01875e2; -1.475e1; 1.] in
  let chan = open_in Sys.argv.(1) in
  let data = read_lines chan in
  List.iter (fun x ->
      let h = (horner p x) in
      let e = (exn_horner p x) in
      Printf.printf "%h %h %h %s\n" x h (Exn.to_float e) (Exn.string_expansion ~sep:"::" e);
    ) data
