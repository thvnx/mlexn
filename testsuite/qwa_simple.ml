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

let qwa x =
  Printf.printf "add_float     %s\n" (Qwa.to_string (Qwa.add_float (Qwa.of_float x) x));
  Printf.printf "add_fast      %s\n" (Qwa.to_string (Qwa.add_fast (Qwa.of_float x) (Qwa.of_float x)));
  Printf.printf "add           %s\n" (Qwa.to_string (Qwa.add (Qwa.of_float x) (Qwa.of_float x)));
  Printf.printf "sub_fast      %s\n" (Qwa.to_string (Qwa.sub_fast (Qwa.of_float x) (Qwa.of_float x)));
  Printf.printf "sub           %s\n" (Qwa.to_string (Qwa.sub (Qwa.of_float x) (Qwa.of_float x)));
  Printf.printf "mul_float     %s\n" (Qwa.to_string (Qwa.mul_float (Qwa.of_float x) x));
  Printf.printf "mul_float_fma %s\n" (Qwa.to_string (Qwa.mul_float_fma (Qwa.of_float x) x));
  Printf.printf "mul           %s\n" (Qwa.to_string (Qwa.mul (Qwa.of_float x) (Qwa.of_float x)));
  Printf.printf "mul_fma       %s\n" (Qwa.to_string (Qwa.mul_fma (Qwa.of_float x) (Qwa.of_float x)));
  Printf.printf "div           %s\n" (Qwa.to_string (Qwa.div (Qwa.of_float x) (Qwa.of_float x)));
  Printf.printf "div_fma       %s\n" (Qwa.to_string (Qwa.div_fma (Qwa.of_float x) (Qwa.of_float x)))

let _ =
  List.iter (fun x ->
      Printf.printf "#QWA tests, x = %h\n" x;
      qwa x;
      Printf.printf "\n") [1.; 2.; 4.; 8.; ~-. 1.; ~-. 2.; ~-. 4.; ~-. 8.]
