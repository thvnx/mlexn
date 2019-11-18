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

let dwa x =
  Printf.printf "add_float      %s\n" (Dwa.to_string (Dwa.add_float (Dwa.of_float x) x));
  Printf.printf "sloppy_add     %s\n" (Dwa.to_string (Dwa.sloppy_add (Dwa.of_float x) (Dwa.of_float x)));
  Printf.printf "add            %s\n" (Dwa.to_string (Dwa.add (Dwa.of_float x) (Dwa.of_float x)));
  Printf.printf "sub            %s\n" (Dwa.to_string (Dwa.sub (Dwa.of_float x) (Dwa.of_float x)));
  Printf.printf "mul_float      %s\n" (Dwa.to_string (Dwa.mul_float (Dwa.of_float x) x));
  Printf.printf "mul_float_fast %s\n" (Dwa.to_string (Dwa.mul_float_fast (Dwa.of_float x) x));
  Printf.printf "mul_float_fma  %s\n" (Dwa.to_string (Dwa.mul_float_fma (Dwa.of_float x) x));
  Printf.printf "mul            %s\n" (Dwa.to_string (Dwa.mul (Dwa.of_float x) (Dwa.of_float x)));
  Printf.printf "mul_fma1       %s\n" (Dwa.to_string (Dwa.mul_fma1 (Dwa.of_float x) (Dwa.of_float x)));
  Printf.printf "mul_fma        %s\n" (Dwa.to_string (Dwa.mul_fma (Dwa.of_float x) (Dwa.of_float x)));
  Printf.printf "div_float1     %s\n" (Dwa.to_string (Dwa.div_float1 (Dwa.of_float x) x));
  Printf.printf "div_float2     %s\n" (Dwa.to_string (Dwa.div_float2 (Dwa.of_float x) x));
  Printf.printf "div_float      %s\n" (Dwa.to_string (Dwa.div_float (Dwa.of_float x) x));
  Printf.printf "div1           %s\n" (Dwa.to_string (Dwa.div1 (Dwa.of_float x) (Dwa.of_float x)));
  Printf.printf "div            %s\n" (Dwa.to_string (Dwa.div (Dwa.of_float x) (Dwa.of_float x)));
  Printf.printf "div_fma        %s\n" (Dwa.to_string (Dwa.div_fma (Dwa.of_float x) (Dwa.of_float x)))

let _ =
  List.iter (fun x ->
      Printf.printf "#DWA tests, x = %h\n" x;
      dwa x;
      Printf.printf "\n") [1.; 2.; 4.; 8.; ~-. 1.; ~-. 2.; ~-. 4.; ~-. 8.]
