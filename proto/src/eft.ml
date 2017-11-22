(* This file is part of cexn.

   cexn prototype is free software: you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   cexn prototype is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with cexn prototype.  If not, see
   <http://www.gnu.org/licenses/>. *)

type error_free_float = { high : float; low : float }


let check_fp_class f =
  match classify_float f with
    FP_subnormal -> failwith "subnormal number"
  | FP_nan -> failwith "nan number"
  | FP_infinite -> failwith "infinite number"
  | FP_zero
    | FP_normal -> ()


let fast_two_sum a b =
  if a < b then
    failwith "fastTwoSum: a >= b assertion failed";
  check_fp_class a;
  check_fp_class b;
  let sum = a +. b in
  let err = b -. (sum -. a) in
  { high = sum; low = err }
