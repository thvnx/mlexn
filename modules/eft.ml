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

type error_free_float = {high : float; low : float}

let to_float e = e.high

let check_fpclass f =
  match classify_float f with
    FP_subnormal -> failwith "subnormal number"
  | FP_nan       -> failwith "nan number"
  | FP_infinite  -> failwith "infinite number"
  | FP_zero
  | FP_normal    -> ()

let fast_two_sum ?wa:(wa = false) op1 op2 =
  let eft a b =
    let sum = a +. b in
    let err = b -. (sum -. a) in
    {high = sum; low = err}
  in
  match wa with
  | false -> eft op1 op2
  | true  ->
    check_fpclass op1;
    check_fpclass op2;
    if op1 < op2 then
      failwith "fast_two_sum requires a >= b";
    eft op1 op2

let two_sum ?wa:(wa = false) op1 op2 =
  let eft a b =
    let sum = a +. b in
    let t1 = sum -. a in
    let t2 = sum -. t1 in
    let e1 = b -. t1 in
    let e2 = a -. t2 in
    let err = e2 +. e1 in
    {high = sum; low = err}
  in
  match wa with
  | false -> eft op1 op2
  | true  ->
    check_fpclass op1;
    check_fpclass op2;
    eft op1 op2

let split ?wa:(wa = false) op =
  let eft a =
    let c = 134217729. *. a in
    let a_big = c -. a in
    let a_hi = c -. a_big in
    let a_lo = a -. a_hi in
    {high = a_hi; low = a_lo}
  in
  match wa with
  | false -> eft op
  | true  ->
    check_fpclass op;
    eft op

let two_product ?wa:(wa = false) op1 op2 =
  let eft a b =
    let x = a *. b in
    let sa = split a in
    let sb = split b in
    let err1 = x -. (sa.high *. sb.high) in
    let err2 = err1 -. (sa.low *. sb.high) in
    let err3 = err2 -. (sa.high *. sb.low) in
    let y = (sa.low *. sb.low) -. err3 in
    {high = x; low = y}
  in
  match wa with
  | false -> eft op1 op2
  | true  ->
    check_fpclass op1;
    check_fpclass op2;
    eft op1 op2

let print_error_free_float ?endline:(endline = true) ?sep:(sep = " ") e =
  Printf.printf "%h%s%h%s" e.high sep e.low (match endline with true -> "\n" | false -> "")
