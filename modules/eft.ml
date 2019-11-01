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

type error_free_transformation = { hi : float; lo : float }

let to_float e = e.hi +. e.lo

let check_fpclass f =
  match classify_float f with
    FP_subnormal -> failwith "subnormal number"
  | FP_nan       -> failwith "nan number"
  | FP_infinite  -> failwith "infinite number"
  | FP_zero
  | FP_normal    -> ()

let fast_two_sum ?wa:(wa = false) op1 op2 =
  let eft a b =
    let x = a +. b in
    let y = b -. (x -. a) in
    {hi = x; lo = y}
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
    let x = a +. b in
    let b_virtual = x -. a in
    let a_virtual = x -. b_virtual in
    let b_roundoff = b -. b_virtual in
    let a_roundoff = a -. a_virtual in
    let y = a_roundoff +. b_roundoff in
    {hi = x; lo = y}
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
    {hi = a_hi; lo = a_lo}
  in
  match wa with
  | false -> eft op
  | true  ->
    check_fpclass op;
    eft op

let two_product ?wa:(wa = false) op1 op2 =
  let eft a b =
    let x = a *. b in
    let a = split a in
    let b = split b in
    let err1 = x -. (a.hi *. b.hi) in
    let err2 = err1 -. (a.lo *. b.hi) in
    let err3 = err2 -. (a.hi *. b.lo) in
    let y = (a.lo *. b.lo) -. err3 in
    {hi = x; lo = y}
  in
  match wa with
  | false -> eft op1 op2
  | true  ->
    check_fpclass op1;
    check_fpclass op2;
    eft op1 op2

let to_string ?sep:(sep = " ") e =
  Printf.sprintf "%h%s%h" e.hi sep e.lo
