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

type double_word = float * float

let of_float f = (f, 0.)
let to_float w = match w with (x, y) -> x +. y

let to_string ?sep:(sep = " ") w =
  match w with (x, y) -> Printf.sprintf "%h%s%h" x sep y

let add_float x y =
  match x with (xh, xl) ->
    let (sh, sl) = Eft.two_sum xh y in
    let v = (xl +. sl) in
    let (zh, zl) = Eft.fast_two_sum sh v in
    (zh, zl)

let sloppy_add x y =
  match x, y with (xh, xl), (yh, yl) ->
    let (sh, sl) = Eft.two_sum xh yh in
    let v = xl +. yl in
    let w = sl +. v in
    let (zh, zl) = Eft.fast_two_sum sh w in
    (zh, zl)

let add x y =
  match x, y with (xh, xl), (yh, yl) ->
    let (sh, sl) = Eft.two_sum xh yh in
    let (th, tl) = Eft.two_sum xl yl in
    let c = sl +. th in
    let (vh, vl) = Eft.fast_two_sum sh c in
    let w = tl +. vl in
    let (zh, zl) = Eft.fast_two_sum vh w in
    (zh, zl)

let mul_float x y =
  match x with (xh, xl) ->
    let (ch, cl1) = Eft.two_product xh y in
    let cl2 = xl *. y in
    let (th, tl1) = Eft.fast_two_sum ch cl2 in
    let tl2 = tl1 +. cl1 in
    let (zh, zl) = Eft.fast_two_sum th tl2 in
    (zh, zl)

let mul_float_fast x y =
  match x with (xh, xl) ->
    let (ch, cl1) = Eft.two_product xh y in
    let cl2 = xl *. y in
    let cl3 = cl1 +. cl2 in
    let (zh, zl) = Eft.fast_two_sum ch cl3 in
    (zh, zl)

let mul_float_fma x y =
  match x with (xh, xl) ->
    let (ch, cl1) = Eft.two_product_fma xh y in
    let cl3 = Float.fma xl y cl1 in
    let (zh, zl) = Eft.fast_two_sum ch cl3 in
    (zh, zl)

let mul x y =
  match x, y with (xh, xl), (yh, yl) ->
    let (ch, cl1) = Eft.two_product xh yh in
    let tl1 = xh *. yl in
    let tl2 = xl *. yh in
    let cl2 = tl1 +. tl2 in
    let cl3 = cl1 +. cl2 in
    let (zh, zl) = Eft.fast_two_sum ch cl3 in
    (zh, zl)

let mul_fma1 x y =
  match x, y with (xh, xl), (yh, yl) ->
    let (ch, cl1) = Eft.two_product_fma xh yh in
    let tl = xh *. yl in
    let cl2 = Float.fma xl yh tl in
    let cl3 = cl1 +. cl2 in
    let (zh, zl) = Eft.fast_two_sum ch cl3 in
    (zh, zl)

let mul_fma x y =
  match x, y with (xh, xl), (yh, yl) ->
    let (ch, cl1) = Eft.two_product_fma xh yh in
    let tl0 = xl *. yl in
    let tl1 = Float.fma xh yl tl0 in
    let cl2 = Float.fma xl yh tl1 in
    let cl3 = cl1 +. cl2 in
    let (zh, zl) = Eft.fast_two_sum ch cl3 in
    (zh, zl)

let div_float1 x y =
  match x with (xh, xl) ->
    let th = xh /. y in
    let (ph, pl) = Eft.two_product th y in
    let (dh, d_) = Eft.two_sum xh (~-. ph) in
    let d__ = xl -. pl in
    let dl = d_ +. d__ in
    let d = dh +. dl in
    let tl = d /. y in
    let (zh, zl) = Eft.fast_two_sum th tl in
    (zh, zl)

let div_float2 x y =
  match x with (xh, xl) ->
    let th = xh /. y in
    let (ph, pl) = Eft.two_product th y in
    let dh = xh -. ph in
    let dl = xl -. pl in
    let d = dh +. dl in
    let tl = d /. y in
    let (zh, zl) = Eft.fast_two_sum th tl in
    (zh, zl)

let div_float x y =
  match x with (xh, xl) ->
    let th = xh /. y in
    let (ph, pl) = Eft.two_product th y in
    let dh = xh -. ph in
    let dt = dh -. pl in
    let d = dt +. xl in
    let tl = d /. y in
    let (zh, zl) = Eft.fast_two_sum th tl in
    (zh, zl)

let div1 x y =
  match x, y with (xh, xl), (yh, yl) ->
    let th = xh /. yh in
    let (rh, rl) = mul_float (yh, yl) th in
    let (ph, pl) = Eft.two_sum xh (~-. rh) in
    let dh = pl -. rl in
    let dl = dh +. xl in
    let d = ph +. dl in
    let tl = d /. yh in
    let (zh, zl) = Eft.fast_two_sum th tl in
    (zh, zl)

let div x y =
  match x, y with (xh, xl), (yh, yl) ->
    let th = xh /. yh in
    let (rh, rl) = mul_float (yh, yl) th in
    let ph = xh -. rh in
    let dl = xl -. rl in
    let d = ph +. dl in
    let tl = d /. yh in
    let (zh, zl) = Eft.fast_two_sum th tl in
    (zh, zl)

let div_fma x y =
  match x, y with (xh, xl), (yh, yl) ->
    let th = 1. /. yh in
    let rh = 1. -. yh *. th in
    let rl = ~-. (yl *. th) in
    let (eh, el) = Eft.fast_two_sum rh rl in
    let (dh, dl) = mul_float_fma (eh, el) th in
    let (mh, ml) = add_float (dh, dl) th in
    let (zh, zl) = mul_fma (xh, xl) (mh, ml) in
    (zh, zl)
