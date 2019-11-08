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
(* TODO need to change rounding mode for proper conversion *)
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

let to_triple_word a b c =
  let (d0, d1) = Eft.two_sum a b in
  let e = vecsum [d0; d1; c] in
  let r = vecsum_err_branch e in
  match List.length r with
  | 0 -> (0., 0., 0.)
  | 1 -> (List.nth r 0, 0., 0.)
  | 2 -> (List.nth r 0, List.nth r 1, 0.)
  | _ -> (List.nth r 0, List.nth r 1, List.nth r 2)

let add x y =
  match (x, y) with (x0, x1, x2), (y0, y1, y2) ->
    let z = List.merge (fun x y -> ~- (Float.compare x y)) [x0; x1; x2] [y0; y1; y2] in
    let e = vecsum z in
    let r = vecsum_err_branch e in
    match List.length r with
    | 0 -> (0., 0., 0.)
    | 1 -> (List.nth r 0, 0., 0.)
    | 2 -> (List.nth r 0, List.nth r 1, 0.)
    | _ -> (List.nth r 0, List.nth r 1, List.nth r 2)

let sub x y =
  match y with (y0, y1, y2) -> add x (~-. y0, ~-. y1, ~-. y2)

let mul x y =
  match (x, y) with (x0, x1, x2), (y0, y1, y2) ->
    let (z00p, z00m) = Eft.two_product_fma x0 y0 in
    let (z01p, z01m) = Eft.two_product_fma x0 y1 in
    let (z10p, z10m) = Eft.two_product_fma x1 y0 in
    let b = vecsum [z00m; z01p; z10p] in
    let c = Float.fma x1 y1 (List.nth b 2) in
    let z31 = Float.fma x0 y2 z10m in
    let z32 = Float.fma x2 y0 z01m in
    let z3 = z31 +. z32 in
    let e = vecsum [z00p; List.nth b 0; List.nth b 1; c; z3] in
    let e = List.rev e in
    let r = vecsum_err_branch (List.tl e) in
    match List.length r with
    | 0 -> (List.hd e, 0., 0.)
    | 1 -> (List.hd e, List.nth r 0, 0.)
    | _ -> (List.hd e, List.nth r 0, List.nth r 1)

let mul_fast x y =
  match (x, y) with (x0, x1, x2), (y0, y1, y2) ->
    let (z00p, z00m) = Eft.two_product_fma x0 y0 in
    let (z01p, z01m) = Eft.two_product_fma x0 y1 in
    let (z10p, z10m) = Eft.two_product_fma x1 y0 in
    let b = vecsum [z00m; z01p; z10p] in
    let c = Float.fma x1 y1 (List.nth b 2) in
    let z31 = Float.fma x0 y2 z10m in
    let z32 = Float.fma x2 y0 z01m in
    let z3 = z31 +. z32 in
    let s3 = c +. z3 in
    let e = vecsum [z00p; List.nth b 0; List.nth b 1; s3] in
    let e = List.rev e in
    let r = vecsum_err_branch (List.tl e) in
    match List.length r with
    | 0 -> (List.hd e, 0., 0.)
    | 1 -> (List.hd e, List.nth r 0, 0.)
    | _ -> (List.hd e, List.nth r 0, List.nth r 1)

let mul_dwa y x =
  match (x, y) with (x0, x1), (y0, y1, y2) ->
    let (z00p, z00m) = Eft.two_product_fma x0 y0 in
    let (z01p, z01m) = Eft.two_product_fma x0 y1 in
    let (z10p, z10m) = Eft.two_product_fma x1 y0 in
    let b = vecsum [z00m; z01p; z10p] in
    let c = Float.fma x1 y1 (List.nth b 2) in
    let z31 = Float.fma x0 y2 z10m in
    let z3 = z31 +. z01m in
    let e = vecsum [z00p; List.nth b 0; List.nth b 1; c; z3] in
    let e = List.rev e in
    let r = vecsum_err_branch (List.tl e) in
    match List.length r with
    | 0 -> (List.hd e, 0., 0.)
    | 1 -> (List.hd e, List.nth r 0, 0.)
    | _ -> (List.hd e, List.nth r 0, List.nth r 1)

let mul_dwa_fast y x =
  match (x, y) with (x0, x1), (y0, y1, y2) ->
    let (z00p, z00m) = Eft.two_product_fma x0 y0 in
    let (z01p, z01m) = Eft.two_product_fma x0 y1 in
    let (z10p, z10m) = Eft.two_product_fma x1 y0 in
    let b = vecsum [z00m; z01p; z10p] in
    let c = Float.fma x1 y1 (List.nth b 2) in
    let z31 = Float.fma x0 y2 z10m in
    let z3 = z31 +. z01m in
    let s3 = c +. z3 in
    let e = vecsum [z00p; List.nth b 0; List.nth b 1; s3] in
    let e = List.rev e in
    let r = vecsum_err_branch (List.tl e) in
    match List.length r with
    | 0 -> (List.hd e, 0., 0.)
    | 1 -> (List.hd e, List.nth r 0, 0.)
    | _ -> (List.hd e, List.nth r 0, List.nth r 1)

let reciprocal x =
  match x with (x0, x1, _) ->
    let a = (1. +. 2. *. Float.epsilon) /. x0 in
    let h11 = Float.fma a x0 (~-. (1. +. 2. *. Float.epsilon)) in
    let h1 = Float.fma (~-. a) x1 (~-. h11) in
    let (b01, b11) = Eft.two_product_fma a (1. -. 2. *. Float.epsilon) in
    let b12 = Float.fma a h1 b11 in
    let b = Eft.fast_two_sum b01 b12 in
    let i = sub (of_float 2.) (mul_dwa x b) in
    mul_dwa i b

let div z x =
  match x with (x0, x1, _) ->
    let a = (1. +. 2. *. Float.epsilon) /. x0 in
    let h11 = Float.fma a x0 (~-. (1. +. 2. *. Float.epsilon)) in
    let h1 = Float.fma (~-. a) x1 (~-. h11) in
    let (b01, b11) = Eft.two_product_fma a (1. -. 2. *. Float.epsilon) in
    let b12 = Float.fma a h1 b11 in
    let b = Eft.fast_two_sum b01 b12 in
    let i = sub (of_float 2.) (mul_dwa x b) in
    let a = mul_dwa z b in
    mul a i

let sqrt x =
  match x with (x0, x1, _) ->
    let a = (1. +. 4. *. Float.epsilon) /. (Float.sqrt x0) in
    let ap = 0.5 *. a in
    let (h01, h111) = Eft.two_product_fma a x0 in
    let h11 = Float.fma a x1 h111 in
    let (h012, h112) = Eft.two_product_fma ap h01 in
    let h02 = 1.5 -. h012 in
    let h12 = ~-. (Float.fma ap h11 h112) in
    let (b01, b11) = Eft.two_product_fma a h02 in
    let b12 = Float.fma a h12 b11 in
    let b = Eft.fast_two_sum b01 b12 in
    let bp = Dwa.mul (Dwa.of_float 0.5) b in
    let i1 = mul_dwa x b in
    let i2 = sub (of_float 1.5) (mul_dwa i1 bp) in
    mul i1 i2
