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

let sum33 x y z =
  let (u, v) = Eft.two_sum x y in
  let (r0, w) = Eft.two_sum u z in
  let (r1, r2) = Eft.two_sum v w in
  (r0, r1, r2)

let sum32 x y z =
  let (u, v) = Eft.two_sum x y in
  let (r0, w) = Eft.two_sum u z in
  let r1 = v +. w in
  (r0, r1)

let sum31 x y z =
  (x +. y) +. z

let add_fast a b =
  match a, b with
    (a0, a1, a2, a3), (b0, b1, b2, b3) ->
    let (r0, e0) = Eft.two_sum a0 b0 in
    let (s1, e1) = Eft.two_sum a1 b1 in
    let (s2, e2) = Eft.two_sum a2 b2 in
    let (s3, e3) = Eft.two_sum a3 b3 in
    let (r1, e4) = Eft.two_sum e0 s1 in
    let (r2, e5, e6) = sum33 s2 e1 e4 in
    let (r3, e7) = sum32 s3 e2 e5 in
    let r4 = sum31 e3 e7 e6 in
    renormalize [r0; r1; r2; r3; r4]

let add a b =
  let double_accumulate u v x =
    let (s, v) = Eft.two_sum v x in
    let (s, u) = Eft.two_sum u s in
    let s = ref s in
    let u = ref u in
    let v = ref v in
    if !u = 0. then
      begin
        u := !s;
        s := 0.;
      end;
    if !v = 0. then
      begin
        v := !u;
        u := !s;
        s := 0.;
      end;
    (!s, !u, !v)
  in
  match (a, b) with
    (a0, a1, a2, a3), (b0, b1, b2, b3) ->
    let x = List.merge (fun x y -> ~- (Float.compare x y)) [a0; a1; a2; a3] [b0; b1; b2; b3] in
    let rec cc ?u:(u = 0.) ?v:(v = 0.) ?k:(k = 0) ?i:(i = 0) ?acc:(acc = []) xi x =
      if k < 4 && i < 8 then
        begin
          match x with
          | h::t -> let (s, u, v) = double_accumulate u v xi in
            if s <> 0.
            then cc ~u:u ~v:v ~k:(k+1) ~i:(i+1) ~acc:(s::acc) h t
            else cc ~u:u ~v:v ~k:k ~i:(i+1) ~acc:(acc) h t
          | [] -> (acc, u, v)
        end
      else
        (acc, u, v)
    in
    let (c, u, v) = cc (List.hd x) (List.tl x) in
    let c = List.rev c in
    match List.length c with
    | 0 -> (u, v, 0., 0.)
    | 1 -> (List.nth c 0, u, v, 0.)
    | 2 -> (List.nth c 0, List.nth c 1, u, v)
    | 3 -> (List.nth c 0, List.nth c 1, List.nth c 2, u)
    | _ -> (List.nth c 0, List.nth c 1, List.nth c 2, List.nth c 3)

let sub_fast a b =
  match b with (b0, b1, b2, b3) -> add_fast a (~-. b0, ~-. b1, ~-. b2, ~-. b3)

let sub a b =
  match b with (b0, b1, b2, b3) -> add a (~-. b0, ~-. b1, ~-. b2, ~-. b3)

let mul_float a b =
  match a with (a0, a1, a2, a3) ->
    let (s0, e0) = Eft.two_product a0 b in
    let (s1, e1) = Eft.two_product a1 b in
    let (s2, e2) = Eft.two_product a2 b in
    let s3 =  a3 *. b in
    let (s1, e3) = Eft.two_sum e0 s1 in
    let (s2, e4, e5) = sum33 s2 e1 e3 in
    let (s3, e6) = sum32 s3 e2 e4 in
    renormalize [s0; s1; s2; s3; (e6 +. e5)]

let mul_float_fma a b =
  match a with (a0, a1, a2, a3) ->
    let (s0, e0) = Eft.two_product_fma a0 b in
    let (s1, e1) = Eft.two_product_fma a1 b in
    let (s2, e2) = Eft.two_product_fma a2 b in
    let s3 =  a3 *. b in
    let (s1, e3) = Eft.two_sum e0 s1 in
    let (s2, e4, e5) = sum33 s2 e1 e3 in
    let (s3, e6) = sum32 s3 e2 e4 in
    renormalize [s0; s1; s2; s3; (e6 +. e5)]

let sum63 a b c d e f =
  let (s1, e1, e2) = sum33 a b c in
  let (s2, e3, e4) = sum33 d e f in
  let (r1, e5) = Eft.two_sum s1 s2 in
  let (r2, e6) = Eft.two_sum e1 e3 in
  let (r2, e7) = Eft.two_sum r2 e5 in
  let r3 = sum31 (e2 +. e4) e6 e7 in
  (r1, r2, r3)

let sum92 a b c d e f g h i =
  let (s1, e1) = Dwa.add (Eft.two_sum a b) (Eft.two_sum c d) in
  let (s2, e2) = Dwa.add (Eft.two_sum e f) (Eft.two_sum g h) in
  let (s3, e3) = Dwa.add (s1, e1) (s2, e2) in
  Dwa.add_float (s3, e3) i

let sum91 a b c d e f g h i =
  a +. b +. c +. d +. e +. f +. g +. h +. i

let mul a b =
  match (a, b) with
    (a0, a1, a2, a3), (b0, b1, b2, b3) ->
    let (p00, q00) = Eft.two_product a0 b0 in
    let (p01, q01) = Eft.two_product a0 b1 in
    let (p10, q10) = Eft.two_product a1 b0 in
    let (p02, q02) = Eft.two_product a0 b2 in
    let (p11, q11) = Eft.two_product a1 b1 in
    let (p20, q20) = Eft.two_product a2 b0 in
    let (p03, q03) = Eft.two_product a0 b3 in
    let (p12, q12) = Eft.two_product a1 b2 in
    let (p21, q21) = Eft.two_product a2 b1 in
    let (p30, q30) = Eft.two_product a3 b0 in
    let p13 = a1 *. b3 in
    let p22 = a2 *. b2 in
    let p31 = a3 *. b1 in
    let s0 = p00 in
    let (s1, e1, e2) = sum33 q00 p01 p10 in
    let (s2, e3, e4) = sum63 e1 q01 q10 p02 p11 p20 in
    let (s3, e5) = sum92 e2 e3 q02 q11 q20 p03 p12 p21 p30 in
    let s4 = sum91 e4 e5 q03 q12 q21 q30 p13 p22 p31 in
    renormalize [s0; s1; s2; s3; s4]

let mul_fma a b =
  match (a, b) with
    (a0, a1, a2, a3), (b0, b1, b2, b3) ->
    let (p00, q00) = Eft.two_product_fma a0 b0 in
    let (p01, q01) = Eft.two_product_fma a0 b1 in
    let (p10, q10) = Eft.two_product_fma a1 b0 in
    let (p02, q02) = Eft.two_product_fma a0 b2 in
    let (p11, q11) = Eft.two_product_fma a1 b1 in
    let (p20, q20) = Eft.two_product_fma a2 b0 in
    let (p03, q03) = Eft.two_product_fma a0 b3 in
    let (p12, q12) = Eft.two_product_fma a1 b2 in
    let (p21, q21) = Eft.two_product_fma a2 b1 in
    let (p30, q30) = Eft.two_product_fma a3 b0 in
    let p13 = a1 *. b3 in
    let p22 = a2 *. b2 in
    let p31 = a3 *. b1 in
    let s0 = p00 in
    let (s1, e1, e2) = sum33 q00 p01 p10 in
    let (s2, e3, e4) = sum63 e1 q01 q10 p02 p11 p20 in
    let (s3, e5) = sum92 e2 e3 q02 q11 q20 p03 p12 p21 p30 in
    let s4 = sum91 e4 e5 q03 q12 q21 q30 p13 p22 p31 in
    renormalize [s0; s1; s2; s3; s4]
