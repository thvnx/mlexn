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

let add_fast a b =
  let sum33 x y z =
    let (u, v) = Eft.two_sum x y in
    let (r0, w) = Eft.two_sum u z in
    let (r1, r2) = Eft.two_sum v w in
    (r0, r1, r2)
  in
  let sum32 x y z =
    let (u, v) = Eft.two_sum x y in
    let (r0, w) = Eft.two_sum u z in
    let r1 = v +. w in
    (r0, r1)
  in
  let sum31 x y z =
    (x +. y) +. z
  in
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
