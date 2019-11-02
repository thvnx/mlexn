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

type expansion = float list

let grow_expansion e b =
  let rec exn ?acc:(acc = []) e b =
    match e with
    | h::t -> let (x, y) = Eft.two_sum b h in exn ~acc:(y::acc) t x
    | []   -> match acc with [] -> [b] | _ -> List.rev (b::acc)
  in
  exn e b

let rec expansion_sum e f =
  match f with
  | h::t -> expansion_sum (grow_expansion e h) t
  | []   -> e

let expansion_neg e =
  List.map (fun f -> Float.neg f) e

let expansion_diff e f =
  let f = expansion_neg f in
  expansion_sum e f

let fast_expansion_sum e f =
  let rec exn acc q g =
    match g with
    | h::t -> let (x, y) = Eft.two_sum q h in exn (y::acc) x t
    | []   -> match acc with [] -> [q] | _ -> q::acc
  in
  let g = List.merge (fun x y -> Float.compare x y) e f in
  match List.length g with
  | 0 | 1 -> g
  | _ ->
    let (x, y) = Eft.fast_two_sum (List.nth g 1) (List.nth g 0) in
    List.rev (exn [y] x (List.tl (List.tl g)))

let scale_expansion e b =
  let rec exn acc q e b =
    match e with
    | h::t ->
      let (p_hi, p_lo) = Eft.two_product h b in
      let (s_hi, s_lo) = Eft.two_sum q p_lo in
      let (f_hi, f_lo) = Eft.fast_two_sum p_hi s_hi in
      exn (f_lo::s_lo::acc) f_hi t b
    | []   -> match acc with [] -> [q] | _ -> q::acc
  in
  match e with
  | h::t ->
    let (x, y) = Eft.two_product h b in
    List.rev (exn [y] x t b)
  | []   -> []

let zero_elimination e =
  match List.filter (fun x -> x <> 0.) e with
  | [] -> [0.]
  | f  -> f

let expansion_product e f =
  let rec exn ?acc:(acc = []) e f =
    match f with
    | h::t -> exn ~acc:((zero_elimination (scale_expansion e h))::acc) e t
    | []   -> acc
  in
  let rec step ?acc:(acc = []) g =
    match g with
    | h1::h2::t -> step ~acc:((fast_expansion_sum h1 h2)::acc) t
    | h::[]     -> h::acc
    | []        -> acc
  in
  let rec distillation g =
    match List.length g with
    | 0 -> []
    | 1 -> List.hd g
    | _ -> distillation (step g)
  in
  distillation (exn e f)

let compress e =
  let rec traversal ?acc:(acc = []) q e =
    match e with
    | h::t ->
      let (x, y) = Eft.fast_two_sum q h in
      if y <> 0. then traversal ~acc:(x::acc) y t
      else traversal ~acc:acc x t
    | [] -> match acc with [] -> [q] | _ -> q::acc
  in
  let e = List.rev e in
  let e = traversal (List.hd e) ((List.tl e)) in
  let e = List.rev e in
  traversal (List.hd e) ((List.tl e))

let of_float f = [f]
let to_float e = List.hd (List.rev (compress e))

let to_string ?comp:(comp = true) ?sep:(sep = " ") e =
  let rec print ?acc:(acc = "") e =
    match e with
    | h::t -> print ~acc:(Printf.sprintf "%s%h%s" acc h (match t with [] -> "" | _ -> sep)) t
    | []   -> acc
  in
  print (List.rev (match comp with true -> compress e | false -> e))

let compare e f =
  match to_float (expansion_diff e f) with
  | 0. -> 0
  | f  -> if f > 0. then 1 else ~-1

(* TODO expansion_division *)
(* TODO expansion_sqrt *)
