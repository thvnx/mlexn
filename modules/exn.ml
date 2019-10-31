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

let rec grow_expansion ?acc:(acc = []) e b =
  match e with
  | h::t -> let q = Eft.two_sum b h in grow_expansion ~acc:(q.low::acc) t q.high
  | []   -> match acc with [] -> [b] | _ -> b::acc

let rec expansion_sum e f =
  match f with
  | h::t -> expansion_sum (grow_expansion e h) t
  | []   -> e

let fast_expansion_sum e f =
  let rec exn acc q g =
    match g with
    | h::t -> let eft = Eft.two_sum q h in exn (eft.low::acc) eft.high t
    | []   -> match acc with [] -> [q] | _ -> q::acc
  in
  let g = List.merge (fun x y -> Float.compare x y) e f in
  match List.length g with
  | 0 | 1 -> g
  | _ ->
    let eft = Eft.fast_two_sum (List.nth g 1) (List.nth g 0) in
    exn [eft.low] eft.high (List.tl (List.tl g))

let scale_expansion e b =
  let rec exn acc q e b =
    match e with
    | h::t ->
      let tp = Eft.two_product h b in
      let ts = Eft.two_sum q tp.low in
      let fts = Eft.fast_two_sum tp.high ts.high in
      exn (fts.low::ts.low::acc) fts.high t b
    | []   -> match acc with [] -> [q] | _ -> q::acc
  in
  match e with
  | h::t ->
    let eft = Eft.two_product h b in
    exn [eft.low] eft.high t b
  | []   -> []

let zero_elimination e =
  List.filter (fun x -> x <> 0.) e

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
      let eft = Eft.fast_two_sum q h in
      if eft.low <> 0. then traversal ~acc:(eft.high::acc) eft.low t
      else traversal ~acc:acc eft.high t
    | [] -> match acc with [] -> [q] | _ -> q::acc
  in
  let e = List.rev e in
  let e = traversal (List.hd e) ((List.tl e)) in
  let e = List.rev e in
  traversal (List.hd e) ((List.tl e))

let print_expansion ?endline:(endline = true) ?sep:(sep = " ") e =
  let rec print e =
    match e with
    | h::t -> Printf.printf "%h%s" h (match t with [] -> "" | _ -> sep); print t
    | []   -> match endline with true -> Printf.printf "\n" | false -> ()
  in
  print e
