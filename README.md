[![Build Status](https://travis-ci.com/thvnx/mlexn.svg?branch=master)](https://travis-ci.com/thvnx/mlexn)

# mlexn

OCaml implementation for floating-point expansions.

## usage

### Horner's polynomial evaluation example

Let `exn_horner` the expansion-based Horner's polynomial evaluation:

```ocaml
let exn_horner p x =
  let rec eval a p x =
    let a = Exn.compress a in (* speeds up evaluation 300x *)
    match p with
    | h::t -> eval (Exn.grow_expansion (Exn.scale_expansion a x) h) t x
    | []   -> a
  in
  eval (Exn.of_float (List.hd p)) (List.tl p) x
```

With _p(x)=(0.75-x)^5(1-x)^11_, the resulting expansion would be _2^n-1_
elements long (131071 for _n=16_) without `Exn.compress`. Using it is usefull to
keep minimizing the expansion compute and memory footprints. For example, with
_x=0.754321_, resulting expansion is 14 elements long using compression.

The following figure shows how much using expansion can be efficient to obtain
accurate results. Below _p(x)_ (which is subject to accuracy problems because of
its multiple roots), have been evaluated around one of its roots:

![](testsuite/horner.svg)
