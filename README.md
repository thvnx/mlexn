[![Build Status](https://travis-ci.com/thvnx/mlexn.svg?branch=master)](https://travis-ci.com/thvnx/mlexn)

# mlexn

OCaml implementation for floating-point expansions.

_mlexn_ provides six modules:
- `Eft` implementing error-free transformations,
- `Exn` implementing expansions as introduced in:

> Adaptive Precision Floating-Point Arithmetic and Fast Robust Geometric
> Predicates - Richard Shewchuk, J. Discrete Comput Geom, 1997, 18:305.
> https://doi.org/10.1007/PL00009321

- `Cxn` implementing complex expansions,
- `Dwa` implementing double-word arithmetic as summarized/improved in:

> Tight and rigourous error bounds for basic building blocks of double-word
> arithmetic - Mioara Joldes, Jean-Michel Muller, Valentina Popescu, ACM
> Transactions on Mathematical Software, Association for Computing Machinery,
> 2017, 44 (2), pp.1-27. https://doi.org/10.1145/3121432

- `Twa` implementing triple-word arithmetic as in:

> Algorithms for triple-word arithmetic - Nicolas Fabiano, Jean-Michel Muller,
> Joris Picot. IEEE Transactions on Computers, Institute of Electrical and
> Electronics Engineers, 2019, 68 (11), pp.1573-1583.
> https://dx.doi.org/10.1109/TC.2019.2918451

- and, `Qwa` implementing quadruple-word arithmetic:

> Algorithms for quad-double precision floating-point arithmetic - Yozo Hida,
> Xiaoye S. Li, David H. Bailey, In Proceedings of the IEEE Symposium on
> Computer Arithmetic (ARITH’16), 2001, pp.155–162.
> https://doi.org/10.1109/ARITH.2001.930115

## documentation

Documentation can be generated thanks to package _odoc_:

```bash
dune build @doc
```

then, see `_build/default/_doc/_html/index.html`.

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

With _p(x) = (0.75 - x)^5 (1 - x)^11_, the resulting expansion would be _2^n -
1_ elements long (131071 for _n = 16_) without `Exn.compress`. Using it is
usefull to keep minimizing the expansion compute and memory footprints. For
example, with _x=0.754321_, resulting expansion is 14 elements long using
compression.

The following figure shows how much using expansion can be efficient to obtain
accurate results. Below _p(x)_ (which is subject to accuracy problems because of
its multiple roots), have been evaluated around one of its roots:

![](testsuite/horner.svg)
