Decompress - Pure OCaml implementation of Zlib
==============================================

[![Build Status](https://travis-ci.org/mirage/decompress.svg?branch=master)](https://travis-ci.org/mirage/decompress)

Decompress is a pure implementation of `zlib`. The goal is to create an
available package for Mirage OS which implements `zlib` in OCaml (instead a C
code).

We respect the interface of `zlib` and all flush mode is available
(experimental):

1. `sync` performs the following tasks:
 * if there is some buffered but not yet compressed data, then this data is
   compressed into one or several blocks
 * a new type 0 block with empty contents is appended
2. `partial` is a deprecated flush method
3. `full` is a variant of the `sync` method flush. The difference lies in the
  LZ77 step. The full flush is a sync flush where the dictionary is emptied:
  after a full flush, the deflater will refrain from using copy symbols which
  reference sequences appearing before the flush point.

The interface proposed is a non-blocking interface.

Home page: http://din.osau.re/

Contact: Romain Calascibetta `<romain.calascibet ta@gmail.com>`

## Installation

Decompress can be installed with `opam`:

    opam install decompress
    
## Checkseum & Optint, linking with Decompress

From benchmarks, the biggest bottleneck of `decompress` seems to be the
computation of the ADLER-32. From this acknowledge, we decide to externalize
this part of `decompress` to 2 sub-libraries:
[`checkseum`](https://github.com/dinosaure/checkseum.git) and
[`optint`](https://github.com/dinosaure/optint.git).

`checkseum` (and, by this way, `decompress`) uses a trick about linking and let
the end-user to choose which implementation he wants. We provide 2
implementations: `checkseum.c` and `checkseum.ocaml`. Currently, `decompress`
**does not** choose an implementation.

When you want to use `decompress`, you **must** choose which implementation
you want and link with `decompress` **and** `checkseum.{c,ocaml}`.

NOTE: currently the end-user need to put `checkseum.{c,ocaml}` as the first
dependency __before__ `decompress` in `dune` file, like:

```
(executable
 ((name ...)
  (libraries (checkseum.c decompress))))
```

Otherwise, the end-user should have a linking error (see [#47](https://github.com/mirage/decompress/issues/47)).

## RFC 1951

This distribution provides an implementation of `zlib` and an implementation of
[RFC 1951](https://www.ietf.org/rfc/rfc1951.txt) - which is a subset of `zlib`.
You can use both if you link with `decompress` - or just use the RFC 1951
implementation by the `rfc1951` package.

The biggest difference between `zlib` and `rfc1951` is:
- no header
- input/output is not aligned on byte
- no checksum

## Sample programs

A good example is provided in `bin/easy.ml` with the signature:

```ocaml
val compress   : ?level:int -> string -> string
val uncompress : string -> string
```

And you can compile this program with:

    ocamlbuild -use-ocamlfind -package checkseum.c,decompress bin/easy.native

But keep in your mind, it's an easy example and it's not optimized for a
productive environment - so, don't copy/paste and think.

## Build Requirements

 * OCaml >= 4.03.0
 * `base-bytes` meta-package
 * Bigarray module (provided by the standard library of OCaml)
 * `dune` to build the project
 * `checkseum` & `optint` to compute ADLER-32 checksum
