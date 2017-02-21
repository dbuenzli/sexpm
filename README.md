sexpm — s-expression codecs and updates for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

sexpm codecs sequences of s-expressions to different representations.

sexpm is distributed under the ISC license.

Homepage: http://erratique.ch/software/sexpm  

## Installation

sexpm can be installed with `opam`:

    opam install sexpm

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is automatically generated from
the source interfaces. It can be consulted [online][doc] or via
`odig doc sexpm`.

[doc]: http://erratique.ch/software/sexpm/doc

## Sample programs

If you installed sexpm with `opam` sample programs are located in
the directory `opam config var sexpm:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory of the distribution. They can be built and run
with:

    topkg build --tests true && topkg test 
