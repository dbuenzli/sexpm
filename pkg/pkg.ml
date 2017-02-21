#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let bos = Conf.with_pkg "bos"
let cmdliner = Conf.with_pkg "cmdliner"

let () =
  Pkg.describe "sexpm" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  let bos = Conf.value c bos in
  Ok [ Pkg.mllib "src/sexpm.mllib";
       Pkg.bin ~cond:(cmdliner && bos) "test/sexptrip";
       Pkg.test "test/test";
       Pkg.test "test/test_codec"; ]
