#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "edn" @@ fun c ->
  Ok [ Pkg.mllib ~api: ["Edn"] "src/edn.mllib";
       Pkg.test "test/test"]
