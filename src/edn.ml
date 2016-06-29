open Lexing

type t = Edn_common.value

include Edn_writer

let parse lexbuf =
  Edn_parser.prog Edn_read.read lexbuf |> function
  | Some v -> v
  | None -> raise End_of_file

let from_string s =
  parse (Lexing.from_string s)

let from_channel ch =
  parse (Lexing.from_channel ch)

let stream_from_channel ch =
  let lexbuf = Lexing.from_channel ch in
  let f _ = Edn_parser.prog Edn_read.read lexbuf in
  Stream.from f

module Json = Edn_json
module Util = Edn_util

module Errors = Edn_common
