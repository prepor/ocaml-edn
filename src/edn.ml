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

(* This is Seq.of_dispenser from OCaml 4.14, present for compatibility with
   older versions of OCaml *)
let of_dispenser it =
  let rec c () =
    match it () with
    | None -> Seq.Nil
    | Some x -> Seq.Cons (x, c)
  in
  c

let seq_from_channel ch =
  let lexbuf = Lexing.from_channel ch in
  let f () = Edn_parser.prog Edn_read.read lexbuf in
  of_dispenser f

module Json = Edn_json
module Util = Edn_util

module Errors = Edn_common
