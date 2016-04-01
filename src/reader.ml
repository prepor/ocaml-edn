let from_string s =
  let lexbuf = Lexing.from_string s in
  Edn_parser.prog Read.read lexbuf |> function
  | Some v -> v
  | None -> raise (Common.Error "No EDN")
