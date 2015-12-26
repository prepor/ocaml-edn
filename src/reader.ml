let from_string s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Read.read lexbuf |> function
  | Some v -> v
  | None -> raise (Common.Error "No EDN")
