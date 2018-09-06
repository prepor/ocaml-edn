let rec write buf (edn : Edn_common.value) =
  match edn with
  | `Assoc xs -> write_assoc buf xs
  | `Vector xs -> write_vector buf xs
  | `List xs -> write_list buf xs
  | `Set xs -> write_set buf xs
  | `Int v -> write_int buf v
  | `Float v -> write_float buf v
  | `String v -> write_string buf v
  | `Char s -> write_char buf s
  | `Keyword v -> write_keyword buf v
  | `Symbol v -> write_symbol buf v
  | `Decimal v -> write_decimal buf v
  | `BigInt v -> write_big_int buf v
  | `Bool v -> write_bool buf v
  | `Null -> write_nil buf
  | `Tag v -> write_tag buf v
and write_whitespaced buf xs =
  let n = List.length xs - 1 in
  List.iteri (fun i x ->
      write buf x;
      if i < n then Buffer.add_char buf ' ')
    xs;
and write_assoc buf xs =
  Buffer.add_char buf '{';
  let n = List.length xs - 1 in
  List.iteri (fun i (k, v) ->
      write buf k;
      Buffer.add_char buf ' ';
      write buf v;
      if i < n then Buffer.add_char buf ' ';)
    xs;
  Buffer.add_char buf '}';
and write_list buf xs =
  Buffer.add_char buf '(';
  write_whitespaced buf xs;
  Buffer.add_char buf ')'
and write_vector buf xs =
  Buffer.add_char buf '[';
  write_whitespaced buf xs;
  Buffer.add_char buf ']'
and write_set buf xs =
  Buffer.add_char buf '#';
  Buffer.add_char buf '{';
  write_whitespaced buf xs;
  Buffer.add_char buf '}'
and write_int buf v =
  Buffer.add_string buf (string_of_int v)
and write_float buf v =
  Buffer.add_string buf (string_of_float v)
and write_nil buf =
  Buffer.add_string buf "nil"
and write_big_int buf v =
  Buffer.add_string buf (v ^ "N")
and write_decimal buf v =
  Buffer.add_string buf (v ^ "M")
and write_symbol buf = function
  | (Some prefix), v ->
    Buffer.add_string buf prefix;
    Buffer.add_char buf '/';
    Buffer.add_string buf v
  | None, v ->
    Buffer.add_string buf v
and write_keyword buf v =
  Buffer.add_char buf ':';
  write_symbol buf v
and write_bool buf = function
  | true -> Buffer.add_string buf "true"
  | false -> Buffer.add_string buf "false"
and write_tag buf (prefix, v, form) =
  Buffer.add_char buf '#';
  write_symbol buf (prefix, v);
  Buffer.add_char buf ' ';
  write buf form
and write_string buf v =
  Buffer.add_char buf '"';
  Buffer.add_string buf (String.escaped v);
  Buffer.add_char buf '"';
and write_char buf v =
  Buffer.add_string buf v

let to_string edn =
  let buf = Buffer.create 256 in
  write buf edn;
  Buffer.contents buf
