type 'a or_error = [ `Ok of 'a | `Error of string ]

type t = Edn.t

let map_keys =
  let f (k, v) =
    match k with
    | `String k -> (k, v)
    | `Keyword (_, k) -> (k, v)
    | k -> raise (CConv.ConversionFailure (Printf.sprintf "Only strings and keywords supported as key values, not %s" (Edn.to_string k))) in
  List.map f

let source =
  let module D = CConv.Decode in
  let rec src = {D.emit=fun dec (x:t) -> match x with
      | `Bool b -> dec.D.accept_bool src b
      | `Int i -> dec.D.accept_int src i
      | `Float f -> dec.D.accept_float src f
      | `String s -> dec.D.accept_string src s
      | `Null -> dec.D.accept_unit src ()
      | `List l -> dec.D.accept_list src l
      | `Assoc l -> dec.D.accept_record src (map_keys l)
      | `Decimal s -> dec.D.accept_string src s
      | `BigInt s -> dec.D.accept_string src s
      | `Tag (None, v, form) -> dec.D.accept_tuple src [`String v; form]
      | `Tag (Some prefix, v, form) -> dec.D.accept_tuple src [`String (Printf.sprintf "%s/%s" prefix v); form]
      | `Keyword (None, v) -> dec.D.accept_string src v
      | `Keyword (Some prefix, v) -> dec.D.accept_string src (Printf.sprintf "%s/%s" prefix v)
      | `Symbol (None, v) -> dec.D.accept_string src v
      | `Symbol (Some prefix, v) -> dec.D.accept_string src (Printf.sprintf "%s/%s" prefix v)
      | `Char s -> dec.D.accept_string src s
      | `Vector l -> dec.D.accept_list src l
      | `Set l -> dec.D.accept_list src l
    } in
  src

let output =
  let module E = CConv.Encode in
  { E.unit= `Null;
    bool = (fun b -> `Bool b);
    float = (fun f -> `Float f);
    char = (fun c -> `String (String.make 1 c));
    nativeint = (fun i -> `Int (Nativeint.to_int i));
    int32 = (fun i -> `Int (Int32.to_int i));
    int64 = (fun i -> `Int (Int64.to_int i));
    int = (fun i -> `Int i);
    string = (fun s -> `String s);
    list = (fun l -> `List l);
    option = (function None -> `List [] | Some x -> `List [x]);
    record = (fun l -> `Assoc (List.map (fun (k, v) -> (`String k, v)) l));
    tuple = (fun l -> `List l);
    sum = (fun name l -> match l with
        | [] -> `String name
        | _::_ -> `List (`String name :: l));
  }

let encode src x = CConv.encode src output x

let decode dec x = CConv.decode source dec x

let decode_exn dec x = CConv.decode_exn source dec x

let to_string src (x : Edn.t) =
  Edn.to_string (encode src x)

let of_string dec s =
  try
    let x = Edn.from_string s in
    decode dec x
  with Edn.Errors.Error e ->
    `Error e

let of_string_exn dec s =
  let x = Edn.from_string s in
  decode_exn dec x
