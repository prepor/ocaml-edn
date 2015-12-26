type json = [ `Assoc of (string * json) list
            | `Bool of bool
            | `Float of float
            | `Int of int
            | `List of json list
            | `Null
            | `String of string ]

let rec to_json (edn : Common.value) =
  match edn with
  | `Keyword v -> `String (to_json_pair v)
  | `Symbol v -> `String (to_json_pair v)
  | `Assoc xs -> `Assoc (List.map (fun (k, v) -> (to_json_key k), to_json v) xs)
  | `List xs -> `List (List.map to_json xs)
  | `Vector xs -> `List (List.map to_json xs)
  | `Set xs -> `List (List.map to_json xs)
  | `Char v -> `String v
  | `Decimal v -> `String v
  | `BigInt v -> `String v
  | `Tag (prefix, v, form) -> `List [`String (to_json_pair (prefix, v));
                                     (to_json form)]
  | `String v -> `String v
  | `Int v -> `Int v
  | `Float v -> `Float v
  | `Bool v -> `Bool v
  | `Null -> `Null
and to_json_pair = function
  | (Some prefix), v -> prefix ^ "/" ^ v
  | None, v -> v
and to_json_key = function
  | `Keyword v -> to_json_pair v
  | `String v -> v
  | `Symbol v -> to_json_pair v
  | v -> Writer.to_string v

let rec from_json ?(keywordize=false) (json : json) : Common.value =
  match json with
  | `Assoc xs -> `Assoc (List.map (fun (k, v) -> ((if keywordize then `Keyword (None, k) else `String k),
                                                  (from_json v)))
                           xs)
  | `Bool v -> `Bool v
  | `Float v -> `Float v
  | `Int v -> `Int v
  | `List xs -> `List (List.map from_json xs)
  | `Null -> `Null
  | `String v -> `String v
