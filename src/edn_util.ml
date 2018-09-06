exception Type_error of string * Edn_common.value
exception Undefined of string * Edn_common.value

let typeof (v : Edn_common.value) = match v with
  | `Assoc _ -> "map"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
  | `Int _ -> "int"
  | `List _ -> "list"
  | `Vector _ -> "vector"
  | `Set _ -> "set"
  | `Null -> "nil"
  | `String _ -> "string"
  | `Keyword _ -> "keyword"
  | `Symbol _ -> "symbol"
  | `Char _ -> "char"
  | `BigInt _ -> "bigint"
  | `Decimal _ -> "decimal"
  | `Tag _ -> "tag"

let typerr msg edn = raise (Type_error (msg ^ typeof edn, edn))

let assoc name m =
  try List.assoc name m
  with Not_found -> `Null

let member name = function
  | `Assoc m -> assoc name m
  | edn -> typerr ("Can't get member '" ^ (Edn_writer.to_string name) ^ "' of non-map type ") edn

let index i v =
  let find l =
    let len = List.length l in
    let wrapped_index = if i < 0 then len + i else i in
    if wrapped_index < 0 || wrapped_index >= len then
      raise (Undefined ("Index " ^ string_of_int i ^ " out of bounds", v))
    else List.nth l wrapped_index in match v with
  | `List l -> find l
  | `Vector l -> find l
  | edn -> typerr ("Can't get index " ^ string_of_int i
                   ^ " of non-array type ") edn

let map f = function
  | `List l -> `List (List.map f l)
  | `Vector l -> `Vector (List.map f l)
  | `Set l -> `Set (List.map f l)
  | edn -> typerr "Can't map function over non-array type " edn

let to_assoc = function
  | `Assoc m -> m
  | edn -> typerr "Expected map, got " edn

let to_option f = function
  | `Null -> None
  | x -> Some (f x)

let to_bool = function
  | `Bool b -> b
  | edn -> typerr "Expected bool, got " edn

let to_bool_option = function
  | `Bool b -> Some b
  | `Null -> None
  | edn -> typerr "Expected bool or nil, got " edn

let to_number = function
  | `Int i -> float i
  | `Float f -> f
  | edn -> typerr "Expected number, got " edn

let to_number_option = function
  | `Int i -> Some (float i)
  | `Float f -> Some f
  | `Null -> None
  | edn -> typerr "Expected number or null, got " edn

let to_float = function
  | `Float f -> f
  | edn -> typerr "Expected float, got " edn

let to_float_option = function
  | `Float f -> Some f
  | `Null -> None
  | edn -> typerr "Expected float or null, got " edn

let to_int = function
  | `Int i -> i
  | edn -> typerr "Expected int, got " edn

let to_int_option = function
  | `Int i -> Some i
  | `Null -> None
  | edn -> typerr "Expected int or null, got " edn

let to_list = function
  | `List l -> l
  | `Vector l -> l
  | `Set l -> l
  | edn -> typerr "Expected list, got " edn

let to_string = function
  | `String s -> s
  | edn -> typerr "Expected string, got " edn

let to_string_option = function
  | `String s -> Some s
  | `Null -> None
  | edn -> typerr "Expected string or null, got " edn

let convert_each f = function
  | `List l -> List.map f l
  | `Vector l -> List.map f l
  | `Set l -> List.map f l
  | edn -> typerr "Can't convert each element of non-array type " edn

let keys o =
  to_assoc o |> List.map (fun (key, _) -> key)

let values o =
  to_assoc o |> List.map (fun (_, value) -> value)

let combine (first : Edn_common.value) (second : Edn_common.value) =
  match (first, second) with
  | (`Assoc a, `Assoc b) -> (`Assoc (a @ b) : Edn_common.value)
  | (_, _) -> raise (Invalid_argument "Expected two objects, check inputs")
