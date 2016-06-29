type value = [
  | `Assoc of (value * value) list
  | `List of value list
  | `Vector of value list
  | `Set of value list
  | `Null
  | `Bool of bool
  | `String of string
  | `Char of bytes
  | `Symbol of (string option * string)
  | `Keyword of (string option * string)
  | `Int of int
  | `BigInt of string
  | `Float of float
  | `Decimal of string
  | `Tag of (string option * string * value) ]

exception Error of string
