type t = [
  | `Assoc of (t * t) list
  | `List of t list
  | `Vector of t list
  | `Set of t list
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
  | `Tag of (string option * string * t) ]

val to_string : t -> string

val from_string : string -> t

module Errors : sig
  exception Error of string
end

module Util : sig
  exception Type_error of string * t

  exception Undefined of string * t

  val keys : t -> t list

  val values : t -> t list

  val combine : t -> t -> t

  val member : t -> t -> t

  val index : int -> t -> t

  val map : (t -> t) -> t -> t

  val to_assoc : t -> (t * t) list

  val to_option : (t -> 'a) -> t -> 'a option

  val to_bool : t -> bool

  val to_bool_option : t -> bool option

  val to_number : t -> float

  val to_number_option : t -> float option

  val to_float : t -> float

  val to_float_option : t -> float option

  val to_int : t -> int

  val to_int_option : t -> int option

  val to_list : t -> t list

  val to_string : t -> string

  val to_string_option : t -> string option

end
