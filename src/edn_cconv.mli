type t = Edn.t

val source : t CConv.Decode.source
val output : t CConv.Encode.output
val encode : 'a CConv.Encode.encoder -> 'a -> t
val decode : 'a CConv.Decode.decoder -> t -> 'a CConv.or_error
val decode_exn : 'a CConv.Decode.decoder -> t -> 'a
val to_string : t CConv.Encode.encoder -> t -> string
val of_string : 'a CConv.Decode.decoder -> string -> 'a CConv.or_error
val of_string_exn : 'a CConv.Decode.decoder -> string -> 'a
