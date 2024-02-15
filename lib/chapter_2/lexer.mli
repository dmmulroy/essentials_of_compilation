type t

val make : string -> t
val next_token : t -> t * Token.t
