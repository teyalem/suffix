type t

module Astr : sig
  type t

  val of_string : string -> t
end

val export_dot : t -> string

val suffix : Astr.t -> t

val is_suffix : t -> string -> bool

val lcs : string list -> string option
