type t

val make : int -> t

val acquire : t -> unit Lwt.t
val release : t -> unit Lwt.t
