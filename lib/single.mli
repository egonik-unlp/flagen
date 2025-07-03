open Common

type universe

val step : universe -> unit
val run_simulation : universe -> unit
val genesis : dimensions -> int -> int -> universe
val get_coord : universe -> xy_coords -> int
val set_coord : universe -> xy_coords -> int -> unit
