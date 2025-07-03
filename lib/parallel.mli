type universe_parallel = {
  dims : Common.dimensions;
  mutable walkers : (int * int) array;
  maps : Common.map_wrapper array;
  steps : int;
}

val genesis_parallel : Common.dimensions -> int -> int -> universe_parallel
val step_simultaneous : universe_parallel -> unit
val run_parallel_simulation : universe_parallel -> unit
