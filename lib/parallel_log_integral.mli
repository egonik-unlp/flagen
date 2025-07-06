type universe_parallel = {
  dims : Common_owl.dimensions;
  mutable walkers : Owl.Mat.mat;
  maps : Common_owl.map_wrapper array;
  steps : int;
}

val genesis_parallel : Common_owl.dimensions -> int -> int -> universe_parallel
val step_simultaneous_in : universe_parallel -> unit
val step_simultaneous : universe_parallel -> unit
val parallell_integral : universe_parallel -> int
val compute_final_image : universe_parallel -> Owl.Mat.mat
val run_parallel_simulation : universe_parallel -> unit
