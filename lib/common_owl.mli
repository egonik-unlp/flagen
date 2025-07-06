type dimensions = { width : int; height : int }
type map_wrapper = { content : Owl.Mat.mat }
type xy_coords = int * int

val generate_new_coordinates : dimensions -> xy_coords -> int * int
val create_map : dimensions -> map_wrapper
val compute_integral : map_wrapper -> float
val create_walkers : int -> dimensions -> Owl.Mat.mat
val retrieve_walker : Owl.Mat.mat -> int -> float * float
val paraboloide_hiperbolico : Owl.Mat.mat -> float

val filter_function :
  Owl.Mat.mat -> map_wrapper -> (Owl.Mat.mat -> float) -> unit

val save_to_csv : string -> Owl.Mat.mat -> unit
