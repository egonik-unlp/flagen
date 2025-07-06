type dimensions = { width : int; height : int }
type map_wrapper = { content : int array array }
type xy_coords = int * int

val create_map : dimensions -> map_wrapper
val create_walkers : int -> dimensions -> xy_coords array
val generate_new_coordinates : dimensions -> xy_coords -> xy_coords
val filter_function : xy_coords -> map_wrapper -> (int -> int -> int) -> unit
val create_csv_from_matrix : string -> map_wrapper -> unit
val paraboloide_hiperbolico : int -> int -> int
val compute_integral_of_map : map_wrapper -> int
