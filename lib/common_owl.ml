open Owl

type dimensions = { width : int; height : int } [@@deriving show]
type map_wrapper = { content : Mat.mat } [@@deriving show]
type xy_coords = int * int [@@deriving show]

let int_in_range ~min ~max =
  if max < min then invalid_arg "int_in_range: max < min"
  else Random.int (max - min + 1) + min

let generate_new_coordinates (coordinates : dimensions) ((x, y) : xy_coords) =
  let rec gen max old =
    let candidate = old + int_in_range ~min:(-1) ~max:1 in
    match candidate with
    | candidate when candidate >= max || candidate < 0 -> gen max candidate
    | _ -> candidate
  in
  let final_candidates = (gen coordinates.width x, gen coordinates.height y) in
  final_candidates

(* let create_map dims = *)
(*   let content = Array.make_matrix dims.width dims.height 0 in *)
(*   { content } *)

let create_map dims = { content = Mat.zeros dims.width dims.height }
let compute_integral map = Mat.sum' map.content

let create_walkers n_walkers dims =
  Mat.concat_vertical
    (Mat.uniform ~a:0.0 ~b:(float_of_int dims.width) 1 n_walkers)
    (Mat.uniform ~a:0.0 ~b:(float_of_int dims.height) 1 n_walkers)

let retrieve_walker walkers n_walker =
  let x = Mat.get walkers 0 n_walker in
  let y = Mat.get walkers 1 n_walker in
  (x, y)

let paraboloide_hiperbolico coord : float =
  let xx, yy = Mat.(get coord 0 0, get coord 1 0) in
  Float.sub (Float.div xx 10.0 ** 2.0) (Float.div yy 40.0 ** 2.0)

(**)
(* let filter_function ((x, y) : xy_coords) map func = *)
(*   if map.content.(x).(y) <= func x y then *)
(*     map.content.(x).(y) <- map.content.(x).(y) + 100000 *)

let filter_function (coord : Mat.mat) map func =
  let x, y =
    Mat.(get coord 0 0 |> int_of_float, get coord 1 0 |> int_of_float)
  in
  let vxy = Mat.get map.content x y in
  let nvxy = if vxy <= func coord then vxy +. 100.0 else vxy in
  Mat.set map.content x y nvxy

(* let create_csv_from_matrix (filename : string) (matrix : map_wrapper) = *)
(*   let oc = open_out filename in *)
(*   Array.iter *)
(*     (fun row -> *)
(*       let line = *)
(*         row |> Array.map string_of_int |> Array.to_list |> String.concat "," *)
(*       in *)
(*       output_string oc (line ^ "\n")) *)
(*     matrix.content; *)
(*   close_out oc *)

let save_to_csv (filename : string) (matrix : Mat.mat) =
  Mat.save_npy ~out:filename matrix
