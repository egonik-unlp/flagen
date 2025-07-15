type dimensions = { width : int; height : int } [@@deriving show]
type map_wrapper = { content : int array array } [@@deriving show]
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

let create_map dims =
  let content = Array.make_matrix dims.width dims.height 0 in
  { content }

let create_walkers n_walkers dims =
  let gen max = int_in_range ~min:0 ~max in
  Array.init n_walkers (fun _ -> (gen dims.width, gen dims.height))

let compute_integral_of_map map_wrapper =
  Array.fold_left
    (fun outer_accum row -> outer_accum + Array.fold_left ( + ) 0 row)
    0 map_wrapper.content

let paraboloide_hiperbolico x y : int =
  let xx = float_of_int x in
  let yy = float_of_int y in
  let j = Float.sub (Float.div xx 10.0 ** 2.0) (Float.div yy 40.0 ** 2.0) in
  int_of_float j

let filter_function ((x, y) : xy_coords) map func =
  if map.content.(x).(y) <= func x y then
    map.content.(x).(y) <- map.content.(x).(y) + 100000

let create_csv_from_matrix (filename : string) (matrix : map_wrapper) =
  let oc = open_out filename in
  Array.iter
    (fun row ->
      let line =
        row |> Array.map string_of_int |> Array.to_list |> String.concat ","
      in
      output_string oc (line ^ "\n"))
    matrix.content;
  close_out oc
