type dimensions = { width : int; height : int } [@@deriving show]
type map_wrapper = { content : int array array } [@@deriving show]
type xy_coords = int * int [@@deriving show]

let generate_new_coordinates (coordinates : dimensions) ((x, y) : xy_coords) =
  let rec gen max old =
    let candidate = old + Random.int_in_range ~min:(-1) ~max:1 in
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
  let gen max = Random.int_in_range ~min:0 ~max in
  Array.init n_walkers (fun _ -> (gen dims.width, gen dims.height))

(* let generate_new_coordinates coordinates (x, y) = *)
(*   let rec gen max old = *)
(*     let candidate = old + Random.int_in_range ~min:(-1) ~max:1 in *)
(*     match candidate with *)
(*     | candidate when candidate >= max || candidate < 0 -> gen max candidate *)
(*     | _ -> candidate *)
(*   in *)
(*   let final_candidates = (gen coordinates.width x, gen coordinates.height y) in *)
(*   final_candidates *)

let filter_function ((x, y) : xy_coords) map =
  if map.content.(x).(y) <= x * x then
    map.content.(x).(y) <- map.content.(x).(y) + 10000

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
