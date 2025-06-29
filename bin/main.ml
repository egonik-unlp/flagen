let () = Random.init 123123

type dimensions = { width : int; height : int } [@@deriving show]
type map_wrapper = { content : int array array } [@@deriving show]
type xy_coords = int * int [@@deriving show]

type universe = {
  dims : dimensions;
  mutable walkers : (int * int) array;
  map : map_wrapper;
  steps : int;
}
[@@deriving show]

let universe_dims = { width = 1920; height = 1080 }

let genesis dims n_walkers steps =
  let create_map dims =
    let content = Array.make_matrix dims.width dims.height 0 in
    { content }
  in
  let create_walkers n_walkers dims =
    let gen max = Random.int_in_range ~min:0 ~max in
    Array.init n_walkers (fun _ -> (gen dims.width, gen dims.height))
  in
  let map = create_map dims in
  let walkers = create_walkers n_walkers dims in
  { dims; walkers; map; steps }

let generate_new_coordinates coordinates (x, y) =
  let rec gen max old =
    let candidate = old + Random.int_in_range ~min:(-1) ~max:1 in
    match candidate with
    | candidate when candidate >= max || candidate < 0 -> gen max candidate
    | _ -> candidate
  in
  let final_candidates = (gen coordinates.width x, gen coordinates.height y) in
  final_candidates

let step universe =
  let coords =
    Array.map
      (fun (x, y) -> generate_new_coordinates universe.dims (x, y))
      universe.walkers
  in
  universe.walkers <- coords;
  Array.iter
    (fun (x, y) ->
      let cvalue = universe.map.content.(x).(y) in
      universe.map.content.(x).(y) <- cvalue + 1)
    coords;
  ()

let run_simulation simulation =
  let rec aux step_n =
    match step_n with
    | progress when progress <= simulation.steps ->
        step simulation;
        aux (step_n + 1)
    | _ -> ()
  in
  aux 0

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

let () =
  Printexc.record_backtrace true;
  try
    let simulation = genesis universe_dims 20 1_000_000 in
    run_simulation simulation;
    (* print_endline (show_map_wrapper simulation.map); *)
    create_csv_from_matrix "resultado.csv" simulation.map
  with ex ->
    Printf.eprintf "Exception: %s\n" (Printexc.to_string ex);
    Printf.eprintf "Backtrace: %s\n" (Printexc.get_backtrace ())
