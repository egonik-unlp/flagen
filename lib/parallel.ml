open Common

type universe_parallel = {
  dims : dimensions;
  mutable walkers : (int * int) array;
  maps : map_wrapper array;
  steps : int;
}
[@@deriving show]

let genesis_parallel dims n_walkers steps : universe_parallel =
  let maps = Array.init n_walkers (fun _ -> create_map dims) in
  let walkers = create_walkers n_walkers dims in
  { dims; walkers; maps; steps }

let step_simultaneous (universe : universe_parallel) =
  universe.walkers <-
    (Array.map
       (fun (x, y) -> generate_new_coordinates universe.dims (x, y))
       universe.walkers
      : xy_coords array);

  Array.iter2
    (fun (x, y) map -> filter_function (x, y) map paraboloide_hiperbolico)
    universe.walkers universe.maps

let run_parallel_simulation (simulation : universe_parallel) =
  let rec aux step_n =
    match step_n with
    | progress when progress <= simulation.steps ->
        step_simultaneous simulation;
        aux (step_n + 1)
    | _ -> ()
  in
  aux 0
