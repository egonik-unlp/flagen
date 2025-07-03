open Common

type universe = {
  dims : dimensions;
  mutable walkers : (int * int) array;
  map : map_wrapper;
  steps : int;
}
[@@deriving show]

let get_coord universe ((x, y) : xy_coords) : int = universe.map.content.(x).(y)

let set_coord (universe : universe) ((x, y) : xy_coords) (value : int) : unit =
  universe.map.content.(x).(y) <- value

let step (universe : universe) =
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

let run_simulation (simulation : universe) =
  let rec aux step_n =
    match step_n with
    | progress when progress <= simulation.steps ->
        step simulation;
        aux (step_n + 1)
    | _ -> ()
  in
  aux 0

let genesis dims n_walkers steps : universe =
  let map = create_map dims in
  let walkers = create_walkers n_walkers dims in
  { dims; walkers; map; steps }
