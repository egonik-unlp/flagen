open Common_owl
open Owl

type universe_parallel = {
  dims : dimensions;
  mutable walkers : Mat.mat;
  maps : map_wrapper array;
  steps : int;
}
[@@deriving show]

let genesis_parallel dims n_walkers steps : universe_parallel =
  let maps = Array.init n_walkers (fun _ -> create_map dims) in
  let walkers = create_walkers n_walkers dims in
  { dims; walkers; maps; steps }
(* let step_simultaneous (universe : universe_parallel) = *)
(*   universe.walkers <- *)
(*     (Array.map *)
(*        (fun (x, y) -> generate_new_coordinates universe.dims (x, y)) *)
(*        universe.walkers *)
(*       : xy_coords array); *)
(**)
(*   Array.iter2 *)
(*     (fun (x, y) map -> filter_function (x, y) map paraboloide_hiperbolico) *)
(*     universe.walkers universe.maps *)

let step_simultaneous_in (universe : universe_parallel) =
  let walkers = universe.walkers in
  Mat.iteri_cols
    (fun i_walker walker ->
      let x, y =
        Mat.(get walker 0 0 |> int_of_float, get walker 1 0 |> int_of_float)
      in
      let xn, yn = generate_new_coordinates universe.dims (x, y) in
      let ret =
        Mat.(
          set_fancy
            [ L [ 0; 1 ]; I i_walker ]
            walkers
            (of_array [| float_of_int xn; float_of_int yn |] 2 1))
      in
      ret)
    walkers;
  Mat.iteri_cols
    (fun index coord ->
      let map = universe.maps.(index) in
      filter_function coord map paraboloide_hiperbolico)
    universe.walkers

let step_simultaneous (universe : universe_parallel) =
  Mat.iteri_cols
    (fun i_walker walker ->
      let x, y =
        Mat.(get walker 0 1 |> int_of_float, get walker 0 1 |> int_of_float)
      in
      let xn, yn = generate_new_coordinates universe.dims (x, y) in
      Mat.(
        set_fancy
          [ L [ 0; 1 ]; I i_walker ]
          walker
          (of_array [| float_of_int xn; float_of_int yn |] 2 1)))
    universe.walkers;
  Mat.iteri_cols
    (fun index coord ->
      let map = universe.maps.(index) in
      filter_function coord map paraboloide_hiperbolico)
    universe.walkers

let parallell_integral (simulation : universe_parallel) : int =
  Array.fold_left
    (fun acc map -> acc + int_of_float (compute_integral map))
    0 simulation.maps

let compute_final_image universe : Mat.mat =
  let base = create_map universe.dims in
  Array.fold_left (* map_wrapper pero tendria que ser *)
    (fun accum matrix ->
      let x = Mat.(accum + matrix.content) in
      x) (* tipo : Matriz pero deberia ser map_wrapper *)
    base.content universe.maps

let run_parallel_simulation (simulation : universe_parallel) =
  let rec aux step_n =
    match step_n with
    (*
    | progress when progress <= simulation.steps && progress mod 100000 == 0 ->
        step_simultaneous_in simulation;
        print_endline "computing integral";
        (* let integral = parallell_integral simulation in *)
        Printf.printf "step = %d" progress;
        (* Printf.printf "Step = %d Integral value = %d\n" progress integral; *)
        aux (step_n + 1)
    *)
    | progress when progress <= simulation.steps ->
        step_simultaneous_in simulation;
        aux (step_n + 1)
    | _ -> ()
  in
  aux 0
