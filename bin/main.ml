let () = Random.init 02216390836

open Flagen.Common
open Flagen.Parallel

let universe_dims = { width = 1920; height = 1080 }

let () =
  Printexc.record_backtrace true;
  try
    let simulation = genesis_parallel universe_dims 20 5_000_000 in
    run_parallel_simulation simulation;
    Array.iteri
      (fun slice_n array ->
        let filename = "matrices/resultado_" ^ string_of_int slice_n ^ ".csv" in
        create_csv_from_matrix filename array)
      simulation.maps;
    let base = Array.make_matrix universe_dims.width universe_dims.height 0 in
    let single_image =
      Array.fold_left
        (fun acc matrix ->
          Array.mapi
            (fun i row_a ->
              Array.mapi (fun j b -> b + matrix.content.(i).(j)) row_a)
            acc)
        base simulation.maps
    in
    create_csv_from_matrix "matrices/acumulado.csv" { content = single_image }
  with ex ->
    Printf.eprintf "Exception: %s\n" (Printexc.to_string ex);
    Printf.eprintf "Backtrace: %s\n" (Printexc.get_backtrace ())
