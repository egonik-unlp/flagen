let () = Random.self_init ()

open Flagen.Common_owl
open Flagen.Parallel_log_integral

let universe_dims = { width = 1920; height = 1080 }

let () =
  Printexc.record_backtrace true;
  try
    let simulation = genesis_parallel universe_dims 5 10_000_000 in
    run_parallel_simulation simulation;
    Array.iteri
      (fun slice_n array ->
        let filename = "matrices/resultado_" ^ string_of_int slice_n ^ ".npy" in
        save_to_csv filename array.content)
      simulation.maps;
    (* let base = Array.make_matrix universe_dims.width universe_dims.height 0 in *)
    let single_image = compute_final_image simulation in
    save_to_csv "matrices/acumulado.npy" single_image
  with ex ->
    Printf.eprintf "Exception: %s\n" (Printexc.to_string ex);
    Printf.eprintf "Backtrace: %s\n" (Printexc.get_backtrace ())
