(** Simple test program. It will kill itself after N seconds (random) *)
let () =
  let lower_bound = Sys.argv.(1) |> int_of_string in
  let upper_bound = Sys.argv.(2) |> int_of_string in
  Random.self_init ();
  print_endline "Started";
  let sleep = Random.int (upper_bound - lower_bound) + lower_bound in
  prerr_endline ("Sleeping: " ^ string_of_int sleep);
  Unix.select [] [] [] (float sleep /. 1000.0) |> ignore;
  prerr_endline "Wokeup";
  print_endline "Stopping";
  ()
