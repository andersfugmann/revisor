(** Simple test program. It will kill itself after N seconds (random) *)
let () =
  if false then exit 0;
  let lower_bound = Sys.argv.(1) |> int_of_string in
  let upper_bound = Sys.argv.(2) |> int_of_string in
  Random.self_init ();
  print_endline "Started";
  let sleep =
    lower_bound +
    match upper_bound - lower_bound with
    | 0 -> 0
    | n -> Random.int n
  in
  prerr_endline ("Sleeping: " ^ string_of_int sleep);
  Unix.select [] [] [] (float sleep /. 1000.0) |> ignore;
  prerr_endline "Wokeup";
  print_endline "Stopping";
  ()
