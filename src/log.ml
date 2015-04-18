open Batteries

let now () = Unix.gettimeofday () *. 1000. |> truncate
let start = now ()

let log fmt =
  Printf.ksprintf
    (fun s ->
       try Unix.write Unix.stderr s 0 (String.length s) |> ignore
       with _ -> ())
    ("%05d: " ^^ fmt ^^ "\n") (now () - start)
