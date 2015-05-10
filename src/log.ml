open Batteries

let now () = Unix.gettimeofday () *. 1000. |> truncate
let start = now ()

let int_of_level = function
  | `Fatal -> 1
  | `Error -> 2
  | `Warning -> 3
  | `Info -> 4
  | `Debug -> 5

let lvl = int_of_level `Debug

let log level fmt =
  Printf.ksprintf
    (fun s ->
       match int_of_level level <= lvl with
       | true ->
         begin
           try Unix.write Unix.stderr s 0 (String.length s) |> ignore
           with _ -> ()
         end
       | false -> ()
    )
    ("%05d: " ^^ fmt ^^ "\n") (now () - start)
