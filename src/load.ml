(** Load files from a given directory *)
open Batteries

let conf_dir = "etc"
let suffix = ".json"

(** Change a spec into a list of specs if multiple processes are
    requested *)
let expand = function
  | { Process.processes = 1; _ } as p -> Enum.singleton p
  | { Process.processes; name; _} as p when processes > 1 ->
    Enum.init processes
      (fun n -> { p with Process.processes = 1;
                         name = Printf.sprintf "%s:%d" name n
                }
      )
  | { Process.processes; name; _} -> failwith (Printf.sprintf "%s: illegal number of processes (%d)" name processes)

let spec_of_json f json =
  json
  |> Process.of_yojson
  |> function `Ok v -> v
            | `Error s -> failwith ("Error while loading config file: " ^ f ^ ". Error was: " ^ s)

let load_file f =
  f
  |> tap print_endline
  |> Yojson.Safe.from_file
  |> (function
      | `List l -> Enum.map (spec_of_json f) (List.enum l)
      | j -> Enum.singleton (spec_of_json f j)
    )

let load dir =
  Sys.readdir dir
  |> Array.enum
  |> Enum.filter (fun f -> Filename.check_suffix f suffix)
  |> Enum.map (fun l -> dir ^ "/" ^ l)
  |> Enum.concat_map load_file
  |> Enum.concat_map expand

(** The idea is a a reload finds all files in dir and sends a reload event.
    For those tasks where no changes have been made, we leave them be.
    Removed processes are killed,
    and changed processes are restarted.
*)
