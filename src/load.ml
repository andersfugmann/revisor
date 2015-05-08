(** Load files from a given directory *)
open Batteries

let conf_dir = "etc"
let suffix = ".json"


(** Change a spec into a list of specs if multiple processes are
    requested *)
let expand = function
  | { Process.processes = 1; _ } as p -> [ p ]
  | { Process.processes; name; _} as p when processes > 1 ->
    List.init processes
      (fun n -> { p with Process.processes = 1;
                         name = Printf.sprintf "%s:%d" name n
                }
      )
  | { Process.processes; name; _} -> failwith (Printf.sprintf "%s: illegal number of processes (%d)" name processes)

let load_file f =
  f
  |> tap print_endline
  |> Yojson.Safe.from_file
  |> Process.of_yojson
  |> function `Ok v -> v
            | `Error s -> failwith ("Error while loading config file: " ^ f ^ ". Error was: " ^ s)

let load dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f suffix)
  |> List.map (fun l -> dir ^ "/" ^ l)
  |> List.map load_file
  |> List.map expand
  |> List.flatten

(** The idea is a a reload finds all files in dir and sends a reload event.
    For those tasks where no changes have been made, we leave them be.
    Removed processes are killed,
    and changed processes are restarted.
*)
