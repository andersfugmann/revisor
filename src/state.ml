open Batteries

(* The target state could hold information about restarts etc. *)
type pid = int [@@deriving yojson]
type ts = int [@@deriving yojson]

type current_state = Running of pid * pid list
                   | Stopping of pid option * pid list * ts
                   | Stopped [@@deriving yojson]
type state = {
  ts: int; (* Time in the state (ms) *)
  state: current_state;
} [@@deriving yojson]

let run_dir = "run"
let suffix = ".state"

let now () = Unix.gettimeofday () *. 1000.0 |> truncate

(** Really not a good validation *)
let validate_state s =
  { s with state = Stopped }

let load filename =
  let state =
    filename
    |> Yojson.Safe.from_file
    |> state_of_yojson
    |> (function `Ok v -> v | `Error s -> failwith s)
  in
  let name =
    Filename.basename filename
    |> Filename.chop_extension
  in
  (name, state)

let save_state name state =
  let file = (run_dir ^ "/" ^ name ^ suffix) in
  match state.state with
  | Stopped -> Unix.unlink file
  | _ -> state_to_yojson state
         |> Yojson.Safe.to_file file

let init () =
  Sys.readdir run_dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f suffix)
  |> List.map (fun l -> run_dir ^ "/" ^ l)
  |> List.map load
  |> List.map (Tuple.Tuple2.map2 validate_state)
  |> List.filter (function (_, { state; _ }) -> state <> Stopped)
  |> List.enum
  |> Hashtbl.of_enum

let name_of_state = function
  | Running _  -> "Running"
  | Stopping _ -> "Stopping"
  | Stopped    -> "Stopped"

let state_eq = function
  | Running _  -> (function Running _  -> true | _ -> false)
  | Stopping _ -> (function Stopping _ -> true | _ -> false)
  | Stopped    -> (function Stopped    -> true | _ -> false)


let update_state t name new_state =
  let now = now () in
  let state = match Hashtbl.Exceptionless.find t name with
    | Some state ->
      begin
        match state_eq new_state state.state with
        | true -> state
        | false ->
          Log.log "%s: %s -> %s after %d msec"
            name
            (name_of_state state.state)
            (name_of_state new_state)
            (now - state.ts);
          { state with ts = now }
      end
    | None -> { ts = now; state = Stopped }
  in
  let state = { state with state = new_state } in
  save_state name state;
  match new_state with
  | Stopped -> Hashtbl.remove t name
  | _ -> Hashtbl.replace t name state

let state t name =
  (** Assume non existent names to be stopped *)
  match Hashtbl.Exceptionless.find t name with
  | Some s -> s.state
  | None -> Stopped

let keys t =
  Hashtbl.keys t
