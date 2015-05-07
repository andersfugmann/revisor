open Batteries

(* The target state could hold information about restarts etc. *)
type ts = int [@@deriving yojson]
type pid_t = int [@@deriving yojson]
type pid = pid_t * ts [@@deriving yojson]

type current_state = Running of pid * pid
                   | Stopping of pid option * pid option * ts
                   | Stopped [@@deriving yojson]
type state = {
  ts: int; (* Time in the state (ms) *)
  state: current_state;
} [@@deriving yojson]

let run_dir = "run"
let suffix = ".state"

let now () = Unix.gettimeofday () *. 1000.0 |> truncate

let is_running (pid, start_time) =
  match Process.start_time pid with
  | start_time' -> start_time' = start_time
  | exception _ ->
    (* File not there, no access et. al *)
    false

let validate_state state =
  let new_state = match state.state with
    | Running (p1, p2) ->
      begin
        match is_running p1 && is_running p2 with
        | true -> Running (p1, p2)
        | false ->
          Stopping (Option.filter is_running (Some p1), Option.filter is_running (Some p1), now ())
      end
    | Stopping (p1, p2, ts) ->
      Stopping (Option.filter is_running p1, Option.filter is_running p2, ts)
    | Stopped -> Stopped
  in

  { state with state = new_state}

let pids_of_state = function
  | Running ((p1, _), (p2, _)) ->
    [p1; p2]
  | Stopping (p1, p2, _) ->
    List.filter_map (Option.map fst) [p1; p2]
  | Stopped ->
    []

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

let save name state =
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
  let state = Hashtbl.Exceptionless.find t name
              |> Option.default { ts = now; state = Stopped }
  in
  let state = match state_eq new_state state.state with
    | true -> state
    | false ->
      Log.log "%s: %s -> %s after %d msec"
        name
        (name_of_state state.state)
        (name_of_state new_state)
        (now - state.ts);
      { state with ts = now }
  in
  let state = { state with state = new_state } in
  save name state;
  Hashtbl.replace t name state

let state t name =
  (** Assume non existent names to be stopped *)
  match Hashtbl.Exceptionless.find t name with
  | Some s -> s.state
  | None -> Stopped

let keys t =
  Hashtbl.keys t
