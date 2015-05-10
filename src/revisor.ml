open Batteries
open Log

(* Everything is keys on pd.name. That is unique *)

(* Could move state validation to seperate file with the spec of the state. *)

type target_state = Enabled
                  | Disabled [@@deriving yojson]

type name = string
type ts = int [@@deriving yojson]
type pid_t = int [@@deriving yojson]
type pid = pid_t * ts [@@deriving yojson]

type state = Running of pid * pid
           | Stopping of pid option * pid option * ts
           | Stopped [@@deriving yojson]

type elt = { spec: Process.t;
             state: (ts * state);
             target: target_state;
           } [@@deriving yojson]

type t = {
  procs: (name, elt) Hashtbl.t;
  pids: (pid_t, name) Hashtbl.t;
}

let now () = Unix.gettimeofday () *. 1000.0 |> truncate

let save name elt =
  let file = (Config.run_dir ^ "/" ^ name ^ Config.run_suffix) in
  elt_to_yojson elt
  |> Yojson.Safe.to_file file

let load filename =
  filename
  |> Yojson.Safe.from_file
  |> elt_of_yojson
  |> (function `Ok elt -> Some elt
            | `Error s ->
              log "Error loading file: %s" s;
              None)


let name_of_state = function
  | Running _  -> "Running"
  | Stopping _ -> "Stopping"
  | Stopped    -> "Stopped"

let state_eq = function
  | Running _  -> (function Running _  -> true | _ -> false)
  | Stopping _ -> (function Stopping _ -> true | _ -> false)
  | Stopped    -> (function Stopped    -> true | _ -> false)


let update_state t name new_state =
  let update new_state pd =
    let (old_ts, old_state) = pd.state in
    let state = match state_eq new_state old_state with
      | true -> (old_ts, new_state)
      | false ->
        let now = now () in
        Log.log "%s: %s -> %s after %d msec"
          name
          (name_of_state old_state)
          (name_of_state new_state)
          (now - old_ts);
        (now, new_state)
    in
    { pd with state; }
  in
  Hashtbl.modify name (update new_state) t.procs


let state_of_name t name =
  let s = Hashtbl.find t.procs name in
  snd s.state

let state_of_pid t pid =
  Hashtbl.find t.pids pid
  |> state_of_name t

let spec_of_name t name =
  let s = Hashtbl.find t.procs name in
  s.spec

let name_of_pid t pid =
  Hashtbl.find t.pids pid

let kill signal pid =
  match Process.is_running pid with
  | true -> Unix.kill (fst pid) signal
  | false -> ()

let update_pid t name (pid, _) =
  Hashtbl.add t.pids pid name

let remove_pid t (pid, _) =
  Hashtbl.remove t.pids pid

(* Create a function for chaning state - To log and save state changes *)
let process_start t name =
  match state_of_name t name with
  | Running _ -> ()
  | Stopping _ -> ()
  | Stopped  ->
    (* Start the process *)
    let pd = spec_of_name t name in
    let (pid, r_pid) = Process.start pd in
    update_pid t name pid;
    update_pid t name r_pid;
    Running (pid, r_pid) |> update_state t name

let process_stop t name =
  let now = now () in
  let new_state = match state_of_name t name with
    | Running (pid, r_pid) ->
      kill Sys.sigterm pid;
      Stopping (Some pid, Some r_pid, now)
    | Stopping (None, None, _) ->
      failwith "Transitional state. This should not happen."
    | Stopping (p1, p2, ts) when ts + 5 < now ->
      Option.may (kill Sys.sigkill) p1;
      Option.may (kill Sys.sigkill) p2;
      Stopping (p1, p2, now)
    | Stopping _ as s -> s
    | Stopped as s -> s
  in
  update_state t name new_state

let process_term t name s_pid =
  match state_of_name t name with
  | Running (pid, r_pid) when s_pid = fst pid ->
    (* log "Received signal from %s (%d)" name pid; *)
    remove_pid t pid;
    Stopping (None, Some r_pid, now ()) |> update_state t name

  | Running (pid, r_pid) when s_pid = fst r_pid ->
    (* log "Failure: one of the redirect processes for %s died" name; *)
    kill Sys.sigterm pid;
    Stopping (Some pid, None, now ()) |> update_state t name

  | Stopping (Some pid, None, _ts) when s_pid = fst pid ->
    Stopped |> update_state t name
  | Stopping (None, Some r_pid, _ts) when s_pid = fst r_pid ->
    Stopped |> update_state t name
  | Stopping (Some pid, Some r_pid, _ts) when s_pid = fst pid ->
    (* The main process finally stopped. Send the logger a term *)
    kill Sys.sigterm r_pid;
    Stopping (None, Some r_pid, now()) |> update_state t name

  | Stopping (Some pid, Some r_pid, ts) when s_pid = fst r_pid ->
    (* This is strange. We send a kill signal to main process, and now the
       redicrect process terminated? *)
    Stopping (Some pid, None, ts) |> update_state t name

  | state ->
    log "Illegal state for term: %s" (state_to_yojson state |> Yojson.Safe.pretty_to_string);

    failwith (Printf.sprintf "pid %d is not a member of %s" s_pid name)

let string_of_target = function
  | Enabled -> "enabled"
  | Disabled -> "disabled"

let check t name =
  match Hashtbl.find t.procs name with
  | { state = _, Stopping (_, _, ts);  _ } when ts + 5 < (now ()) ->
    process_stop t name
  | { state = _, Stopping (None, None, _); _ } ->
    process_stop t name
  | { state = _, Running _; target = Disabled; _ } ->
    process_stop t name
  | { state = _, Stopped; target = Enabled; _} ->
    process_start t name
  | _ -> ()

let check_all t =
  t.procs
  |> Hashtbl.keys
  |> Enum.iter (check t)

let pids_of_state = function
  | Running(p1, p2) -> [p1; p2]
  | Stopping(p1, p2, _) -> List.filter_map identity [p1; p2]
  | Stopped -> []

let init () =
  Sys.readdir Config.run_dir
  |> Array.enum
  |> Enum.filter (fun f -> Filename.check_suffix f Config.run_suffix)
  |> Enum.map (fun l -> Config.run_dir ^ "/" ^ l)
  |> Enum.filter_map load
  |> Enum.map (fun elt -> elt.spec.Process.name, elt)
  |> Hashtbl.of_enum

let reload_spec t spec =
  let open Process in
  match Hashtbl.find_option t.procs spec.name with
  | None ->
      Hashtbl.add t.procs spec.name { spec; state = (now(), Stopped); target = Enabled; }
  | Some { spec = spec'; _ } when spec = spec' -> ()
  | Some { state; _ } -> (* Apparently the state has been changed *)
    let target = match spec.auto_start with
      | true -> Enabled
      | false -> Disabled
    in
    Hashtbl.replace t.procs spec.name { spec ; target; state };
    process_stop t spec.name
