open Batteries
open Log

type target_state = Enabled
                  | Disabled [@@deriving yojson]

type name = string
type ts = int [@@deriving yojson]
type pid_t = int [@@deriving yojson]
type pid = pid_t * ts [@@deriving yojson]

type state = Running of pid * pid
           | Stopping of pid option * pid option * ts
           | Stopped [@@deriving yojson]

type t = { spec: Process.t;
           state: (ts * state);
           target: target_state;
         } [@@deriving yojson]

let now () = Unix.gettimeofday () *. 1000.0 |> truncate

let save t =
  let file = (Config.run_dir ^ "/" ^ t.spec.Process.name ^ Config.run_suffix) in
  to_yojson t
  |> Yojson.Safe.to_file file

let load filename =
  filename
  |> Yojson.Safe.from_file
  |> of_yojson
  |> (function `Ok elt -> Some elt
            | `Error s ->
              log `Warning "Error loading file: %s" s;
              None)

let name_of_state = function
  | Running _  -> "Running"
  | Stopping _ -> "Stopping"
  | Stopped    -> "Stopped"

let state_eq = function
  | Running _  -> (function Running _  -> true | _ -> false)
  | Stopping _ -> (function Stopping _ -> true | _ -> false)
  | Stopped    -> (function Stopped    -> true | _ -> false)


let update_state t new_state =
  let (old_ts, old_state) = t.state in
  let state = match state_eq new_state old_state with
    | true -> (old_ts, new_state)
    | false ->
      let now = now () in
      Log.log `Debug "%s: %s -> %s after %d msec"
        t.spec.Process.name
        (name_of_state old_state)
        (name_of_state new_state)
        (now - old_ts);
      (now, new_state)
  in
  { t with state; } |> tap save

let kill signal pid =
  match Process.is_running pid with
  | true -> Unix.kill (fst pid) signal
  | false -> ()

(* Create a function for chaning state - To log and save state changes *)
let process_start t =
  let new_state = match snd t.state with
    | Running _ as s -> s
    | Stopping _ as s -> s
    | Stopped  ->
      (* Start the process *)
      let (pid, r_pid) = Process.start t.spec in
      Running (pid, r_pid)
  in
  update_state t new_state

let process_stop t =
  let now = now () in
  let new_state = match snd t.state with
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
  update_state t new_state

let process_term t s_pid =
  let new_state = match snd t.state with
    | Running (pid, r_pid) when s_pid = fst pid ->
      (* log "Received signal from %s (%d)" name pid; *)
      Stopping (None, Some r_pid, now ())

    | Running (pid, r_pid) when s_pid = fst r_pid ->
      (* log "Failure: one of the redirect processes for %s died" name; *)
      kill Sys.sigterm pid;
      Stopping (Some pid, None, now ())

    | Stopping (Some pid, None, _ts) when s_pid = fst pid ->
      Stopped
    | Stopping (None, Some r_pid, _ts) when s_pid = fst r_pid ->
      Stopped
    | Stopping (Some pid, Some r_pid, _ts) when s_pid = fst pid ->
      (* The main process finally stopped. Send the logger a term *)
      kill Sys.sigterm r_pid;
      Stopping (None, Some r_pid, now())

    | Stopping (Some pid, Some r_pid, ts) when s_pid = fst r_pid ->
      (* This is strange. We sent a kill signal to main process, and now the
         redicrect process terminated? *)
      Stopping (Some pid, None, ts)

    | state ->
      log `Fatal "Illegal state for term: %s" (state_to_yojson state |> Yojson.Safe.pretty_to_string);
      failwith (Printf.sprintf "pid %d is not a member of %s" s_pid t.spec.Process.name)
  in
  update_state t new_state

let string_of_target = function
  | Enabled -> "enabled"
  | Disabled -> "disabled"

let rec check = function
  | { state = _, Stopping (_, _, ts); _ } as t when ts + 5 < (now ()) ->
    process_stop t |> check
  | { state = _, Stopping (None, None, _); _ } as t ->
    process_stop t |> check
  | { state = _, Running _; target = Disabled; _ } as t ->
    process_stop t |> check
  | { state = _, Stopped; target = Enabled; _} as t ->
    process_start t |> check
  | t -> t

let pids t =
  match snd t.state with
  | Running(p1, p2) -> [p1; p2]
  | Stopping(p1, p2, _) -> List.filter_map identity [p1; p2]
  | Stopped -> []

let set_target t target =
  log `Info "Setting target of process %s to %s" t.spec.Process.name (string_of_target target);
  { t with target } |> check

let update_spec spec = function
  | None ->
      { spec; state = (now(), Stopped); target = Enabled; }
  | Some ({ spec = spec'; _ } as s) when spec = spec' -> s
  | Some { state; _ } -> (* Apparently the state has been changed *)
    let target = match spec.Process.auto_start with
      | true -> Enabled
      | false -> Disabled
    in
    { spec ; target; state } |> process_stop |> check
