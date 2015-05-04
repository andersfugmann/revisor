open Batteries
open Log
open State

(* Everything is keys on pd.name. That is unique *)

type event =
  | Term of pid_t
  | Start of string
  | Stop of string

type target_state = Enabled
                  | Disabled

type name = string
type pid = int

type t = {
  process_tbl: (name, Process.t) Hashtbl.t;
  state_tbl: (name, State.state) Hashtbl.t;
  pid_tbl: (pid, name) Hashtbl.t;
  target_tbl: (name, target_state) Hashtbl.t;
}

let now () = Unix.gettimeofday () *. 1000.0 |> truncate

(* Create a function for chaning state - To log and save state changes *)
let process_start t name = function
  | Running _ as s -> s
  | Stopping _ as s -> s
  | Stopped  ->
    (* Start the process *)
    let pd = Hashtbl.find t.process_tbl name in
    let (pid, r_pid) = Process.start pd in
    List.iter (fun (p, _) -> Hashtbl.add t.pid_tbl p name) [pid; r_pid];
    Running (pid, r_pid)

let process_stop = function
  | State.Running (pid, r_pid) ->
    Unix.kill (fst pid) Sys.sigterm;
    Stopping (Some pid, Some r_pid, now ())
  | Stopping (None, None, _) ->
    Stopped
  | Stopping _ as s -> s
  | Stopped as s -> s

let process_term t name s_pid = function
  | Running (pid, r_pid) when s_pid = fst pid ->
    (* log "Received signal from %s (%d)" name pid; *)
    Hashtbl.remove t.pid_tbl (fst pid);
    Unix.kill (fst r_pid) Sys.sigterm;
    Stopping (None, Some r_pid, now ())

  | Running (pid, r_pid) when s_pid = fst r_pid ->
    (* log "Failure: one of the redirect processes for %s died" name; *)
    Unix.kill (fst pid) Sys.sigterm;
    Stopping (Some pid, None, now ())

  | Stopping (Some pid, r_pid, ts) when s_pid = fst pid ->
    Option.may (fun pid -> Unix.kill (fst pid) Sys.sigterm) r_pid;
    Stopping (None, r_pid, ts)

  | Stopping (pid, Some r_pid, ts) when s_pid = fst r_pid ->
    Option.may (fun pid -> Unix.kill (fst pid) Sys.sigterm) pid;
    Stopping (pid, None, ts)

  | _ ->
    failwith (Printf.sprintf "pid %d is not a member of %s" s_pid name)


let process_event t event =
  let name, new_state = match event with
    | Start name ->
      let s = State.state t.state_tbl name in
      name, process_start t name s
    | Stop name ->
      let s = State.state t.state_tbl name in
      name, process_stop s

    | Term pid ->
      let name = Hashtbl.find t.pid_tbl pid in
      let s = State.state t.state_tbl name in
      name, process_term t name pid s
  in
  (* At this point, we update the state table.
     We want to have a system to keep track of state changes for
     processes and keep track of how long a process has been in a given3 state
  *)
  State.update_state t.state_tbl name new_state


let string_of_target = function
  | Enabled -> "enabled"
  | Disabled -> "disabled"

(** Check is a process needs to be started or stopped
    During booting we dont want the stop or start any processes
*)
let check t name =
  let state = State.state t.state_tbl name in
  let target = Hashtbl.find t.target_tbl name in
  match state, target with
  | Stopping (_, _, ts), _ when ts + 5 < (now ()) ->
    Some (Stop name)
  | Stopping (None, None, _), _ ->
    Some (Stop name)
  | Running _, Disabled ->
    Some (Stop name)
  | Stopped, Enabled ->
    Some (Start name)
  | _ -> None

let init () =
  {
    process_tbl = Hashtbl.create 0;
    state_tbl = Hashtbl.create 0;
    pid_tbl = Hashtbl.create 0;
    target_tbl = Hashtbl.create 0;
  }
