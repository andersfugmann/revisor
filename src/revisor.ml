open Batteries
open Log
open State

(* Everything is keys on pd.name. That is unique *)

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
let process_start t name =
  match State.state t.state_tbl name with
  | Running _ -> ()
  | Stopping _ -> ()
  | Stopped  ->
    (* Start the process *)
    let pd = Hashtbl.find t.process_tbl name in
    let (pid, r_pid) = Process.start pd in
    Hashtbl.add t.pid_tbl (fst pid) name;
    Hashtbl.add t.pid_tbl (fst r_pid) name;
    Running (pid, r_pid) |> State.update_state t.state_tbl name

let process_stop t name =
  match State.state t.state_tbl name with
  | State.Running (pid, r_pid) ->
    Unix.kill (fst pid) Sys.sigterm;
    Stopping (Some pid, Some r_pid, now ()) |> State.update_state t.state_tbl name
  | Stopping (None, None, _) ->
    Stopped |> State.update_state t.state_tbl name
  | Stopping _ -> ()
  | Stopped -> ()

let process_term t name s_pid =
  match State.state t.state_tbl name with
  | Running (pid, r_pid) when s_pid = fst pid ->
    (* log "Received signal from %s (%d)" name pid; *)
    Hashtbl.remove t.pid_tbl (fst pid);
    Unix.kill (fst r_pid) Sys.sigterm;
    Stopping (None, Some r_pid, now ()) |> State.update_state t.state_tbl name

  | Running (pid, r_pid) when s_pid = fst r_pid ->
    (* log "Failure: one of the redirect processes for %s died" name; *)
    Unix.kill (fst pid) Sys.sigterm;
    Stopping (Some pid, None, now ()) |> State.update_state t.state_tbl name


  | Stopping (Some pid, r_pid, ts) when s_pid = fst pid ->
    Option.may (fun pid -> Unix.kill (fst pid) Sys.sigterm) r_pid;
    Stopping (None, r_pid, ts) |> State.update_state t.state_tbl name


  | Stopping (pid, Some r_pid, ts) when s_pid = fst r_pid ->
    Option.may (fun pid -> Unix.kill (fst pid) Sys.sigterm) pid;
    Stopping (pid, None, ts) |> State.update_state t.state_tbl name

  | _ ->
    failwith (Printf.sprintf "pid %d is not a member of %s" s_pid name)

let string_of_target = function
  | Enabled -> "enabled"
  | Disabled -> "disabled"

(** Check is a process needs to be started or stopped
    During booting we dont want the stop or start any processes
*)

(* TODO: Merge current_state and target state into the same table.
   Disabled, Stopped state can be elliminated, and its easier to check
   to state change actions needed (Just iterate over keys, values)

   Oh. And the check function can do that itself, as the hashtbl
   contains all needed information. Just be careful not to alter the
   table while iterating.
   (* Maybe map is the right thing to do? Is there an effective inplace map? *)
*)

let check t name =
  let state = State.state t.state_tbl name in
  let target = Hashtbl.find t.target_tbl name in
  match state, target with
  | Stopping (_, _, ts), _ when ts + 5 < (now ()) ->
    process_stop t name
  | Stopping (None, None, _), _ ->
    process_stop t name
  | Running _, Disabled ->
    process_stop t name
  | Stopped, Enabled ->
    process_start t name
  | _ -> (* Oh - Catch all *) ()

let init () =
  {
    process_tbl = Hashtbl.create 0;
    state_tbl = Hashtbl.create 0;
    pid_tbl = Hashtbl.create 0;
    target_tbl = Hashtbl.create 0;
  }
