open Batteries
open Log
open State

(* Everything is keys on pd.name. That is unique *)

type event =
  | Term of pid
  | Start of string
  | Stop of string

type target_state = Enabled
                  | Disabled

let process_tbl = Hashtbl.create 0 (* name -> process decription *)
let state_tbl = State.init ()
let pid_tbl = Hashtbl.create 0 (* pid -> name *)
let target_tbl = Hashtbl.create 0


let now () = Unix.gettimeofday () *. 1000.0 |> truncate
(* Just process state change requests. Cannot be used to enable / disable processes *)

(* Create a function for chaning state - To log and save state changes *)
let process_start name = function
  | Running _ as s -> s
  | Stopping _ as s -> s
  | Stopped  ->
    (* Start the process *)
    let pd = Hashtbl.find process_tbl name in
    let (pid, pids) =  Process.start pd in
    List.iter (fun p -> Hashtbl.add pid_tbl p name) (pid :: pids);
    Running (pid, pids)

let process_stop = function
  | State.Running (pid, pids) ->
    Unix.kill pid Sys.sigterm;
    Stopping (Some pid, pids, now ())
  | Stopping (None, [], _) ->
    Stopped
  | Stopping _ as s -> s
  | Stopped as s -> s

let process_term name s_pid = function
  | Running (pid, pids) when s_pid = pid ->
    (* log "Received signal from %s (%d)" name pid; *)
    Hashtbl.remove pid_tbl pid;
    List.iter (fun pid -> Unix.kill pid Sys.sigterm) pids;
    Stopping (None, pids, now ())

  | Running (pid, pids) when List.mem pid pids ->
    (* log "Failure: one of the redirect processes for %s died" name; *)
    let pids = List.filter ((<>) s_pid) pids in
    Unix.kill pid Sys.sigterm;
    Stopping (Some pid, pids, now ())

  | Stopping (Some pid, pids, ts) when s_pid = pid ->
    List.iter (fun pid -> Unix.kill pid Sys.sigterm) pids;
    Stopping (None, pids, ts)

  | Stopping (pid, pids, ts) when List.mem s_pid pids ->
    let pids = List.filter ((<>) s_pid) pids in
    Stopping (pid, pids, ts)
  | _ ->
    failwith (Printf.sprintf "pid %d is not a member of %s" s_pid name)


let process_event event =
  let name, new_state = match event with
    | Start name ->
      let s = State.state state_tbl name in
      name, process_start name s
    | Stop name ->
      let s = State.state state_tbl name in
      name, process_stop s

    | Term pid ->
      let name = Hashtbl.find pid_tbl pid in
      let s = State.state state_tbl name in
      name, process_term name pid s
  in
  (* At this point, we update the state table.
     We want to have a system to keep track of state changes for
     processes and keep track of how long a process has been in a given3 state
  *)
  State.update_state state_tbl name new_state


let string_of_target = function
  | Enabled -> "enabled"
  | Disabled -> "disabled"
(* Check is a process needs to be started or stopped
   During booting we dont want the stop or start any processes
*)
let check name =
  let state = State.state state_tbl name in
  let target = Hashtbl.find target_tbl name in
  match state, target with
  | Stopping (_, _, ts), _ when ts + 5 < (now ()) ->
    Some (Stop name)
  | Stopping (None, [], _), _ ->
    Some (Stop name)
  | Running _, Disabled ->
    Some (Stop name)
  | Stopped, Enabled ->
    Some (Start name)
  | _ -> None

let rec event_loop queue =
  match Queue.Exceptionless.take queue with
  | Some event ->
    begin
        process_event event;
    end;
    event_loop queue

  | None ->
    Enum.append
      (Hashtbl.keys target_tbl)
      (State.keys state_tbl)
    |> List.of_enum
    |> List.sort_unique compare
    |> List.filter_map check
    |> List.iter (fun e -> Queue.push e queue);

    begin
      match Queue.is_empty queue with
      | true ->
        (* Instead of sleeping, wait for a command message.
           We assume that signals from childs will interrupt the ZMQ recv.
           This means that we are 100% reactive *)
        Unix.sleep 1
      | false -> ()
    end;
    event_loop queue

(** Need to implement own handling of wait_pid *)
let rec handle_child_death queue signal =
  match Unix.waitpid [Unix.WNOHANG] (-1) with
  | 0, _ -> ()
  | pid, Unix.WSTOPPED sig_no ->
    (* log "Child stopped: %d(%d)" pid sig_no; *)
    Extern.ptrace_cont pid sig_no |> ignore;
    handle_child_death queue signal
  | pid, Unix.WEXITED _s ->
    (* log "Child exited: %d(%d)" pid s; *)
    Queue.add (Term pid) queue;
    handle_child_death queue signal
  | pid, Unix.WSIGNALED _s ->
    (* log "Child signaled: %d(%d)" pid s; *)
    Queue.add (Term pid) queue;
    handle_child_death queue signal
  | exception Unix.Unix_error(Unix.ECHILD, "waitpid", _) -> ()
  | exception e ->
    log "Exception in signal handler: %s" (Printexc.to_string e)

let _ =
  Printf.eprintf "Revisor started\n%!";
  let queue = Queue.create () in
  Sys.(set_signal Sys.sigchld (Signal_handle (handle_child_death queue)));
  (* let _sig_thread = Thread.create handle_child_death queue in *)

  Load.load "etc"
  |> List.iter (fun pd ->
      let open Process in
      pd |> Process.to_yojson |> Yojson.Safe.pretty_to_string |> print_endline;
      (* Set target_state to enabled *)
      Hashtbl.add process_tbl pd.name pd;
      Hashtbl.add target_tbl pd.name Enabled
    );

  event_loop queue
