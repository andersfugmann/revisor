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

let process_tbl = Hashtbl.create 0 (* name -> process decription *)
let state_tbl = State.init ()
let pid_tbl = Hashtbl.create 0 (* pid -> name *)
let target_tbl = Hashtbl.create 0


let now () = Unix.gettimeofday () *. 1000.0 |> truncate
(* Just process state change requests. Cannot be used to enable / disable processes *)

let rec reap_children queue =
  match Unix.waitpid [Unix.WNOHANG] (-1) with
  | 0, _ -> ()
  | pid, Unix.WSTOPPED sig_no ->
    (* log "Child stopped: %d(%d)" pid sig_no; *)
    Extern.ptrace_cont pid sig_no |> ignore;
    reap_children queue
  | pid, Unix.WEXITED _s ->
    (* log "Child exited: %d(%d)" pid s; *)
    Queue.add (Term pid) queue;
    reap_children queue
  | pid, Unix.WSIGNALED _s ->
    (* log "Child signaled: %d(%d)" pid s; *)
    Queue.add (Term pid) queue;
    reap_children queue
  | exception Unix.Unix_error(Unix.ECHILD, "waitpid", _) -> ()
  | exception e ->
    log "Exception in signal handler: %s" (Printexc.to_string e)


(* Create a function for chaning state - To log and save state changes *)
let process_start name = function
  | Running _ as s -> s
  | Stopping _ as s -> s
  | Stopped  ->
    (* Start the process *)
    let pd = Hashtbl.find process_tbl name in
    let (pid, r_pid) =  Process.start pd in
    List.iter (fun (p, _) -> Hashtbl.add pid_tbl p name) [pid; r_pid];
    Running (pid, r_pid)

let process_stop = function
  | State.Running (pid, r_pid) ->
    Unix.kill (fst pid) Sys.sigterm;
    Stopping (Some pid, Some r_pid, now ())
  | Stopping (None, None, _) ->
    Stopped
  | Stopping _ as s -> s
  | Stopped as s -> s

let process_term name s_pid = function
  | Running (pid, r_pid) when s_pid = fst pid ->
    (* log "Received signal from %s (%d)" name pid; *)
    Hashtbl.remove pid_tbl (fst pid);
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
  | Stopping (None, None, _), _ ->
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
    (* See if any state changes are needed *)
    Enum.append
      (Hashtbl.keys target_tbl)
      (State.keys state_tbl)
    |> List.of_enum
    |> List.sort_unique compare
    |> List.filter_map check
    |> List.iter (fun e -> Queue.push e queue);

    reap_children queue;

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
let rec handle_child_death _signal =
  (* We really dont need to do anything, as the main loop will
     reap dead children *)
  ()

let _ =
  Printf.eprintf "Revisor started\n%!";
  let queue = Queue.create () in
  Sys.(set_signal Sys.sigchld (Signal_handle handle_child_death));
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
