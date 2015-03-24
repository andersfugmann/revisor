open Batteries
open Log

(* Everything is keys on pd.name. That is unique *)
type pid = int
type ts = int
type current_state = Running of pid * pid list * ts
                   | Stopping of pid option * pid list * ts
                   (* | Starting of ts (* This state is used if we need to monitor a daemon process *) *)
                   | Stopped of ts

(* The target state could hold information about restarts etc. *)
type target_state = Enabled
                  | Disabled

type state = {
  target_state: target_state;
  current_state: current_state;
}

let process_tbl = Hashtbl.create 0 (* name -> process decription *)
let state_tbl = Hashtbl.create 0 (* name -> state *)
let pid_tbl = Hashtbl.create 0 (* pid -> name *)

type event =
  | Term of pid
  | Start of string
  | Stop of string

let now () = Unix.gettimeofday () |> truncate
(* Just process state change requests. Cannot be used to enable / disable processes *)
let process_event event =
  let now = now () in
  let name, new_c_state = match event with
    | Term s_pid ->
      begin
        let name = Hashtbl.find pid_tbl s_pid in
        let state = Hashtbl.find state_tbl name in
        name, match state.current_state with
        | Running (pid, pids, ts) when s_pid = pid ->
          log "Received signal from %s (%d)" name pid;
          log "Process was running for %d secs" (now - ts);
          Hashtbl.remove pid_tbl pid;
          List.iter (fun pid -> Unix.kill pid Sys.sigterm) pids;
          Stopping (None, pids, now)
        | Running (pid, pids, ts) when List.mem pid pids ->
          log "Failure: one of the redirect processes for %s died" name;
          log "Process was running for %d secs" (now - ts);
          let pids = List.filter ((<>) s_pid) pids in
          Unix.kill pid Sys.sigterm;
          Stopping (Some pid, pids, now)

        | Stopping (Some pid, pids, ts) when s_pid = pid ->
          log "Process %s took %d secs to stop" name (now - ts);
          List.iter (fun pid -> Unix.kill pid Sys.sigterm) pids;
          Stopping (None, pids, ts)

        | Stopping (pid, pids, ts) when List.mem s_pid pids ->
          let pids = List.filter ((<>) s_pid) pids in
          log "pipe stopped for %s after %d secs" name (now - ts);
          Stopping (pid, pids, ts)

        | _ -> failwith (Printf.sprintf "pid %d is not a member of %s" s_pid name)
      end
    | Start name ->
      begin
        let state = Hashtbl.find state_tbl name in
        name, match state.current_state with
        | (Running _) as s -> s
        | (Stopping _) as s -> s
        | Stopped ts ->
          (* Start the process *)
          log "Starting process %s. In stopped state for %d seconds" name (now - ts);
          let pd = Hashtbl.find process_tbl name in
          let (pid, pids) =  Process.start pd in
          List.iter (fun p -> Hashtbl.add pid_tbl p name) (pid :: pids);
          Running (pid, pids, now)
      end
    | Stop name ->
      begin
        let state = Hashtbl.find state_tbl name in
        name, match state.current_state with
        | Running (pid, pids, ts) ->
          log "Stopping process: %s. Running time: %d secs." name (now - ts);
          Unix.kill pid Sys.sigterm;
          Stopping (Some pid, pids, now)
        | Stopping (None, [], ts) ->
          log "Process %s entered stopped state after %d secs." name (now - ts);
          Stopped now
        | Stopping _ as s -> s
        | Stopped _ as s -> s
      end
  in
  (* What is name???? *)
  let state = Hashtbl.find state_tbl name in
  Hashtbl.replace state_tbl name
    { state with current_state = new_c_state }

let check name = function
  | { current_state = Stopping (_, _, ts); _ } when ts + 5 < (now ()) ->
    Some (Stop name)
  | { current_state = Stopping (None, [], _); _ } ->
    Some (Stop name)
  | { current_state = Running _; target_state = Disabled } ->
    Some (Stop name)
  | { current_state = Stopped _; target_state = Enabled } ->
    Some (Start name)
  | _ -> None

let rec event_loop queue =
  match Queue.Exceptionless.take queue with
  | Some event ->
    process_event event;
    event_loop queue
  | None ->
    state_tbl
    |> Hashtbl.enum
    |> Enum.filter_map (uncurry check)
    |> Enum.iter (fun e -> Queue.push e queue);
    begin
      match Queue.is_empty queue with
      | true -> Unix.sleep 1;
      | false -> ()
    end;
    event_loop queue

let rec handle_child_death queue signal =
  match Unix.waitpid [ Unix.WNOHANG ] (-1) with
  | 0, _ -> ()
  | _, Unix.WSTOPPED _ -> handle_child_death queue signal
  | pid, Unix.WEXITED _
  | pid, Unix.WSIGNALED _ ->
    (* log "Received term signal for pid %d: %d" pid (now ()); *)
    Queue.add (Term pid) queue;
    handle_child_death queue signal
  | exception e ->
    log "Exception in signal handler: %s" (Printexc.to_string e)

(*
let check_pd = function
  | { name; pid = Some pid; _ } as pd ->
    begin
      try Unix.kill pid 0 with
      | e ->
        Printf.eprintf "Process hung: %s:%s\n%!" name (Printexc.to_string e);
        pd.pid <- None;
        Queue.add (Start name) tasks
    end
  | { name; pid = None; _ } ->
    Printf.eprintf "Process not started: %s\n%!" name;
    Queue.add (Start name) tasks
*)

let _ =
  Printf.eprintf "Revisor started\n%!";
  let queue = Queue.create () in
  Sys.(set_signal Sys.sigchld (Signal_handle (handle_child_death queue)));

  let now = now () in
  let open Process in
  Random.enum_int 20
  |> Enum.take 500
  |> Enum.mapi (fun i sleep ->
      let name = Printf.sprintf "sleep:%d:%d" i (sleep + 1) in
      { name;
        command = "/bin/sleep";
        args = [| name; string_of_int (sleep + 1) |];
        uid = None;
        gid = None;
        nice = None;
        environment =  [];
      })
  |> Enum.iter (fun pd ->
      Hashtbl.add process_tbl pd.name pd;
      Hashtbl.add state_tbl pd.name
        { current_state = Stopped now; target_state = Enabled}
    );

  event_loop queue
