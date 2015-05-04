open Batteries
open Log

let rec reap_children () =
  match Unix.waitpid [Unix.WNOHANG] (-1) with
  | 0, _ ->
    None
  | pid, Unix.WSTOPPED sig_no ->
    (* log "Child stopped: %d(%d)" pid sig_no; *)
    Extern.ptrace_cont pid sig_no |> ignore;
    reap_children ()
  | pid, Unix.WEXITED _s ->
    (* log "Child exited: %d(%d)" pid s; *)
    Some pid
  | pid, Unix.WSIGNALED _s ->
    (* log "Child signaled: %d(%d)" pid s; *)
    Some pid
  | exception Unix.Unix_error(Unix.ECHILD, "waitpid", _) ->
    None
  | exception e ->
    log "Exception in signal handler: %s" (Printexc.to_string e);
    None

(** Reload old state *)
let update_state t (name, state) =
  log "Reattach process: %s" name;
  let pids = State.pids_of_state state.State.state in
  List.iter (Extern.ptrace_seize %> ignore) pids;
  List.iter (fun p -> Hashtbl.add t.Revisor.pid_tbl p name) pids;
  Hashtbl.add t.Revisor.state_tbl name state


(** Need to implement own handling of wait_pid *)
let rec handle_child_death _signal =
  (* We really dont need to do anything, as the main loop will
     reap dead children *)
  ()

let handle_event t evt name =
  Revisor.process_event t evt;
  Revisor.check t name
  |> Option.may (Revisor.process_event t)

let rec event_loop t =
  Enum.from_while reap_children
  |> Enum.iter (fun p ->
      p
      |> Hashtbl.Exceptionless.find t.Revisor.pid_tbl
      |> Option.may (handle_event t (Revisor.Term p))
    );

  (* See if any state changes are needed *)
  Enum.append
    (Hashtbl.keys t.Revisor.target_tbl)
    (State.keys t.Revisor.state_tbl)
  |> List.of_enum
  |> List.sort_unique compare
  |> List.filter_map (Revisor.check t)
  |> List.iter (Revisor.process_event t);

  Unix.sleep 1;
  event_loop t


let main () =
  let t = Revisor.init () in

  Load.load Config.conf_dir
  |> List.iter (fun pd ->
      let open Process in
      pd |> Process.to_yojson |> Yojson.Safe.pretty_to_string |> print_endline;
      (* Set target_state to enabled *)
      Hashtbl.add t.Revisor.process_tbl pd.name pd;
      Hashtbl.add t.Revisor.target_tbl pd.name Revisor.Enabled (* FIXME *)
    );

  List.iter (update_state t) (State.init ());
  event_loop t

let _ =
  Printf.eprintf "Revisor started\n%!";
  Sys.(set_signal Sys.sigchld (Signal_handle handle_child_death));
  main ()
