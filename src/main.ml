open Batteries
open Log

let rec reap_children t =
  match Unix.waitpid [Unix.WNOHANG] (-1) with
  | 0, _ ->
    None
  | pid, Unix.WSTOPPED sig_no ->
    (* log "Child stopped: %d(%d)" pid sig_no; *)
    (* Detatch if we dont want to listen anymore *)
    (match Hashtbl.mem t.Revisor.pids pid with
    | true -> Extern.ptrace_cont pid sig_no
    | false -> Extern.ptrace_detach pid) |> ignore;

    reap_children t
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

(** Need to implement own handling of wait_pid *)
let rec handle_child_death _signal =
  (* We really dont need to do anything, as the main loop will
     reap dead children *)
  ()

let rec event_loop t =
  Enum.from_while (fun () -> reap_children t)
  |> Enum.iter (fun p -> Revisor.process_term t (Hashtbl.find t.Revisor.pids p) p);

  (* See if any state changes are needed *)
  Revisor.check_all t;

  Unix.sleep 1;
  event_loop t

(** We have states for running or stopping processes.
    If all stopped processes also had a state, this would not be a problem.
*)

let reload t =
  Load.load Config.conf_dir
  |> Enum.iter (fun spec ->
      spec |> Process.to_yojson |> Yojson.Safe.pretty_to_string |> print_endline;
      Revisor.reload_spec t spec
    )

let attach (_name, pid) =
  (* No need to detatch, as we do that when reaping child processes *)
  Process.is_running pid (* This first test is simply and optimization *)
  && Extern.ptrace_seize (fst pid) == 0
  && Process.is_running pid


let main () =
  let procs = Revisor.init () in
  (* Attach all pids *)

  let (pids, terminated) =
    procs
    |> Hashtbl.enum
    |> Enum.map (fun (name, e) -> name, Revisor.pids_of_state (snd e.Revisor.state))
    |> Enum.concat_map (fun (name, pids) -> List.map (fun p -> name, p) pids |> List.enum)
    |> Enum.switch attach
  in
  (* pids are all the pids that are attached and running *)
  (* missing are all pids that are not ours or are not running *)
  let pids =
    pids |> Enum.map (fun (name, (pid, _)) -> pid, name) |> Hashtbl.of_enum
  in

  let t = { Revisor.procs; pids } in

  (* Send process term signal to all processes. *)
  terminated
  |> Enum.iter (fun (name, (s_pid, _)) -> Revisor.process_term t name s_pid);

  reload t;

  event_loop t

let _ =
  Printf.eprintf "Revisor started\n%!";
  Sys.(set_signal Sys.sigchld (Signal_handle handle_child_death));
  main ()
