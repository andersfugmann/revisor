open Batteries
open Log

let reap_children t =
  match Unix.waitpid [Unix.WNOHANG] (-1) with
  | 0, _ ->
    None
  | pid, Unix.WSTOPPED sig_no ->
    (* log "Child stopped: %d(%d)" pid sig_no; *)
    (* Detatch if we dont want to listen anymore *)
    (match Hashtbl.mem t.Revisor.pids pid with
    | true -> Extern.ptrace_cont pid sig_no
    | false ->
      log `Info "Detach pid: %d. Not ours" pid;
      Extern.ptrace_detach pid) |> ignore;
    None
  | pid, Unix.WEXITED _s ->
    (* log `Debug "Child exited: %d(%d)" pid s; *)
    Some pid
  | pid, Unix.WSIGNALED _s ->
    (* log `Debug "Child signaled: %d(%d)" pid s; *)
    Some pid
  | exception Unix.Unix_error(Unix.ECHILD, "waitpid", _) ->
    None
  | exception e ->
    log `Debug "Exception in signal handler: %s" (Printexc.to_string e);
    None

(* Setup an alarm, and match on that. I dont like control flow using signals *)
let rec event_loop signal_fd t =
  begin
    match Unix.select [signal_fd] [] [] 1.0 with
    | (_ :: _, _, _) ->
      ExtUnixAll.signalfd_read signal_fd |> ignore;
      reap_children t
      |> Option.may
      @@ fun p ->
      let name = Hashtbl.find t.Revisor.pids p in
      Revisor.process_term t name p;
      Revisor.check t name
    | _ ->
      (* Could be starved, if a process dies every second *)
      Revisor.check_all t
  end;
  event_loop signal_fd t

let reload t =
  let specs = Load.load Config.conf_dir in
  specs
  |> List.iter (fun spec ->
      spec |> Process.to_yojson |> Yojson.Safe.pretty_to_string |> print_endline;
      Revisor.reload_spec t spec
    );

  let current =
    specs
    |> List.map (fun s -> s.Process.name)
    |> Set.of_list
  in
  log `Info "Number of processes: %d" (Set.cardinal current);

  Hashtbl.keys t.Revisor.procs
  |> Set.of_enum
  |> (flip Set.diff) current
  |> Set.iter (fun name -> Revisor.set_target t name Revisor.Disabled)

let attach (_name, pid) =
  (* No need to detatch, as we do that when reaping child processes *)
  Process.is_running pid (* This first test is simply and optimization *)
  && Extern.ptrace_seize (fst pid) == 0
  && Process.is_running pid


let main () =
  Unix.(sigprocmask SIG_BLOCK [ Sys.sigchld ]) |> ignore;
  let signal_fd = ExtUnixAll.signalfd ~sigs:[ Sys.sigchld ] ~flags:[] () in

  let procs = Revisor.init () in
  (* Attach all pids *)

  let (pids, terminated) =
    procs
    |> Hashtbl.enum
    |> Enum.map (fun (name, e) -> name, Revisor.pids_of_state (snd e.Revisor.state))
    |> Enum.concat_map (fun (name, pids) -> List.map (fun p -> name, p) pids |> List.enum)
    |> Enum.switch attach
  in
  log `Info "Reattached to %d pids" (Enum.count pids);
  log `Info "Pids stopped while down: %d" (Enum.count terminated);
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

  event_loop signal_fd t

let _ =
  log `Info "Revisor started\n%!";
  main ()
