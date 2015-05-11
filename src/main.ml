open Batteries
open Log

type name = string
type t = {
  procs: (name, Revisor.t) Hashtbl.t;
  pids: (int, name) Hashtbl.t;
}

let reap_children t =
  match Unix.waitpid [Unix.WNOHANG] (-1) with
  | 0, _ ->
    None
  | pid, Unix.WSTOPPED sig_no ->
    (* log "Child stopped: %d(%d)" pid sig_no; *)
    (* Detatch if we dont want to listen anymore *)
    (match Hashtbl.mem t.pids pid with
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

let update_proc t proc =
  Hashtbl.replace t.procs proc.Revisor.spec.Process.name proc;
  match Revisor.is_running proc with
  | false -> ()
  | true ->
    Revisor.pids proc
    |> List.map fst
    |> List.iter (fun p -> Hashtbl.replace t.pids p proc.Revisor.spec.Process.name)

(* Setup an alarm, and match on that. I dont like control flow using signals *)
let rec event_loop signal_fd t =
  begin
    match Unix.select [signal_fd] [] [] 1.0 with
    | (_ :: _, _, _) ->
      ExtUnixAll.signalfd_read signal_fd |> ignore;
      reap_children t
      |> Option.may
        (fun p ->
           let name = Hashtbl.find t.pids p in
           Hashtbl.remove t.pids p;
           Hashtbl.find t.procs name
           |> (fun proc -> Revisor.process_term proc p)
           |> Revisor.check
           |> update_proc t
        )
    | _ ->
      (* Could be starved, if a process dies every second *)
      Hashtbl.map_inplace (fun _ proc -> Revisor.check proc) t.procs
  end;
  event_loop signal_fd t

let reload t =
  let specs = Load.load Config.conf_dir in
  specs
  |> List.iter (fun spec ->
      spec |> Process.to_yojson |> Yojson.Safe.pretty_to_string |> print_endline;
      Hashtbl.modify_opt spec.Process.name (fun proc -> Some (Revisor.update_spec spec proc)) t.procs
    );

  let current =
    specs
    |> List.map (fun s -> s.Process.name)
    |> Set.of_list
  in
  log `Info "Number of processes: %d" (Set.cardinal current);

  Hashtbl.keys t.procs
  |> Set.of_enum
  |> (flip Set.diff) current
  |> Set.iter (fun name -> Hashtbl.modify name (fun proc -> Revisor.set_target proc Revisor.Disabled) t.procs)

let attach (_name, pid) =
  (* No need to detatch, as we do that when reaping child processes *)
  Process.is_running pid (* This first test is simply and optimization *)
  && Extern.ptrace_seize (fst pid) == 0
  && Process.is_running pid

let load_state () =
  Sys.readdir Config.run_dir
  |> Array.enum
  |> Enum.filter (fun f -> Filename.check_suffix f Config.run_suffix)
  |> Enum.map (fun l -> Config.run_dir ^ "/" ^ l)
  |> Enum.filter_map Revisor.load
  |> Enum.map (fun t -> t.Revisor.spec.Process.name, t)
  |> Hashtbl.of_enum


let main () =
  Unix.(sigprocmask SIG_BLOCK [ Sys.sigchld ]) |> ignore;
  let signal_fd = ExtUnixAll.signalfd ~sigs:[ Sys.sigchld ] ~flags:[] () in

  let procs = load_state () in
  (* Attach all pids *)

  let (pids, terminated) =
    procs
    |> Hashtbl.enum
    |> Enum.map (fun (name, e) -> name, Revisor.pids e)
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

  (* Send process term signal to all processes. *)
  terminated
  |> Enum.iter (fun (name, (s_pid, _)) -> Hashtbl.modify name (fun p -> Revisor.process_term p s_pid) procs);

  let t = { procs; pids } in
  reload t;

  event_loop signal_fd t

let _ =
  log `Info "Revisor started\n%!";
  main ()
