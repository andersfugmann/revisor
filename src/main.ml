open Batteries
open Log

type name = string
type t = {
  procs: (name, Revisor.t) Hashtbl.t;
  pids: (int, name) Hashtbl.t;
}

(* Stuff for getting commands *)

let reap_children t =
  match Unix.(waitpid [WNOHANG] (-1)) with
  | 0, _ ->
    None
  (* failwith "Cannot happen" (* None (* Some pending?? *) *) *)
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
    Some pid
  | pid, Unix.WSIGNALED _s ->
    Some pid
  | exception Unix.Unix_error(Unix.ECHILD, "waitpid", _) ->
    None
  | exception e ->
    log `Debug "Exception in signal handler: %s" (Printexc.to_string e);
    None

let update_proc t proc =
  Hashtbl.modify_opt proc.Revisor.spec.Process.name
    (fun p ->
       Option.may (fun p -> Revisor.pids p
                            |> List.map fst
                            |> List.iter (Hashtbl.remove t.pids)) p;
       Revisor.pids proc
       |> List.map fst
       |> List.iter (fun p ->
           Hashtbl.add t.pids p proc.Revisor.spec.Process.name);
       Some (proc)
    ) t.procs

let reload t =
  let specs = Load.load Config.conf_dir in
  specs
  |> List.iter
    (fun spec ->
       spec
       |> Process.to_yojson
       |> Yojson.Safe.pretty_to_string
       |> print_endline;
       Hashtbl.find_option t.procs spec.Process.name
       |> Revisor.update_spec spec
       |> update_proc t
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
  |> Set.iter (fun name ->
      Hashtbl.modify name
        (fun proc ->
           Revisor.set_target proc Revisor.Disabled)
        t.procs)

let exec t f patterns =
  let p = t.procs
          |> Hashtbl.enum
          |> Enum.filter (fst %> Commands.match_name patterns)
          |> Enum.map snd
          |> List.of_enum
          |> List.map f
  in
  List.iter (update_proc t) p;
  p

let process_command t = function
  | Commands.Status p ->
    0, exec t identity p
  | Commands.Stop p ->
    0, exec t (flip Revisor.set_target Revisor.Disabled) p
  | Commands.Start p ->
    0, exec t (flip Revisor.set_target Revisor.Enabled) p
  | Commands.Restart p ->
    (* Restarting a process also sets it in enabled state *)
    0, exec t (Revisor.process_stop %> (flip Revisor.set_target Revisor.Enabled)) p  (* We just stop the processes *)
  | Commands.Reload  ->
    reload t;
    (0, [])



let handle_command t socket =
  ZMQ.Socket.recv socket
  |> Yojson.Safe.from_string
  |> Commands.of_yojson
  |> (function `Ok v -> v | `Error s -> failwith s)
  |> process_command t
  |> Commands.result_to_yojson
  |> Yojson.Safe.to_string
  |> ZMQ.Socket.send socket

let rec event_loop signal_fd socket t =
  t.procs
  |> Hashtbl.values
  |> List.of_enum
  |> List.map Revisor.check
  |> List.iter (update_proc t);

  begin
    let socket_fd = ZMQ.Socket.get_fd socket in
    match Unix.select [signal_fd; socket_fd] [] [] 1.0 with
    | [], [], [] -> ()
    | [f], [], [] when socket_fd = f ->
      handle_command t socket
    | _ ->
      ExtUnixAll.signalfd_read signal_fd |> ignore;
      (* Why are we loosing signals ? *)
      Enum.from_while (fun () -> reap_children t)
      |> Enum.iter (fun p ->
          let name = try Hashtbl.find t.pids p with
            | _ -> failwith (Printf.sprintf "pid %d not found in %s" p
                               (dump (Hashtbl.enum t.pids |> List.of_enum)))
          in
          Hashtbl.remove t.pids p;
          Hashtbl.find t.procs name
          |> (fun proc -> Revisor.process_term proc p)
          |> Revisor.check
          |> update_proc t
        );
  end;
  event_loop signal_fd socket t

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
  log `Info "Loaded %d states." (Hashtbl.length procs);

  (* Attach all pids *)
  let (pids, terminated) =
    procs
    |> Hashtbl.enum
    |> Enum.concat_map (fun (name, e) -> Revisor.pids e |> List.map (fun p -> name, p) |> List.enum)
    |> Enum.switch attach
  in
  log `Info "Reattached to %d pids" (Enum.count pids);
  log `Info "Pids stopped while down: %d" (Enum.count terminated);

  (* pids are all the pids that are attached and running *)
  (* missing are all pids that are not ours or are not running *)
  let pids =
    pids |> Enum.map (fun (name, (pid, _)) -> pid, name) |> Hashtbl.of_enum
  in

  let t = { procs; pids } in

  (* Send process term signal to all processes. *)
  terminated
  |> Enum.iter (fun (name, (s_pid, _)) -> Revisor.process_term (Hashtbl.find t.procs name) s_pid |> update_proc t);

  reload t;

  let ctx = ZMQ.Context.create() in
  let socket =
    let s = ZMQ.Socket.create ctx ZMQ.Socket.rep in
    ZMQ.Socket.bind s Config.endpoint;
    s
  in

  event_loop signal_fd socket t

let _ =
  log `Info "Revisor started\n%!";
  main ()
