(*
A list of known process definitions.

If a process dies, we get sig_child and stop the process.
A message is posted on the command list to start the process again.


Process description should have a list of processes, as we want to
start N processes

We dont like daemon processes.

We dont want to optimize for speed (lookups) yet.

All running processes creates a file in /var/run/revisor.
If the revisor is restarted, we reload all the pids.

Commands are put on a prio-queue to be executed after a given time.

*)

open Batteries

type process_description =
  { name: string;
    mutable pid: int option;
    command: string;
    args: string array;
  }

type events =
| Exited of int (* Pid *)
| Start of string (* Name *)



type t = process_description list

let start_process pd =
  match Unix.fork () with
  | 0 -> (* Child *)
    begin
      Printf.eprintf "Started child.\n%!";
      try
        Unix.execv pd.command pd.args
      with
      | e -> Printf.eprintf "execv failed: %s\n%!" (Printexc.to_string e)
    end
  | pid ->
    Printf.eprintf "Forked child pid: %d\n%!" pid;
    pd.pid <- Some pid

let tasks = Queue.create ()
let processes = ref []

let find = function
  | `Name n -> List.find (function { name; _ } -> name = n) !processes
  | `Pid p -> List.find (function { pid; _ } -> pid = Some p) !processes

let handle_child_death _ =
  match Unix.wait () with
  | _, Unix.WSTOPPED _ -> ()
  | pid, Unix.WEXITED _
  | pid, Unix.WSIGNALED _ ->
    Queue.add (Exited pid) tasks

let rec check_pd = function
  | { name; pid = Some pid; _ } as pd ->
    begin
      try Unix.kill pid 0 with
      | e ->
        Printf.eprintf "Process hung: %s:%s\n%!" name (Printexc.to_string e);
        pd.pid <- None;
        Queue.add (Start name) tasks
    end
  | { name; pid = None; _ } ->
    Printf.eprintf "Process not yet started: %s\n%!" name;
    Queue.add (Start name) tasks

let handle_event = function
  | Start name ->
    Printf.eprintf "Starting process: %s\n%!" name;
    let pd = find (`Name name) in
    start_process pd
  | Exited pid ->
    let pd = find (`Pid pid) in
    check_pd pd
(*
    (* Start it again immediatly *)
  Printf.eprintf "Process died: %s\n%!" pd.name;
    pd.pid <- None;
  Queue.add (Start pd.name) tasks
*)
let rec process_events () =
  let _ =
    try
      handle_event (Queue.take tasks);
    with
    | Queue.Empty ->
      Unix.sleep 1
  in
  process_events ()

let _ =
  Printf.eprintf "Revisor started\n%!";
  Sys.(set_signal Sys.sigchld (Signal_handle handle_child_death));

  (* Construct a process description *)
  let sleep = { name = "Sleep";
                pid = None;
                command = "/bin/sleep";
                args = [| "sleep"; "5" |]
              }
  in

  processes := [ sleep ];
  List.iter check_pd !processes;
  process_events ()
