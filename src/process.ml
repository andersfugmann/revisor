open Batteries

type t =
  { name: string;
    command: string;
    args: string array;
    uid: int option;
    gid: int option;
    nice: int option;
    environment: (string * string) list;
  }

let redirect ?uid ?gid ?nice filename =
  let (read_fd, write_fd) = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
    Option.may Unix.setuid uid;
    Option.may Unix.setgid gid;
    Option.may (Unix.nice %> ignore) nice;
    Pipe.run read_fd filename;
    exit 0
  | pid -> (pid, write_fd)

let start pd =
  let open Unix in
  (*
  let (stdout_pid, new_stdout) =
    redirect ?uid:pd.uid ?gid:pd.gid ?nice:pd.nice (Printf.sprintf "log/%s.log" pd.name)
  in
  let (stderr_pid, new_stderr) =
    redirect ?uid:pd.uid ?gid:pd.gid ?nice:pd.nice (Printf.sprintf "log/%s.err" pd.name)
  in
*)
  match Unix.fork () with
  | 0 ->
    begin
      Option.may setuid pd.uid;
      Option.may setgid pd.gid;
      Option.may (nice %> ignore) pd.nice;
      (* dup2 new_stderr stderr;
         dup2 new_stdout stdout; *)

      (* Setup the environment. Currenly always inherit parent env *)
      List.iter (uncurry putenv) pd.environment;
      Printf.eprintf "Started child.\n%!";
      try
        Unix.execv pd.command pd.args
      with
      | e -> Printf.eprintf "execv failed: %s\n%!" (Printexc.to_string e);
        failwith "exec failed"
    end
  | pid ->
    Printf.eprintf "Forked child pid: %d\n%!" pid;
    pid, (* [stdout_pid; stderr_pid] *) [];
