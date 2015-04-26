open Batteries
open Log

type env = {
  key: string;
  value: string;
} [@@deriving yojson]

type t = {
  name: string;
  command: string;
  args: string list;
  uid: int option;
  gid: int option;
  nice: int option;
  processes: int [@default 1];
  environment: env list;
} [@@deriving yojson]

(* Can be block signal delivery while starting processes??? *)
let start_time =
  (* Read field 22 (%llu) *)
  let re = Str.regexp "[0-9]+ ([^)]*) . \\(\\([-]?[0-9]+ \\)*\\)" in
  fun pid ->
    let s =
      Printf.sprintf "/proc/%d/stat" pid
      |> File.lines_of
      |> Enum.get_exn
    in
    match Str.string_match re s 0 with
    | false ->
      failwith "Cannot decode stat for process pid"
    | true ->
      Str.matched_group 1 s
      |> String.nsplit ~by:" "
      |> (flip List.nth) 18
      |> int_of_string


let redirect name ?uid ?gid ?nice filename =
  let (read_fd, write_fd) = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
    Option.may Unix.setuid uid;
    Option.may Unix.setgid gid;
    Option.may (Unix.nice %> ignore) nice;

    (* Should do exec *)
    let read_fd_str =
      read_fd |> ExtUnixAll.int_of_file_descr |> string_of_int
    in
    Unix.execv "_build/redirect" [|name; read_fd_str; filename|]
  | pid ->
    let start_time = start_time pid in
    Unix.close read_fd;
    ((pid, start_time), write_fd)

let start pd =
  let open Unix in
  let (stdout_pid, new_stdout) =
    redirect (pd.name ^ ":stdout") ?uid:pd.uid ?gid:pd.gid ?nice:pd.nice (Printf.sprintf "log/%s.log" pd.name)
  in
  let (stderr_pid, new_stderr) =
    redirect (pd.name ^ ":stderr") ?uid:pd.uid ?gid:pd.gid ?nice:pd.nice (Printf.sprintf "log/%s.err" pd.name)
  in
  match Unix.fork () with
  | 0 ->
    begin
      Option.may setuid pd.uid;
      Option.may setgid pd.gid;
      Option.may (nice %> ignore) pd.nice;
      dup2 new_stderr stderr;
      dup2 new_stdout stdout;

      (* Setup the environment. Currenly always inherit parent env *)
      List.iter (function {key; value} -> putenv key value) pd.environment;
      log "Started child.";
      try
        Unix.execv pd.command (Array.of_list (pd.name :: pd.args))
      with
      | e -> log "execv failed: %s" (Printexc.to_string e);
        (* Should log perror *)
        failwith "exec failed"
    end
  | pid ->
    Unix.close new_stdout;
    Unix.close new_stderr;
    let start_time = start_time pid in
    (* log "Forked child pid: %d" pid; *)
    (pid, start_time), [stdout_pid; stderr_pid];
