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


let redirect name ?uid ?gid ?nice fn1 fn2 =
  let (read_fd1, write_fd1) = Unix.pipe () in
  let (read_fd2, write_fd2) = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
    Option.may Unix.setuid uid;
    Option.may Unix.setgid gid;
    Option.may (Unix.nice %> ignore) nice;

    (* Should do exec *)
    let read_fd_str1 =
      read_fd1 |> ExtUnixAll.int_of_file_descr |> string_of_int
    in
    let read_fd_str2 =
      read_fd2 |> ExtUnixAll.int_of_file_descr |> string_of_int
    in
    Unix.execv "_build/redirect" [|name; read_fd_str1; fn1; read_fd_str2; fn2|]
  | pid ->
    Unix.close read_fd1;
    Unix.close read_fd2;
    (pid, write_fd1, write_fd2)

let start pd =
  let open Unix in
  let (redirect_pid, new_stdout, new_stderr) =
    redirect (pd.name ^ ":redirect") ?uid:pd.uid ?gid:pd.gid ?nice:pd.nice
      (Printf.sprintf "log/%s.log" pd.name)
      (Printf.sprintf "log/%s.err" pd.name)

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
    let p_start_time = start_time pid in
    let r_start_time = start_time redirect_pid in
    (* log "Forked child pid: %d" pid; *)
    ((pid, p_start_time), (redirect_pid, r_start_time));
