open Batteries

(**
   This file really needs a rewrite.
   It would read all bytes from its file descrs, and
   write byte for byte.

   A global state indicates if a newline has just been encounterd
*)
type t = {
  fd_in: Unix.file_descr;
  filename: string;
  mutable fd_out: Unix.file_descr;
  mutable new_line: bool;
}

let open_file filename =
  Unix.(openfile filename [O_WRONLY; O_APPEND; O_CREAT] 0o644)

(* We cannot write a complete line. *)
let rec write_char t ch =
  let open Unix in
  let str = match t.new_line with
    | true ->
      let now = gettimeofday () in
      let ts = localtime now in
      let millis = (Pervasives.truncate (now *. 1000.0)) mod 1000 in
      Printf.sprintf "[%04d-%02d-%02d %02d:%02d:%02d.%03d] %c"
        (ts.tm_year+1900) (ts.tm_mon+1)
        ts.tm_mday ts.tm_hour ts.tm_min ts.tm_sec millis
        ch
    | false ->
      String.of_char ch
  in
  t.new_line <- ch = '\n';
  str

let write_bytes t bytes =
  let buffer = Buffer.create (Bytes.length bytes + 27) in

  Bytes.iter (fun ch -> Buffer.add_string buffer (write_char t ch)) bytes;
  Unix.write t.fd_out (Buffer.to_bytes buffer) 0 (Buffer.length buffer) |> ignore


let read_size = 256
let rec read t =
  let buffer = Bytes.create read_size in
  match Unix.read t.fd_in buffer 0 read_size with
  | n when n > 0 ->
    (* Cut away bytes not read yet *)
    let b = Bytes.extend buffer 0 (n - read_size) in
    write_bytes t b
    (* No - Lets see if select will call us again. read in_fd out *)
  | _ -> exit 0

let rotate = ref false

let handle_sighub t _ =
  List.iter (fun t -> write_bytes t "Sighup retrieved. Rotating.\n") t;
  rotate := true

let run t =
  (* List.iter (fun t -> Unix.set_nonblock t.fd_in) t; *)
  Sys.set_signal Sys.sighup
    (Sys.Signal_handle (handle_sighub t));

  let in_fds = List.map (fun t -> t.fd_in) t in
  while true do
    (* Use select to read from fd's *)
    try
      let (fd_rdy, _, _) = Unix.select in_fds [] [] (-1.0) in
      (* Read from all t's in ins *)
      t
      |> List.filter (fun t -> List.mem t.fd_in fd_rdy)
      |> List.iter read;
    with
    | Unix.Unix_error(Unix.EINTR, _, _) -> ();

    (* Test if we need to rorate *)
    if !rotate then
      begin
        List.iter (fun t ->
            Unix.close t.fd_out;
            t.fd_out <- open_file t.filename;
            t.new_line <- true;
            write_bytes t "Log file rotated\n") t;

        rotate := false
      end
  done

let () =
  let rec read_data n =
    match (n < Array.length Sys.argv) with
    | true ->
      let fd_in = Sys.argv.(n) |> int_of_string |> ExtUnixAll.file_descr_of_int in
      let filename = Sys.argv.(n+1) in
      let fd_out = open_file filename in
      { fd_in; filename; fd_out; new_line = true } :: read_data (n + 2)
    | false -> []
  in
  run (read_data 1)
