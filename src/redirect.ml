open Batteries

let open_file filename =
  Unix.(openfile filename [O_WRONLY; O_APPEND; O_CREAT] 0o644)


(* We cannot write a complete line. *)
let rec write_ch bol ch =
  let open Unix in
  let str = match bol with
    | true ->
      let now = gettimeofday () in
      let ts = localtime now in
      let millis = (Pervasives.truncate (now *. 1000.0)) mod 1000 in
      Printf.sprintf "[%04d-%02d-%02d %02d:%02d:%02d.%03d] %c"
        (ts.tm_year+1900) (ts.tm_mon+1) ts.tm_mday ts.tm_hour ts.tm_min ts.tm_sec millis ch
    | false ->
      String.of_char ch
  in
  let bol = ch = '\n' in
  (str, bol)

let write_bytes (fd, bol) bytes =
  let buffer = Buffer.create (Bytes.length bytes + 27) in

  let (buffer, bol) =
    String.fold_left
      (fun (acc, state) ch ->
         let (s, state) = write_ch state ch in
         Buffer.add_string acc s; (acc, state))
      (buffer, bol) (Bytes.to_string bytes)
  in
  (* Ok. Lets actually write it *)
  Unix.write fd (Buffer.to_bytes buffer) 0 (Buffer.length buffer) |> ignore;
  (fd, bol)

let rec read in_fd out =
  let buffer = Bytes.create 256 in
  match Unix.read in_fd buffer 0 256 with
  | n when n > 0 ->
    let b = Bytes.extend buffer 0 (n - 256) in
    let out = write_bytes out b in
    read in_fd out
  | _ -> out
  | exception _ -> out

let run data =
  let out_fds =
    ref (List.map (fun (_, filename) -> (filename, open_file filename, true)) data)
  in
  let in_fds = List.map (fun (in_fd, _) ->
      Unix.set_nonblock in_fd; in_fd) data
  in
  let rotate = ref false in

  Sys.set_signal Sys.sighup
    (Sys.Signal_handle
       (fun _ ->
          List.iter (fun (_, fd, _) ->
              write_bytes (fd, false) "\nSighup retrieved. Rotating.\n" |> ignore) !out_fds;
          rotate := true));

  while true do
    (* Use select to read from fd's *)
    Unix.select in_fds [] [] (-1.0) |> ignore;

    out_fds := List.map2
        (fun in_fd (fn, out_fd, s) ->
           let (_, s) = read in_fd (out_fd, s) in
           (fn, out_fd, s)
        )
        in_fds !out_fds;

    if !rotate then begin
        out_fds := List.map
            (fun (fn, _, _) ->
               let out_fd = (open_file fn) in
               write_bytes (out_fd, true) "Log file rotated\n" |> ignore;
               (fn, out_fd, true)
            ) !out_fds;
        rotate := false;
    end
  done

let () =
  let rec read_data n =
    match (n < Array.length Sys.argv) with
    | true ->
      let fd = Sys.argv.(n) |> int_of_string |> ExtUnixAll.file_descr_of_int in
      let filename = Sys.argv.(n+1) in
      (fd, filename) :: read_data (n+2)
    | false -> []
  in
  run (read_data 1)
