open Batteries

let open_file filename =
  Unix.(openfile filename [O_WRONLY; O_APPEND; O_CREAT] 0o644)

let write fd line =
  let open Unix in
  let now = gettimeofday () in
  let ts = localtime now in
  let millis = (Pervasives.truncate (now *. 1000.0)) mod 1000 in
  let str = Printf.sprintf "[%04d-%02d-%02d %02d:%02d:%02d.%03d] %s\n"
      (ts.tm_year+1900) (ts.tm_mon+1) ts.tm_mday ts.tm_hour ts.tm_min ts.tm_sec millis
      line
  in
  write fd str 0 (String.length str) |> ignore

let run in_fd filename =
  let fd = ref (open_file filename) in
  let rotate = ref false in
  let ch_in = Unix.in_channel_of_descr in_fd in

  Sys.set_signal Sys.sighup
    (Sys.Signal_handle
       (fun _ -> write !fd "Sighup retrieved. Rotating."; rotate := true));

  while true do
    let line = input_line ch_in in
    begin
      match !rotate with
      | true ->
        Unix.close !fd;
        fd := (open_file filename);
        rotate := false;
        write !fd "Log file rotated"
      | false -> ()
    end;
    write !fd line
  done
