open Batteries

let send_command socket command =
  command
  |> Commands.to_yojson
  |> Yojson.Safe.to_string
  |> ZMQ.Socket.send socket;

  (* Receieve the reply *)
  ZMQ.Socket.recv socket
  |> Yojson.Safe.from_string
  |> Commands.result_of_yojson
  |> (function `Ok v -> v
             | `Error s -> failwith s)

let string_of_proc p =
  Revisor.to_yojson p
  |> Yojson.Safe.to_string

let () =
  let ctx = ZMQ.Context.create() in
  let socket =
    let s = ZMQ.Socket.create ctx ZMQ.Socket.req in
    ZMQ.Socket.connect s Config.endpoint;
    s
  in
  let (code, ps) =
    Sys.argv |> Array.to_list |> List.tl |> Commands.parse
    |> send_command socket
  in
  ps
  |> List.map string_of_proc
  |> List.iter print_endline;
  exit code;
