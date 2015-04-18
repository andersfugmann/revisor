open Batteries
(** Client program.
    Send off a command to the server and wait for the reply.
    - Consider just sending all the arguments from the client program over the socket and let
    the server handle decoding (interpreting) and the the reply is just a string on stdout.
    The result should contain an exit status:
    0: Everything ok. Command accepted
    1: Error while executing the command - i.e. name not found
    2: Bad command
*)

(* Commands are sent as json *)

let (|+) a b = a |> (tap b)

type options = {
  echo_commands: bool;
  read_from_stdin: bool;
  ignore_errors: bool;
}

let parse_args () =
  let open BatOptParse in
  let echo_opt = StdOpt.store_true ()
  and stdin_opt = StdOpt.store_true ()
  and ignore_opt = StdOpt.store_true () in

  let parser =
    OptParser.make
      ~usage:"Send commands to a running revisor process"
      ()
  in

  OptParser.add parser
    ~help:"Echo commands as they are sent to the server"
    ~short_name:'e'
    ~long_name:"echo"
    echo_opt;


  OptParser.add parser
    ~help:"Read list of commands from stdin"
    ~short_name:'s'
    ~long_name:"stdin"
    stdin_opt;

  OptParser.add parser
    ~help:"Send all commands to server process even if one has a non-zero exit status"
    ~short_name:'i'
    ~long_name:"ignore_error"
    ignore_opt;

  OptParser.parse_argv parser |> ignore;
  (* Read and return the values. *)
  {
    echo_commands = echo_opt.Opt.option_get () |> Option.get;
    read_from_stdin = stdin_opt.Opt.option_get () |> Option.get;
    ignore_errors = ignore_opt.Opt.option_get () |> Option.get;
  }

let commands opts =
  match opts.read_from_stdin with
  | true -> IO.lines_of IO.stdin
  | false ->
    Sys.argv
    |> Array.to_list
    |> List.tl (* Throw away argv.(0) *)
    |> List.filter (fun s -> s.[0] != '-')
    |> String.concat " "
    |> Enum.singleton

let may pred f =
  match pred with
  | true -> f
  | false -> ignore

let send_command opts socket command =
  command
  |> tap (may opts.echo_commands print_endline)
  |> Client_commands.request_to_yojson
  |> Yojson.Safe.to_string
  |> ZMQ.Socket.send socket;

  (* Receieve the reply *)
  ZMQ.Socket.recv socket
  |> Yojson.Safe.from_string
  |> Client_commands.reply_of_yojson
  |> (function (`Ok v) -> v
            | `Error s -> failwith s)


let () =
  let opts = parse_args () in

  let ctx = ZMQ.Context.create() in
  let socket =
    let s = ZMQ.Socket.create ctx ZMQ.Socket.req in
    ZMQ.Socket.connect s Client_commands.endpoint;
    s
  in
  (* Create an enumeration of commands *)
  commands opts
  |> Enum.map (send_command opts socket)
  |> Enum.map (fun (c, v) -> print_endline v; c)
  |> Enum.reduce max
  |> exit
