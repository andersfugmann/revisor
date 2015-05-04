(** Listen for incomming requests from the client *)

type t = {
  ctx: ZMQ.Context.t;
  socket: ZMQ.Socket.t;
}

let init () =
  let ctx = ZMQ.Context.create () in
  (* Bind to the socket *)
  let s = ZMQ.Socket.create ctx ZMQ.Socket.rep in
  ZMQ.Socket.connect s Config.socket;

  { ctx; s }

exception Unknown_command of string
exception Wrong_arguments

let parse_command str =
  let parse = function

  let (cmd, arg) = match String.nsplit cmd with
    | [c; d] -> (c, Some d)
    | [c] -> (c, None)
    | _ -> raise (Unknown_command str)
  in
  (parse cmd) arg

let expand names =
  names |>
  List.map


let revc t state queue  =
  let msg = ZMQ.Socket.recv ~block:false t.socket in
  match String.nsplit " " msg with
  | "start" :: progs ->

    (function Some x -> Start x | None -> raise Wrong_arguments)
  | "stop"    -> (function Some x -> Stop x | None -> raise Wrong_arguments)
  | "restart" -> (function Some x -> Restart x | None -> raise Wrong_arguments)
  | "start"   -> (fun x -> Status (Option.default "all" x))
  | "reload"  -> (function None -> Reload | Some _ -> raise Wrong_arguments)
