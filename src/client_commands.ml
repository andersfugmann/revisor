(** Commands send from the client to the server.

    Commands are send marshalled through a unix socket.

    Response is a string printed to stdout.

*)
open Batteries

let endpoint = "ipc://var/revisor.sock"

type process = Name of string
             | All

(* Use typerep to serialize names *)
type t =
  | Status of process
  | Stop of process
  | Start of process
  | Restart of process
  | State
  | Reload


type request = string [@@deriving yojson]
type reply = (int * string) [@@deriving yojson]
