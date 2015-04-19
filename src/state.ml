open Batteries

(* The target state could hold information about restarts etc. *)
type pid = int
type ts = int

type current_state = Running of pid * pid list
                   | Stopping of pid option * pid list * ts
                   (* | Starting of ts (* This state is used if we need to monitor a daemon process *) *)
                   | Stopped

type state = {
  ts: int; (* Time in the state (ms) *)
  state: current_state;
}

let now () = Unix.gettimeofday () *. 1000.0 |> truncate

let init () = Hashtbl.create 0

let name_of_state = function
  | Running _ -> "Running"
  | Stopping _ -> "Stopping"
  | Stopped -> "Stopped"

let state_eq = function
  | Running _  -> (function Running _  -> true | _ -> false)
  | Stopping _ -> (function Stopping _ -> true | _ -> false)
  | Stopped    -> (function Stopped    -> true | _ -> false)

let update_state t name new_state =
  let now = now () in
  let state = match Hashtbl.Exceptionless.find t name with
    | Some state ->
      begin
        match state_eq new_state state.state with
        | true -> state
        | false ->
          Log.log "%s: %s -> %s after %d msec"
            name
            (name_of_state state.state)
            (name_of_state new_state)
            (now - state.ts);
          { state with ts = now }
      end
    | None -> { ts = now; state = Stopped }
  in
  let state = { state with state = new_state } in
  Hashtbl.replace t name state

let state t name =
  (** Assume non existent names to be stopped *)
  match Hashtbl.Exceptionless.find t name with
  | Some s -> s.state
  | None -> Stopped
