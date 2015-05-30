(** Commands used by client and server *)

type pattern = string [@@deriving yojson]

type t =
  | Status of pattern list
  | Stop of pattern list
  | Start of pattern list
  | Restart of pattern list
  | Reload
      [@@deriving yojson]

type result = int * Revisor.t list
                [@@deriving yojson]


let parse = function
  | "status" :: args -> Status args
  | "stop" :: args -> Stop args
  | "start" :: args -> Start args
  | "restart" :: args -> Restart args
  | "reload" :: [] -> Reload
  | "reload" :: _ -> failwith "reload does not accept args"
  | cmd :: _ -> failwith ("unknown command: " ^ cmd)
  | [] -> failwith "No command given"

let match_name = function
  | [ x ] when String.lowercase x = "all" -> fun _ -> true
  | ptrns ->
    let patterns = List.map Str.regexp_case_fold ptrns in
    fun name ->
      List.exists (fun re ->
          Str.string_match re name 0 &&
          Str.matched_string name = name)
        patterns
