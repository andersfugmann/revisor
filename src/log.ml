let log fmt =
  Printf.ksprintf
    (fun s ->
       try Unix.write Unix.stderr s 0 (String.length s) |> ignore
       with _ -> ())
    (fmt ^^ "\n")
