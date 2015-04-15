external _ptrace_seize : int -> int = "caml_ptrace_seize"
external _ptrace_cont  : int -> int -> int = "caml_ptrace_cont"

open Ctypes
open PosixTypes
open Foreign

module Ptrace = struct
  let seize = 0x4206 (* From ptrace.h *)
  let cont = 7
end

let ptrace = foreign "ptrace" (int @-> int @-> ptr void @-> ptr void @-> returning long)

let posix_sig_number = foreign "caml_convert_signal_number" (int @-> returning int)

let const_ptr_value i = Ctypes.ptr_of_raw_address (Nativeint.of_int i)
let ptrace_seize pid = ptrace Ptrace.seize pid null null |> Signed.Long.to_int
let ptrace_cont pid signal =
  let ptr_sig_no =
    signal |> posix_sig_number |> const_ptr_value
  in
  ptrace Ptrace.cont pid null ptr_sig_no |> Signed.Long.to_int
