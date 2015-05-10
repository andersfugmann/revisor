external ptrace_seize  : int -> int = "caml_ptrace_seize"
external ptrace_detach : int -> int = "caml_ptrace_detach"
external ptrace_cont   : int -> int -> int = "caml_ptrace_cont"
