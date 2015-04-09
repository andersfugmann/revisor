#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

CAMLprim value caml_ptrace_seize(value pid) {
    CAMLparam1(pid);
    int res = ptrace(PTRACE_SEIZE, Int_val(pid), NULL, NULL);
    CAMLreturn(Val_int(res));
}

CAMLprim value caml_ptrace_cont(value pid, value sig) {
    CAMLparam2(pid, sig);
    int sig_no = caml_convert_signal_number(Int_val(sig));
    int res = ptrace(PTRACE_CONT, Int_val(pid), NULL, sig_no);
    CAMLreturn(Val_int(res));
}
