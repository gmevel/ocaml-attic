(*
 *  OCaml code binding code from C
 *
 * see e·g· this short tutorial:
 *     http://www.linux-nantes.org/~fmonnier/OCaml/ocaml-wrapping-c.html
 * or draw inspiration from the source code of module Unix:
 *     https://github.com/ocaml/ocaml/tree/trunk/otherlibs/unix
 *
 * to compile:
 *     ocamlopt ctest.c unix.cmxa test.ml
 *)

external hello_world : int -> unit = "hello_world"

let () =
  hello_world 42

external _setsockopt_dontfrag : Unix.file_descr -> bool = "setsockopt_dontfrag"
external _get_errno : unit -> int = "get_errno"

let setsockopt_dontfrag sock =
  if not @@ _setsockopt_dontfrag sock then
    raise @@ Unix.Unix_error (Unix.EUNKNOWNERR (_get_errno ()), "setsockopt_dontfrag", "")

let () =
  let sock_ux = Unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
  begin try
    setsockopt_dontfrag sock_ux ; (* → should fail *)
    Printf.printf "set IP_DONTFRAG on UNIX socket: ok\n"
  with Unix.Unix_error (_, "setsockopt_dontfrag", _) ->
    Printf.printf "set IP_DONTFRAG on UNIX socket: fail\n"
  end ;
  let sock_ip = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  begin try
    setsockopt_dontfrag sock_ip ;
    Printf.printf "set IP_DONTFRAG on IPv4 socket: ok\n"
  with Unix.Unix_error (_, "setsockopt_dontfrag", _) ->
    Printf.printf "set IP_DONTFRAG on IPv4 socket: fail\n"
  end ;
  let sock_ip6 = Unix.socket Unix.PF_INET6 Unix.SOCK_DGRAM 0 in
  begin try
    setsockopt_dontfrag sock_ip6 ;
    Printf.printf "set IP_DONTFRAG on IPv6 socket: ok\n"
  with Unix.Unix_error (_, "setsockopt_dontfrag", _) ->
    Printf.printf "set IP_DONTFRAG on IPv6 socket: fail\n"
  end ;
  let file = Unix.stdin in
  begin try
    setsockopt_dontfrag file ;
    Printf.printf "set IP_DONTFRAG on regular file: ok\n"
  with Unix.Unix_error (_, "setsockopt_dontfrag", _) ->
    Printf.printf "set IP_DONTFRAG on regular file: fail\n"
  end
