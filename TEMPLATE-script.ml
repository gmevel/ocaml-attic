#!/bin/env ocaml

(*
 *  Script name
 *)

(* for a polyglot file:
 *

      #!/bin/sh
      type _' (*' 2>&-
          sh script here
      *)

 *
 *)

(* how to load libraries (here, zarith) *)

(* METHOD 1: pass them as arguments to `ocaml` in the shebang (use `env -S`) *)

(* METHOD 2: use a polyglot file, declared as a sh script, that calls `ocaml`
 * onitself with the right options:
 * https://stackoverflow.com/questions/7604284/multiline-shebang-in-ocaml/#28768867
 *)

(* METHOD 3: use OCaml toplevel directives (but editor linting yells, and the
 * source code cannot be compiled) *)

(* 1) the basic way (absolute path…): *)
#directory "/home/glen/.opam/system/lib/zarith" ;;
#load "zarith.cma" ;;
(* 2) or using topfind: *)
try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH") with Not_found -> () ;;
#use "topfind" ;;
#require "zarith" ;;

