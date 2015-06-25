(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2015     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** Coq compiler : coqc *)

(** For improving portability, coqc is now an OCaml program instead
    of a shell script. We use as much as possible the Sys and Filename
    module for better portability, but the Unix module is still used
    here and there (with explicitly qualified function names Unix.foo).

    We process here the commmand line to extract names of files to compile,
    then we compile them one by one while passing by the rest of the command
    line to a process running "coqtop -batch -compile <file>".
*)

(* Environment *)

let environment = Unix.environment ()

let binary = ref "coqtop"
let image = ref ""

let verbose = ref false

let rec make_compilation_args = function
  | [] -> []
  | file :: fl ->
      let file_noext =
        if Filename.check_suffix file ".v" then
          Filename.chop_suffix file ".v"
        else file
      in
      (if !verbose then "-compile-verbose" else "-compile")
      :: file_noext :: (make_compilation_args fl)

(* compilation of files [files] with command [command] and args [args] *)

let compile command args files =
  let args' = command :: args @ (make_compilation_args files) in
  match Sys.os_type with
  | "Win32" ->
     let pid =
        Unix.create_process_env command (Array.of_list args') environment
        Unix.stdin Unix.stdout Unix.stderr
     in
     let status = snd (Unix.waitpid [] pid) in
     let errcode =
       match status with Unix.WEXITED c|Unix.WSTOPPED c|Unix.WSIGNALED c -> c
     in
     exit errcode
  | _ ->
     Unix.execvpe command (Array.of_list args') environment

let usage () =
  Usage.print_usage_coqc () ;
  flush stderr ;
  exit 1

(* parsing of the command line *)
let extra_arg_needed = ref true

let parse_args args =
  let files = ref [] in
  let rec parse params args =
    let open CArguments in
    let (x,params) =
      parse_args
	(fun s -> Pp.msg_warning (Pp.strbrk s))
	(fun s -> Pp.msg_error (Pp.strbrk s)) all
    in
    let new_params =
      List.rev (List.flatten (List.map param_to_args params)) @ args
    in
    match x with
    | Exec rem ->
      begin
	match rem with
	  [] -> rem
	| ("-verbose"|"--verbose") :: rem ->
	  verbose := true ; parse new_params rem
	| "-image" :: f :: rem -> image := f ; parse new_params rem
	| "-image" :: [] ->	usage ()
	| "-byte" :: rem -> binary := "coqtop.byte"; parse new_params rem
	| "-opt" :: rem -> binary := "coqtop"; parse new_params rem
	| "-libdir" :: _ :: rem ->
          print_string "Warning: option -libdir deprecated and ignored\n";
          flush stdout;
          parse new_params rem
	| ("-db"|"-debugger") :: rem ->
          print_string "Warning: option -db/-debugger deprecated and ignored\n";
          flush stdout;
          parse new_params rem
	| f :: rem ->
	  if Sys.file_exists f then
	    (files := f :: !files ;
	     parse new_params rem)
	  else
	    let fv = f ^ ".v" in
	    if Sys.file_exists fv then
	      (files := f :: !files ;
	       parse new_params rem)
	    else begin
	      prerr_endline ("coqc: "^f^": no such file or directory") ;
	      exit 1
	    end
      end
    | Error s ->
      begin
	prerr_endline ("Error: " ^ s) ;
	exit 1
      end
    | PrintModUid ls ->
      begin
	prerr_endline "coqc does not support -print-mod-uid" ;
	exit 1
      end
  in
  let args = parse [] args in
  (!files, args)

(* main: we parse the command line, define the command to compile files
 * and then call the compilation on each file *)


let main () =
  let cfiles, args = parse_args (List.tl (Array.to_list Sys.argv)) in
    if cfiles = [] && !extra_arg_needed then begin
      prerr_endline "coqc: too few arguments" ;
      usage ()
    end;
    let coqtopname =
      if !image <> "" then !image
      else Filename.concat Envars.coqbin (!binary ^ Coq_config.exec_extension)
    in
      (*  List.iter (compile coqtopname args) cfiles*)
      Unix.handle_unix_error (compile coqtopname args) cfiles

let _ = Printexc.print main ()
