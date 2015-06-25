(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2015     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Pp
open Errors
open Util
open System
open Flags
open Names
open Libnames
open States
open Coqinit

let () = at_exit flush_all

let ( / ) = Filename.concat

let fatal_error info anomaly =
  let msg = info ++ fnl () in
  pp_with ~pp_tag:Ppstyle.pp_tag !Pp_control.err_ft msg;
  flush_all ();
  exit (if anomaly then 129 else 1)

let get_version_date () =
  try
    let ch = open_in (Envars.coqlib () / "revision") in
    let ver = input_line ch in
    let rev = input_line ch in
    let () = close_in ch in
    (ver,rev)
  with e when Errors.noncritical e ->
    (Coq_config.version,Coq_config.date)

let print_header () =
  let (ver,rev) = get_version_date () in
  ppnl (str "Welcome to Coq " ++ str ver ++ str " (" ++ str rev ++ str ")");
  pp_flush ()

let warning s = msg_warning (strbrk s)

let toploop = ref None

let color : [`ON | `AUTO | `OFF] ref = ref `AUTO
let set_color = function
| "on" -> color := `ON
| "off" -> color := `OFF
| "auto" -> color := `AUTO
| _ -> prerr_endline ("Error: on/off/auto expected after option color"); exit 1

let init_color () =
  let has_color = match !color with
  | `OFF -> false
  | `ON -> true
  | `AUTO ->
    Terminal.has_style Unix.stdout &&
    Terminal.has_style Unix.stderr &&
    (* emacs compilation buffer does not support colors by default,
       its TERM variable is set to "dumb". *)
    Unix.getenv "TERM" <> "dumb"
  in
  if has_color then begin
    let colors = try Some (Sys.getenv "COQ_COLORS") with Not_found -> None in
    match colors with
    | None ->
      (** Default colors *)
      Ppstyle.init_color_output ()
    | Some "" ->
      (** No color output *)
      ()
    | Some s ->
      (** Overwrite all colors *)
      Ppstyle.clear_styles ();
      Ppstyle.parse_config s;
      Ppstyle.init_color_output ()
  end

let toploop_init = ref begin fun x ->
  let () = init_color () in
  let () = CoqworkmgrApi.(init !Flags.async_proofs_worker_priority) in
  x
  end

let toploop_run = ref (fun () ->
  if Dumpglob.dump () then begin
    if_verbose warning "Dumpglob cannot be used in interactive mode.";
    Dumpglob.noglob ()
  end;
  Coqloop.loop();
  (* Initialise and launch the Ocaml toplevel *)
  Coqinit.init_ocaml_path();
  Mltop.ocaml_toploop())

let output_context = ref false

let memory_stat = ref false

let print_memory_stat () =
  if !memory_stat then
    ppnl
      (str "total heap size = " ++ int (CObj.heap_size_kb ()) ++ str " kbytes")

let _ = at_exit print_memory_stat

let engagement = ref None
let set_engagement c = engagement := Some c
let engage () =
  match !engagement with Some c -> Global.set_engagement c | None -> ()

let type_in_type = ref false
let set_type_in_type () = type_in_type := true
let set_hierarchy () = if !type_in_type then Global.set_type_in_type ()

let set_batch_mode () = batch_mode := true

let user_warning = ref false
(** User explicitly set warning *)

let set_warning p =
  let () = user_warning := true in
  match p with
  | "all" -> make_warn true
  | "none" -> make_warn false
  | _ -> prerr_endline ("Error: all/none expected after option w"); exit 1

let toplevel_default_name = DirPath.make [Id.of_string "Top"]
let toplevel_name = ref (Some toplevel_default_name)
let set_toplevel_name dir =
  if DirPath.is_empty dir then error "Need a non empty toplevel module name";
  toplevel_name := Some dir
let unset_toplevel_name () = toplevel_name := None

let remove_top_ml () = Mltop.remove ()

let inputstate = ref ""
let set_inputstate s =
  let () = msg_warning (str "The inputstate option is deprecated and discouraged.") in
  inputstate:=s
let inputstate () = if not (String.is_empty !inputstate) then intern_state !inputstate

let outputstate = ref ""
let set_outputstate s =
  let () = msg_warning (str "The outputstate option is deprecated and discouraged.") in
  outputstate:=s
let outputstate () = if not (String.is_empty !outputstate) then extern_state !outputstate

let set_include d p implicit =
  let p = dirpath_of_string p in
  push_include d p implicit

let load_vernacular_list = ref ([] : (string * bool) list)
let add_load_vernacular verb s =
  load_vernacular_list := ((CUnix.make_suffix s ".v"),verb) :: !load_vernacular_list
let load_vernacular () =
  List.iter
    (fun (s,b) ->
      if Flags.do_beautify () then
	with_option beautify_file (Vernac.load_vernac b) s
      else
	Vernac.load_vernac b s)
    (List.rev !load_vernacular_list)

let load_vernacular_obj = ref ([] : string list)
let add_vernac_obj s = load_vernacular_obj := s :: !load_vernacular_obj
let load_vernac_obj () =
  List.iter (fun f -> Library.require_library_from_file None f None)
    (List.rev !load_vernacular_obj)

let require_prelude () =
  let vo = Envars.coqlib () / "theories/Init/Prelude.vo" in
  let vio = Envars.coqlib () / "theories/Init/Prelude.vio" in
  let m =
    if Sys.file_exists vo then vo else
    if Sys.file_exists vio then vio else vo in
  Library.require_library_from_dirpath [Coqlib.prelude_module,m] (Some true)

let require_list = ref ([] : string list)
let add_require s = require_list := s :: !require_list
let require () =
  if !load_init then silently require_prelude ();
  List.iter (fun s -> Library.require_library_from_file None s (Some false))
    (List.rev !require_list)

let compile_list = ref ([] : (bool * string) list)

let glob_opt = ref false

let add_compile verbose s =
  set_batch_mode ();
  Flags.make_silent true;
  if not !glob_opt then Dumpglob.dump_to_dotglob ();
  (** make the file name explicit; needed not to break up Coq loadpath stuff. *)
  let s =
    let open Filename in
    if is_implicit s
    then concat current_dir_name s
    else s
  in
  compile_list := (verbose,s) :: !compile_list

let compile_file (v,f) =
  if Flags.do_beautify () then
    with_option beautify_file (Vernac.compile v) f
  else
    Vernac.compile v f

let compile_files () =
  match !compile_list with
    | [] -> ()
    | [vf] -> compile_file vf (* One compilation : no need to save init state *)
    | l ->
      let init_state = States.freeze ~marshallable:`No in
      let coqdoc_init_state = Lexer.location_table () in
      List.iter
        (fun vf ->
	  States.unfreeze init_state;
	  Lexer.restore_location_table coqdoc_init_state;
          compile_file vf)
        (List.rev l)

(*s options for the virtual machine *)

let boxed_val = ref false
let use_vm = ref false

let set_vm_opt () =
  Vm.set_transp_values (not !boxed_val);
  Vconv.set_use_vm !use_vm

(** Options for proof general *)

let set_emacs () =
  Flags.print_emacs := true;
  Pp.make_pp_emacs ();
  Vernacentries.qed_display_script := false;
  color := `OFF

(** GC tweaking *)

(** Coq is a heavy user of persistent data structures and symbolic ASTs, so the
    minor heap is heavily sollicited. Unfortunately, the default size is far too
    small, so we enlarge it a lot (128 times larger).

    To better handle huge memory consumers, we also augment the default major
    heap increment and the GC pressure coefficient.
*)

let init_gc () =
  let param =
    try ignore (Sys.getenv "OCAMLRUNPARAM"); true
    with Not_found -> false
  in
  let control = Gc.get () in
  let tweaked_control = { control with
    Gc.minor_heap_size = 33554432; (** 4M *)
(*     Gc.major_heap_increment = 268435456; (** 32M *) *)
    Gc.space_overhead = 120;
  } in
  if param then ()
  else Gc.set tweaked_control

(*s Parsing of the command line.
    We no longer use [Arg.parse], in order to use share [Usage.print_usage]
    between coqtop and coqc. *)

let usage () =
  Envars.set_coqlib Errors.error;
  init_load_path ();
  if !batch_mode then Usage.print_usage_coqc ()
  else begin
    Mltop.load_ml_objects_raw_rex
      (Str.regexp (if Mltop.is_native then "^.*top.cmxs$" else "^.*top.cma$"));
    Usage.print_usage_coqtop ()
  end

let print_style_tags () =
  let () = init_color () in
  let tags = Ppstyle.dump () in
  let iter (t, st) =
    let st = match st with Some st -> st | None -> Terminal.make () in
    let opt =
      Terminal.eval st ^
      String.concat "." (Ppstyle.repr t) ^
      Terminal.reset ^ "\n"
    in
    print_string opt
  in
  let make (t, st) = match st with
  | None -> None
  | Some st ->
    let tags = List.map string_of_int (Terminal.repr st) in
    let t = String.concat "." (Ppstyle.repr t) in
    Some (t ^ "=" ^ String.concat ";" tags)
  in
  let repr = List.map_filter make tags in
  let () = Printf.printf "COQ_COLORS=\"%s\"\n" (String.concat ":" repr) in
  let () = List.iter iter tags in
  flush_all ()

let filter_opts = ref false
let exitcode () = if !filter_opts then 2 else 0

let verb_compat_ntn = ref false
let no_compat_ntn = ref false

let print_where = ref false
let print_config = ref false
let print_tags = ref false

let set_worker_id opt s =
  assert (s <> "master");
  Flags.async_proofs_worker_id := s

let vio_tasks = ref []

let add_vio_task f =
  set_batch_mode ();
  Flags.make_silent true;
  vio_tasks := f :: !vio_tasks

let check_vio_tasks () =
  let rc =
    List.fold_left (fun acc t -> Vio_checking.check_vio t && acc)
      true (List.rev !vio_tasks) in
  if not rc then exit 1

let vio_files = ref []
let vio_files_j = ref 0
let vio_checking = ref false
let add_vio_file f =
  set_batch_mode ();
  Flags.make_silent true;
  vio_files := f :: !vio_files

let set_vio_checking_j opt j =
  try vio_files_j := int_of_string j
  with Failure _ ->
    prerr_endline ("The first argument of " ^ opt ^ " must the number");
    prerr_endline "of concurrent workers to be used (a positive integer).";
    prerr_endline "Makefiles generated by coq_makefile should be called";
    prerr_endline "setting the J variable like in 'make vio2vo J=3'";
    exit 1

let schedule_vio_checking () =
  if !vio_files <> [] && !vio_checking then
    Vio_checking.schedule_vio_checking !vio_files_j !vio_files
let schedule_vio_compilation () =
  if !vio_files <> [] && not !vio_checking then
    Vio_checking.schedule_vio_compilation !vio_files_j !vio_files

let get_native_name s =
  (* We ignore even critical errors because this mode has to be super silent *)
  try
    String.concat "/" [Filename.dirname s;
      Nativelib.output_dir; Library.native_name_from_filename s]
  with _ -> ""

let to_channel = function
  | CArguments.Socket (host, port) -> Some (Spawned.Socket (host, port))
  | CArguments.Stdfds -> Some Spawned.AnonPipe

let process =
  let open CArguments in
  function
    I (path) -> push_ml_include path
  | Q (path, logical) -> set_include path logical false
  | R (path, logical) -> set_include path logical true
  | CheckVioTasks (tno, tfile) -> add_vio_task (tno, tfile)
  | ScheduleVioChecking (j,files) ->
    vio_checking := true ;
    vio_files_j := j ;
    List.iter add_vio_file files
  | ScheduleVio2Vo (j, files) ->
    vio_files_j := j ;
    List.iter add_vio_file files
  | Coqlib path -> Flags.coqlib_spec:=true; Flags.coqlib:=path
  | AsyncProofs mode -> Flags.async_proofs_mode := mode
  | AsyncProofsJ j -> Flags.async_proofs_n_workers := j
  | AsyncProofsCache c -> Flags.async_proofs_cache := Some c
  | AsyncProofsTacJ j -> Flags.async_proofs_n_tacworkers := j
  | AsyncProofsWorkerPriority p -> Flags.async_proofs_worker_priority := p
  | AsyncProofsPrivateFlags fs ->
    Flags.async_proofs_private_flags := Some fs
  | WorkerId id -> Flags.async_proofs_worker_id := id
  | Compat v -> Flags.compat_version := get_compat_version v
  | Compile (verbose, file) -> add_compile verbose file
  | DumpGlob f -> Dumpglob.dump_into_file f ; glob_opt := true
  | FeedbackGlob -> Dumpglob.feedback_glob ()
  | ExcludeDir path -> exclude_search_in_dirname path
  | InitFile f -> set_rcfile f
  | InputState f -> set_inputstate f
  | LoadMLObj path -> Mltop.dir_ml_load path
  | LoadMLSrc path -> Mltop.dir_ml_use path
  | LoadVernacObj path -> add_vernac_obj path
  | LoadVernacSrc (path, v) -> add_load_vernacular v path
  | OutputState path -> set_outputstate path
  | Require path -> add_require path
  | Top name -> set_toplevel_name (dirpath_of_string name)
  | WithGeoProof b -> Coq_config.with_geoproof := b
  | MainChannel chn -> Spawned.main_channel := to_channel chn
  | ControlChannel chn -> Spawned.control_channel := to_channel chn
  | Vio2Vo p -> add_compile false p; Flags.compilation_mode := Vio2Vo
  | Toploop name -> toploop := Some name
  | Warning level -> set_warning level
  | AsyncDelegate ->
    Flags.async_proofs_full := true;
  | AsyncProofsReopenBranch ->
    Flags.async_proofs_never_reopen_branch := true;
  | Batch -> set_batch_mode ()
  | TestMode -> test_mode := true
  | Beautify -> make_beautify true
  | Boot -> boot := true; no_load_rc ()
  | Backtrace -> Backtrace.record_backtrace true
  | Color c -> set_color c
  | Config -> print_config := true
  | Debug -> set_debug ()
  | Emacs -> set_emacs ()
  | FilterOps -> filter_opts := true
  | IdeSlave -> toploop := Some "coqidetop"; Flags.ide_slave := true
  | ImpredicativeSet -> set_engagement Declarations.ImpredicativeSet
  | IndicesMatter -> Indtypes.enforce_indices_matter ()
  | JustParsing -> Vernac.just_parsing := true
  | Memory -> memory_stat := true
  | NoInit -> load_init := false
  | NoCompatNotations -> no_compat_ntn := true
  | NoGlob -> Dumpglob.noglob (); glob_opt := true
  | NativeCompiler ->
    if Coq_config.no_native_compiler then
      warning "Native compilation was disabled at configure time."
    else native_compiler := true
  | NoTop -> unset_toplevel_name ()
  | OutputContext -> output_context := true
  | NoRc -> no_load_rc ()
  | Quiet -> Flags.make_silent true; Flags.make_warn false
  | BuildVio -> Flags.compilation_mode := Flags.BuildVio
  | ListTags -> print_tags := true
  | Time -> Flags.time := true
  | TypeInType -> set_type_in_type ()
  | Unicode -> add_require "Utf8_core"
  | VerboseCompatNotations -> verb_compat_ntn := true
  | VM -> use_vm := true
  | Help -> usage ()
  | Version -> Usage.version (exitcode ())
  | Where -> print_where := true

let parse_args arglist =
  try
    let open CArguments in
    let (action, params) =
      parse_args warning error arglist
    in
    List.iter process params ;
    match action with
    | PrintModUid rem ->
      let s = String.concat " " (List.map get_native_name rem) in
      print_endline s;
      exit 0
    | Exec rem -> rem
    | Error err -> assert false (* fatal_error (Errors.print (Errors.UserError(err,"")) false *)
  with
    UserError(_, s) as e ->
      if Pp.is_empty s then exit 1
      else fatal_error (Errors.print e) false
  | any -> fatal_error (Errors.print any) (Errors.is_anomaly any)

let init arglist =
  init_gc ();
  Sys.catch_break false; (* Ctrl-C is fatal during the initialisation *)
  Lib.init();
  (* Default Proofb Mode starts with an alternative default. *)
  Goptions.set_string_option_value ["Default";"Proof";"Mode"] "Classic";
  begin
    try
      let extras = parse_args arglist in
      (* If we have been spawned by the Spawn module, this has to be done
       * early since the master waits us to connect back *)
      Spawned.init_channels ();
      Envars.set_coqlib Errors.error;
      if !print_where then (print_endline(Envars.coqlib ()); exit(exitcode ()));
      if !print_config then (Usage.print_config (); exit (exitcode ()));
      if !print_tags then (print_style_tags (); exit (exitcode ()));
      if !filter_opts then (print_string (String.concat "\n" extras); exit 0);
      init_load_path ();
      Option.iter Mltop.load_ml_object_raw !toploop;
      let extras = !toploop_init extras in
      if not (List.is_empty extras) then begin
        prerr_endline ("Don't know what to do with "^String.concat " " extras);
        prerr_endline "See -help for the list of supported options";
        exit 1
      end;
      if_verbose print_header ();
      inputstate ();
      Mltop.init_known_plugins ();
      set_vm_opt ();
      engage ();
      set_hierarchy ();
      (* Be careful to set these variables after the inputstate *)
      Syntax_def.set_verbose_compat_notations !verb_compat_ntn;
      Syntax_def.set_compat_notations (not !no_compat_ntn);
      if (not !batch_mode || List.is_empty !compile_list)
         && Global.env_is_initial ()
      then Option.iter Declaremods.start_library !toplevel_name;
      init_library_roots ();
      load_vernac_obj ();
      require ();
      Stm.init ();
      load_rcfile();
      load_vernacular ();
      compile_files ();
      schedule_vio_checking ();
      schedule_vio_compilation ();
      check_vio_tasks ();
      outputstate ()
    with any ->
      let any = Errors.push any in
      flush_all();
      let msg =
        if !batch_mode then mt ()
        else str "Error during initialization:" ++ fnl ()
      in
      let is_anomaly e = Errors.is_anomaly e || not (Errors.handled e) in
      fatal_error (msg ++ Coqloop.print_toplevel_error any) (is_anomaly (fst any))
  end;
  if !batch_mode then begin
    flush_all();
    if !output_context then
      Pp.ppnl (with_option raw_print Prettyp.print_full_pure_context ());
    Profile.print_profile ();
    exit 0
  end

let init_toplevel = init

let start () =
  let () = init_toplevel (List.tl (Array.to_list Sys.argv)) in
  (* In batch mode, Coqtop has already exited at this point. In interactive one,
     dump glob is nothing but garbage ...  *)
  if not !user_warning then make_warn true;
  !toploop_run ();
  exit 1

(* [Coqtop.start] will be called by the code produced by coqmktop *)
