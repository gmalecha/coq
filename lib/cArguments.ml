type pipe =
  Stdfds
| Socket of string * int

type param =
| I of string
| Q of string * string
| R of string * string
| Require of string
| InitFile of string
| InputState of string
| CheckVioTasks of int list * string
| ScheduleVioChecking of int * string list
| ScheduleVio2Vo of int * string list
| Coqlib of string
| AsyncProofs of Flags.async_proofs
| AsyncProofsJ of int
| AsyncProofsCache of Flags.cache
| AsyncProofsTacJ of int
| AsyncProofsWorkerPriority of Flags.priority
| AsyncProofsPrivateFlags of string
| WorkerId of string
| Compat of string
| Compile of bool (* verbose *) * string
| DumpGlob of string
| FeedbackGlob
| ExcludeDir of string
| LoadMLObj of string
| LoadMLSrc of string
| LoadVernacObj of string
| LoadVernacSrc of string * bool
| OutputState of string
| Top of string
| WithGeoProof of bool
| MainChannel of pipe
| ControlChannel of pipe
| Vio2Vo of string
| Toploop of string
| Warning of string
| AsyncDelegate
| AsyncProofsReopenBranch
| Batch
| TestMode
| Beautify
| Boot
| Backtrace
| Color of string
| Debug
| Emacs
| FilterOps
| IdeSlave
| ImpredicativeSet
| IndicesMatter
| JustParsing
| Memory
| NoInit
| NoCompatNotations
| NoGlob
| NativeCompiler
| NoTop
| OutputContext
| NoRc
| Quiet
| ListTags
| Time
| TypeInType
| Unicode
| VerboseCompatNotations
| VM
| BuildVio
| Config
| Where
| Version
| Help

type command =
| PrintModUid of string list
| Error of string
| Exec of string list

let get_priority opt s =
  try Flags.priority_of_string s
  with Invalid_argument _ ->
    prerr_endline ("Error: low/high expected after "^opt); exit 1

let get_async_proofs_mode opt = function
  | "off" -> Flags.APoff
  | "on" -> Flags.APon
  | "lazy" -> Flags.APonLazy
  | _ -> prerr_endline ("Error: on/off/lazy expected after "^opt); exit 1

let get_cache opt = function
  | "force" -> Some Flags.Force
  | _ -> prerr_endline ("Error: force expected after "^opt); exit 1

let error_missing_arg s =
  prerr_endline ("Error: extra argument expected after option "^s);
  prerr_endline "See -help for the syntax of supported options";
  exit 1

let get_host_port opt s =
  match CString.split ':' s with
  | [host; port] -> Socket (host, int_of_string port)
  | ["stdfds"] -> Stdfds
  | _ ->
     prerr_endline ("Error: host:port or stdfds expected after option "^opt);
     exit 1

let get_bool opt = function
  | "yes" -> true
  | "no" -> false
  | _ -> prerr_endline ("Error: yes/no expected after option "^opt); exit 1

let get_int opt n =
  try int_of_string n
  with Failure _ ->
    prerr_endline ("Error: integer expected after option "^opt); exit 1

let get_priority opt s =
  try Flags.priority_of_string s
  with Invalid_argument _ ->
    prerr_endline ("Error: low/high expected after "^opt); exit 1

let get_task_list s = List.map int_of_string (Str.split (Str.regexp ",") s)

let is_not_dash_option = function
  | Some f when String.length f > 0 && f.[0] <> '-' -> true
  | _ -> false

let parse_args warning error arglist =
  let args = ref arglist in
  let params = ref [] in
  let return x = (x, List.rev !params) in
  let rec parse () = match !args with
  | [] -> return (Exec [])
  | opt :: rem ->
    args := rem;
    let next () = match !args with
      | x::rem -> args := rem; x
      | [] -> error_missing_arg opt
    in
    let next_parse f = f opt (next ()) in
    let peek_next () = match !args with
      | x::_ -> Some x
      | [] -> None
    in
    let push (x : param) =
      params := x :: !params ;
      parse ()
    in

    begin match opt with

    (* Complex options with many args *)
    |"-I"|"-include" ->
      push (I (next ()))
    |"-Q" ->
      let path = next () in
      let logical = next () in
      push (Q(path, logical))
    |"-R" ->
      let path = next () in
      let logical =
	match next () with
	| "-as" -> next ()
	| x -> x
      in
      push (R (path, logical))

    (* Options with two arg *)
    |"-check-vio-tasks" ->
      let tno = get_task_list (next ()) in
      let tfile = next () in
      push (CheckVioTasks (tno,tfile))
    |"-schedule-vio-checking" ->
      let j = next_parse get_int in
      let vios = ref [next ()] in
      while is_not_dash_option (peek_next ()) do
	vios := next() :: !vios
      done ;
      push (ScheduleVioChecking (j, !vios))
    |"-schedule-vio2vo" ->
      let j = next_parse get_int in
      let vos = ref [next ()] in
      while is_not_dash_option (peek_next ()) do
	vos := next() :: !vos
      done ;
      push (ScheduleVio2Vo (j, !vos))

    (* Options with one arg *)
    |"-coqlib" -> push (Coqlib (next ()))
    |"-async-proofs" -> push (AsyncProofs (get_async_proofs_mode opt (next())))
    |"-async-proofs-j" -> push (AsyncProofsJ (next_parse get_int))
    |"-async-proofs-cache" ->
      if next () = "force" then
	push (AsyncProofsCache Flags.Force)
      else
	return (Error ("Error: 'force' expected after " ^ opt))
    |"-async-proofs-tac-j" -> push (AsyncProofsTacJ (next_parse get_int))
    |"-async-proofs-worker-priority" ->
      push (AsyncProofsWorkerPriority (get_priority opt (next ())))
    |"-async-proofs-private-flags" ->
      push (AsyncProofsPrivateFlags (next ()))
    |"-worker-id" -> push (WorkerId (next ()))
    |"-compat" -> push (Compat (next ()))
    |"-compile" -> push (Compile (false, next ()))
    |"-compile-verbose" -> push (Compile (true, next ()))
    |"-dump-glob" -> push (DumpGlob (next ()))
    |"-feedback-glob" -> push FeedbackGlob
    |"-exclude-dir" -> push (ExcludeDir (next ()))
    |"-init-file" -> push (InitFile (next ()))
    |"-inputstate"|"-is" -> push (InputState (next ()))
    |"-load-ml-object" -> push (LoadMLObj (next ()))
    |"-load-ml-source" -> push (LoadMLSrc (next ()))
    |"-load-vernac-object" -> push (LoadVernacObj (next ()))
    |"-load-vernac-source"|"-l" -> push (LoadVernacSrc(next (), false))
    |"-load-vernac-source-verbose"|"-lv" -> push (LoadVernacSrc(next (), true))
    |"-outputstate" -> push (OutputState (next ()))
    |"-print-mod-uid" -> return (PrintModUid rem)
    |"-require" -> push (Require (next ()))
    |"-top" -> push (Top (next ()))
    |"-with-geoproof" -> push (WithGeoProof (next_parse get_bool))
    |"-main-channel" -> push (MainChannel (get_host_port opt (next())))
    |"-control-channel" -> push (ControlChannel (get_host_port opt (next())))
    |"-vio2vo" -> push (Vio2Vo (next ()))
    |"-toploop" -> push (Toploop (next ()))
    |"-w" -> push (Warning (next ()))

    (* Options with zero arg *)
    |"-async-queries-always-delegate"
    |"-async-proofs-always-delegate"
    |"-async-proofs-full" ->
      push AsyncDelegate
    |"-async-proofs-never-reopen-branch" ->
      push AsyncProofsReopenBranch
    |"-batch" -> push Batch
    |"-test-mode" -> push TestMode
    |"-beautify" -> push Beautify
    |"-boot" -> push Boot
    |"-bt" -> push Backtrace
    |"-color" -> push (Color (next ()))
    |"-config"|"--config" -> push Config
    |"-debug" -> push Debug
    |"-emacs" -> push Emacs
    |"-filteropts" -> push FilterOps
    |"-h"|"-H"|"-?"|"-help"|"--help" -> push Help
    |"-ideslave" -> push IdeSlave
    |"-impredicative-set" -> push ImpredicativeSet
    |"-indices-matter" -> push IndicesMatter
    |"-just-parsing" -> push JustParsing
    |"-m"|"--memory" -> push Memory
    |"-noinit"|"-nois" -> push NoInit
    |"-no-compat-notations" -> push NoCompatNotations
    |"-no-glob"|"-noglob" -> push NoGlob
    |"-native-compiler" -> push NativeCompiler
    |"-notop" -> push NoTop
    |"-output-context" -> push OutputContext
    |"-q" -> push NoRc
    |"-quiet"|"-silent" -> push Quiet
    |"-quick" -> push BuildVio
    |"-list-tags" -> push ListTags
    |"-time" -> push Time
    |"-type-in-type" -> push TypeInType
    |"-unicode" -> push Unicode
    |"-v"|"--version" -> push Version
    |"-verbose-compat-notations" -> push VerboseCompatNotations
    |"-vm" -> push VM
    |"-where" -> push Where

    (* Deprecated options *)
    |"-byte" -> warning "option -byte deprecated, call with .byte suffix" ; parse ()
    |"-opt" -> warning "option -opt deprecated, call with .opt suffix" ; parse ()
    |"-full" -> warning "option -full deprecated" ; parse ()
    |"-notactics" -> warning "Obsolete option \"-notactics\"."; parse ()
    |"-emacs-U" ->
      warning "Obsolete option \"-emacs-U\", use -emacs instead.";
      push Emacs
    |"-v7" -> error "This version of Coq does not support v7 syntax" ; parse ()
    |"-v8" -> warning "Obsolete option \"-v8\"." ; parse ()
    |"-lazy-load-proofs" -> warning "Obsolete option \"-lazy-load-proofs\"." ; parse ()
    |"-dont-load-proofs" -> warning "Obsolete option \"-dont-load-proofs\"." ; parse ()
    |"-force-load-proofs" -> warning "Obsolete option \"-force-load-proofs\"." ; parse ()
    |"-unsafe" -> warning "Obsolete option \"-unsafe\"."; ignore (next ()) ; parse ()
    |"-quality" -> warning "Obsolete option \"-quality\"." ; parse ()
    |"-xml" -> warning "Obsolete option \"-xml\"." ; parse ()

    (* Unknown option *)
    | s -> return (Exec (opt :: rem))
    end
  in
  parse ()

let string_of_pipe = function
    Stdfds -> "stdfds"
  | Socket (h,p) -> h ^ ":" ^ string_of_int p

let param_to_args = function
  | I path -> ["-I"; path]
  | Q (p,l) -> ["-Q"; p; l]
  | R (p,l) -> ["-R"; p; l]
  | Require p -> ["-require"; p]
  | InitFile p -> ["-init-file"; p]
  | InputState p -> ["-is"; p]
  | CheckVioTasks (l,s) ->
    ["-check-vio-tasks"; String.concat "," (List.map string_of_int l); s]
  | ScheduleVioChecking (j,files) ->
    ["-schedule-vio-checking"; string_of_int j; String.concat " " files]
  | ScheduleVio2Vo (j,files) ->
    ["-schedule-vio2vo"; string_of_int j] @ files
  | Coqlib p ->
    ["-coqlib"; p]
  | AsyncProofs f ->
    ["-async-proofs";
      match f with
      | Flags.APoff -> "off"
      | Flags.APon -> "on"
      | Flags.APonLazy -> "lazy"]
  | AsyncProofsJ j ->
    ["-async-proofs-j"; string_of_int j]
  | AsyncProofsCache f ->
    ["-async-proofs-cache"; match f with Flags.Force -> "force"]
  | AsyncProofsTacJ j ->
    ["-async-proofs-tac-j"; string_of_int j]
  | AsyncProofsWorkerPriority p ->
    ["-async-proofs-worker-priority"; Flags.string_of_priority p]
  | AsyncProofsPrivateFlags f ->
    ["-async-proofs-private-flags"; f]
  | WorkerId s ->
    ["-worker-id"; s]
  | Compat s ->
    ["-compat"; s]
  | Compile (v,f) ->
    if v then
      ["-compile-verbose"; f]
    else
      ["-compile"; f]
  | DumpGlob s ->
    ["-dump-glob"; s]
  | FeedbackGlob ->
    ["-feedback-glob"]
  | ExcludeDir s ->
    ["-exclude-dir"; s]
  | LoadMLObj s ->
    ["-load-ml-object"; s]
  | LoadMLSrc s ->
    ["-load-ml-source"; s]
  | LoadVernacObj s ->
    ["-load-vernac-obj"; s]
  | LoadVernacSrc (s,v) ->
    if v then
      ["-load-vernac-source-verbose"; s]
    else
      ["-load-vernac-source"; s]
  | OutputState s ->
    ["-outputstate"; s]
  | Top s ->
    ["-top"; s]
  | WithGeoProof b ->
    ["-with-geoproof"; string_of_bool b]
  | MainChannel p -> ["-main-channel"; string_of_pipe p]
  | ControlChannel p -> ["-control-channel"; string_of_pipe p]
  | Vio2Vo f -> ["-vi2vo"; f]
  | Toploop s -> ["-toploop"; s]
  | Warning s -> ["-w"; s]
  | AsyncDelegate ->
    ["-async-proofs-always-delegate"]
  | AsyncProofsReopenBranch ->
    ["-async-proofs-never-reopen-branch"]
  | Batch -> ["-batch"]
  | TestMode -> ["-test-mode"]
  | Beautify -> ["-beautify"]
  | Boot -> ["-boot"]
  | Backtrace -> ["-bt"]
  | Color s -> ["-color"; s]
  | Debug -> ["-debug"]
  | Emacs -> ["-emacs"]
  | FilterOps -> ["-filterops"]
  | IdeSlave -> ["-ideslave"]
  | ImpredicativeSet -> ["-impredicative-set"]
  | IndicesMatter -> ["-indices-matter"]
  | JustParsing -> ["-just-parsing"]
  | Memory -> ["-m"]
  | NoInit -> ["-noinit"]
  | NoCompatNotations -> ["-no-compat-notations"]
  | NoGlob -> ["-no-glob"]
  | NativeCompiler -> ["-native-compiler"]
  | NoTop -> ["-notop"]
  | OutputContext -> ["-output-context"]
  | NoRc -> ["-q"]
  | Quiet -> ["-quiet"]
  | ListTags -> ["-list-tags"]
  | Time -> ["-time"]
  | TypeInType -> ["-type-in-type"]
  | Unicode -> ["-unicode"]
  | VerboseCompatNotations -> ["-verbose-compat-notations"]
  | VM -> ["-vm"]
  | BuildVio -> ["-quick"]
  | Config -> ["-config"]
  | Where -> ["-where"]
  | Version -> ["-version"]
  | Help -> ["-help"]
