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

val parse_args :
  ((* warning : *) string -> unit) ->
  ((* error : *) string -> unit) ->
  string list -> command * param list

val param_to_args : param -> string list
