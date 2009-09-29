(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, * CNRS-Ecole Polytechnique-INRIA Futurs-Universite Paris Sud *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*i $Id$ i*)

open Names
open Term
open Environ
open Evd

val short_version : unit -> string
val version : unit -> string

type printing_state = {
  mutable printing_implicit : bool;
  mutable printing_coercions : bool;
  mutable printing_raw_matching : bool;
  mutable printing_no_notation : bool;
  mutable printing_all : bool;
  mutable printing_evar_instances : bool;
  mutable printing_universes : bool;
  mutable printing_full_all : bool
}

val printing_state : printing_state

type bktk_info = { start : GText.mark;
                   stop : GText.mark;
                   state_num : int;
                   pending_proofs : string list;
                   proof_stack_depth : int;
}

val record_interp : bktk_info Stack.t -> GText.mark -> GText.mark -> (int * string list * int) -> unit

val backtrack : bktk_info Stack.t -> (GText.mark -> bool) -> unit

type reset_mark

type undo_info = identifier list * int

val undo_info : unit -> undo_info

type reset_info = reset_mark * undo_info * bool ref

val compute_reset_info : unit -> reset_info
val reset_initial : unit -> unit
val reset_to : reset_mark -> unit
val reset_to_mod : identifier -> unit

val init : unit -> string list
val interp : bool -> string -> reset_info * (Util.loc * Vernacexpr.vernac_expr)
val interp_last : Util.loc * Vernacexpr.vernac_expr -> unit
val interp_and_replace : string ->
      (reset_info * (Util.loc * Vernacexpr.vernac_expr)) * string

(* type hyp = (identifier * constr option * constr) * string *)

type hyp = env * evar_map *
           ((identifier*string) * constr option * constr) * (string * string)
type meta = env * evar_map * string
type concl = env * evar_map * constr * string
type goal = hyp list * concl

val get_current_goals : unit -> goal list

val get_current_pm_goal : unit -> goal

val print_no_goal : unit -> string

val process_exn : exn -> string*(Util.loc option)

val hyp_menu : hyp -> (string * string) list
val concl_menu : concl -> (string * string) list

val is_in_coq_lib : string -> bool
val is_in_coq_path : string -> bool
val is_in_loadpath : string -> bool

val make_cases : string -> string list list


type tried_tactic =
  | Interrupted
  | Success of int (* nb of goals after *)
  | Failed

(* Message to display in lower status bar. *)

val current_status : unit -> string
