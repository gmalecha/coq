(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Pp
open Names
open Term
open Environ
open Pattern
open Globnames
open CStream

(** {6 Search facilities. } *)

type glob_search_about_item =
  | GlobSearchSubPattern of constr_pattern
  | GlobSearchString of string

type filter_function = global_reference -> env -> constr -> bool
type display_function = global_reference -> env -> constr -> unit

(** {6 Generic filter functions} *)

val blacklist_filter : filter_function
(** Check whether a reference is blacklisted. *)

val module_filter : DirPath.t list * bool -> filter_function
(** Check whether a reference pertains or not to a set of modules *)

val search_about_filter : glob_search_about_item -> filter_function
(** Check whether a reference matches a SearchAbout query. *)

(** {6 Specialized search functions}

[search_xxx gl pattern modinout] searches the hypothesis of the [gl]th
goal and the global environment for things matching [pattern] and
satisfying module exclude/include clauses of [modinout]. *)

val search_by_head : int option -> constr_pattern -> DirPath.t list * bool -> std_ppcmds
val search_rewrite : int option -> constr_pattern -> DirPath.t list * bool -> std_ppcmds
val search_pattern : int option -> constr_pattern -> DirPath.t list * bool -> std_ppcmds
val search_about   : int option -> (bool * glob_search_about_item) list
  -> DirPath.t list * bool -> std_ppcmds

val string_to_constr_pattern : ?env:env -> string -> constr_pattern

val string_list_to_dirpath : string list -> Names.DirPath.t

type search_constraint =
  (** Whether the name satisfies a regexp *)
  | Name_Pattern of Str.regexp
  (** Whether the object type satisfies a pattern *)
  | Type_Pattern of constr_pattern
  (** Whether some subtype of object type satisfies a pattern *)
  | SubType_Pattern of constr_pattern
  (** Whether the object pertains to a module *)
  | In_Module of Names.DirPath.t
  (** Bypass the Search blacklist *)
  | Include_Blacklist
  (** Combinators **)
  | Not of search_constraint
  | And of search_constraint * search_constraint
  | Or of search_constraint * search_constraint
  | True
  | False

val all_of : search_constraint list -> search_constraint
val any_of : search_constraint list -> search_constraint
val toggle : bool -> search_constraint -> search_constraint

type 'a coq_object = {
  coq_object_prefix : string list;
  coq_object_qualid : string list;
  coq_object_object : global_reference;
  coq_object_extra  : 'a
}

val compile : search_constraint -> filter_function

val interface_search : filter_function ->
  (string coq_object) list

val interface_search_stream : filter_function ->
  (constr coq_object) stream


(** {6 Generic search function} *)

val generic_search : int option -> display_function -> unit
val generic_search_stream : int option ->
  (global_reference -> env -> constr -> 'b stream -> 'b stream) ->
  'b stream
(** This function iterates over all hypothesis of the goal numbered
    [glnum] (if present) and all known declarations. *)
