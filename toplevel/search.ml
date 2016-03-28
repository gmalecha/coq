(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Pp
open Util
open Names
open Term
open Declarations
open Libobject
open Environ
open Pattern
open Printer
open Libnames
open Globnames
open Nametab
open Goptions
open CStream

type filter_function = global_reference -> env -> constr -> bool
type display_function = global_reference -> env -> constr -> unit

(* This option restricts the output of [SearchPattern ...],
[SearchAbout ...], etc. to the names of the symbols matching the
query, separated by a newline. This type of output is useful for
editors (like emacs), to generate a list of completion candidates
without having to parse thorugh the types of all symbols. *)

let search_output_name_only = ref false

let _ =
  declare_bool_option
    { optsync  = true;
      optdepr  = false;
      optname  = "output-name-only search";
      optkey   = ["Search";"Output";"Name";"Only"];
      optread  = (fun () -> !search_output_name_only);
      optwrite = (:=) search_output_name_only }

let stream_results = ref false

let _ =
  declare_bool_option
    { optsync  = true;
      optdepr  = false;
      optname  = "output-name-only search";
      optkey   = ["Search";"Stream"];
      optread  = (fun () -> !stream_results);
      optwrite = (:=) stream_results }

type glob_search_about_item =
  | GlobSearchSubPattern of constr_pattern
  | GlobSearchString of string

module SearchBlacklist =
  Goptions.MakeStringTable
    (struct
      let key = ["Search";"Blacklist"]
      let title = "Current search blacklist : "
      let member_message s b =
	str "Search blacklist does " ++ (if b then mt () else str "not ") ++ str "include " ++ str s
      let synchronous = true
     end)

(* The functions iter_constructors and iter_declarations implement the behavior
   needed for the Coq searching commands.
   These functions take as first argument the procedure
   that will be called to treat each entry.  This procedure receives the name
   of the object, the assumptions that will make it possible to print its type,
   and the constr term that represent its type. *)

let iter_constructors indsp u fn env nconstr =
  for i = 1 to nconstr do
    let typ = Inductiveops.type_of_constructor env ((indsp, i), u) in
    fn (ConstructRef (indsp, i)) env typ
  done

let stream_constructors (type a) indsp u (fn : _ -> _ -> _ -> a stream -> a stream) env nconstr after =
  let rec stream_constructors i =
    if i > nconstr then
      after
    else
      let typ = Inductiveops.type_of_constructor env ((indsp, i), u) in
      fn (ConstructRef (indsp, i)) env typ
        (stream_constructors (i+1))
  in
  stream_constructors 1

let iter_named_context_name_type f =
  let open Context.Named.Declaration in
  List.iter (fun decl -> f (get_id decl) (get_type decl))

(* General search over hypothesis of a goal *)
let iter_hypothesis glnum (fn : global_reference -> env -> constr -> unit) =
  let env = Global.env () in
  let iter_hyp idh typ = fn (VarRef idh) env typ in
  let evmap,e = Pfedit.get_goal_context glnum in
  let pfctxt = named_context e in
  iter_named_context_name_type iter_hyp pfctxt

let stream_named_context_name_type f x =
  let open Context.Named.Declaration in
  List.fold_right (fun decl acc ->
      f (get_id decl) (get_type decl) acc) x

let stream_hypothesis glnum (fn : global_reference -> env -> constr -> 'a stream -> 'a stream) =
  let env = Global.env () in
  let iter_hyp idh typ = fn (VarRef idh) env typ in
  let evmap,e = Pfedit.get_goal_context glnum in
  let pfctxt = named_context e in
  stream_named_context_name_type iter_hyp pfctxt


(* General search over declarations *)
let iter_declarations (fn : global_reference -> env -> constr -> unit) =
  let open Context.Named.Declaration in
  let env = Global.env () in
  let iter_obj (sp, kn) lobj = match object_tag lobj with
  | "VARIABLE" ->
    begin try
      let decl = Global.lookup_named (basename sp) in
      fn (VarRef (get_id decl)) env (get_type decl)
    with Not_found -> (* we are in a section *) () end
  | "CONSTANT" ->
    let cst = Global.constant_of_delta_kn kn in
    let gr = ConstRef cst in
    let typ = Global.type_of_global_unsafe gr in
      fn gr env typ
  | "INDUCTIVE" ->
    let mind = Global.mind_of_delta_kn kn in
    let mib = Global.lookup_mind mind in
    let iter_packet i mip =
      let ind = (mind, i) in
      let u = Declareops.inductive_instance mib in
      let i = (ind, u) in
      let typ = Inductiveops.type_of_inductive env i in
      let () = fn (IndRef ind) env typ in
      let len = Array.length mip.mind_user_lc in
      iter_constructors ind u fn env len
    in
    Array.iteri iter_packet mib.mind_packets
  | _ -> ()
  in
  try Declaremods.iter_all_segments iter_obj
  with Not_found -> ()

let stream_flat_map_c (type a) (type b)
    (f : a -> b stream -> b stream)
: a stream -> b stream =
  let open CStream in
  let rec stream_flat_map (s : a stream) : b stream =
    { stream_run =
        fun cons nil ->
          s.stream_run
            (fun x xs -> (f x (stream_flat_map xs)).stream_run cons nil)
            nil }
  in
  stream_flat_map

(* General search over declarations *)
let stream_declarations fn =
  let open Context.Named.Declaration in
  let env = Global.env () in
  let iter_obj (sp, kn) lobj after =
    match object_tag lobj with
  | "VARIABLE" -> begin
    match try Some (Global.lookup_named (basename sp))
          with Not_found -> None
    with
    | None -> after
    | Some decl -> fn (VarRef (get_id decl)) env (get_type decl) after
    end
  | "CONSTANT" ->
    let cst = Global.constant_of_delta_kn kn in
    let gr = ConstRef cst in
    let typ = Global.type_of_global_unsafe gr in
    fn gr env typ after
  | "INDUCTIVE" ->
    let mind = Global.mind_of_delta_kn kn in
    let mib = Global.lookup_mind mind in
    let iter_packet i mip after =
      let ind = (mind, i) in
      let u = Declareops.inductive_instance mib in
      let i = (ind, u) in
      let typ = Inductiveops.type_of_inductive env i in
      fn (IndRef ind) env typ
        (let len = Array.length mip.mind_user_lc in
         stream_constructors ind u fn env len after)
    in
    stream_of_array iter_packet mib.mind_packets after
  | _ -> after
  in
  Declaremods.stream_all_segments iter_obj stream_done

let generic_search glnumopt fn =
  (match glnumopt with
  | None -> ()
  | Some glnum -> iter_hypothesis glnum fn);
  iter_declarations fn

let generic_search_stream glnumopt fn =
  let hyps = match glnumopt with
    | None -> fun x -> x
    | Some glnum ->
      stream_hypothesis glnum fn
  in
  hyps (stream_declarations fn)


(** Standard display *)
let plain_display accu ref env c =
  let pr = pr_global ref in
  if !search_output_name_only then
    accu := pr :: !accu
  else begin
    let pc = pr_lconstr_env env Evd.empty c in
    accu := hov 2 (pr ++ str":" ++ spc () ++ pc) :: !accu
  end

let format_display l = prlist_with_sep fnl (fun x -> x) (List.rev l)

(** Filters *)

(** This function tries to see whether the conclusion matches a pattern. *)
(** FIXME: this is quite dummy, we may find a more efficient algorithm. *)
let rec pattern_filter pat ref env typ =
  let typ = strip_outer_cast typ in
  if Constr_matching.is_matching env Evd.empty pat typ then true
  else match kind_of_term typ with
  | Prod (_, _, typ)
  | LetIn (_, _, _, typ) -> pattern_filter pat ref env typ
  | _ -> false

let rec head_filter pat ref env typ =
  let typ = strip_outer_cast typ in
  if Constr_matching.is_matching_head env Evd.empty pat typ then true
  else match kind_of_term typ with
  | Prod (_, _, typ)
  | LetIn (_, _, _, typ) -> head_filter pat ref env typ
  | _ -> false

let full_name_of_reference ref =
  let (dir,id) = repr_path (path_of_global ref) in
  DirPath.to_string dir ^ "." ^ Id.to_string id

(** Whether a reference is blacklisted *)
let blacklist_filter ref env typ =
  let l = SearchBlacklist.elements () in
  let name = full_name_of_reference ref in
  let is_not_bl str = not (String.string_contains ~where:name ~what:str) in
  List.for_all is_not_bl l

let module_filter (mods, outside) ref env typ =
  let sp = path_of_global ref in
  let sl = dirpath sp in
  let is_outside md = not (is_dirpath_prefix_of md sl) in
  let is_inside md = is_dirpath_prefix_of md sl in
  if outside then List.for_all is_outside mods
  else List.is_empty mods || List.exists is_inside mods

let name_of_reference ref = Id.to_string (basename_of_global ref)

let search_about_filter query gr env typ = match query with
| GlobSearchSubPattern pat ->
  Constr_matching.is_matching_appsubterm ~closed:false env Evd.empty pat typ
| GlobSearchString s ->
  String.string_contains ~where:(name_of_reference gr) ~what:s


(** SearchPattern *)

let search_pattern gopt pat mods =
  let ans = ref [] in
  let filter ref env typ =
    let f_module = module_filter mods ref env typ in
    let f_blacklist = blacklist_filter ref env typ in
    let f_pattern () = pattern_filter pat ref env typ in
    f_module && f_pattern () && f_blacklist
  in
  let iter ref env typ =
    if filter ref env typ then plain_display ans ref env typ
  in
  let () = generic_search gopt iter in
  format_display !ans

(** SearchRewrite *)

let eq = Coqlib.glob_eq

let rewrite_pat1 pat =
  PApp (PRef eq, [| PMeta None; pat; PMeta None |])

let rewrite_pat2 pat =
  PApp (PRef eq, [| PMeta None; PMeta None; pat |])

let search_rewrite gopt pat mods =
  let pat1 = rewrite_pat1 pat in
  let pat2 = rewrite_pat2 pat in
  let ans = ref [] in
  let filter ref env typ =
    let f_module = module_filter mods ref env typ in
    let f_blacklist = blacklist_filter ref env typ in
    let f_pattern () =
      pattern_filter pat1 ref env typ ||
      pattern_filter pat2 ref env typ
    in
    f_module && f_pattern () && f_blacklist
  in
  let iter ref env typ =
    if filter ref env typ then plain_display ans ref env typ
  in
  let () = generic_search gopt iter in
  format_display !ans

(** Search *)

let search_by_head gopt pat mods =
  let ans = ref [] in
  let filter ref env typ =
    let f_module = module_filter mods ref env typ in
    let f_blacklist = blacklist_filter ref env typ in
    let f_pattern () = head_filter pat ref env typ in
    f_module && f_pattern () && f_blacklist
  in
  let iter ref env typ =
    if filter ref env typ then plain_display ans ref env typ
  in
  let () = generic_search gopt iter in
  format_display !ans

(** SearchAbout *)

(*
let search_about gopt items mods =
  let ans = ref [] in
  let filter ref env typ =
    let eqb b1 b2 = if b1 then b2 else not b2 in
    let f_module = module_filter mods ref env typ in
    let f_about (b, i) = eqb b (search_about_filter i ref env typ) in
    let f_blacklist = blacklist_filter ref env typ in
    f_module && List.for_all f_about items && f_blacklist
  in
  let iter ref env typ =
    if filter ref env typ then plain_display ans ref env typ
  in
  let () = generic_search gopt iter in
  format_display !ans
  *)
type search_constraint =
  | Name_Pattern of Str.regexp
  | Type_Pattern of constr_pattern
  | SubType_Pattern of constr_pattern
  | In_Module of Names.DirPath.t
  | Include_Blacklist
  | Not of search_constraint
  | And of search_constraint * search_constraint
  | Or of search_constraint * search_constraint
  | True
  | False

let string_list_to_dirpath m =
  let path = String.concat "." m in
  let m = Pcoq.parse_string Pcoq.Constr.global path in
  let (_, qid) = Libnames.qualid_of_reference m in
  try Nametab.full_name_module qid
  with Not_found ->
    Errors.errorlabstrm "Search.interface_search"
      (str "Module " ++ str path ++ str " not found.")

type 'a coq_object = {
  coq_object_prefix : string list;
  coq_object_qualid : string list;
  coq_object_object : global_reference;
  coq_object_extra : 'a;
}

let opt_search =
  let opt_and = function False , _ | _ , False -> False
                       | True , x  | x , True -> x
                       | x , y -> And (x,y)
  in
  let opt_or = function True , _ | _ , True -> True
                      | False , x | x , False -> x
                      | x , y -> Or (x,y)
  in
  let opt_not = function True -> False
                       | False -> True
                       | Not x -> x
                       | x -> Not x
  in
  let rec opt_search = function
      And (a,b) -> opt_and (opt_search a,opt_search b)
    | Or (a,b) -> opt_or (opt_search a,opt_search b)
    | Not a -> opt_not (opt_search a)
    | x -> x
  in
  opt_search

let toggle b s =
  if b then Not s else s
let rec all_of = function
    [] -> True
  | [x] -> x
  | x :: xs -> And (x, all_of xs)
let rec any_of = function
    [] -> False
  | [x] -> x
  | x :: xs -> Or (x, any_of xs)

let string_to_constr_pattern ?env s =
  let env =
    match env with
      None -> Global.env ()
    | Some e -> e
  in
  let constr = Pcoq.parse_string Pcoq.Constr.lconstr_pattern s in
  let (_, pat) = Constrintern.intern_constr_pattern env constr in
  pat

let rec compile = function
  | Not x ->
    let f = compile x in
    fun gr e c -> not (f gr e c)
  | And (a,b) ->
    let f = compile a in
    let g = compile b in
    fun gr e c -> f gr e c && g gr e c
  | Or (a,b) ->
    let f = compile a in
    let g = compile b in
    fun gr e c -> f gr e c || g gr e c
  | True -> fun _ _ _ -> true
  | False -> fun _ _ _ -> false
  | Name_Pattern re ->
    fun gr e c ->
      let id = Names.Id.to_string (Nametab.basename_of_global gr) in
      Str.string_match re id 0
  | Type_Pattern pat ->
    fun gr env constr -> Constr_matching.is_matching env Evd.empty pat constr
  | SubType_Pattern pat ->
    fun gr env constr ->
      Constr_matching.is_matching_appsubterm ~closed:false
        env Evd.empty pat constr
  | In_Module mdl ->
    fun gr env constr ->
      let path = Libnames.dirpath (Nametab.path_of_global gr) in
      Libnames.is_dirpath_prefix_of mdl path
  | Include_Blacklist ->
    blacklist_filter

let print_function gref env constr : constr coq_object =
  let fullpath = DirPath.repr (Nametab.dirpath_of_global gref) in
  let qualid = Nametab.shortest_qualid_of_global Id.Set.empty gref in
  let (shortpath, basename) = Libnames.repr_qualid qualid in
  let shortpath = DirPath.repr shortpath in
  (* [shortpath] is a suffix of [fullpath] and we're looking for the missing
     prefix *)
  let rec prefix full short accu = match full, short with
    | _, [] ->
      let full = List.rev_map Id.to_string full in
      (full, accu)
    | _ :: full, m :: short ->
      prefix full short (Id.to_string m :: accu)
    | _ -> assert false
  in
  let (prefix, qualid) = prefix fullpath shortpath [Id.to_string basename] in
  { coq_object_prefix = prefix;
    coq_object_qualid = qualid;
    coq_object_object = gref;
    coq_object_extra  = constr (* string_of_ppcmds (pr_lconstr_env env Evd.empty constr) *)
  }

let interface_search_stream (filter_function : filter_function)
: (constr coq_object) stream =
  generic_search_stream None
    (fun gl e c after ->
       if filter_function gl e c then
         stream_yield_then (print_function gl e c) after
       else after)

let interface_search (filter_function : filter_function) =
  let ans = ref [] in
  let print_function ref env constr =
    let fullpath = DirPath.repr (Nametab.dirpath_of_global ref) in
    let qualid = Nametab.shortest_qualid_of_global Id.Set.empty ref in
    let (shortpath, basename) = Libnames.repr_qualid qualid in
    let shortpath = DirPath.repr shortpath in
    (* [shortpath] is a suffix of [fullpath] and we're looking for the missing
       prefix *)
    let rec prefix full short accu = match full, short with
    | _, [] ->
      let full = List.rev_map Id.to_string full in
      (full, accu)
    | _ :: full, m :: short ->
      prefix full short (Id.to_string m :: accu)
    | _ -> assert false
    in
    let (prefix, qualid) = prefix fullpath shortpath [Id.to_string basename] in
    let answer = {
      coq_object_prefix = prefix;
      coq_object_qualid = qualid;
      coq_object_object = ref;
      coq_object_extra  = string_of_ppcmds (pr_lconstr_env env Evd.empty constr);
    } in
    ans := answer :: !ans;
  in
  let iter ref env typ =
    if filter_function ref env typ then print_function ref env typ
  in
  let () = generic_search None iter in (* TODO: chose a goal number? *)
  !ans

let plain_display_simple ref env c =
  let pr = pr_global ref in
  if !search_output_name_only then
    pr
  else begin
    let pc = pr_lconstr_env env Evd.empty c in
    hov 2 (pr ++ str":" ++ spc () ++ pc)
  end

let format_display_stream
    (x : _ stream) : Pp.std_ppcmds =
  let rec format_display_stream acc xs =
    xs.stream_run
      (fun x xs -> format_display_stream (fun z -> Pp.fnl() ++ x ++ acc z) xs)
      (fun _ -> acc Pp.mt)
  in
  x.stream_run
    (fun x xs -> format_display_stream (fun z -> x ++ z ()) xs)
    (fun _ -> Pp.mt ())

let search_about_stream gopt items mods =
  let filter ref env typ =
    let eqb b1 b2 = if b1 then b2 else not b2 in
    let f_module = module_filter mods ref env typ in
    let f_about (b, i) = eqb b (search_about_filter i ref env typ) in
    let f_blacklist = blacklist_filter ref env typ in
    if f_module && List.for_all f_about items && f_blacklist then
      stream_yield_then (plain_display_simple ref env typ)
    else
      fun x -> x
  in
  format_display_stream
    (generic_search_stream gopt filter)

let search_about_nostream gopt items mods =
  let ans = ref [] in
  let filter ref env typ =
    let eqb b1 b2 = if b1 then b2 else not b2 in
    let f_module = module_filter mods ref env typ in
    let f_about (b, i) = eqb b (search_about_filter i ref env typ) in
    let f_blacklist = blacklist_filter ref env typ in
    f_module && List.for_all f_about items && f_blacklist
  in
  let iter ref env typ =
    if filter ref env typ then plain_display ans ref env typ
  in
  let () = generic_search gopt iter in
  format_display !ans

let search_about gopt items mods =
  if !stream_results then
    search_about_stream gopt items mods
  else
    search_about_nostream gopt items mods
