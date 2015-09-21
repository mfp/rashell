(* Rashell_Git -- Git Commands

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)

(** Interface to git.

    These commands all assume that the current working directory is a git
    repository. *)

val topleveldir : ?workdir:string -> unit -> string Lwt.t
(** Show the absolute path of the top-level directory. *)


(** {6 Install, list and run hooks} *)

val hook_list : ?workdir:string -> unit -> string list Lwt.t
(** The list of installed hooks.  This is the list of absolute paths
    to these hooks. *)

val hook_install : ?workdir:string -> ?force:bool -> string -> string -> unit Lwt.t
(** [hook_install name program] install the given [program] as a hook
    called [name], through a symlink. *)

val hook_install_script : ?workdir:string -> ?force:bool -> ?perm:Unix.file_perm -> string -> string -> unit Lwt.t
(** [hook_install_script name script] install a new hook called [name]
    containing the given [script].

    @param perm defaults to [0o700]. *)

val hook_run : ?workdir:string -> ?important:bool -> string * (string array) -> unit Lwt.t
(** [hook_run (cmd, argv)] execute the given hook, if it exists.

    The hook is always executed from the top-level directory of the
    repository.

    @param important If set, the absence of the hook is reported as an
    error. *)

val branch_checkout : ?workdir:string -> ?create:bool -> ?start:string -> string -> unit Lwt.t
(** [branch_checkout branch] check out the given [branch].

    @param create If set, the branch is created, as for the [-B] flag.
    @param start If set, use this starting point instead of [HEAD]. *)

val clone :
  ?workdir:string ->
  ?template:string ->
  ?bare:bool ->
  ?mirror:bool ->
  ?origin:string ->
  ?branch:string ->
  ?config:(string * string) list ->
  ?depth:int ->
  ?single_branch:bool ->
  ?recursive:bool ->
  ?destdir:string ->
  string -> unit Lwt.t
(** Wrapper for the clone command. *)
