(* Rashell_Git -- Git Commands

   Rashell (https://github.com/michipili/rashell)
   This file is part of Rashell

   Copyright © 2015 Michael Grünewald

   This file must be used under the terms of the CeCILL-B.
   This source file is licensed as described in the file COPYING, which
   you should have received as part of this distribution. The terms
   are also available at
   http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.txt *)
open Lwt.Infix
open Rashell_Posix
open Rashell_Configuration
open Rashell_Command

module Maybe =
  Lemonade_Maybe

let ( / ) =
  Filename.concat

let ( $ ) f x =
  f x

let flag f option = match option with
  | true -> [| f |]
  | false -> [| |]

let option option moptarg = match moptarg with
  | Some(optarg) -> [| option; optarg |]
  | None -> [| |]

let maybe_transform f = function
  | None -> [| |]
  | Some(x) -> f(x)

let topleveldir ?workdir () =
  exec_utility ~chomp:true
    (command ?workdir ("", [| ac_path_git; "rev-parse"; "--show-toplevel" |]))

let _hook_dir_from_topleveldir dir =
  dir / ".git" / "hooks"

let _hook_dir ?workdir () =
  Lwt.map _hook_dir_from_topleveldir(topleveldir ?workdir ())

let hook_list ?workdir () =
  let predicate =
    And[
      Or[
        Has_kind S_REG;
        Has_kind S_LNK;
      ];
      Not(Name("*.*"))
    ]
  in
  let%lwt dir = _hook_dir ?workdir () in
  Lwt_stream.to_list (find predicate [ dir ])

let hook_install ?workdir ?force name program =
  let%lwt dir = _hook_dir ?workdir () in
  Lwt_stream.to_list (ln ?force ~symbolic:true [ program ] (dir / name))
  |> Lwt.map ignore

let hook_install_script ?workdir ?(force = false) ?(perm = 0o700) name script =
  let%lwt dir = _hook_dir ?workdir () in
  Lwt_io.with_file
    ~flags:Unix.([O_WRONLY; O_CREAT; O_TRUNC] @(if force then [] else [ O_EXCL ]))
    ~perm
    ~mode:Lwt_io.output
    (dir / name) (fun ch -> Lwt_io.write ch script)

let hook_run ?workdir ?(important = false) (cmd, argv) =
  let%lwt dir = topleveldir ?workdir () in
  let hookdir = _hook_dir_from_topleveldir dir in
  

let branch_checkout ?workdir ?(create = false) ?start branch =
  let reallycreate = match create, start with
    | _, Some(_) -> true
    | true, None -> true
    | false, None -> false
  in
  let argv =
    Array.concat [
      [| ac_path_git; "checkout"; |];
      (flag "-B" reallycreate);
      [| branch |];
      (maybe_transform (fun x -> [| x |]) start)
    ]
  in
  exec_utility
    (command ?workdir ("",  argv))
  >>= fun _ -> Lwt.return_unit


let clone ?workdir ?template ?(bare = false) ?(mirror = false) ?origin ?branch ?(config = []) ?depth ?(single_branch = false) ?(recursive = false) ?destdir repository =
  let open Printf in
  let configuration =
    Array.concat $ List.map (fun (k,v) -> [| "--config"; sprintf "%s=%s" k v; |]) config
  in
  let argv =
    Array.concat [
      [| ac_path_git; "clone"; |];
      (option "--template" template);
      (flag "--bare" bare);
      (flag "--mirror" mirror);
      (option "--origin" origin);
      (option "--branch" branch);
      configuration;
      (option "--depth" (Maybe.map string_of_int depth));
      (flag "--single-branch" single_branch);
      (flag "--recursive" recursive);
      [| repository |];
      (match destdir with
       | Some(x) -> [| x |]
       | None -> [| |]);
    ]
  in
  exec_utility
    (command ?workdir ("",  argv))
  >>= fun _ -> Lwt.return_unit
