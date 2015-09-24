open Lwt.Infix

module Git =
  Rashell_Git

let list () =
  Git.hook_list ()
  >>= Lwt_list.iter_s Lwt_io.printl

let co () =
  Git.branch_checkout ~start:"master" "releng/v4.0.0"

let clone () =
  Git.clone ~workdir:"/Users/michael/obj" "git@github.com:michipili/rashell.git"

let () =
  Lwt_main.run(clone())
