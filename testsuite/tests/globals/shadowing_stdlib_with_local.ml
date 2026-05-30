(*
   test
  (targets amd64)
  (run (exit 0))
*)

(* exit is defined in stdlib *)

let foo () =
  let exit _ = () in
  exit 1

let main = 
  let () = foo () in
  exit 0
