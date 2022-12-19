(* ocamlfind ocamlopt -package lwt,lwt.unix,lwt_ppx,eio_domainslib_interface,domainslib  -linkpkg -o checkMVar checkMVar.ml 
 *)
 open Printf
 open Base_mvar_interface
 (* open Effect *)
 (* open Effect.Deep *)
 
 (* open Lwt.Infix *)

 module Final_monad = Monad.Make_monad (struct 
 type 'a t = 'a Lwt.t
 let return = Lwt.return
 end
)
let m = MVar.create_empty ()
 
(* let mn = Lwt_mvar.create_empty () *)

let main () =
   Lwt_main.run ( 
     let p = Lwt.bind
      (let v = (Final_monad.take_monad m) in v) 
      (* (let v = Lwt_mvar.take mn in v) *)
                      (fun v ->  printf "\nLwt: Hello %d%!" v; Lwt.return ())
              in p
   )
     (* printf "\nBoth the domains are done completed%!" *)
 
 let  _ = main ()
