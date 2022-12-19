(* ocamlfind ocamlopt -package lwt,lwt.unix,lwt_ppx,eio_domainslib_interface,domainslib  -linkpkg -o testModule testModule.ml 
 *)
 open Printf
 open Base_mvar_interface
 (* open Effect
 open Effect.Deep *)
 
 (* open Lwt.Infix *)

(* 
 module Final_monad = MVar.Make_monad (struct 
 type 'a t = 'a Lwt.t
 let return = Lwt.return
 end
) *)

 let m = MVar.create_empty ()
 
 let main () =
   let lwt_join = Domain.spawn ( fun _ ->
    Lwt_main.run ( 
      let p = Lwt.bind
       (          
       let v = (MVar.take_monad m) in v) 
                 (fun _ -> printf "\nLwt: Hello1 %d%!" (Lwt.suspend_cnt ()); Lwt.return ())     
               in p
    );
     
   ) in
();
Unix.sleep 1;

MVar.put 42 m;
   (* Lwt_main.run (
    Unix.sleep 1;
    (* printf "\nLwt: Hello2 %d%!" (Lwt.suspend_cnt ()); *)
    MVar.put_monad 42 m >>= fun _ ->
    Lwt.return (Printf.printf "\nSuspend Count in the second Domain : %!" )
  ); *)


   
   let _ = Domain.join lwt_join in ();
   
     printf "\nBoth the domains are done completed%!"
 
 let  _ = main ()
