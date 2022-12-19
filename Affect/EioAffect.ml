open Printf
(* open Effect *)
(* open Effect.Deep *)

(* open Fiber *)
(* open Affect *)
(* open Eio.Std *)

exception Abort_take of string

exception Cancel

let m = Eio_domainslib_interface.MVar.create_empty ();;


let main () =
  let eio_domain =
    Domain.spawn ( fun _ ->
        Eio_linux.run @@ fun _env ->
            Eio.Std.Switch.run @@ fun sw ->
            (        
                    Eio.Fiber.fork ~sw (
                        fun () ->
                                printf "\nEio Fiber 1 started ";
                                let v = 52 in
                                Eio_domainslib_interface.MVar.put v m;
                                printf "Eio Fiber 1 ends"
                    ); 
                )
    ) in 

    ignore (Fiber.run (fun () -> 

        ignore (Fiber.spawn 
            ( fun () -> 
              Printf.printf "\nAffect 1: Reaching here inside Affect libray";
              printf "\nAffect 1: Inside First fibre ";
              let v = try Eio_domainslib_interface.MVar.take m with
                      | _ -> Printf.printf "\nError is happening in take function"; 0
              in
              Printf.printf "\nAffect 1: Take value %d" v
            ));

        ignore (Fiber.spawn 
            ( fun () -> 
              Printf.printf "\nAffect 2: Reaching here inside Affect libray";
              printf "\nAffect 2: Inside Second fibre ";
            ));

     ));

     let _ = Domain.join eio_domain in 
     Printf.printf "\nBoth the domains are done!"

let _ = main ()