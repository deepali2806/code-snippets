open Printf
(* open Effect *)
open Effect.Deep

(* open Fiber *)
(* open Affect *)
(* open Eio.Std *)

exception Abort_take of string

exception Cancel

let m = Eio_domainslib_interface.MVar.create_empty ();;

let main () =
  let comp () =

    Eio_linux.run @@ fun env ->
    ignore (Fiber.run (fun () ->  
    Eio.Std.Switch.run @@ fun _ ->
    (       
           
            let main ~domain_mgr =  
            Eio.Fiber.both (
                fun () ->
                        Eio.Domain_manager.run domain_mgr 
                        (
                            fun () -> printf "\nDomain 1 Eio Fiber started ";
                        (* Fiber.yield(); *)
                                (* let v = 52 in *)
                                let v = Eio_domainslib_interface.MVar.take m in
                                printf "Eio Fiber 1 ends %d" v
                        );
                        
            )
            (   fun () ->
                (* Eio.Domain_manager.run domain_mgr (
                    fun () -> *)
                   (try      
                   ignore (Fiber.run (fun () ->  
                                    let _ = (Fiber.spawn 
                                    ( fun () -> 
                                        Printf.printf "\nReaching here inside Affect libray";
                                        printf "\nInside Second fibre ";
                                        let v =  42 in
                                        Eio_domainslib_interface.MVar.put v m;
                                        Printf.printf "\nTake value "
                                        )
                                    ) in ();

                   ))
                        with 
                        | _ -> printf "\nError happened in calling Affect function from Eio");
                        printf "\nFrom Domain 2"
                (* ) *)
            )
            in 
            main ~domain_mgr:(Eio.Stdenv.domain_mgr env);      

            (* let _ = (Fiber.spawn 
                                    ( fun () -> 
                                        Printf.printf "\nReaching here inside Affect libray";
                                        printf "\nInside Second fibre ";
                                        let v =  42 in
                                        Eio_domainslib_interface.MVar.put v m;
                                        Printf.printf "\nTake value "
                                        )
                                    ) in (); *)
            
        )
      )
    )
    in
      match_with comp ()
      { retc = (fun () -> ());
        exnc = (function
          | Exit ->  Printf.printf "\nReached at Exit exception";()
          | e -> Printf.printf "\nReaching here in exception\n";
          Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()) 
          );
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Eio_domainslib_interface.Sched.Stuck -> Some (fun (k : (a,_) continuation) -> Printf.printf "\nReached at stuck";
              discontinue k Exit
              )
          | _ -> None }

let _ = main ()

(* let flip = let () = Random.self_init () in Random.bool

let test_abort () =
  printf "\nTesting potential abort";
  let did_close = ref false in
  let sub () =
    let finally () = did_close := true in
    Fun.protect ~finally @@ fun () ->
    Fiber.yield ();
    printf "\nFiber finished";
  in
  let main () =
    let f = Fiber.spawn sub in
    Fiber.yield ();
    if flip () then (printf " Aborting fiber!"; Fiber.abort f)
  in
  ignore (Fiber.run main);
  assert (!did_close)

let _ = test_abort () *)