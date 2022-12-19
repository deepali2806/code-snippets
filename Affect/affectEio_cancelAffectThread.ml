open Printf
open Effect.Deep

exception Cancel
exception Abort_take of string

(* let m = Eio_domainslib_interface.MVar.create_empty ();; *)

(* let main () =
let  comp () =
    Eio_linux.run ( fun _env ->
    ignore (Fiber.run (fun () ->
    Eio.Std.Switch.run @@ fun sw ->
    (   
        Eio.Fiber.fork ~sw (
            fun () -> 
                    printf "\nEio Fiber 1 started %!";
                    (* let v = Eio_domainslib_interface.MVar.take m in *)
                    let v = 42 in 
                    printf "\nEio Fibre 1 Ends %d%!" v
        );
        
        printf "\nREaching in between two threads%!";

       (try  let _ = Fiber.spawn (
            fun () -> 
                printf "\nTest: Affect Spawning new fibers%!"; 
                (* let v = 52 in
                Eio_domainslib_interface.MVar.put v m; *)
                printf "\nAffect : Fiber ended%!"
        ) in () with
        | _ -> printf "Error Happened in Fiber.spawn");


        Eio.Fiber.fork ~sw (
            fun () -> 
            (* let v = 52 in
                Eio_domainslib_interface.MVar.put v m; *)
            printf "\nSecond Eio Fiber just checking it %!"
        );
        


        (* let _ = Fiber.spawn (
            fun () -> 
                printf "\nTest:2 Affect Spawning new fibers%!"; 
                (* let v = 52 in
                Eio_domainslib_interface.MVar.put v m; *)
                printf "\nAffect2 : Fiber ended%!"
        ) in () *)

          
        (* printf "\nAffect Fiber finished%!"; *)

    )
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
          | _ -> Printf.printf "In Effect"; None }    

let _ = main () *)


let main () =
let  comp () =
    Eio_linux.run ( fun _env ->
    ignore (Fiber.run (fun () ->
    Eio.Std.Switch.run @@ fun sw ->
    (   
        Eio.Fiber.fork ~sw (
            fun () -> 
                    printf "\nEio Fiber 1 %!"
        );
        
        (* printf "\nReaching in between two threads%!";*)

        let _ = Fiber.spawn (
            fun () -> 
                printf "\nAffect Fiber 1 %!"
        ) in ();

        Eio.Fiber.fork ~sw (
            fun () -> 
            printf "\nEio Fiber 2 %!"
        );
    )
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
          | _ -> Printf.printf "In Effect"; None }    

let _ = main ()
