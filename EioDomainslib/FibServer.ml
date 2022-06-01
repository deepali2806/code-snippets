open Eio_linux
open Eio.Std
let num_domains = try int_of_string Sys.argv.(1) with _ -> 2
let n = try int_of_string Sys.argv.(2) with _ -> 41
module T = Domainslib.Task

(* This is not the actual server implementation. It just takes input n and sends it to domainslib task(That is what we have to do in server implementation). 
Observation : Since we are using same task pool, somewhow domainslib tasks are serialized. 
*)

let rec fib n =
  if n < 2 then 1
  else fib (n-1) + fib (n-2)

let rec fib_par pool n =
  if n <= 40 then fib n
  else
    let a = T.async pool (fun _ -> fib_par pool (n-1)) in
    let b = T.async pool (fun _ -> fib_par pool (n-2)) in
    T.await pool a + T.await pool b

let m = Array.make 5 (Eio_domainslib_interface.MVar.create_empty ());;

let main =
  let pool = T.setup_pool ~num_additional_domains:(num_domains - 1) () in

    run @@ fun _env ->
      (* let clock = Eio.Stdenv.clock _env in (); *)
    T.run pool (fun () -> 

   Switch.run @@ fun sw ->
    ( 
         
            Fiber.fork ~sw (
                fun () ->
                        for i = 0 to 4 do
                            Fiber.fork ~sw (
                            fun () -> 
                                let _ = T.async pool (
                                fun _ ->
                                Printf.printf "\nDomains %d fibpar starting %!" i;
                                let v = fib_par pool (n+i) in
                                let _ = Eio_domainslib_interface.MVar.put v (m.(i)) in
                                Printf.printf "\nDomainslib %d Ends %!" i
                            ) in
                                traceln "\nBefore mvar take%!";
                                let p = Eio_domainslib_interface.MVar.take (m.(i)) in
                                traceln "\nEio Fibre %d Ends Ans %d: %d%!" i (n+i) p
                            )
                        done
                    
            );       
            
        );

    );

    T.teardown_pool pool
